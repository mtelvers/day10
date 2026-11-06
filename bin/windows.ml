type t = {
  config : Config.t;
  network : string;
  username : string;
}

let hostname = "builder"

(* An ordinary user, as on the other platforms, and one without the right to
   create symlinks: a Windows container cannot run a relative symlink that
   sits on a bind mount, and opam's root is one.  OCaml's configure makes
   opt/bin/flexlink.exe a copy instead of a link when the user cannot create
   native symlinks, which is what keeps the compiler buildable from 5.4 on.
   See windows-symlink-exec.md. *)
let username = "opam"

(* opam's root in the container, which is the directory the cache keeps: the
   switch, the repository, opam's own Cygwin and its package database -- and,
   so that a layer build has nothing to download, opam.exe and the driver. *)
let opam_root = Path.(({|C:\Users\|} ^ username) / "AppData" / "Local" / "opam")

(* The top layer of the base image, as ctr snapshot ls lists it. *)
let image = "sha256:5ae66e790cc84572a3bb9646fcbd13b3dbf1af9252e013167791737880626b0b"

(* What every container is prepared on: the image with the user added.  A
   user lives in the container's own registry, which no layer keeps, so the
   base build commits a snapshot of its own for the user. *)
let user_snapshot = "day10-" ^ username

let snapshot_exists key = fst (Os.spawn ~stdout:Os.null ~stderr:Os.null "ctr" [ "snapshot"; "info"; key ]) = 0

(* The user is not known to the host, so a directory it is to write has to
   grant the Users group, which it is in on both sides; what is created under
   the directory afterwards, from either side, inherits the grant. *)
let grant_users dir = Os.quiet "icacls" [ dir; "/grant"; "*S-1-5-32-545:(OI)(CI)F" ]

(* The newest opam whose root the opam libraries in day10-install still read:
   2.6 bumped the root format, 2.3 to 2.5 write the one 2.2 introduced. *)
let opam_url ~arch = "https://github.com/ocaml/opam/releases/download/2.5.2/opam-2.5.2-" ^ arch ^ "-windows.exe"

let day10_install_url ~arch = "https://github.com/mtelvers/day10-install/releases/download/1.4.0/day10-install-1.4.0-" ^ arch ^ "-windows.exe"

(* Added to the container's own environment rather than replacing it, so the
   system variables Windows needs stay. *)
let env ~(config : Config.t) =
  [
    ("CI", "true");
    ("OPAMROOT", opam_root);
    ("OPAMYES", "1");
    (* opam runs pacman only at this level; its own Cygwin it installs into
       without asking. *)
    ("OPAMCONFIRMLEVEL", "unsafe-yes");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
    ("OPAMJOBS", string_of_int (Option.value ~default:Os.default_jobs config.opam_jobs));
  ]

let strings xs = `List (List.map (fun x -> `String x) xs)

let make_config_json ~layers ~cwd ~argv ~hostname ~username ~env ~mounts ~network : Yojson.Safe.t =
  `Assoc
    [
      ("ociVersion", `String "1.1.0");
      ( "process",
        `Assoc
          [
            ("terminal", `Bool false);
            ("user", `Assoc [ ("username", `String username) ]);
            ("args", strings argv);
            ("env", strings (List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) env));
            ("cwd", `String cwd);
          ] );
      ("root", `Assoc [ ("path", `String ""); ("readonly", `Bool false) ]);
      ("hostname", `String hostname);
      ("mounts", `List (Mount.user_mounts mounts));
      ( "windows",
        `Assoc
          [
            ("layerFolders", strings layers);
            ("ignoreFlushesDuringBoot", `Bool true);
            ("network", `Assoc [ ("allowUnqualifiedDNSQuery", `Bool true); ("networkNamespace", `String network) ]);
          ] );
    ]

let init ~(config : Config.t) = { config; network = Os.capture "hcn-namespace" [ "create" ] |> String.trim; username }
let deinit ~t = ignore (Os.exec [ "hcn-namespace"; "delete"; t.network ])
let config ~t = t.config

(* opam's uname is Unix only; its arch poll asks Windows itself. *)
let arch () = OpamSysPoll.arch OpamVariable.Map.empty |> Option.value ~default:"x86_64"

(* The layers are the user's own, so nothing more than unlink is needed. *)
let remove = List.iter (Os.rm ~recursive:true)

let temp_dir path () = if Sys.file_exists path then try Os.rm ~recursive:true path with Unix.Unix_error _ | Sys_error _ -> ()

(* Windows has no fork, so one at a time. *)
let fork ?np:_ f xs = List.iter f xs

(* The snapshot a container is prepared on, let go of when the run ends: see
   Cleanup. *)
let snapshot name () = Os.quiet "ctr" [ "snapshot"; "rm"; name ]

(* A snapshot named [name] prepared on [parent], the OCI config written for it
   in [temp_dir], and ctr run on that.  The snapshot outlives the run: what
   becomes of it is the caller's. *)
let prepare_and_run ~t ~parent ~username ~name ~temp_dir ~build_log ~tee ~commands ~mounts =
  let mounts_json = Path.(temp_dir / "mounts.json") in
  let _ = Os.retry_exec ~stdout:mounts_json [ "ctr"; "snapshot"; "prepare"; "--mounts"; name; parent ] in
  let layers = Json_layers.read_layers mounts_json in
  let argv = [ "cmd"; "/c"; String.concat " && " commands ] in
  let config = make_config_json ~layers ~cwd:{|C:\|} ~argv ~hostname ~username ~env:(env ~config:t.config) ~mounts ~network:t.network in
  let config_json = Path.(temp_dir / "config.json") in
  let () = Os.write_to_file config_json (Yojson.Safe.pretty_to_string config) in
  Os.exec ~stdout:build_log ~stderr:build_log ~tee [ "ctr"; "run"; "--cni"; "--rm"; "--config"; config_json; name ]

(* One container for a layer: prepared on the user's snapshot, let go of when
   the run ends. *)
let container ~t ~temp_dir ~build_log ~tee ~commands ~mounts =
  let name = Filename.basename temp_dir in
  Cleanup.with_resource (snapshot name) @@ fun () ->
  prepare_and_run ~t ~parent:user_snapshot ~username:t.username ~name ~temp_dir ~build_log ~tee ~commands ~mounts

(* The user's snapshot, made when there is none: the image's administrator
   adds the user, and the snapshot is committed under the fixed name.  The
   commit fails when another day10 has committed one meanwhile, and the
   active snapshot is removed either way. *)
let add_user ~t ~temp_dir ~build_log =
  if not (snapshot_exists user_snapshot) then (
    let dir = Path.(temp_dir / "user") in
    let () = Os.mkdir dir in
    let name = Filename.basename temp_dir ^ "-user" in
    let result =
      prepare_and_run ~t ~parent:image ~username:"ContainerAdministrator" ~name ~temp_dir:dir ~build_log ~tee:t.config.log
        ~commands:[ "net user " ^ username ^ " /add" ]
        ~mounts:[]
    in
    let committed = result = 0 && Os.exec [ "ctr"; "snapshot"; "commit"; user_snapshot; name ] = 0 in
    let () = if not committed then Os.quiet "ctr" [ "snapshot"; "rm"; name ] in
    if result <> 0 then failwith (Printf.sprintf "Adding the %s user to the image failed with status %d" username result))

(* The Unix layer under opam, as the distribution names it: cygwin, which
   opam sees for win32, or msys2. *)
let unix_layer ~(config : Config.t) : (module S.UNIX_LAYER) =
  match config.os_distribution with
  | "msys2" -> (module Msys2)
  | _ -> (module Cygwin)

(* What a container leaves in opam's root that is not part of the layer: locks,
   the environment and repository caches opam rebuilds, and what the Unix
   layer keeps elsewhere. *)
let tidy ~config root =
  let module L = (val unix_layer ~config) in
  List.iter Os.rm
    ([
       Path.(root / "lock");
       Path.(root / "conf.lock");
       Path.(root / "default" / ".opam-switch" / "lock");
       Path.(root / "default" / ".opam-switch" / "environment");
       Path.(root / "default" / ".opam-switch" / "packages" / "cache");
       Path.(root / "repo" / "conf.lock");
     ]
    @ L.discard root
    @ Os.ls ~extn:".cache" Path.(root / "repo"))

(* A build never runs on a bind mount: a Windows container cannot run a
   program through a relative symlink on one, nor remove or replace a file in
   use there, which pacman and gpg both do.  Every mount is a way in or out.
   The container copies the layers it is given onto its own drive, builds
   there, and copies out what the build wrote, which the archive attribute
   marks once it has been cleared from everything copied in.  See
   windows-symlink-exec.md. *)
let control_dst = {|C:\day10ctl|}
let layers_dst = {|C:\day10cache|}
let repo_dst = {|C:\day10repo|}
let out_dst = {|C:\day10out|}

let ro src dst = { Mount.ty = "bind"; src; dst; options = [ "rbind"; "rprivate"; "ro" ] }
let rw src dst = { Mount.ty = "bind"; src; dst; options = [ "rw"; "rbind"; "rprivate" ] }

(* robocopy's status is a bit mask in which 8 and above is a failure. *)
let robocopy src dst flags = String.concat " " ([ "robocopy"; src; dst ] @ flags @ [ "/R:0"; "/W:0"; "/MT:8"; "/NFL"; "/NDL"; "/NJH"; "/NJS"; "/NP" ]) ^ "\r\nif errorlevel 8 exit /b 1"

(* Skipping what is already there, so that copying the layers top first
   leaves each path as the topmost layer has it, as Os.copy_tree does.
   /SL copies a symlink as a link. *)
let copy_in = [ "/E"; "/XC"; "/XN"; "/XO"; "/SL"; "/COPY:DAT"; "/DCOPY:T" ]

let clear_archive =
  {|param([string]$root)
function Walk([IO.DirectoryInfo]$d) {
  foreach ($e in $d.EnumerateFileSystemInfos()) {
    if ($e -is [IO.DirectoryInfo]) {
      if (-not ($e.Attributes -band [IO.FileAttributes]::ReparsePoint)) { Walk $e }
    } elseif ($e.Attributes -band [IO.FileAttributes]::Archive) {
      $e.Attributes = $e.Attributes -band (-bnot [IO.FileAttributes]::Archive)
    }
  }
}
Walk ([IO.DirectoryInfo]$root)
|}

(* The control directory: run.cmd, the scripts it calls, and the overlay of
   files the host makes for the build.  [script] writes one there and gives
   its path in the container. *)
let control ~temp_dir =
  let dir = Path.(temp_dir / "control") in
  let () = Os.mkdir dir in
  (dir, fun name contents -> Os.write_to_file Path.(dir / name) contents; Path.(control_dst / name))

(* [before] copies in, [commands] are the build, whose status is the
   container's, and [after] copies out whatever the build left. *)
let run_cmd ~script ~before ~commands ~after =
  script "run.cmd"
    (String.concat "\r\n"
       ([ "@echo off"; "mkdir " ^ opam_root ^ " 2>nul" ]
       @ before
       @ [ "set PATH=%PATH%;" ^ opam_root; String.concat " && " commands; "set day10_status=%errorlevel%" ]
       @ after @ [ "exit /b %day10_status%"; "" ]))

let run ~t ~temp_dir opam_repository build_log =
  let config = t.config in
  let module L = (val unix_layer ~config) in
  let rootfs = Path.(temp_dir / "fs") in
  let () = Os.mkdir rootfs in
  let () = grant_users rootfs in
  let () = add_user ~t ~temp_dir ~build_log in
  let control_dir, script = control ~temp_dir in
  (* Both into opam's root, where the cache keeps them: a layer build then
     has nothing to fetch. *)
  let opam_exe = Path.(opam_root / "opam.exe") in
  let commands =
    (* --fail, or a missing release is downloaded as a page of HTML and
       fails later, as something else. *)
    ("curl.exe --fail -L -o " ^ opam_exe ^ " " ^ opam_url ~arch:config.arch)
    :: L.install ~opam_root ~script
    @ [
        opam_exe ^ " init -k local -a " ^ {|C:\opam-repository|} ^ " --bare -y" ^ L.init_options ~opam_root;
        opam_exe ^ " switch create default --empty";
        "curl.exe --fail -L -o " ^ Path.(opam_root / "day10-install.exe") ^ " " ^ day10_install_url ~arch:config.arch;
      ]
  in
  (* All of it, empty directories too, since nothing came before. *)
  let after = [ robocopy opam_root out_dst [ "/E"; "/SL"; "/COPY:DAT"; "/DCOPY:T" ] ] in
  let run = run_cmd ~script ~before:[] ~commands ~after in
  let mounts = [ ro control_dir control_dst; rw rootfs out_dst; ro opam_repository {|C:\opam-repository|} ] in
  let result = container ~t ~temp_dir ~build_log ~tee:config.log ~commands:[ run ] ~mounts in
  let () = L.keep ~temp_dir rootfs in
  let () = tidy ~config rootfs in
  let () = Os.write_to_file Path.(temp_dir / "status") (string_of_int result) in
  result

let build ~t ~temp_dir build_log pkg ordered_hashes =
  let config = t.config in
  let module L = (val unix_layer ~config) in
  let () =
    if Option.is_some config.build_command || Option.is_some config.directory then
      failwith "Building a project from its own sources is not supported on Windows yet: only health-check is"
  in
  let os_key = Config.os_key ~config in
  let target = Path.(temp_dir / "fs") in
  let () = Os.mkdir target in
  let () = grant_users target in
  let control_dir, script = control ~temp_dir in
  let sources = ordered_hashes @ [ "base" ] in
  (* What the host makes from the layers without assembling them: the Unix
     layer's merged database and the switch state of every package the
     layers installed.  Copied in last, after the archive attribute is
     cleared, so that the layer keeps them as Os.clense_tree kept them. *)
  let overlay = Path.(control_dir / "overlay") in
  let () = Os.mkdir overlay in
  let () = L.assemble ~layers:(List.map (fun hash -> Path.(config.dir / os_key / hash)) sources) ~target:overlay in
  let () =
    let packages hash = Path.(config.dir / os_key / hash / "fs" / "default" / ".opam-switch" / "packages") in
    match List.concat_map (fun hash -> if Sys.file_exists (packages hash) then Array.to_list (Sys.readdir (packages hash)) else []) sources with
    | [] -> ()
    | names ->
        let dir = Path.(overlay / "default" / ".opam-switch") in
        let () = Os.mkdir ~parents:true dir in
        Opamh.dump_state_of (List.sort_uniq String.compare names) Path.(dir / "switch-state")
  in
  let repo_default = Path.(opam_root / "repo" / "default") in
  let before =
    List.map (fun hash -> robocopy Path.(layers_dst / hash / "fs") opam_root (copy_in @ [ "/XD"; Path.(layers_dst / hash / "fs" / "repo" / "default") ])) sources
    @ [
        robocopy repo_dst repo_default [ "/MIR" ];
        "powershell -NoProfile -ExecutionPolicy Bypass -File " ^ script "clear-archive.ps1" clear_archive;
        "if errorlevel 1 exit /b 1";
        robocopy Path.(control_dst / "overlay") opam_root [ "/E"; "/COPY:DAT" ];
      ]
  in
  (* What the build wrote, leaving out the repository, which is the
     solution's, and the sources and build trees, which no layer keeps. *)
  let after =
    [
      robocopy opam_root out_dst
        [ "/S"; "/A"; "/SL"; "/COPY:DAT"; "/DCOPY:T"; "/XD"; repo_default; Path.(opam_root / "default" / ".opam-switch" / "sources"); Path.(opam_root / "default" / ".opam-switch" / "build") ];
    ]
  in
  let run = run_cmd ~script ~before ~commands:(Build_command.for_package ~config pkg) ~after in
  let mounts = [ ro control_dir control_dst; ro Path.(config.dir / os_key) layers_dst; ro Path.(temp_dir / "opam-repository") repo_dst; rw target out_dst ] in
  let result = container ~t ~temp_dir ~build_log ~tee:config.log ~commands:[ run ] ~mounts in
  let () = L.keep ~temp_dir target in
  let () = tidy ~config target in
  result
