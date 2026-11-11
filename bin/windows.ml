type t = {
  config : Config.t;
  network : string;
  username : string;
}

let hostname = "builder"

(* The image's own administrator, which may create symlinks, so that a build
   sees the native links a normal Windows install would. *)
let username = "ContainerAdministrator"

(* opam's root in the container, which is the directory the cache keeps: the
   switch, the repository, opam's own Cygwin and its package database -- and,
   so that a layer build has nothing to download, opam.exe and the driver.
   Short, rather than opam's default under the user's profile, because a
   switch's build directory sits under it and Windows limits a path to 260
   characters: dune-site's deepest temporary file is 262 under the profile. *)
let opam_root = {|C:\opam|}

(* The top layer of the base image, as ctr snapshot ls lists it: what every
   container is prepared on. *)
let image = "sha256:5ae66e790cc84572a3bb9646fcbd13b3dbf1af9252e013167791737880626b0b"

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

(* containerd serves a fixed number of instances of its pipe, and a ctr that
   finds them all in use fails with this before it has done anything, so it
   is always safe to run again.  Several builds at once make it common. *)
let pipe_busy = "All pipe instances are busy"

let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.equal (String.sub s i m) sub || at (i + 1)) in
  at 0

(* ctr, run again while containerd's pipe is busy.  What the call printed is
   read back from [stderr] to tell that apart from a real failure, so a
   temporary file takes it when the caller does not say where; a short
   output is all a busy pipe leaves.  Still busy after every try, day10
   stops rather than record a layer as failed for containerd's sake. *)
let ctr ?(stdout = Os.null) ?stderr ?tee args =
  let err, temporary = match stderr with Some err -> (err, false) | None -> (Filename.temp_file "ctr" ".err", true) in
  let busy () =
    match Os.read_from_file err with
    | output -> String.length output < 4096 && contains ~sub:pipe_busy output
    | exception Sys_error _ -> false
  in
  let rec go tries =
    match Os.exec ~stdout ~stderr:err ?tee ("ctr" :: args) with
    | 0 -> 0
    | status when not (busy ()) -> status
    | _ when tries > 1 ->
        OpamConsole.note "containerd's pipe is busy, running ctr %s again" (List.hd args);
        Unix.sleepf (0.5 +. Random.float 1.5);
        go (tries - 1)
    | _ ->
        OpamConsole.error "containerd's pipe was still busy after every try: ctr %s" (String.concat " " args);
        raise (OpamStd.Sys.Exit 1)
  in
  Fun.protect ~finally:(fun () -> if temporary then try Sys.remove err with Sys_error _ -> ()) (fun () -> go 60)

(* The snapshot a container is prepared on, let go of when the run ends: see
   Cleanup. *)
let snapshot name () = ignore (ctr ~stdout:Os.null [ "snapshot"; "rm"; name ])

(* One container: a snapshot prepared on the image, the OCI config written
   for it, and ctr run on that; the snapshot is let go of when the run ends. *)
let container ~t ~temp_dir ~build_log ~tee ~commands ~mounts =
  let name = Filename.basename temp_dir in
  Cleanup.with_resource (snapshot name) @@ fun () ->
  let mounts_json = Path.(temp_dir / "mounts.json") in
  let () =
    match ctr ~stdout:mounts_json ~stderr:Path.(temp_dir / "prepare.err") [ "snapshot"; "prepare"; "--mounts"; name; image ] with
    | 0 -> ()
    | status ->
        OpamConsole.error "ctr snapshot prepare %s failed with status %d: %s" name status (String.trim (Os.read_from_file Path.(temp_dir / "prepare.err")));
        raise (OpamStd.Sys.Exit 1)
  in
  let layers = Json_layers.read_layers mounts_json in
  let argv = [ "cmd"; "/c"; String.concat " && " commands ] in
  let config = make_config_json ~layers ~cwd:{|C:\|} ~argv ~hostname ~username:t.username ~env:(env ~config:t.config) ~mounts ~network:t.network in
  let config_json = Path.(temp_dir / "config.json") in
  let () = Os.write_to_file config_json (Yojson.Safe.pretty_to_string config) in
  ctr ~stdout:build_log ~stderr:build_log ~tee [ "run"; "--cni"; "--rm"; "--config"; config_json; name ]

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

(* robocopy's status is a bit mask in which 8 and above is a failure.  Even
   with every listing turned off it prints an empty line, one per layer in
   the build log, so what it says is shown only when it fails. *)
let robocopy src dst flags =
  let log = {|C:\Windows\Temp\day10-robocopy.log|} in
  String.concat " " ([ "robocopy"; src; dst ] @ flags @ [ "/R:0"; "/W:0"; "/MT:8"; "/NFL"; "/NDL"; "/NJH"; "/NJS"; "/NP"; ">" ^ log ])
  ^ "\r\nif errorlevel 8 (type " ^ log ^ " & exit /b 1)"

(* Skipping what is already there, so that copying the layers top first
   leaves each path as the topmost layer has it.  /SL copies a symlink as a
   link, and /COPY:DAT keeps the attributes, among them the SYSTEM attribute
   that marks a Cygwin symlink of the kind an unprivileged user gets. *)
let copy_in = [ "/E"; "/XC"; "/XN"; "/XO"; "/SL"; "/COPY:DAT"; "/DCOPY:T" ]

(* Stopping at the first error with a status cmd sees: otherwise a failure
   leaves everything marked, and the layer is the whole tree. *)
let clear_archive =
  {|param([Parameter(Mandatory)][string]$root)
$ErrorActionPreference = "Stop"
trap { Write-Error $_ -ErrorAction Continue; exit 1 }
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
        (* Fall back to opam's cache when a package's upstream sources are
           gone or have changed, as the Linux base does.  Set through opam,
           which writes its config as it would itself; the backslashes keep
           the quotes the value needs past the C runtime's argument parsing. *)
        opam_exe ^ {| option --global archive-mirrors=\"https://opam.ocaml.org/cache\"|};
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
  let control_dir, script = control ~temp_dir in
  let sources = ordered_hashes @ [ "base" ] in
  (* What the host makes from the layers without assembling them: the Unix
     layer's merged database and the switch state of every package the
     layers installed.  Copied in last, after the archive attribute is
     cleared, so that the layer keeps them. *)
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
        "powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File " ^ script "clear-archive.ps1" clear_archive ^ " " ^ opam_root;
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
