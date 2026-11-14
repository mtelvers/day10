type t = {
  config : Config.t;
  network : string;
  username : string;
}

let hostname = "builder"

(* What the backend runs to prepare and tear down a container -- ctr, wclayer,
   robocopy, hcn-namespace -- is run without a note: a log is read for the
   build, and these mean nothing to whoever reads it.  A failure says what
   failed, with the command's own output. *)

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

(* TEMP and TMP, which the image puts in the user's profile, under AppData,
   where they cost a test that makes its directories there 48 characters of
   the 260 Windows allows a path.  Outside opam's root, so no layer keeps
   what is left there. *)
let temp = {|C:\tmp|}

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
    ("OPAM_REPO_CI", "true");
    ("OPAMROOT", opam_root);
    ("TEMP", temp);
    ("TMP", temp);
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
let deinit ~t = ignore (Os.spawn ~stdout:Os.null "hcn-namespace" [ "delete"; t.network ])
let config ~t = t.config

(* opam's uname is Unix only; its arch poll asks Windows itself. *)
let arch () = OpamSysPoll.arch OpamVariable.Map.empty |> Option.value ~default:"x86_64"

(* The layers are the user's own, so nothing more than unlink is needed. *)
let remove = List.iter (Os.rm ~recursive:true)

let temp_dir path () = if Sys.file_exists path then try Os.rm ~recursive:true path with Unix.Unix_error _ | Sys_error _ -> ()

(* Windows has no fork, so one at a time. *)
let fork ?np:_ f xs = List.iter f xs

(* Failures that come before ctr has done anything, so that it is always safe
   to run it again.  containerd serves a fixed number of instances of its
   pipe, and a ctr that finds them all in use fails at once; several builds
   at once make it common.  The NAT network is created by the first container
   to need it, and two first containers started together both try, the one
   that loses failing before it starts. *)
let transient = [ "All pipe instances are busy"; "A network with this name already exists" ]

let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.equal (String.sub s i m) sub || at (i + 1)) in
  at 0

(* ctr, run again after a transient failure.  What the call printed is read
   back from [stderr] to tell that apart from a real failure, so a temporary
   file takes it when the caller does not say where; a short output is all a
   transient failure leaves.  Still failing after every try, day10 stops
   rather than record a layer as failed for containerd's sake. *)
let ctr ?(stdout = Os.null) ?stderr ?tee args =
  let err, temporary = match stderr with Some err -> (err, false) | None -> (Filename.temp_file "ctr" ".err", true) in
  let transient () =
    match Os.read_from_file err with
    | output -> if String.length output < 4096 then List.find_opt (fun sub -> contains ~sub output) transient else None
    | exception Sys_error _ -> None
  in
  let rec go tries =
    match fst (Os.spawn ~stdout ~stderr:err ?tee "ctr" args) with
    | 0 -> 0
    | status -> (
        match transient () with
        | None -> status
        | Some reason when tries > 1 ->
            OpamConsole.note "ctr %s: %s, running it again" (List.hd args) reason;
            Unix.sleepf (0.5 +. Random.float 1.5);
            go (tries - 1)
        | Some reason ->
            OpamConsole.error "ctr %s still failed after every try: %s" (String.concat " " args) reason;
            raise (OpamStd.Sys.Exit 1))
  in
  Fun.protect ~finally:(fun () -> if temporary then try Sys.remove err with Sys_error _ -> ()) (fun () -> go 60)

(* The snapshot a container is prepared on, let go of when the run ends: see
   Cleanup. *)
let snapshot name () = ignore (ctr ~stdout:Os.null [ "snapshot"; "rm"; name ])

(* Where run.cmd, which carries the build, and the scripts it calls are put in
   the snapshot before the container starts: outside opam's root, so that no
   layer keeps them. *)
let scripts_dir = {|C:\day10|}
let run_cmd = Path.(scripts_dir / "run.cmd")

(* The snapshot's view of C:, mounted on the host at [dir] for as long as [f]
   runs and let go of however it ends.  hcsshim's wclayer does what Docker
   does for docker cp on a stopped container: activates the scratch layer over
   its parents, with no container started.  The directory is left empty and
   then removed, so that nothing of a mount is left in the layer. *)
let mounted ~parents ~scratch ~dir f =
  let () = Os.mkdir dir in
  let lflags = List.concat_map (fun p -> [ "-l"; p ]) parents in
  (match fst (Os.spawn ~stdout:Os.null "wclayer" ([ "mount" ] @ lflags @ [ scratch; dir ])) with
  | 0 -> ()
  | status ->
      OpamConsole.error "wclayer mount %s failed with status %d" scratch status;
      raise (OpamStd.Sys.Exit 1));
  (* Said rather than raised, as a release must not mask what it may be
     unwinding from: a snapshot still mounted cannot be removed, and the
     directory left behind is the mount, not an empty one. *)
  Cleanup.with_resource
    (fun () ->
      match fst (Os.spawn ~stdout:Os.null "wclayer" [ "unmount"; scratch; dir ]) with
      | 0 -> ( try Unix.rmdir dir with Unix.Unix_error (e, _, _) -> OpamConsole.warning "%s is still there after wclayer unmount: %s" dir (Unix.error_message e))
      | status -> OpamConsole.warning "wclayer unmount %s failed with status %d; the snapshot is still mounted at %s" scratch status dir)
    (fun () -> f dir)

(* One container: a snapshot prepared on the image, [fill] given the
   snapshot's C: on the host before the container starts, the container run
   on it with no mounts at all, and [collect] given the same view once it has
   stopped.  The build itself never touches a bind mount: a Windows container
   cannot run a program through a relative symlink on one, nor remove or
   replace a file in use there, which pacman and gpg both do.  See
   windows-symlink-exec.md.  The snapshot is let go of when this ends. *)
let container ~t ~temp_dir ~build_log ~tee ~fill ~collect =
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
  (* The parent layers, top first, and then the scratch, as layerFolders
     takes them. *)
  let layers = Json_layers.read_layers mounts_json in
  let parents = List.filteri (fun i _ -> i < List.length layers - 1) layers and scratch = List.nth layers (List.length layers - 1) in
  let dir = Path.(temp_dir / "mnt") in
  let () = mounted ~parents ~scratch ~dir fill in
  let argv = [ "cmd"; "/c"; run_cmd ] in
  let config = make_config_json ~layers ~cwd:{|C:\|} ~argv ~hostname ~username:t.username ~env:(env ~config:t.config) ~mounts:[] ~network:t.network in
  let config_json = Path.(temp_dir / "config.json") in
  let () = Os.write_to_file config_json (Yojson.Safe.pretty_to_string config) in
  let result = ctr ~stdout:build_log ~stderr:build_log ~tee [ "run"; "--cni"; "--rm"; "--config"; config_json; name ] in
  let () = mounted ~parents ~scratch ~dir collect in
  result

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

(* Where a path in the container is on the host while its snapshot is
   mounted at [dir]: every path day10 puts there is on C:. *)
let on_host dir path =
  if String.length path >= 3 && String.equal (String.sub path 0 3) {|C:\|} then Path.(dir / String.sub path 3 (String.length path - 3))
  else invalid_arg ("Windows.on_host: " ^ path)

(* robocopy, whose status is a bit mask in which 8 and above is a failure.
   What it says goes to [log] and is shown, with the command, only when it
   fails; day10 then stops, since a copy that failed says nothing about the
   package. *)
let robocopy ~log src dst flags =
  match fst (Os.spawn ~stdout:log ~stderr:log "robocopy" ([ src; dst ] @ flags @ [ "/R:0"; "/W:0"; "/MT:8"; "/NFL"; "/NDL"; "/NJH"; "/NJS"; "/NP" ])) with
  | status when status < 8 -> ()
  | status ->
      OpamConsole.error "robocopy %s %s failed with status %d:\n%s" src dst status (Os.read_from_file log);
      raise (OpamStd.Sys.Exit 1)

(* Copying out reads what the container wrote, with the permissions it gave:
   Cygwin's ssh-keysign, say, is readable by no account the host knows.  /ZB
   falls back to backup mode, as the administrator day10 runs as may, for a
   file it is refused.  Not /B, which reads through every symlink, /SL or not,
   and fails on one whose target, C:\opam's certificates, exists only in the
   container. *)
let backup = "/ZB"

(* Skipping what is already there, so that copying the layers top first
   leaves each path as the topmost layer has it.  /SL copies a symlink as a
   link, and /COPY:DAT keeps the attributes, among them the SYSTEM attribute
   that marks a Cygwin symlink of the kind an unprivileged user gets. *)
let copy_in = [ "/E"; "/XC"; "/XN"; "/XO"; "/SL"; "/COPY:DAT"; "/DCOPY:T" ]

(* Clears the archive attribute from everything under the root, so that what
   the build writes afterwards is all that has it.  Stopping at the first
   error with a status day10 sees: otherwise a failure leaves everything
   marked, and the layer is the whole tree. *)
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

let clear_archive_bits ~temp_dir root =
  let script = Path.(temp_dir / "clear-archive.ps1") in
  let () = Os.write_to_file script clear_archive in
  match fst (Os.spawn "powershell" [ "-NoProfile"; "-NonInteractive"; "-ExecutionPolicy"; "Bypass"; "-File"; script; root ]) with
  | 0 -> ()
  | status ->
      OpamConsole.error "Clearing the archive attribute under %s failed with status %d" root status;
      raise (OpamStd.Sys.Exit 1)

(* Written into the snapshot before the container starts: run.cmd, which
   carries the build, and the scripts it calls.  [script name contents] writes
   one and gives its path in the container. *)
let scripts dir =
  let () = Os.mkdir ~parents:true (on_host dir scripts_dir) in
  fun name contents ->
    let path = Path.(scripts_dir / name) in
    Os.write_to_file (on_host dir path) contents;
    path

(* The build's status is the container's. *)
let write_run_cmd ~script commands =
  ignore
    (script (Filename.basename run_cmd)
       (String.concat "\r\n"
          [ "@echo off"; "mkdir " ^ opam_root ^ " 2>nul"; "mkdir " ^ temp ^ " 2>nul"; "set PATH=%PATH%;" ^ opam_root; String.concat " && " commands; "exit /b %errorlevel%"; "" ]))

(* The packages Cygwin's setup and pacman download. *)
let download_caches root = [ Path.(root / ".cygwin" / "cache"); Path.(root / "msys64" / "var" / "cache" / "pacman") ]

let run ~t ~temp_dir opam_repository build_log =
  let config = t.config in
  let module L = (val unix_layer ~config) in
  let rootfs = Path.(temp_dir / "fs") in
  let () = Os.mkdir rootfs in
  let log = Path.(temp_dir / "robocopy.log") in
  (* Both into opam's root, where the cache keeps them: a layer build then
     has nothing to fetch. *)
  let opam_exe = Path.(opam_root / "opam.exe") in
  let fill dir =
    let script = scripts dir in
    let () = robocopy ~log opam_repository (on_host dir {|C:\opam-repository|}) [ "/E" ] in
    write_run_cmd ~script
      ((* --fail, or a missing release is downloaded as a page of HTML and
          fails later, as something else. *)
       ("curl.exe --fail -L -o " ^ opam_exe ^ " " ^ opam_url ~arch:config.arch)
       :: L.install ~opam_root ~script
      @ [
          opam_exe ^ " init -k local -a " ^ {|C:\opam-repository|} ^ " --bare -y" ^ L.init_options ~opam_root;
          (* Fall back to opam's cache when a package's upstream sources are
             gone or have changed, as the Linux base does.  Set through opam,
             which writes its config as it would itself; the backslashes keep
             the quotes the value needs past the C runtime's argument
             parsing. *)
          opam_exe ^ {| option --global archive-mirrors=\"https://opam.ocaml.org/cache\"|};
          opam_exe ^ " switch create default --empty";
          "curl.exe --fail -L -o " ^ Path.(opam_root / "day10-install.exe") ^ " " ^ day10_install_url ~arch:config.arch;
        ])
  in
  (* All of it, empty directories too, since nothing came before. *)
  let collect dir =
    let root = on_host dir opam_root in
    robocopy ~log root rootfs ([ "/E"; "/SL"; "/COPY:DAT"; "/DCOPY:T"; backup; "/XD" ] @ download_caches root)
  in
  let result = container ~t ~temp_dir ~build_log ~tee:config.log ~fill ~collect in
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
  let log = Path.(temp_dir / "robocopy.log") in
  let sources = ordered_hashes @ [ "base" ] in
  let layer hash = Path.(config.dir / os_key / hash) in
  (* What the host makes from the layers without assembling them: the Unix
     layer's merged database and the switch state of every package the
     layers installed.  Copied in last, after the archive attribute is
     cleared, so that the layer keeps them. *)
  let overlay = Path.(temp_dir / "overlay") in
  let () = Os.mkdir overlay in
  let () = L.assemble ~layers:(List.map layer sources) ~target:overlay in
  let () =
    let packages hash = Path.(layer hash / "fs" / "default" / ".opam-switch" / "packages") in
    match List.concat_map (fun hash -> if Sys.file_exists (packages hash) then Array.to_list (Sys.readdir (packages hash)) else []) sources with
    | [] -> ()
    | names ->
        let dir = Path.(overlay / "default" / ".opam-switch") in
        let () = Os.mkdir ~parents:true dir in
        Opamh.dump_state_of (List.sort_uniq String.compare names) Path.(dir / "switch-state")
  in
  let fill dir =
    let root = on_host dir opam_root in
    let () = List.iter (fun hash -> robocopy ~log Path.(layer hash / "fs") root (copy_in @ [ "/XD"; Path.(layer hash / "fs" / "repo" / "default") ])) sources in
    let () = robocopy ~log Path.(temp_dir / "opam-repository") Path.(root / "repo" / "default") [ "/MIR" ] in
    let () = clear_archive_bits ~temp_dir root in
    let () = robocopy ~log overlay root [ "/E"; "/COPY:DAT" ] in
    write_run_cmd ~script:(scripts dir) (Build_command.for_package ~config pkg)
  in
  (* What the build wrote, leaving out the repository, which is the
     solution's, and the sources, build trees and download caches, which no
     layer keeps. *)
  let collect dir =
    let root = on_host dir opam_root in
    robocopy ~log root target
      ([ "/S"; "/A"; "/SL"; "/COPY:DAT"; "/DCOPY:T"; backup; "/XD"; Path.(root / "repo" / "default"); Path.(root / "default" / ".opam-switch" / "sources"); Path.(root / "default" / ".opam-switch" / "build") ]
      @ download_caches root)
  in
  let result = container ~t ~temp_dir ~build_log ~tee:config.log ~fill ~collect in
  let () = L.keep ~temp_dir target in
  let () = tidy ~config target in
  result
