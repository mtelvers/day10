type t = {
  config : Config.t;
  network : string;
  username : string;
}

let hostname = "builder"
let username = "ContainerAdministrator"

(* opam's root in the container, which is the directory the cache keeps: the
   switch, the repository, opam's own Cygwin and its package database -- and,
   so that a layer build has nothing to download, opam.exe and the driver. *)
let opam_root = {|C:\Users\|} ^ username ^ {|\AppData\Local\opam|}

(* The top layer of the base image, as ctr snapshot ls lists it: what every
   container is prepared on. *)
let image = "sha256:5ae66e790cc84572a3bb9646fcbd13b3dbf1af9252e013167791737880626b0b"

let opam_url ~arch = "https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-" ^ arch ^ "-windows.exe"

let day10_install_url ~arch = "https://github.com/mtelvers/day10-install/releases/download/1.4.0/day10-install-1.4.0-" ^ arch ^ "-windows.exe"

(* Added to the container's own environment rather than replacing it, so the
   system variables Windows needs stay. *)
let env ~(config : Config.t) =
  [
    ("CI", "true");
    ("OPAMYES", "1");
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

(* One container: a snapshot prepared on the image, the OCI config written
   for it, and ctr run on that. *)
let container ~t ~temp_dir ~build_log ~tee ~commands ~mounts =
  let name = Filename.basename temp_dir in
  let mounts_json = Path.(temp_dir / "mounts.json") in
  Cleanup.with_resource (snapshot name) @@ fun () ->
  let _ = Os.retry_exec ~stdout:mounts_json [ "ctr"; "snapshot"; "prepare"; "--mounts"; name; image ] in
  let layers = Json_layers.read_layers mounts_json in
  let argv = [ "cmd"; "/c"; String.concat " && " commands ] in
  let config = make_config_json ~layers ~cwd:{|C:\|} ~argv ~hostname ~username:t.username ~env:(env ~config:t.config) ~mounts ~network:t.network in
  let config_json = Path.(temp_dir / "config.json") in
  let () = Os.write_to_file config_json (Yojson.Safe.pretty_to_string config) in
  Os.exec ~stdout:build_log ~stderr:build_log ~tee [ "ctr"; "run"; "--cni"; "--rm"; "--config"; config_json; name ]

let installed_db root = Path.(root / ".cygwin" / "root" / "etc" / "setup" / "installed.db")

(* What a container leaves in opam's root that is not part of the layer: locks,
   the environment and repository caches opam rebuilds, and the Cygwin
   database, which is kept beside the layer instead so that a build can merge
   every layer's copy. *)
let tidy root =
  List.iter Os.rm
    ([
       Path.(root / "lock");
       Path.(root / "conf.lock");
       Path.(root / "default" / ".opam-switch" / "lock");
       Path.(root / "default" / ".opam-switch" / "environment");
       Path.(root / "default" / ".opam-switch" / "packages" / "cache");
       Path.(root / "repo" / "conf.lock");
       installed_db root;
     ]
    @ Os.ls ~extn:".cache" Path.(root / "repo"))

let run ~t ~temp_dir opam_repository build_log =
  let config = t.config in
  let rootfs = Path.(temp_dir / "fs") in
  let () = Os.mkdir rootfs in
  let commands =
    [
      (* --fail, or a missing release is downloaded as a page of HTML and
         fails later, as something else. *)
      "curl.exe --fail -L -o " ^ {|C:\Windows\opam.exe|} ^ " " ^ opam_url ~arch:config.arch;
      "opam.exe init -k local -a " ^ {|C:\opam-repository|} ^ " --bare -y";
      "opam.exe switch create default --empty";
      (* Into opam's root now that init has made it, where the cache keeps
         them: a layer build then has nothing to fetch. *)
      "copy " ^ {|C:\Windows\opam.exe|} ^ " " ^ Path.(opam_root / "opam.exe");
      "curl.exe --fail -L -o " ^ Path.(opam_root / "day10-install.exe") ^ " " ^ day10_install_url ~arch:config.arch;
    ]
  in
  let mounts =
    [
      { Mount.ty = "bind"; src = rootfs; dst = opam_root; options = [ "rw"; "rbind"; "rprivate" ] };
      { ty = "bind"; src = opam_repository; dst = {|C:\opam-repository|}; options = [ "rbind"; "rprivate" ] };
    ]
  in
  let result = container ~t ~temp_dir ~build_log ~tee:config.log ~commands ~mounts in
  let () = Os.cp (installed_db rootfs) Path.(temp_dir / "installed.db") in
  let () = tidy rootfs in
  let () = Os.write_to_file Path.(temp_dir / "status") (string_of_int result) in
  result

let build ~t ~temp_dir build_log pkg ordered_hashes =
  let config = t.config in
  let () =
    if Option.is_some config.build_command || Option.is_some config.directory then
      failwith "Building a project from its own sources is not supported on Windows yet: only health-check is"
  in
  let os_key = Config.os_key ~config in
  let target = Path.(temp_dir / "fs") in
  let () = Os.mkdir target in
  (* opam's root on the path, for opam.exe and the driver the base left there.
     No quotes, which the container runtime escapes into something cmd does not
     read as quotes, and none needed: set takes the rest of the line, up to the
     && that follows without a space, so no trailing space lands in the path. *)
  let commands = [ "set PATH=%PATH%;" ^ opam_root ^ "&& " ^ String.concat " && " (Build_command.for_package ~config pkg) ] in
  let sources = ordered_hashes @ [ "base" ] in
  let () = List.iter (fun hash -> Os.copy_tree ~source:Path.(config.dir / os_key / hash / "fs") ~target) sources in
  let union = Cygwin.union (List.map (fun hash -> Os.read_from_file Path.(config.dir / os_key / hash / "installed.db")) sources) in
  let () = Os.write_to_file (installed_db target) union in
  let () =
    let packages_dir = Path.(target / "default" / ".opam-switch" / "packages") in
    let state_file = Path.(target / "default" / ".opam-switch" / "switch-state") in
    if Sys.file_exists packages_dir then Opamh.dump_state packages_dir state_file
  in
  let mounts =
    [
      { Mount.ty = "bind"; src = target; dst = opam_root; options = [ "rw"; "rbind"; "rprivate" ] };
      { ty = "bind"; src = Path.(temp_dir / "opam-repository"); dst = Path.(opam_root / "repo" / "default"); options = [ "rbind"; "rprivate" ] };
    ]
  in
  let result = container ~t ~temp_dir ~build_log ~tee:config.log ~commands ~mounts in
  let () = Os.cp (installed_db target) Path.(temp_dir / "installed.db") in
  let () = tidy target in
  let () = List.iter (Os.rm ~recursive:true) [ Path.(target / "default" / ".opam-switch" / "sources"); Path.(target / "default" / ".opam-switch" / "build") ] in
  let () = List.iter (fun hash -> Os.clense_tree ~source:Path.(config.dir / os_key / hash / "fs") ~target) sources in
  result
