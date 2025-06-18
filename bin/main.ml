module Solver = Opam_0install.Solver.Make (Dir_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

type config = {
  dir : string;
  opam_repository : string;
  package : string;
  directory : string option;
}

let hostname = "builder"

let env =
  [
    ("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin");
    ("HOME", "/home/opam");
    ("OPAMYES", "1");
    ("OPAMCONFIRMLEVEL", "unsafe-yes");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
  ]

let win_env =
  [
    ("Path", "C:\\Windows\\system32;C:\\Windows;C:\\Users\\ContainerAdministrator\\AppData\\Local\\opam\\.cygwin\\root\\usr\\i686-w64-mingw32\\sys-root\\mingw\\bin;C:\\Users\\ContainerAdministrator\\AppData\\Local\\opam\\.cygwin\\root\\usr\\x86_64-w64-mingw32\\sys-root\\mingw\\bin;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0\\;C:\\Windows\\System32\\OpenSSH\\;C:\\Users\\ContainerAdministrator\\AppData\\Local\\Microsoft\\WindowsApps");
    ("OPAMYES", "1");
    ("OPAMCONFIRMLEVEL", "unsafe-yes");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
  ]

let init config =
  let root = Os.path [ config.dir; "root" ] in
  if not (Sys.file_exists root) then
    Os.create_directory_exclusively root @@ fun target_dir ->
    let temp_dir = Filename.temp_dir ~temp_dir:config.dir ~perms:0o755 "temp-" "" in
    let rootfs = Os.path [ temp_dir; "fs" ] in
    let () = Os.mkdir rootfs in
    let opam_repository = Os.path [ temp_dir; "opam-repository" ] in
    let () = Os.mkdir opam_repository in
    let () = Os.write_to_file (Os.path [ opam_repository; "repo" ]) {|opam-version: "2.0"|} in
    let build_log = Os.path [ temp_dir; "build.log" ] in
            match Sys.win32 with
            | false ->
    let _ = Os.sudo [ "/usr/bin/env"; "bash"; "-c"; "docker export $(docker run -d debian:12) | sudo tar -C " ^ rootfs ^ " -x" ] in
    let opam = Os.path [ rootfs; "/usr/local/bin/opam" ] in
    let _ = Os.sudo [ "curl"; "-L"; "https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-x86_64-linux"; "-o"; opam ] in
    let _ = Os.sudo [ "sudo"; "chmod"; "+x"; opam ] in
    let _ = Os.sudo [ "cp"; "/home/mtelvers/opam_build/_build/default/bin/main.exe"; Os.path [ rootfs; "/usr/local/bin/opam-build" ] ] in
    let etc_hosts = Os.path [ temp_dir; "hosts" ] in
    let () = Os.write_to_file etc_hosts ("127.0.0.1 localhost " ^ hostname) in
    let argv =
      [
        "/usr/bin/env";
        "bash";
        "-c";
        String.concat " && "
          [
            "apt update";
            "apt upgrade -y";
            "apt install build-essential unzip bubblewrap git sudo curl rsync -y";
            "adduser --disabled-password --gecos '@opam' --no-create-home --home /home/opam opam";
            "chown -R $(id -u opam):$(id -g opam) /home/opam";
            {|echo "opam ALL=(ALL:ALL) NOPASSWD:ALL" > /etc/sudoers.d/opam|};
            "su - opam -c 'opam init -k local -a /home/opam/opam-repository --bare -y'";
            "su - opam -c 'opam switch create default --empty'";
          ];
      ]
    in
    let mounts =
      [
        { Json_config.ty = "bind"; src = etc_hosts; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
        { ty = "bind"; src = opam_repository; dst = "/home/opam/opam-repository"; options = [ "rbind"; "rprivate" ] };
      ]
    in
    let config = Json_config.make ~root:rootfs ~cwd:"/home/opam" ~argv ~hostname ~uid:0 ~gid:0 ~env ~mounts ~network:true in
    let () = Os.write_to_file (Os.path [ temp_dir; "config.json" ]) (Yojson.Safe.pretty_to_string config) in
    let result = Os.sudo ~stdout:build_log ~stderr:build_log [ "runc"; "run"; "-b"; temp_dir; Filename.basename temp_dir ] in
    let _ = Os.rm (Os.path [ rootfs; "home"; "opam"; ".opam"; "repo"; "state-33BF9E46.cache" ] ) in
    let () = Os.write_to_file (Os.path [ temp_dir; "status" ]) (string_of_int result) in
    Unix.rename temp_dir target_dir
            | true ->
    let argv =
      [
        "cmd";
        "/c";
        String.concat " && "
          [
            "set";
            "curl.exe -L -o c:\\windows\\opam.exe https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-x86_64-windows.exe";
            "curl.exe -L -o c:\\windows\\opam-build.exe https://github.com/mtelvers/opam-build/releases/download/1.0.0/opam-build-1.0.0-x86_64-windows.exe";
            (* "net user opam /nopassword /add"; *)
            "opam init -k local -a c:\\opam-repository --bare -y";
            "opam switch create default --empty";
          ];
      ]
    in
    let mounts =
      [
        { Json_config.ty = "bind"; src = rootfs; dst = "c:\\Users\\ContainerAdministrator\\AppData\\Local\\opam"; options = [ "rw"; "rbind"; "rprivate" ] };
        (*{ Json_config.ty = "bind"; src = etc_hosts; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] }; *)
        { ty = "bind"; src = opam_repository; dst = "c:\\opam-repository"; options = [ "rbind"; "rprivate" ] };
      ]
    in
    let config = Json_config.make_ctr ~root:rootfs ~cwd:"c:\\" ~argv ~hostname ~uid:0 ~gid:0 ~env:win_env ~mounts ~network:true in
    let config_json = Os.path [ temp_dir; "config.json" ] in
    let () = Os.write_to_file config_json (Yojson.Safe.pretty_to_string config) in
    let result = Os.exec ~stdout:build_log ~stderr:build_log [ "ctr"; "run"; "--cni"; "--rm"; "--config"; config_json; Filename.basename temp_dir ] in
    let _ = Os.rm (Os.path [ rootfs; "repo"; "state-33BF9E46.cache" ] ) in
    let () = Os.write_to_file (Os.path [ temp_dir; "status" ]) (string_of_int result) in
    Unix.rename temp_dir target_dir

let () = OpamFormatConfig.init ()

(* let root = OpamStateConfig.opamroot ()
let _ = OpamStateConfig.load_defaults root *)
let () = OpamCoreConfig.init ?debug_level:(Some 10) ?debug_sections:(Some (OpamStd.String.Map.singleton "foo" (Some 10))) ()

let std_env
    ?(ocaml_native=true)
    ?opam_version
    ~arch ~os ~os_distribution ~os_family ~os_version
    () =
  function
  | "arch" -> Some (OpamTypes.S arch)
  | "os" -> Some (OpamTypes.S os)
  | "os-distribution" -> Some (OpamTypes.S os_distribution)
  | "os-version" -> Some (OpamTypes.S os_version)
  | "os-family" -> Some (OpamTypes.S os_family)
  | "opam-version"  -> Some (OpamVariable.S (Option.value ~default:OpamVersion.(to_string current) opam_version))
  (* There is no system compliler *)
  | "sys-ocaml-arch"
  | "sys-ocaml-cc"
  | "sys-ocaml-libc"
  | "sys-ocaml-system"
  | "sys-ocaml-version" -> Some (OpamTypes.S "")
  | "ocaml:native" -> Some (OpamTypes.B ocaml_native)
  | "ocaml:version" -> Some (OpamTypes.S "5.3.0")
  | "enable-ocaml-beta-repository" -> None      (* Fake variable? *)
  | v ->
    OpamConsole.warning "Unknown variable %S" v;
    None

let std_env = match Sys.win32 with
  | false -> std_env ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian" ~os_family:"debian" ~os_version:"12" ()
  | true -> std_env ~arch:"x86_64" ~os:"win32" ~os_distribution:"cygwin" ~os_family:"windows" ~os_version:"10.0.20348" ()

let opam_env pkg v =
  (*  if List.mem v OpamPackageVar.predefined_depends_variables then (Some (OpamTypes.B true))
  else *)
  match OpamVariable.Full.to_string v with
  | "version" -> Some (OpamTypes.S (OpamPackage.Version.to_string (OpamPackage.version pkg)))
  | "with-test"
  | "with-dev"
  | "with-dev-setup"
  | "dev"
  | "with-doc" ->
      Some (OpamTypes.B false)
  | "build" -> Some (OpamTypes.B true)
  | "post" -> None
  | x -> std_env x

let solve config ocaml_version pkg =
  let constraints =
    OpamPackage.Name.Map.of_list
      [ (OpamPackage.name ocaml_version, (`Eq, OpamPackage.version ocaml_version)); (OpamPackage.name pkg, (`Eq, OpamPackage.version pkg)) ]
  in
  let pins =
    match config.directory with
    | None -> OpamPackage.Name.Map.empty
    | Some directory ->
        OpamPackage.Name.Map.empty
        |> OpamPackage.Name.Map.add (OpamPackage.Name.of_string config.package)
             (OpamPackage.Version.of_string "dev", OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw (Os.path [ directory; config.package ^ ".opam" ]))))
  in
  let context = Dir_context.create ~env:std_env ~constraints ~pins (Os.path [ config.opam_repository; "packages" ]) in
  let r = Solver.solve context [ OpamPackage.name pkg ] in
  match r with
  | Ok out ->
      let sels = Output.to_map out in
      let depends = Hashtbl.create 100 in
      let classify x =
        match Solver.package_name x with
        | Some pkg -> `Opam pkg
        | None -> `Virtual x
      in
      let () =
        Role_map.iter
          (fun role sel ->
            let impl = Output.unwrap sel in
            Solver.Input.requires role impl |> fst
            |> List.iter (fun dep ->
                   let dep = Input.dep_info dep in
                   let dep_role = dep.dep_role in
                   if dep.dep_importance <> `Restricts then Hashtbl.add depends (classify role) (classify dep_role)))
          sels
      in
      let rec expand role =
        Hashtbl.find_all depends role
        |> List.concat_map (function
             | `Opam dep -> [ dep ]
             | `Virtual _ as role -> expand role)
      in
      let pkgs = Solver.packages_of_result out |> OpamPackage.Set.of_list in
      let pkgnames = OpamPackage.names_of_packages pkgs in
      let deptree =
        OpamPackage.Set.fold
          (fun pkg acc ->
            let opam = Dir_context.load context pkg in
            let deps = OpamFile.OPAM.depends opam |> OpamFilter.partial_filter_formula (opam_env pkg) in
            let with_post = OpamFilter.filter_deps ~build:true ~post:true deps |> OpamFormula.all_names in
            let without_post = OpamFilter.filter_deps ~build:true ~post:false deps |> OpamFormula.all_names in
            let deppost = OpamPackage.Name.Set.diff with_post without_post in
            let depopts = OpamFile.OPAM.depopts opam |> OpamFormula.all_names in
            let depopts = OpamPackage.Name.Set.inter depopts pkgnames |> OpamPackage.Name.Set.to_list in
            let name = OpamPackage.name pkg in
            let deps =
              expand (`Opam name) @ depopts |> OpamPackage.Name.Set.of_list |> fun x ->
              OpamPackage.Name.Set.diff x deppost |> OpamPackage.packages_of_names pkgs
            in
            OpamPackage.Map.add pkg deps acc)
          pkgs OpamPackage.Map.empty
      in
      let rec dfs map pkg =
        let deps = OpamPackage.Map.find pkg deptree in
        OpamPackage.Set.fold
          (fun p acc ->
            match OpamPackage.Map.mem p acc with
            | true -> acc
            | false -> dfs acc p)
          deps (OpamPackage.Map.add pkg deps map)
      in
      dfs OpamPackage.Map.empty pkg
  | Error problem ->
      OpamConsole.error "No solution";
      print_endline (Solver.diagnostics problem);
      OpamPackage.Map.empty

let rec topological_sort pkgs =
  match OpamPackage.Map.is_empty pkgs with
  | true -> []
  | false ->
      (* Find all packages which can be installed *)
      let installable, remainder = OpamPackage.Map.partition (fun _ deps -> OpamPackage.Set.is_empty deps) pkgs in
      let () = assert (not (OpamPackage.Map.is_empty installable)) in
      let installable = OpamPackage.Map.to_list installable |> List.map fst in
      (* Remove the dependency on any installable package from the remaining packages *)
      let pkgs = OpamPackage.Map.map (fun deps -> List.fold_left (fun acc pkg -> OpamPackage.Set.remove pkg acc) deps installable) remainder in
      installable @ topological_sort pkgs

let rec find_all_deps solution deps acc =
  OpamPackage.Set.fold
    (fun dep acc ->
      match OpamPackage.Set.mem dep acc with
      | true -> acc
      | false -> find_all_deps solution (OpamPackage.Map.find dep solution) (OpamPackage.Set.add dep acc))
    deps acc

let hash_of_set s = s |> OpamPackage.Set.to_list |> List.map OpamPackage.to_string |> String.concat " " |> Digest.string |> Digest.to_hex

type build_result =
  | No_solution
  | Dependency_failed
  | Failure of string
  | Success of string

let build_result_to_string = function
  | No_solution -> "no_solution"
  | Dependency_failed -> "dependency_failed"
  | Failure _ -> "failure"
  | Success _ -> "success"

let build_layer config solution dependencies pkg =
  let () = Printf.printf "Layer %s: %!" (OpamPackage.to_string pkg) in
  let deps = OpamPackage.Map.find pkg solution in
  let alldeps = OpamPackage.Map.find pkg dependencies in
  let hash = hash_of_set alldeps in
  let layer_dir = Os.path [ config.dir; hash ] in
  let () = Printf.printf "layer_dir %s\n%!" layer_dir in
  let write_layer target_dir =
    let temp_dir = Filename.temp_dir ~temp_dir:config.dir ~perms:0o755 "temp-" "" in
    let () = Printf.printf "temp_dir %s\n%!" temp_dir in
    let () = Os.write_to_file (Os.path [ temp_dir; "packages" ]) (OpamPackage.Set.to_string deps) in
    let lowerdir = Os.path [ config.dir; "root"; "fs" ] in
    let upperdir = Os.path [ temp_dir; "fs" ] in
    let build_log = Os.path [ temp_dir; "build.log" ] in
    let () = Os.mkdir upperdir in
    (
            match Sys.win32 with
            | false ->
    let pin = if OpamPackage.name_to_string pkg = config.package then [ "opam pin -yn " ^ OpamPackage.to_string pkg ^ " $HOME/src/"; "cd src" ] else [] in
    let argv = [ "/usr/bin/env"; "bash"; "-c"; String.concat " && " (pin @ [ "opam-build -v " ^ OpamPackage.to_string pkg ]) ] in
    let workdir = Os.path [ temp_dir; "work" ] in
    let () = Os.mkdir workdir in
    let () =
      OpamPackage.Set.iter
        (fun dep ->
          assert (
            0
            = Os.sudo
                [
                  "cp";
                  (* "--no-clobber"; *)
                  "--update=none";
                  "--archive";
                  "--no-dereference";
                  "--recursive";
                  "--link";
                  "--no-target-directory";
                  Os.path [ config.dir; hash_of_set (find_all_deps solution (OpamPackage.Set.singleton dep) OpamPackage.Set.empty); "fs" ];
                  upperdir;
                ]))
        deps
    in
    let () =
      let default_switch = Os.path [ temp_dir; "fs"; "home"; "opam"; ".opam"; "default" ] in
      if Sys.file_exists default_switch then Opamh.dump_state default_switch
    in
    let etc_hosts = Os.path [ temp_dir; "hosts" ] in
    let () = Os.write_to_file etc_hosts ("127.0.0.1 localhost " ^ hostname) in
    let mounts =
      [
        { Json_config.ty = "overlay"; src = "overlay"; dst = "/"; options = [ "lowerdir=" ^ lowerdir; "upperdir=" ^ upperdir; "workdir=" ^ workdir ] };
        { ty = "bind"; src = config.opam_repository; dst = "/home/opam/.opam/repo/default"; options = [ "rbind"; "rprivate" ] };
        { ty = "bind"; src = etc_hosts; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
      ]
    in
    let mounts =
      match config.directory with
      | None -> mounts
      | Some src -> mounts @ [{ ty = "bind"; src; dst = "/home/opam/src"; options = [ "rw"; "rbind"; "rprivate" ] }]
    in
    let () = Os.mkdir (Os.path [ temp_dir; "dummy" ]) in
    let config = Json_config.make ~root:"dummy" ~cwd:"/home/opam" ~argv ~hostname ~uid:1000 ~gid:1000 ~env ~mounts ~network:true in
    let () = Os.write_to_file (Os.path [ temp_dir; "config.json" ]) (Yojson.Safe.pretty_to_string config) in
    let result = Os.sudo ~stdout:build_log ~stderr:build_log [ "runc"; "run"; "-b"; temp_dir; Filename.basename temp_dir ] in
    let _ = Os.sudo [ "rm"; "-rf"; Os.path [ upperdir; "tmp" ] ] in
    let _ = Os.sudo [ "rm"; "-rf"; Os.path [ upperdir; "home/opam/.opam/default/.opam-switch/sources" ] ] in
    let () = Os.write_to_file (Os.path [ temp_dir; "status" ]) (string_of_int result) in
    Unix.rename temp_dir target_dir
            | true ->
    let pin = if OpamPackage.name_to_string pkg = config.package then [ "opam pin -yn " ^ OpamPackage.to_string pkg ^ " $HOME/src/"; "cd src" ] else [] in
    let argv = [ "cmd"; "/c"; String.concat " && " (pin @ [ "opam-build -v " ^ OpamPackage.to_string pkg ]) ] in
    let _ = Os.hardlink_tree ~source:(Os.path [ config.dir; "root"; "fs" ]) ~target:upperdir in
    let () =
      OpamPackage.Set.iter
        (fun dep ->
          Os.hardlink_tree
            ~source:(Os.path [ config.dir; hash_of_set (find_all_deps solution (OpamPackage.Set.singleton dep) OpamPackage.Set.empty); "fs"])
              ~target:upperdir)
        deps
    in

    let () =
      let default_switch = Os.path [ temp_dir; "fs"; "default" ] in
      if Sys.file_exists default_switch then Opamh.dump_state default_switch
    in
    let mounts =
      [
        { Json_config.ty = "bind"; src = upperdir; dst = "c:\\Users\\ContainerAdministrator\\AppData\\Local\\opam"; options = [ "rw"; "rbind"; "rprivate" ] };
        { ty = "bind"; src = config.opam_repository; dst = "c:\\users\\ContainerAdministrator\\AppData\\Local\\opam\\repo\\default"; options = [ "rbind"; "rprivate" ] };
      ]
    in
    let config = Json_config.make_ctr ~root:upperdir ~cwd:"c:\\" ~argv ~hostname ~uid:0 ~gid:0 ~env:win_env ~mounts ~network:true in
    let config_json = Os.path [ temp_dir; "config.json" ] in
    let () = Os.write_to_file config_json (Yojson.Safe.pretty_to_string config) in
    let result = Os.exec ~stdout:build_log ~stderr:build_log [ "ctr"; "run"; "--cni"; "--rm"; "--config"; config_json; Filename.basename temp_dir ] in
    let () = Os.write_to_file (Os.path [ temp_dir; "status" ]) (string_of_int result) in
    let _ = Os.rm (Os.path [ upperdir; "repo"; "state-33BF9E46.cache" ] ) in
    Unix.rename temp_dir target_dir
    )
  in
  let () = if not (Sys.file_exists layer_dir) then Os.create_directory_exclusively layer_dir write_layer in
  let () = Os.write_to_file (Os.path [ layer_dir; "last_used" ]) (Unix.time () |> string_of_float) in
  let exit_status = Os.read_from_file (Os.path [ layer_dir; "status" ]) |> int_of_string in
  let log = Os.read_from_file (Os.path [ layer_dir; "build.log" ]) in
  match exit_status with
  | 0 -> Success log
  | _ -> Failure log

let build config ocaml_version package =
  let solution = solve config ocaml_version package in
  if OpamPackage.Map.is_empty solution then
    [ No_solution ]
  else
    let dependencies = OpamPackage.Map.mapi (fun pkg deps -> find_all_deps solution deps (OpamPackage.Set.singleton pkg)) solution in
    let ordered_installation = topological_sort solution in
    List.fold_left
      (fun lst pkg ->
        match lst with
        | [] -> [ build_layer config solution dependencies pkg ]
        | Success _ :: _ -> build_layer config solution dependencies pkg :: lst
        | _ -> Dependency_failed :: lst)
      [] ordered_installation

let ocaml_version = OpamPackage.create (OpamPackage.Name.of_string "ocaml") (OpamPackage.Version.of_string "5.3.0")

open Cmdliner

let output config results = function
  | Some filename ->
    let oc = open_out filename in
    let cmd = Printf.sprintf "git -C %s rev-parse HEAD" config.opam_repository in
    let opam_repo_sha = Os.run cmd |> String.trim in
    let () = Printf.fprintf oc "---\nstatus: %s\ncommit: %s\npackage: %s\n---\n\n"
      (build_result_to_string (List.hd results))
      opam_repo_sha config.package in
    let () = List.rev results |> List.iter (function
      | Success log
      | Failure log -> output_string oc log
      | _ -> ()) in
    close_out oc
  | None ->
    print_string (build_result_to_string (List.hd results))

let run_ci config md =
  init config;
  let package = OpamPackage.of_string (config.package ^ ".dev") in
  let results = build config ocaml_version package in
  output config results md

let run_health_check config md =
  init config;
  let package = OpamPackage.of_string config.package in
  let results = build config ocaml_version package in
  output config results md

let cache_dir_term =
  let doc = "Directory to use for caching (required)" in
  Arg.(required & opt (some string) None & info [ "cache-dir" ] ~docv:"DIR" ~doc)

let opam_repository_term =
  let doc = "Directory containing opam repository (required)" in
  Arg.(required & opt (some string) None & info [ "opam-repository" ] ~docv:"OPAM-REPO" ~doc)

let md_term =
  let doc = "Output results in markdown format" in
  Arg.(value & opt (some string) None & info ["md"] ~docv:"FILE" ~doc)

let find_opam_files dir =
  try
    Sys.readdir dir |> Array.to_list |> List.filter_map (fun name -> if Filename.check_suffix name ".opam" then Some (Filename.remove_extension name) else None)
  with
  | Sys_error _ -> []

let ci_cmd =
  let directory_arg =
    let doc = "Directory to test" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DIRECTORY" ~doc)
  in
  let ci_term =
    Term.(
      const (fun dir opam_repository directory md -> run_ci { dir; opam_repository; package = List.hd (find_opam_files directory); directory = Some directory } md)
      $ cache_dir_term $ opam_repository_term $ directory_arg $ md_term)
  in
  let ci_info = Cmd.info "ci" ~doc:"Run CI tests on a directory" in
  Cmd.v ci_info ci_term

let health_check_cmd =
  let package_arg =
    let doc = "Package name to test" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let health_check_term =
    Term.(
      const (fun dir opam_repository package md -> run_health_check { dir; opam_repository; package; directory = None } md)
      $ cache_dir_term $ opam_repository_term $ package_arg $ md_term)
  in
  let health_check_info = Cmd.info "health-check" ~doc:"Run health check on a package" in
  Cmd.v health_check_info health_check_term

let main_info =
  let doc = "A tool for running CI and health checks" in
  let man =
    [
      `S Manpage.s_description;
      `P "This tool provides CI testing and health checking capabilities.";
      `P "Use '$(mname) ci DIRECTORY' to run CI tests on a directory.";
      `P "Use '$(mname) health-check PACKAGE' to run health checks on a package.";
      `P "Add --md flag to output results in markdown format.";
      `S Manpage.s_examples;
      `P "$(mname) ci --cache-dir /tmp/cache --opam-repository /tmp/opam-repository /path/to/project";
      `P "$(mname) health-check --cache-dir /tmp/cache --opam-repository /tmp/opam-repository package --md";
    ]
  in
  Cmd.info "day10" ~version:"0.0.1" ~doc ~man

let () =
  let default_term = Term.(ret (const (`Help (`Pager, None)))) in
  let cmd = Cmd.group ~default:default_term main_info [ ci_cmd; health_check_cmd ] in
  exit (Cmd.eval cmd)
