module Solver = Opam_0install.Solver.Make (Opam_0install.Dir_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let _ =
  let root = Os.path [ Config.dir; "root" ] in
  if not (Sys.file_exists root) then
    Os.create_directory_exclusively root @@ fun target_dir ->
    let temp_dir = Filename.temp_dir ~temp_dir:Config.dir ~perms:0o755 "temp-" "" in
    let rootfs = Os.path [ temp_dir; "rootfs" ] in
    let () = Os.mkdir rootfs in
    let _ = Os.sudo [ "/usr/bin/env"; "bash"; "-c"; "docker export $(docker run -d debian:12) | sudo tar -C " ^ rootfs ^ " -x" ] in
    let opam = Os.path [ rootfs; "/usr/local/bin/opam" ] in
    let _ = Os.sudo [ "curl"; "-L"; "https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-x86_64-linux"; "-o"; opam ] in
    let _ = Os.sudo [ "sudo"; "chmod"; "+x"; opam ] in
    let _ = Os.sudo [ "cp"; "/home/mtelvers/opam_build/_build/default/bin/main.exe"; Os.path [ rootfs; "/usr/local/bin/opam-build" ] ] in
    let _ = Os.sudo [ "cp"; "/home/mtelvers/opamh/_build/default/opamh.exe"; Os.path [ rootfs; "/usr/local/bin/opamh" ] ] in
    let etc_hosts = Os.path [ temp_dir; "hosts" ] in
    let () = Os.write_to_file etc_hosts ("127.0.0.1 localhost " ^ Config.hostname) in
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
            "su - opam -c 'mkdir opam-repository'";
            {|su - opam -c 'echo opam-version: \"2.0\" > opam-repository/repo'|};
            "su - opam -c 'opam init -k local -a /home/opam/opam-repository --bare -y'";
            "su - opam -c 'opam switch create default --empty'";
          ];
      ]
    in
    let mounts =
      [
        { Json_config.ty = "bind"; src = etc_hosts; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
        (* { ty = "bind"; src = Config.opam_repository; dst = "/home/opam/opam-repository"; options = [ "rbind"; "rprivate" ] }; *)
      ]
    in
    let config = Json_config.make ~root:rootfs ~cwd:"/home/opam" ~argv ~hostname:Config.hostname ~uid:0 ~gid:0 ~env:Config.env ~mounts ~network:true in
    let () = Os.write_to_file (Os.path [ temp_dir; "config.json" ]) (Yojson.Safe.pretty_to_string config) in
    let build_log = Os.path [ temp_dir; "build.log" ] in
    let result = Os.sudo ~stdout:build_log ~stderr:build_log [ "runc"; "run"; "-b"; temp_dir; Filename.basename temp_dir ] in
    let _ = Os.sudo [ "rm"; "-f"; Os.path [ rootfs; "/home/opam/.opam/repo/state-33BF9E46.cache" ] ] in
    let () = Os.write_to_file (Os.path [ temp_dir; "status" ]) (string_of_int result) in
    Unix.rename temp_dir target_dir

let () = OpamFormatConfig.init ()
let root = OpamStateConfig.opamroot ()
let _ = OpamStateConfig.load_defaults root
let () = OpamCoreConfig.init ?debug_level:(Some 10) ?debug_sections:(Some (OpamStd.String.Map.singleton "foo" (Some 10))) ()
let std_env = Opam_0install.Dir_context.std_env ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian" ~os_family:"debian" ~os_version:"12" ()

let opam_file pkg =
  let opam_path = Os.path [ Config.opam_repository; "packages"; OpamPackage.name_to_string pkg; OpamPackage.to_string pkg; "opam" ] in
  OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))

let env pkg v =
  (*  if List.mem v OpamPackageVar.predefined_depends_variables then (Some (OpamTypes.B true))
  else *)
  match OpamVariable.Full.to_string v with
  | "version" -> Some (OpamTypes.S (OpamPackage.Version.to_string (OpamPackage.version pkg)))
  | "with-test"
  | "with-dev-setup"
  | "dev"
  | "with-doc" ->
      Some (OpamTypes.B false)
  | "build" -> Some (OpamTypes.B true)
  | x -> std_env x

let solve ocaml_version pkg =
  let constraints =
    OpamPackage.Name.Map.of_list
      [ (OpamPackage.name ocaml_version, (`Eq, OpamPackage.version ocaml_version)); (OpamPackage.name pkg, (`Eq, OpamPackage.version pkg)) ]
  in
  let context = Opam_0install.Dir_context.create ~env:std_env ~constraints (Os.path [ Config.opam_repository; "packages" ]) in
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
            let opam = opam_file pkg in
            let deps = OpamFile.OPAM.depends opam |> OpamFilter.partial_filter_formula (env pkg) in
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

let build_layer solution dependencies pkg =
  let () = Printf.printf "Layer %s: %!" (OpamPackage.to_string pkg) in
  let deps = OpamPackage.Map.find pkg solution in
  let alldeps = OpamPackage.Map.find pkg dependencies in
  let hash = hash_of_set alldeps in
  let layer_dir = Os.path [ Config.dir; hash ] in
  let () = Printf.printf "layer_dir %s\n%!" layer_dir in
  let write_layer target_dir =
    let temp_dir = Filename.temp_dir ~temp_dir:Config.dir ~perms:0o755 "temp-" "" in
    let () = Printf.printf "temp_dir %s\n%!" temp_dir in
    let argv =
      [
        "/usr/bin/env";
        "bash";
        "-c";
        String.concat " && "
          [
            "if [ -e $HOME/.opam/default/.opam-switch/switch-state ] ; then opamh make-state --output=$HOME/.opam/default/.opam-switch/switch-state --quiet; fi";
            "opam-build -v " ^ OpamPackage.to_string pkg;
          ];
      ]
    in
    let workdir = Os.path [ temp_dir; "work" ] in
    let () = Os.mkdir workdir in
    let lowerdir = Os.path [ Config.dir; "root"; "rootfs" ] in
    let upperdir = Os.path [ temp_dir; "fs" ] in
    let () = Os.mkdir upperdir in
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
                  Os.path [ Config.dir; hash_of_set (find_all_deps solution (OpamPackage.Set.singleton dep) OpamPackage.Set.empty); "fs" ];
                  upperdir;
                ]))
        deps
    in
    let etc_hosts = Os.path [ temp_dir; "hosts" ] in
    let () = Os.write_to_file etc_hosts ("127.0.0.1 localhost " ^ Config.hostname) in
    let mounts =
      [
        { Json_config.ty = "overlay"; src = "overlay"; dst = "/"; options = [ "lowerdir=" ^ lowerdir; "upperdir=" ^ upperdir; "workdir=" ^ workdir ] };
        { ty = "bind"; src = Config.opam_repository; dst = "/home/opam/.opam/repo/default"; options = [ "rbind"; "rprivate" ] };
        { ty = "bind"; src = etc_hosts; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
      ]
    in
    let () = Os.mkdir (Os.path [ temp_dir; "dummy" ]) in
    let config = Json_config.make ~root:"dummy" ~cwd:"/home/opam" ~argv ~hostname:Config.hostname ~uid:1000 ~gid:1000 ~env:Config.env ~mounts ~network:true in
    let () = Os.write_to_file (Os.path [ temp_dir; "config.json" ]) (Yojson.Safe.pretty_to_string config) in
    let build_log = Os.path [ temp_dir; "build.log" ] in
    let result = Os.sudo ~stdout:build_log ~stderr:build_log [ "runc"; "run"; "-b"; temp_dir; Filename.basename temp_dir ] in
    let _ = Os.sudo [ "rm"; "-rf"; Os.path [ upperdir; "tmp" ] ] in
    let () = Os.write_to_file (Os.path [ temp_dir; "status" ]) (string_of_int result) in
    Unix.rename temp_dir target_dir
  in
  let () = if not (Sys.file_exists layer_dir) then Os.create_directory_exclusively layer_dir write_layer in
  Os.read_from_file (Os.path [ layer_dir; "status" ]) |> int_of_string

let build ocaml_version package =
  let solution = solve ocaml_version package in
  let dependencies = OpamPackage.Map.mapi (fun pkg deps -> find_all_deps solution deps (OpamPackage.Set.singleton pkg)) solution in
  let ordered_installation = topological_sort solution in
  List.fold_left
    (fun lst pkg ->
      match lst with
      | [] -> [ build_layer solution dependencies pkg ]
      | acc :: _ when acc = 0 -> build_layer solution dependencies pkg :: lst
      | _ -> lst)
    [] ordered_installation

(*
let package = OpamPackage.of_string "ocamlformat.0.27.0"
let package = OpamPackage.of_string "0install.2.18"
*)

let package = OpamPackage.of_string "merlin.5.4.1-503"
let ocaml_version = OpamPackage.create (OpamPackage.Name.of_string "ocaml") (OpamPackage.Version.of_string "5.3.0")
let _ = build ocaml_version package
