module Solver = Opam_0install.Solver.Make (Dir_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let container = if Sys.win32 then (module Windows : S.CONTAINER) else (module Linux : S.CONTAINER)

module Container = (val container)

let init t =
  let config = Container.config ~t in
  let root = Os.path [ config.dir; "root" ] in
  if not (Sys.file_exists root) then
    Os.create_directory_exclusively root @@ fun target_dir ->
    let temp_dir = Filename.temp_dir ~temp_dir:config.dir ~perms:0o755 "temp-" "" in
    let opam_repository = Os.path [ temp_dir; "opam-repository" ] in
    let () = Os.mkdir opam_repository in
    let () = Os.write_to_file (Os.path [ opam_repository; "repo" ]) {|opam-version: "2.0"|} in
    let build_log = Os.path [ temp_dir; "build.log" ] in
    let _ = Container.run ~t ~temp_dir opam_repository build_log in
    Unix.rename temp_dir target_dir

let () = OpamFormatConfig.init ()

(* let root = OpamStateConfig.opamroot ()
let _ = OpamStateConfig.load_defaults root *)
let () = OpamCoreConfig.init ?debug_level:(Some 10) ?debug_sections:(Some (OpamStd.String.Map.singleton "foo" (Some 10))) ()

let opam_env ~config pkg v =
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
  | x -> Container.std_env ~config x

let solve (config : Config.t) pkg =
  let constraints =
    OpamPackage.Name.Map.of_list
      [ (OpamPackage.name config.ocaml_version, (`Eq, OpamPackage.version config.ocaml_version)); (OpamPackage.name pkg, (`Eq, OpamPackage.version pkg)) ]
  in
  let pins =
    match config.directory with
    | None -> OpamPackage.Name.Map.empty
    | Some directory ->
        OpamPackage.Name.Map.empty
        |> OpamPackage.Name.Map.add (OpamPackage.Name.of_string config.package)
             (OpamPackage.Version.of_string "dev", OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw (Os.path [ directory; config.package ^ ".opam" ]))))
  in
  let context = Dir_context.create ~env:(Container.std_env ~config) ~constraints ~pins (Os.path [ config.opam_repository; "packages" ]) in
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
            let deps = OpamFile.OPAM.depends opam |> OpamFilter.partial_filter_formula (opam_env ~config pkg) in
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
      Ok (dfs OpamPackage.Map.empty pkg)
  | Error problem ->
      OpamConsole.error "No solution";
      Error (Solver.diagnostics problem)

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

let pkg_deps solution =
  List.fold_left
    (fun map pkg ->
      let deps_direct = OpamPackage.Map.find pkg solution in
      let deps_plus_children = OpamPackage.Set.fold (fun pkg acc -> OpamPackage.Set.union acc (OpamPackage.Map.find pkg map)) deps_direct deps_direct in
      OpamPackage.Map.add pkg deps_plus_children map)
    OpamPackage.Map.empty

type build_result =
  | No_solution of string
  | Dependency_failed
  | Failure of string
  | Success of string

let build_result_to_string = function
  | No_solution _ -> "no_solution"
  | Dependency_failed -> "dependency_failed"
  | Failure _ -> "failure"
  | Success _ -> "success"

let build_layer t solution dependencies pkg =
  let config = Container.config ~t in
  let () = Printf.printf "Layer %s: %!" (OpamPackage.to_string pkg) in
  let deps = OpamPackage.Map.find pkg solution in
  let hash = Container.layer_hash ~t (OpamPackage.Set.add pkg (OpamPackage.Map.find pkg dependencies)) in
  let layer_dir = Os.path [ config.dir; hash ] in
  let () = Printf.printf "layer_dir %s\n%!" layer_dir in
  let write_layer target_dir =
    let temp_dir = Filename.temp_dir ~temp_dir:config.dir ~perms:0o755 "temp-" "" in
    let () = Printf.printf "temp_dir %s\n%!" temp_dir in
    let () = Util.save_layer_info (Os.path [ temp_dir; "layer.json" ]) pkg deps in
    let build_log = Os.path [ temp_dir; "build.log" ] in
    let () = Container.build ~t ~temp_dir build_log pkg dependencies deps in
    Unix.rename temp_dir target_dir
  in
  let () = if not (Sys.file_exists layer_dir) then Os.create_directory_exclusively layer_dir write_layer in
  let () = Os.write_to_file (Os.path [ layer_dir; "last_used" ]) (Unix.time () |> string_of_float) in
  let exit_status = Os.read_from_file (Os.path [ layer_dir; "status" ]) |> int_of_string in
  match exit_status with
  | 0 -> Success hash
  | _ -> Failure hash

let reduce dependencies =
  OpamPackage.Map.map (fun u ->
      OpamPackage.Set.filter
        (fun v ->
          let others = OpamPackage.Set.remove v u in
          OpamPackage.Set.fold (fun o acc -> acc || OpamPackage.Set.mem v (OpamPackage.Map.find o dependencies)) others false |> not)
        u)

let build config package =
  match solve config package with
  | Ok solution ->
      (*  let _ = Dot_solution.save ((OpamPackage.to_string package) ^ ".dot") solution in *)
      let t = Container.init ~config in
      init t;
      let ordered_installation = topological_sort solution in
      let dependencies = pkg_deps solution ordered_installation in
      let solution = reduce dependencies solution in
      (* let _ = Dot_solution.save ((OpamPackage.to_string package) ^ ".reduced.dot") solution in *)
      let results =
        List.fold_left
          (fun lst pkg ->
            match lst with
            | [] -> [ build_layer t solution dependencies pkg ]
            | Success _ :: _ -> build_layer t solution dependencies pkg :: lst
            | _ -> Dependency_failed :: lst)
          [] ordered_installation
      in
      Container.deinit ~t;
      results
  | Error s -> [ No_solution s ]

open Cmdliner

let run_list (config : Config.t) =
  let packages = Os.path [ config.opam_repository; "packages" ] in
  let all_packages =
    Array.fold_left
      (fun acc d -> Filename.concat packages d |> Sys.readdir |> Array.fold_left (fun acc d -> OpamPackage.Set.add (OpamPackage.of_string d) acc) acc)
      OpamPackage.Set.empty (Sys.readdir packages)
  in
  let latest =
    OpamPackage.Name.Map.fold
      (fun n vset base -> OpamPackage.Set.add (OpamPackage.create n (OpamPackage.Version.Set.max_elt vset)) base)
      (OpamPackage.to_map all_packages) OpamPackage.Set.empty
  in
  OpamPackage.Set.filter
    (fun pkg ->
      let opam =
        OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw (Os.path [ packages; OpamPackage.name_to_string pkg; OpamPackage.to_string pkg; "opam" ])))
      in
      OpamFilter.eval_to_bool ~default:false (opam_env ~config pkg) (OpamFile.OPAM.available opam))
    latest
  |> OpamPackage.Set.to_list
  |> List.iter (fun x -> print_endline (OpamPackage.to_string x))

let output (config : Config.t) results =
  let opam_repo_sha =
    if Option.is_some config.md || Option.is_some config.json then
      let cmd = Printf.sprintf "git -C %s rev-parse HEAD" config.opam_repository in
      Os.run cmd |> String.trim
    else ""
  in
  let () =
    Option.iter
      (fun filename ->
        let oc = open_out_bin filename in
        let () = Printf.fprintf oc "---\nstatus: %s\ncommit: %s\npackage: %s\n---\n" (build_result_to_string (List.hd results)) opam_repo_sha config.package in
        let () =
          List.rev results
          |> List.iter (function
               | Success hash
               | Failure hash ->
                   let package = Os.read_from_file (Os.path [ config.dir; hash; "package" ]) in
                   Printf.fprintf oc "\n# %s\n\n" package;
                   let log = Os.read_from_file (Os.path [ config.dir; hash; "build.log" ]) in
                   output_string oc log
               | No_solution log -> output_string oc log
               | _ -> ())
        in
        close_out oc)
      config.md
  in
  let () =
    Option.iter
      (fun filename ->
        let build =
          List.filter_map
            (function
              | Success hash
              | Failure hash ->
                  Some (`String hash)
              | _ -> None)
            results
        in
        let j =
          `Assoc
            [
              ("commit", `String opam_repo_sha);
              ("name", `String config.package);
              ("status", `String (build_result_to_string (List.hd results)));
              ("layers", `List build);
            ]
        in
        Yojson.Safe.to_file filename j)
      config.json
  in
  print_endline (build_result_to_string (List.hd results))

let run_ci (config : Config.t) =
  let package = OpamPackage.of_string (config.package ^ ".dev") in
  let results = build config package in
  output config results

let run_health_check (config : Config.t) =
  let package = OpamPackage.of_string config.package in
  let results = build config package in
  output config results

let cache_dir_term =
  let doc = "Directory to use for caching (required)" in
  Arg.(required & opt (some string) None & info [ "cache-dir" ] ~docv:"DIR" ~doc)

let version_term =
  let doc = "OCaml version to use (default 5.3.0)" in
  Arg.(value & opt string "5.3.0" & info [ "version" ] ~docv:"VERSION" ~doc)

let opam_repository_term =
  let doc = "Directory containing opam repository (required)" in
  Arg.(required & opt (some string) None & info [ "opam-repository" ] ~docv:"OPAM-REPO" ~doc)

let md_term =
  let doc = "Output results in markdown format" in
  Arg.(value & opt (some string) None & info [ "md" ] ~docv:"FILE" ~doc)

let json_term =
  let doc = "Output results in json format" in
  Arg.(value & opt (some string) None & info [ "json" ] ~docv:"FILE" ~doc)

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
      const (fun dir version opam_repository directory md json ->
          let ocaml_version = OpamPackage.create (OpamPackage.Name.of_string "ocaml") (OpamPackage.Version.of_string version) in
          run_ci { dir; ocaml_version; opam_repository; package = List.hd (find_opam_files directory); directory = Some directory; md; json })
      $ cache_dir_term $ version_term $ opam_repository_term $ directory_arg $ md_term $ json_term)
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
      const (fun dir version opam_repository package md json ->
          let ocaml_version = OpamPackage.create (OpamPackage.Name.of_string "ocaml") (OpamPackage.Version.of_string version) in
          run_health_check { dir; ocaml_version; opam_repository; package; directory = None; md; json })
      $ cache_dir_term $ version_term $ opam_repository_term $ package_arg $ md_term $ json_term)
  in
  let health_check_info = Cmd.info "health-check" ~doc:"Run health check on a package" in
  Cmd.v health_check_info health_check_term

let list_cmd =
  let list_term =
    Term.(
      const (fun version opam_repository ->
          let ocaml_version = OpamPackage.create (OpamPackage.Name.of_string "ocaml") (OpamPackage.Version.of_string version) in
          run_list { dir = ""; ocaml_version; opam_repository; package = ""; directory = None; md = None; json = None })
      $ version_term $ opam_repository_term)
  in
  let list_info = Cmd.info "list" ~doc:"List packages in opam repository" in
  Cmd.v list_info list_term

let main_info =
  let doc = "A tool for running CI and health checks" in
  let man =
    [
      `S Manpage.s_description;
      `P "This tool provides CI testing and health checking capabilities.";
      `P "Use '$(mname) ci DIRECTORY' to run CI tests on a directory.";
      `P "Use '$(mname) health-check PACKAGE' to run health checks on a package.";
      `P "Use '$(mname) list' list packages in opam repository.";
      `P "Add --md flag to output results in markdown format.";
      `S Manpage.s_examples;
      `P "$(mname) ci --cache-dir /tmp/cache --opam-repository /tmp/opam-repository /path/to/project";
      `P "$(mname) health-check --cache-dir /tmp/cache --opam-repository /tmp/opam-repository package --md";
      `P "$(mname) list --opam-repository /tmp/opam-repository";
    ]
  in
  Cmd.info "day10" ~version:"0.0.1" ~doc ~man

let () =
  let default_term = Term.(ret (const (`Help (`Pager, None)))) in
  let cmd = Cmd.group ~default:default_term main_info [ ci_cmd; health_check_cmd; list_cmd ] in
  exit (Cmd.eval cmd)
