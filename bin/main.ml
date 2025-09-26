module Solver = Opam_0install.Solver.Make (Dir_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let container =
  match Os.run "uname" |> String.trim with
  | "Linux" -> (module Linux : S.CONTAINER)
  | "FreeBSD" -> (module Freebsd : S.CONTAINER)
  | s when String.starts_with ~prefix:"CYGWIN_NT" s -> (module Windows : S.CONTAINER)
  | _ -> (module Dummy : S.CONTAINER)

module Container = (val container)

let init t =
  let config = Container.config ~t in
  let os_dir = Path.(config.dir / Container.os_key ~config) in
  let () = Os.mkdir ~parents:true os_dir in
  let root = Path.(os_dir / "base") in
  if not (Sys.file_exists root) then
    Os.create_directory_exclusively root @@ fun target_dir ->
    let temp_dir = Filename.temp_dir ~temp_dir:config.dir ~perms:0o755 "temp-" "" in
    let opam_repository = Util.create_opam_repository temp_dir in
    let build_log = Path.(temp_dir / "build.log") in
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
    Option.fold ~none:OpamPackage.Name.Map.empty
      ~some:(fun directory ->
        OpamPackage.Name.Map.empty
        |> OpamPackage.Name.Map.add (OpamPackage.Name.of_string config.package)
             (OpamPackage.Version.of_string "dev", OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw Path.((directory / config.package) ^ ".opam")))))
      config.directory
  in
  let context =
    Dir_context.create ~env:(Container.std_env ~config) ~constraints ~pins
      (List.map (fun opam_repository -> Path.(opam_repository / "packages")) config.opam_repositories)
  in
  let r = Solver.solve context [ OpamPackage.name config.ocaml_version; OpamPackage.name pkg ] in
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
  | Error problem -> Error (Solver.diagnostics problem)

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

(*
let reduce dependencies =
  OpamPackage.Map.map (fun u ->
      OpamPackage.Set.filter
        (fun v ->
          let others = OpamPackage.Set.remove v u in
          OpamPackage.Set.fold (fun o acc -> acc || OpamPackage.Set.mem v (OpamPackage.Map.find o dependencies)) others false |> not)
        u)
*)

let extract_dag dag root =
  let rec loop visited to_visit result =
    match to_visit with
    | [] -> result
    | pkg :: rest -> (
        if OpamPackage.Set.mem pkg visited then
          (* OpamPackage already processed, skip it *)
          loop visited rest result
        else
          (* Mark package as visited *)
          let new_visited = OpamPackage.Set.add pkg visited in
          match OpamPackage.Map.find_opt pkg dag with
          | None ->
              (* OpamPackage not found in the original map, skip it *)
              loop new_visited rest result
          | Some deps ->
              (* Add package and its dependencies to result *)
              let new_result = OpamPackage.Map.add pkg deps result in
              (* Add all dependencies to the work list *)
              let deps_list = OpamPackage.Set.fold (fun dep acc -> dep :: acc) deps [] in
              let new_to_visit = deps_list @ rest in
              loop new_visited new_to_visit new_result)
  in
  loop OpamPackage.Set.empty [ root ] OpamPackage.Map.empty

type build_result =
  | Solution of OpamTypes.package_set OpamTypes.package_map
  | No_solution of string
  | Dependency_failed
  | Failure of string
  | Success of string

let build_result_to_string = function
  | Solution _ -> "solution"
  | No_solution _ -> "no_solution"
  | Dependency_failed -> "dependency_failed"
  | Failure _ -> "failure"
  | Success _ -> "success"

let print_build_result = function
  | Solution _ -> OpamConsole.msg "solution"
  | No_solution _ -> OpamConsole.msg "no_solution"
  | Dependency_failed -> OpamConsole.warning "dependency_failed"
  | Failure _ -> OpamConsole.error "failure"
  | Success _ -> OpamConsole.note "success"

let build_layer t pkg hash ordered_deps ordered_hashes =
  let config = Container.config ~t in
  let layer_dir = Path.(config.dir / Container.os_key ~config / hash) in
  let () = Printf.printf "Layer %s: %s\n%!" (OpamPackage.to_string pkg) layer_dir in
  let layer_json = Path.(layer_dir / "layer.json") in
  let write_layer target_dir =
    let temp_dir = Filename.temp_dir ~temp_dir:config.dir ~perms:0o755 "temp-" "" in
    let opam_repo = Util.create_opam_repository temp_dir in
    let () =
      List.iter
        (fun pkg ->
          let opam_relative_path = Path.("packages" / OpamPackage.name_to_string pkg / OpamPackage.to_string pkg) in
          List.find_map
            (fun opam_repository ->
              let opam = Path.(opam_repository / opam_relative_path) in
              if Sys.file_exists opam then Some opam else None)
            config.opam_repositories
          |> Option.iter (fun src ->
                 let dst = Path.(opam_repo / opam_relative_path) in
                 let () = Os.mkdir ~parents:true dst in
                 let () = Os.cp Path.(src / "opam") Path.(dst / "opam") in
                 let src_files = Path.(src / "files") in
                 if Sys.file_exists src_files then
                   let dst_files = Path.(dst / "files") in
                   let () = Os.mkdir dst_files in
                   Sys.readdir src_files |> Array.iter (fun f -> Os.cp Path.(src_files / f) Path.(dst_files / f))))
        (pkg :: ordered_deps)
    in
    let build_log = Path.(temp_dir / "build.log") in
    let r = Container.build ~t ~temp_dir build_log pkg ordered_hashes in
    let () = Unix.rename temp_dir target_dir in
    Util.save_layer_info layer_json pkg ordered_deps ordered_hashes r
  in
  let () = if not (Sys.file_exists layer_dir) then Os.create_directory_exclusively layer_dir write_layer in
  let () = Unix.utimes layer_json 0.0 0.0 in
  let exit_status = Util.load_layer_info_exit_status layer_json in
  match exit_status with
  | 0 -> Success hash
  | _ -> Failure hash

let build config package =
  match solve config package with
  | Ok solution ->
      let () = Option.iter (fun filename -> Dot_solution.save filename solution) config.dot in
      let t = Container.init ~config in
      init t;
      let ordered_installation = topological_sort solution in
      let dependencies = pkg_deps solution ordered_installation in
      (* let solution = reduce dependencies solution in
      let positions = List.mapi (fun i x -> (x, i)) ordered_installation |> List.fold_left (fun acc (x, i) -> OpamPackage.Map.add x i acc) OpamPackage.Map.empty in *)
      (* let _ = Dot_solution.save ((OpamPackage.to_string package) ^ ".reduced.dot") solution in *)
      let results, _ =
        List.fold_left
          (fun (res, m) pkg ->
            let ordered_deps = extract_dag dependencies pkg |> topological_sort |> List.rev |> List.tl in
            let ordered_hashes =
              List.filter_map
                (fun p ->
                  match OpamPackage.Map.find p m with
                  | Success h
                  | Failure h ->
                      Some h
                  | _ -> None)
                ordered_deps
            in
            let hash = Container.layer_hash ~t (pkg :: ordered_deps) in
            (* let ordered_deps = OpamPackage.Map.find pkg dependencies |> OpamPackage.Set.to_list |>
                    List.sort (fun a b -> compare (OpamPackage.Map.find a positions) (OpamPackage.Map.find b positions)) in *)
            match res with
            | [] ->
                let r = build_layer t pkg hash ordered_deps ordered_hashes in
                ([ r ], OpamPackage.Map.add pkg r m)
            | Success _ :: _ ->
                let r = build_layer t pkg hash ordered_deps ordered_hashes in
                (r :: res, OpamPackage.Map.add pkg r m)
            | _ -> (Dependency_failed :: res, OpamPackage.Map.add pkg Dependency_failed m))
          ([], OpamPackage.Map.empty) ordered_installation
      in
      Container.deinit ~t;
      results @ [ Solution solution ]
  | Error s -> [ No_solution s ]

open Cmdliner

let run_list (config : Config.t) =
  let () = Random.self_init () in
  let all_packages =
    List.fold_left
      (fun set opam_repository ->
        let packages = Path.(opam_repository / "packages") in
        Array.fold_left
          (fun acc name ->
            Filename.concat packages name |> Sys.readdir
            |> Array.fold_left
                 (fun acc package ->
                   if package.[0] = '.' then acc else
                   let pkg = OpamPackage.of_string package in
                   let opam = Path.(packages / name / package / "opam") |> OpamFilename.raw |> OpamFile.make |> OpamFile.OPAM.read in
                   match OpamFilter.eval_to_bool ~default:false (opam_env ~config pkg) (OpamFile.OPAM.available opam) with
                   | true -> OpamPackage.Set.add pkg acc
                   | false -> acc)
                 acc)
          set (Sys.readdir packages))
      OpamPackage.Set.empty config.opam_repositories
  in
  OpamPackage.Name.Map.fold
    (fun n vset base -> OpamPackage.Set.add (OpamPackage.create n (OpamPackage.Version.Set.max_elt vset)) base)
    (OpamPackage.to_map all_packages) OpamPackage.Set.empty
  |> OpamPackage.Set.to_list_map (fun x -> (Random.bits (), x))
  |> List.sort compare |> List.map snd
  |> List.iter (fun x -> print_endline (OpamPackage.to_string x))

let output (config : Config.t) results =
  let os_key = Container.os_key ~config in
  let opam_repo_sha =
    List.map
      (fun opam_repository ->
        let cmd = Printf.sprintf "git -C %s rev-parse HEAD" opam_repository in
        Os.run cmd |> String.trim)
      config.opam_repositories
    |> String.concat ""
  in
  let () =
    Option.iter
      (fun filename ->
        let oc = open_out_bin filename in
        let () = Printf.fprintf oc "---\nstatus: %s\ncommit: %s\npackage: %s\n---\n" (build_result_to_string (List.hd results)) opam_repo_sha config.package in
        let () =
          List.rev results
          |> List.iter (function
               | Solution solution ->
                   Printf.fprintf oc "\n# Solution\n\n";
                   output_string oc (Dot_solution.to_string solution)
               | Success hash
               | Failure hash ->
                   let package = Util.load_layer_info_package_name Path.(config.dir / os_key / hash / "layer.json") in
                   Printf.fprintf oc "\n# %s\n\n" package;
                   let build_log = Os.read_from_file Path.(config.dir / os_key / hash / "build.log") in
                   output_string oc build_log
               | No_solution log -> output_string oc log
               | _ -> ())
        in
        close_out oc)
      config.md
  in
  let () =
    Option.iter
      (fun filename ->
        let hash =
          List.find_map
            (function
              | Success hash
              | Failure hash ->
                  Some hash
              | _ -> None)
            results
        in
        let solution =
          List.find_map
            (function
              | Solution s -> Some (Dot_solution.to_string s)
              | No_solution s -> Some s
              | _ -> None)
            results
        in
        let j =
          `Assoc
            ([ ("name", `String config.package); ("status", `String (build_result_to_string (List.hd results))); ("sha", `String opam_repo_sha) ]
            @ Option.fold ~none:[]
                ~some:(fun hash ->
                  let build_log = Os.read_from_file Path.(config.dir / os_key / hash / "build.log") in
                  [ ("layer", `String hash); ("log", `String build_log) ])
                hash
            @ Option.fold ~none:[] ~some:(fun s -> [ ("solution", `String s) ]) solution)
        in
        Yojson.Safe.to_file filename j)
      config.json
  in
  print_build_result (List.hd results)

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

let ocaml_version_term =
  let doc = "OCaml version to use (default 5.3.0)" in
  Arg.(value & opt string "ocaml.5.3.0" & info [ "ocaml-version" ] ~docv:"VERSION" ~doc)

let opam_repository_term =
  let doc = "Directory containing opam repositories (required, can be specified multiple times)" in
  Arg.(non_empty & opt_all string [] & info [ "opam-repository" ] ~docv:"OPAM-REPO" ~doc)

let md_term =
  let doc = "Output results in markdown format" in
  Arg.(value & opt (some string) None & info [ "md" ] ~docv:"FILE" ~doc)

let json_term =
  let doc = "Output results in json format" in
  Arg.(value & opt (some string) None & info [ "json" ] ~docv:"FILE" ~doc)

let dot_term =
  let doc = "Save solution in Graphviz DOT format" in
  Arg.(value & opt (some string) None & info [ "dot" ] ~docv:"FILE" ~doc)

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
      const (fun dir ocaml_version opam_repositories directory md json dot ->
          let ocaml_version = OpamPackage.of_string ocaml_version in
          run_ci { dir; ocaml_version; opam_repositories; package = List.hd (find_opam_files directory); directory = Some directory; md; json; dot })
      $ cache_dir_term $ ocaml_version_term $ opam_repository_term $ directory_arg $ md_term $ json_term $ dot_term)
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
      const (fun dir ocaml_version opam_repositories package md json dot ->
          let ocaml_version = OpamPackage.of_string ocaml_version in
          run_health_check { dir; ocaml_version; opam_repositories; package; directory = None; md; json; dot })
      $ cache_dir_term $ ocaml_version_term $ opam_repository_term $ package_arg $ md_term $ json_term $ dot_term)
  in
  let health_check_info = Cmd.info "health-check" ~doc:"Run health check on a package" in
  Cmd.v health_check_info health_check_term

let list_cmd =
  let list_term =
    Term.(
      const (fun ocaml_version opam_repositories ->
          let ocaml_version = OpamPackage.of_string ocaml_version in
          run_list { dir = ""; ocaml_version; opam_repositories; package = ""; directory = None; md = None; json = None; dot = None })
      $ ocaml_version_term $ opam_repository_term)
  in
  let list_info = Cmd.info "list" ~doc:"List packages in opam repositories" in
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
      `P "$(mname) health-check --cache-dir /tmp/cache --opam-repositories /tmp/opam-repository package --md";
      `P "$(mname) list --opam-repositories /tmp/opam-repository";
    ]
  in
  Cmd.info "day10" ~version:"0.0.1" ~doc ~man

let () =
  let default_term = Term.(ret (const (`Help (`Pager, None)))) in
  let cmd = Cmd.group ~default:default_term main_info [ ci_cmd; health_check_cmd; list_cmd ] in
  exit (Cmd.eval cmd)
