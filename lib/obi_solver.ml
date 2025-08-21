module Solver = Opam_0install.Solver.Make (Opam_0install.Dir_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let opam_file config pkg =
  let opam_path = Obi_config.opam_repository_path config [ "packages"; OpamPackage.name_to_string pkg; OpamPackage.to_string pkg; "opam" ] in
  OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))

let std_env = Opam_0install.Dir_context.std_env ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian" ~os_family:"debian" ~os_version:"12" ()

let env pkg v =
  match OpamVariable.Full.to_string v with
  | "version" -> Some (OpamTypes.S (OpamPackage.Version.to_string (OpamPackage.version pkg)))
  | "with-test"
  | "with-dev-setup"
  | "dev"
  | "with-doc" ->
      Some (OpamTypes.B false)
  | "build" -> Some (OpamTypes.B true)
  | x -> std_env x

let solve config ocaml_version pkg =
  let constraints =
    OpamPackage.Name.Map.of_list
      [ (OpamPackage.name ocaml_version, (`Eq, OpamPackage.version ocaml_version)); (OpamPackage.name pkg, (`Eq, OpamPackage.version pkg)) ]
  in
  let context = Opam_0install.Dir_context.create ~env:std_env ~constraints (Obi_config.opam_repository_path config [ "packages" ]) in
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
            let opam = opam_file config pkg in
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

let get_all_packages config =
  let packages = Obi_config.opam_repository_path config [ "packages" ] in
  Array.fold_left
    (fun acc d -> Filename.concat packages d |> Sys.readdir |> Array.fold_left (fun acc d -> OpamPackage.Set.add (OpamPackage.of_string d) acc) acc)
    OpamPackage.Set.empty (Sys.readdir packages)

let get_latest_packages packages =
  OpamPackage.Name.Map.fold
    (fun n vset base -> OpamPackage.Set.add (OpamPackage.create n (OpamPackage.Version.Set.max_elt vset)) base)
    (OpamPackage.to_map packages) OpamPackage.Set.empty

let get_available_packages config packages =
  let pinned_names = OpamPackage.Name.Set.singleton (OpamPackage.Name.of_string "ocaml") in
  OpamPackage.Set.filter
    (fun pkg ->
      let opam = opam_file config pkg in
      let avail = OpamFile.OPAM.available opam in
      (not (OpamPackage.Name.Set.mem pkg.name pinned_names)) && OpamFilter.eval_to_bool ~default:false (fun v -> std_env (OpamVariable.Full.to_string v)) avail)
    packages