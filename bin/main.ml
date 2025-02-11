module Solver = Opam_0install.Solver.Make (Opam_0install.Switch_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let constraints = OpamPackage.Name.Map.of_list [ (OpamPackage.Name.of_string "ocaml", (`Eq, OpamPackage.Version.of_string "5.3.0")) ]

let _ =
  let root = OpamStateConfig.opamroot () in
  OpamFormatConfig.init ();
  ignore (OpamStateConfig.load_defaults root);
  OpamCoreConfig.init ();
  OpamStateConfig.init ();
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none gt @@ fun st ->
  let context = Opam_0install.Switch_context.create ~constraints st in
  let r = Solver.solve context [ OpamPackage.Name.of_string "obuilder" ] in
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
      let pkgs = Solver.packages_of_result out in
      let pkgnames = pkgs |> List.map OpamPackage.name |> OpamPackage.Name.Set.of_list in
      let () =
        List.iter
          (fun pkg ->
            let depopts = OpamFile.OPAM.depopts (OpamSwitchState.opam st pkg) |> OpamFormula.all_names in
            let depopts = OpamPackage.Name.Set.inter depopts pkgnames |> OpamPackage.Name.Set.to_list in
            let name = OpamPackage.name pkg in
            let deps = expand (`Opam name) @ depopts in
            List.iter (fun p -> Printf.printf "%s -> %s\n" (OpamPackage.to_string pkg) (OpamPackage.Name.to_string p)) deps)
          pkgs
      in
      ()
  | Error problem ->
      OpamConsole.error "No solution";
      print_endline (Solver.diagnostics problem);
      ()
