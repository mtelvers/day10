module Solver = Opam_0install.Solver.Make (Opam_0install.Switch_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let constraints = OpamPackage.Name.Map.of_list [ (OpamPackage.Name.of_string "ocaml", (`Eq, OpamPackage.Version.of_string "5.3.0")) ]

let classify_role x =
  match Solver.package_name x with
  | Some pkg -> `Opam pkg
  | None -> `Virtual x

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
  | Ok sels ->
      let sels = Output.to_map sels in
      Role_map.iter
        (fun role sel ->
          (*
        let _ = match Solver.package_name role with
          | Some pkg -> Printf.printf "opam %s\n" (OpamPackage.Name.to_string pkg)
          | None -> Printf.printf "virtual\n" in
           *)
          let impl = Output.unwrap sel in
          Solver.version impl
          |> Option.iter (fun pkg ->
                 Printf.printf "pkg %s.%s\n" (OpamPackage.name_to_string pkg) (OpamPackage.version_to_string pkg);
                 let name =
                   OpamFile.OPAM.depopts (OpamSwitchState.opam st pkg)
                   |> OpamFormula.all_names |> OpamPackage.Name.Set.to_list |> List.map OpamPackage.Name.to_string
                 in
                 List.iter (Printf.printf "depopt > %s\n") name);
          let pkg = classify_role role in
          let deps, _ = Solver.Input.requires role impl in
          List.iter
            (fun dep ->
              let dep = Input.dep_info dep in
              let dep_role = dep.dep_role in
              if dep.dep_importance <> `Restricts then
                Printf.printf "dep > %s\n" (Solver.package_name dep_role |> Option.map OpamPackage.Name.to_string |> Option.value ~default:"None"))
            deps)
        sels
  | Error problem ->
      OpamConsole.error "No solution";
      print_endline (Solver.diagnostics problem);
      ()
