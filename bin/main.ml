module Solver = Opam_0install.Solver.Make (Opam_0install.Switch_context)

let constraints = OpamPackage.Name.Map.of_list [ (OpamPackage.Name.of_string "ocaml", (`Eq, OpamPackage.Version.of_string "5.3.0")) ]
let pp_pkg = Fmt.of_to_string OpamPackage.to_string

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
  | Ok sels -> Fmt.pr "%a@." Fmt.(list ~sep:(any " ") pp_pkg) (Solver.packages_of_result sels)
  | Error problem ->
      OpamConsole.error "No solution";
      print_endline (Solver.diagnostics problem);
      ()
