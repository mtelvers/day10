(* Concept from opamh *)

let compiler_packages =
  List.map OpamPackage.Name.of_string
    [
      "base-bigarray";
      "base-domains";
      "base-effects";
      "base-nnp";
      "base-threads";
      "base-unix";
      (* add other archs *)
      "host-arch-x86";
      "host-system-other";
      "ocaml";
      "ocaml-base-compiler";
      "ocaml-compiler";
      "ocaml-config";
      "ocaml-options-vanilla";
    ]

let dump_state packages_dir state_file =
  let content = Sys.readdir packages_dir |> Array.to_list in
  let packages = List.filter_map (fun x -> OpamPackage.of_string_opt x) content in
  let sel_compiler = List.filter (fun x -> List.mem (OpamPackage.name x) compiler_packages) packages in
  let new_state =
    let s = OpamPackage.Set.of_list packages in
    { OpamTypes.sel_installed = s; sel_roots = s; sel_pinned = OpamPackage.Set.empty; sel_compiler = OpamPackage.Set.of_list sel_compiler }
  in
  OpamFilename.write (OpamFilename.raw state_file) (OpamFile.SwitchSelections.write_to_string new_state)
