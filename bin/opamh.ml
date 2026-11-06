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

(* The switch state of a switch whose packages directory holds [names]. *)
let dump_state_of names state_file =
  let packages = List.filter_map (fun x -> OpamPackage.of_string_opt x) names in
  let sel_compiler = List.filter (fun x -> List.mem (OpamPackage.name x) compiler_packages) packages in
  let new_state =
    let s = OpamPackage.Set.of_list packages in
    { OpamTypes.sel_installed = s; sel_roots = s; sel_pinned = OpamPackage.Set.empty; sel_compiler = OpamPackage.Set.of_list sel_compiler }
  in
  OpamFilename.write (OpamFilename.raw state_file) (OpamFile.SwitchSelections.write_to_string new_state)

let dump_state packages_dir state_file = dump_state_of (Sys.readdir packages_dir |> Array.to_list) state_file
