
let std_env ?(ocaml_native = true) ?opam_version ~arch ~os ~os_distribution ~os_family ~os_version ~ocaml_version () = function
  | "arch" -> Some (OpamTypes.S arch)
  | "os" -> Some (OpamTypes.S os)
  | "os-distribution" -> Some (OpamTypes.S os_distribution)
  | "os-version" -> Some (OpamTypes.S os_version)
  | "os-family" -> Some (OpamTypes.S os_family)
  | "opam-version" -> Some (OpamVariable.S (Option.value ~default:OpamVersion.(to_string current) opam_version))
  (* There is no system compliler *)
  | "sys-ocaml-arch"
  | "sys-ocaml-cc"
  | "sys-ocaml-libc"
  | "sys-ocaml-system"
  | "sys-ocaml-version" ->
      Some (OpamTypes.S "")
  | "ocaml:native" -> Some (OpamTypes.B ocaml_native)
  | "ocaml:version" -> Some (OpamTypes.S (OpamPackage.version_to_string ocaml_version))
  | "enable-ocaml-beta-repository" -> None (* Fake variable? *)
  | v ->
      OpamConsole.warning "Unknown variable %S" v;
      None

let json_list_of_set s =
  `List (OpamPackage.Set.to_list_map (fun p -> `String (OpamPackage.to_string p)) s)

let save_layer_info name pkg deps =
  Yojson.Safe.to_file name
    (`Assoc
       [ ("package", `String (OpamPackage.to_string pkg));
         ("deps", json_list_of_set deps);
         ("created", `Float (Unix.time ()))])

let solution_save name pkgs =
  Yojson.Safe.to_file name
    (`Assoc
       (OpamPackage.Map.fold
          (fun pkg deps lst -> (OpamPackage.to_string pkg, json_list_of_set deps) :: lst)
          pkgs []))

let solution_load name =
  let open Yojson.Safe.Util in
  Yojson.Safe.from_file name |> to_assoc
  |> List.fold_left
       (fun acc (s, l) ->
         let pkg = s |> OpamPackage.of_string in
         let deps = l |> to_list |> List.map (fun s -> s |> to_string |> OpamPackage.of_string) |> OpamPackage.Set.of_list in
         OpamPackage.Map.add pkg deps acc)
       OpamPackage.Map.empty
