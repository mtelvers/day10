let hash_of_set s = s |> OpamPackage.Set.to_list |> List.map OpamPackage.to_string |> String.concat " " |> Digest.string |> Digest.to_hex

let std_env ?(ocaml_native = true) ?opam_version ~arch ~os ~os_distribution ~os_family ~os_version () = function
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
  | "ocaml:version" -> Some (OpamTypes.S "5.3.0")
  | "enable-ocaml-beta-repository" -> None (* Fake variable? *)
  | v ->
      OpamConsole.warning "Unknown variable %S" v;
      None
