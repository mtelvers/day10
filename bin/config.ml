type t = {
  dir : string;
  ocaml_version : OpamPackage.t;
  opam_repositories : string list;
  package : string;
  arch : string;
  os : string;
  os_distribution : string;
  os_family : string;
  os_version : string;
  directory : string option;
  md : string option;
  json : string option;
  dot : string option;
  with_test : bool;
  tag : string option;
  oci : string option;
  log : bool;
  dry_run : bool;
  fork : int option;
}

let std_env ~(config : t) =
  Util.std_env ~arch:config.arch ~os:config.os ~os_distribution:config.os_distribution ~os_family:config.os_family ~os_version:config.os_version
    ~ocaml_version:config.ocaml_version ()

let os_key ~(config : t) =
  let os =
    List.map
      (fun v -> std_env ~config v |> Option.map OpamVariable.string_of_variable_contents |> Option.value ~default:"unknown")
      [ "os-distribution"; "os-version"; "arch" ]
  in
  String.concat "-" os
