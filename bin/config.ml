type t = {
  dir : string;
  ocaml_version : OpamPackage.t;
  opam_repository : string;
  package : string;
  directory : string option;
  md : string option;
  json : string option;
}
