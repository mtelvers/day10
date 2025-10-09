type t = {
  dir : string;
  ocaml_version : OpamPackage.t;
  opam_repositories : string list;
  package : string;
  arch : string;
  directory : string option;
  md : string option;
  json : string option;
  dot : string option;
  with_test : bool;
  tag : string option;
}
