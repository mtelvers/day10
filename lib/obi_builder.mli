type compiler = {
  version : string;
  ocaml_version : OpamPackage.t;
}

val make_compiler : string -> compiler

val get_compilers : Obi_config.t -> compiler list

val topological_sort : OpamPackage.Set.t OpamPackage.Map.t -> OpamPackage.t list

val find_all_deps : OpamPackage.Set.t OpamPackage.Map.t -> OpamPackage.Set.t -> OpamPackage.Set.t -> OpamPackage.Set.t

val hash_of_set : OpamPackage.Set.t -> string

val build_package : Obi_config.t -> string -> compiler -> OpamPackage.t -> unit