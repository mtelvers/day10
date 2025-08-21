open Cmdliner

type t = {
  opam_repository : string;
  download_cache : string;
  work_dir : string;
  ocaml_versions : string list;
  hostname : string;
  env : (string * string) list;
  limit : int option;
}

val default : t
val config_term : t Term.t
val get_config : ?config_file:string -> unit -> t

(* Config file operations *)
val to_toml_string : t -> string
val from_toml : Toml.Types.table -> t option
val load_config_file : string -> t option
val save_config_file : string -> t -> bool

(* Environment variable loading *)
val from_env_vars : unit -> t

(* Path construction helpers *)
val opam_repository_path : t -> string list -> string
val work_dir_path : t -> string list -> string
val download_cache_path : t -> string