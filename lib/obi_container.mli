type mount = {
  ty : string;
  src : string;
  dst : string;
  options : string list;
}

val make_runtime_config :
  root:string ->
  cwd:string ->
  argv:string list ->
  hostname:string ->
  uid:int ->
  gid:int ->
  env:(string * string) list ->
  mounts:mount list ->
  network:bool ->
  Yojson.Safe.t

val run_container : config_file:string -> name:string -> int