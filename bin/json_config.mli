type mount = {
  ty : string;
  src : string;
  dst : string;
  options : string list;
}

val make :
  cwd:string -> argv:string list -> hostname:string -> uid:int -> gid:int -> env:(string * string) list -> mounts:mount list -> network:bool -> Yojson.Safe.t
