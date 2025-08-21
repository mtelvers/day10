val read_from_file : string -> string
val write_to_file : string -> string -> unit
val append_to_file : string -> string -> unit

val sudo : ?stdout:string -> ?stderr:string -> string list -> int
val run : string -> string
val nproc : int
val mkdir : string -> unit

val fork : ?np:int -> ('a -> unit) -> 'a list -> unit