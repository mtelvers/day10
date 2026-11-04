(* What day10 needs from the platform it runs on.  Building a package in a
   container is most of it, but each platform also does for itself what the
   others do differently: a Linux layer is root-owned and takes sudo to delete,
   a Windows one is the user's and Windows has no fork at all. *)
module type PLATFORM = sig
  type t

  val init : config:Config.t -> t
  val deinit : t:t -> unit
  val config : t:t -> Config.t

  (* Build the base image into temp_dir/fs. *)
  val run : t:t -> temp_dir:string -> string -> string -> int

  (* Build one package's layer into temp_dir/fs, on the layers named. *)
  val build : t:t -> temp_dir:string -> string -> OpamPackage.t -> string list -> int

  (* The host's architecture, as --arch defaults to it. *)
  val arch : unit -> string

  (* Delete these trees, whatever a container left in them, and say so if
     one would not go. *)
  val remove : string list -> unit

  (* A directory in the cache held for a scope, let go of quietly: see
     Cleanup. *)
  val temp_dir : string -> Cleanup.t

  (* Run [f] over the list, in parallel where the platform can, up to [np] at
     a time. *)
  val fork : ?np:int -> ('a -> unit) -> 'a list -> unit
end
