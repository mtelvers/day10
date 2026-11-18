(* What day10 needs from the platform it runs on.  Building a package in a
   container is most of it, but each platform also does for itself what the
   others do differently: a Linux layer is root-owned and takes sudo to delete,
   a Windows one is the user's and Windows has no fork at all. *)
module type PLATFORM = sig
  type t

  val init : config:Config.t -> t
  val deinit : t:t -> unit
  val config : t:t -> Config.t

  (* Build the base image into temp_dir/fs, from the opam repository, logging
     to the build log.  [depexts] are the sets of system packages the
     repository names for this platform: the base fetches them into the
     package manager's cache, so that a build installs any of them with no
     network. *)
  val run : t:t -> temp_dir:string -> depexts:OpamSysPkg.Set.t list -> string -> string -> int

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

  (* Reclaim an orphaned [temp-*] directory a killed build left in the cache:
     release whatever it still holds that a plain delete cannot -- a mounted
     overlay, a container snapshot, a jail's mounts -- then remove it. Only safe
     when nothing is building (the caller ensures it, as [prune --base] does),
     since it does not know which temp dirs are live. *)
  val reclaim_temp : string -> unit

  (* Run [f] over the list, in parallel where the platform can, up to [np] at
     a time. *)
  val fork : ?np:int -> ('a -> unit) -> 'a list -> unit
end

(* The Unix layer under opam on Windows, Cygwin or MSYS2: what the base
   installs before opam init, what opam init is told, and what becomes of
   its package database between layers. *)
module type UNIX_LAYER = sig
  (* The commands the base runs before opam init, with opam's root at
     [opam_root] in the container.  [script name contents] makes a script
     the container can run and gives its path there. *)
  val install : opam_root:string -> script:(string -> string -> string) -> string list

  (* Options for opam init, with a leading space or empty. *)
  val init_options : opam_root:string -> string

  (* Once the [layers] are copied into [target], before a build. *)
  val assemble : layers:string list -> target:string -> unit

  (* After a container: what of opam's [root] is kept beside the layer in
     [temp_dir]. *)
  val keep : temp_dir:string -> string -> unit

  (* What to remove from opam's [root] besides opam's own locks and caches. *)
  val discard : string -> string list
end
