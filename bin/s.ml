module type CONTAINER = sig
  type t

  val init : config:Config.t -> t
  val deinit : t:t -> unit
  val config : t:t -> Config.t
  val run : t:t -> temp_dir:string -> string -> string -> unit
  val build : t:t -> temp_dir:string -> string -> OpamPackage.t -> OpamPackage.Set.t OpamPackage.Map.t -> OpamPackage.Set.t -> unit
  val std_env : string -> OpamTypes.variable_contents option
end
