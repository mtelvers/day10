val solve : Obi_config.t -> OpamPackage.t -> OpamPackage.t -> OpamPackage.Set.t OpamPackage.Map.t

val get_all_packages : Obi_config.t -> OpamPackage.Set.t

val get_latest_packages : OpamPackage.Set.t -> OpamPackage.Set.t

val get_available_packages : Obi_config.t -> OpamPackage.Set.t -> OpamPackage.Set.t