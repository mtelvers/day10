module Config = Obi_config
module Os = Obi_os
module Container = Obi_container
module Solver = Obi_solver
module Solution = Obi_solution
module Builder = Obi_builder
module Report = Obi_report

val setup_directories : Config.t -> string
val solve_packages : Config.t -> string -> unit
val build_packages : Config.t -> string -> unit
val generate_reports : Config.t -> string -> unit
val run : Config.t -> unit