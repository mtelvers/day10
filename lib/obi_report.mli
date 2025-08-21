val emit_page : string -> Tyxml.Html.doc -> unit

val generate_package_report : Obi_config.t -> string -> Obi_builder.compiler -> OpamPackage.t -> unit

type details = {
  compiler : Obi_builder.compiler;
  solution : OpamPackage.Set.t;
  good : OpamPackage.Set.t;
  bad : OpamPackage.Set.t;
  status : OpamPackage.Set.t;
}

type results = {
  commit : string;
  details : details list;
}

val generate_index_report : Obi_config.t -> string list -> Obi_builder.compiler list -> unit