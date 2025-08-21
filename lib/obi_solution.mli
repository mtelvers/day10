val save_json : string -> OpamPackage.Set.t OpamPackage.Map.t -> OpamPackage.Set.t OpamPackage.Map.t

val load_json : string -> OpamPackage.Set.t OpamPackage.Map.t

val save_dot : string -> OpamPackage.Set.t OpamPackage.Map.t -> unit

type layer = {
  package : string;
  deps : string;
  hash : string;
  copy : float;
  build : float;
  tidy : float;
  result : int;
}

type result =
  [ `Success
  | `Failed
  | `No_solution
  ]

type status = {
  package : string;
  compiler : string;
  find_deps : float;
  sort : float;
  layers : layer list;
  result : result;
}

val layer_of_yojson : Yojson.Safe.t -> (layer, string) Result.t
val layer_to_yojson : layer -> Yojson.Safe.t
val result_of_yojson : Yojson.Safe.t -> (result, string) Result.t
val result_to_yojson : result -> Yojson.Safe.t
val status_of_yojson : Yojson.Safe.t -> (status, string) Result.t
val status_to_yojson : status -> Yojson.Safe.t