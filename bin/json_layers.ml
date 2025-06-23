
type parent_layer_paths = {
  parentLayerPaths: string list; [@key "parentLayerPaths"]
} [@@deriving yojson]

type layer = {
  type_ : string;
  source : string;
  target : string;
  options : parent_layer_paths list;
}

type raw_layer = {
  raw_type_ : string; [@key "Type"]
  raw_source : string; [@key "Source"]
  raw_target : string; [@key "Target"]
  raw_options : string list; [@key "Options"]
} [@@deriving yojson]

let parse_option_string str =
  if String.starts_with ~prefix:"parentLayerPaths=" str then
    try
      let json_part = String.sub str 17 (String.length str - 17) in
      let full_json = "{\"parentLayerPaths\":" ^ json_part ^ "}" in
      Yojson.Safe.from_string full_json
      |> parent_layer_paths_of_yojson
      |> Result.to_option
    with
    | _ -> None
  else
    None

let layer_of_raw (raw_layer:raw_layer) =
  {
    type_ = raw_layer.raw_type_;
    source = raw_layer.raw_source;
    target = raw_layer.raw_target;
    options = List.filter_map parse_option_string raw_layer.raw_options;
  }

let layers_of_yojson json =
  match [%of_yojson: raw_layer list] json with
  | Ok raw_layers -> Ok (List.map layer_of_raw raw_layers)
  | Error e -> Error e

let parse_layers json_string =
  let json = Yojson.Safe.from_string json_string in
  layers_of_yojson json

let read_layers path =
  let mounts = Os.read_from_file path in
  match parse_layers mounts with
  | Ok layers -> (layers |> List.map (fun l -> List.map (fun x -> x.parentLayerPaths) l.options) |> List.flatten |> List.flatten) @ (List.map (fun l -> l.source) layers)
  | Error _ -> []

