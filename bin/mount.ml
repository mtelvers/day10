type t = {
  ty : string;
  src : string;
  dst : string;
  options : string list;
}

let make ~ty ~options ~src dst =
  `Assoc [ ("destination", `String dst); ("type", `String ty); ("source", `String src); ("options", `List (List.map (fun x -> `String x) options)) ]

let user_mounts = List.map @@ fun { ty; src; dst; options } -> make ~ty ~options ~src dst
