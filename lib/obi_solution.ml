let save_json name pkgs =
  let () =
    Yojson.Safe.to_file name
      (`Assoc
         (OpamPackage.Map.fold
            (fun pkg deps lst -> (OpamPackage.to_string pkg, `List (OpamPackage.Set.to_list_map (fun dep -> `String (OpamPackage.to_string dep)) deps)) :: lst)
            pkgs []))
  in
  pkgs

let load_json name =
  let open Yojson.Safe.Util in
  Yojson.Safe.from_file name |> to_assoc
  |> List.fold_left
       (fun acc (s, l) ->
         let pkg = s |> OpamPackage.of_string in
         let deps = l |> to_list |> List.map (fun s -> s |> to_string |> OpamPackage.of_string) |> OpamPackage.Set.of_list in
         OpamPackage.Map.add pkg deps acc)
       OpamPackage.Map.empty

let save_dot name pkgs =
  let header =
    {|digraph opam {
  rankdir="LR";
  node [shape="box",fontcolor="#ffffff",color="#ef7a08",fillcolor="#ef7a08",style="filled"];
  edge [color="#888888"];
|}
  in
  let graph =
    OpamPackage.Map.fold
      (fun pkg deps acc ->
        acc @ OpamPackage.Set.to_list_map (fun dep -> "  \"" ^ OpamPackage.to_string pkg ^ "\" -> \"" ^ OpamPackage.to_string dep ^ "\";") deps)
      pkgs []
    |> String.concat "\n"
  in
  Obi_os.write_to_file name (header ^ graph ^ "\n}\n")

type layer = {
  package : string;
  deps : string;
  hash : string;
  copy : float;
  build : float;
  tidy : float;
  result : int;
}
[@@deriving yojson]

type result =
  [ `Success
  | `Failed
  | `No_solution
  ]
[@@deriving yojson]

type status = {
  package : string;
  compiler : string;
  find_deps : float;
  sort : float;
  layers : layer list;
  result : result;
}
[@@deriving yojson]