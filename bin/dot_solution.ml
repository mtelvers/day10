let save name pkgs =
  let quoted package = "\"" ^ OpamPackage.to_string package ^ "\"" in
  let graph =
    OpamPackage.Map.to_list pkgs
    |> List.filter_map (fun (pkg, deps) ->
           match OpamPackage.Set.to_list deps with
           | [] -> None
           | [ p ] -> Some ("  " ^ quoted pkg ^ " -> " ^ quoted p ^ ";")
           | lst -> Some ("  " ^ quoted pkg ^ " -> {" ^ (lst |> List.map quoted |> String.concat " ") ^ "}"))
    |> String.concat "\n"
  in
  Os.write_to_file name ("digraph opam {\n" ^ graph ^ "\n}\n")
