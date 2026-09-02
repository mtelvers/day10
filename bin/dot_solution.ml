let to_string pkgs =
  let quoted package = "\"" ^ OpamPackage.to_string package ^ "\"" in
  (* Graphviz takes its nodes from the edges, so only a package that no edge
     will mention has to name itself. *)
  let depended_on = OpamPackage.Map.fold (fun _ deps acc -> OpamPackage.Set.union deps acc) pkgs OpamPackage.Set.empty in
  let graph =
    OpamPackage.Map.to_list pkgs
    |> List.filter_map (fun (pkg, deps) ->
           match OpamPackage.Set.to_list deps with
           (* Without this a solution of a single dependency-free package -- a
              conf package with nothing but depexts -- renders exactly like no
              solution at all. *)
           | [] -> if OpamPackage.Set.mem pkg depended_on then None else Some ("  " ^ quoted pkg ^ ";")
           | [ p ] -> Some ("  " ^ quoted pkg ^ " -> " ^ quoted p ^ ";")
           | lst -> Some ("  " ^ quoted pkg ^ " -> {" ^ (lst |> List.map quoted |> String.concat " ") ^ "}"))
    |> String.concat "\n"
  in
  "digraph opam {\n" ^ graph ^ "\n}\n"

let save name pkgs = Os.write_to_file name (to_string pkgs)
