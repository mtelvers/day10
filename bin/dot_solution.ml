let save name pkgs =
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
  Os.write_to_file name (header ^ graph ^ "\n}\n")
