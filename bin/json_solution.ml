let save name pkgs =
  let () =
    Yojson.Safe.to_file name
      (`Assoc
         (OpamPackage.Map.fold
            (fun pkg deps lst -> (OpamPackage.to_string pkg, `List (OpamPackage.Set.to_list_map (fun dep -> `String (OpamPackage.to_string dep)) deps)) :: lst)
            pkgs []))
  in
  pkgs

let load name =
  let open Yojson.Safe.Util in
  Yojson.Safe.from_file name |> to_assoc
  |> List.fold_left
       (fun acc (s, l) ->
         let pkg = s |> OpamPackage.of_string in
         let deps = l |> to_list |> List.map (fun s -> s |> to_string |> OpamPackage.of_string) |> OpamPackage.Set.of_list in
         OpamPackage.Map.add pkg deps acc)
       OpamPackage.Map.empty
