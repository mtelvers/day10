(* Cygwin's setup keeps one file, /etc/setup/installed.db, with a header line
   and then a line per package: the name, the archive it came from, and a
   flag.  Every Windows layer's copy is the whole file, so merging layers leaves
   one of them standing and every depext the others installed looks
   uninstalled.  The union below puts them all back, keeping the highest version
   of each package as Dpkg.union does. *)

let header = "INSTALLED.DB 3"

(* "name name-1.2.3-1.tar.xz 0": the version is what follows the name in the
   archive's stem. *)
let parse line =
  match String.split_on_char ' ' line with
  | [ name; archive; _ ] when String.starts_with ~prefix:(name ^ "-") archive ->
      let stem = Filename.remove_extension (Filename.remove_extension archive) in
      Some (name, String.sub stem (String.length name + 1) (String.length stem - String.length name - 1))
  | _ -> None

module Names = Map.Make (String)

let union contents =
  let best, order =
    List.concat_map (String.split_on_char '\n') contents
    |> List.fold_left
         (fun (best, order) line ->
           match parse line with
           | None -> (best, order)
           | Some (name, version) -> (
               match Names.find_opt name best with
               | None -> (Names.add name (version, line) best, name :: order)
               | Some (current, _) -> ((if OpamVersionCompare.compare version current > 0 then Names.add name (version, line) best else best), order)))
         (Names.empty, [])
  in
  String.concat "\n" (header :: List.rev_map (fun name -> snd (Names.find name best)) order) ^ "\n"
