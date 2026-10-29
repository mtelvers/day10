(* dpkg keeps one file, /var/lib/dpkg/status, holding a stanza per package:
   RFC822-style fields, stanzas separated by a blank line, continuation lines
   indented.  Every layer's copy is the whole file, so merging layers leaves one
   of them standing and every depext the others installed looks uninstalled.
   The union below puts them all back. *)

let stanzas content =
  let lines = String.split_on_char '\n' content in
  let finish current acc = if current = [] then acc else List.rev current :: acc in
  let current, acc =
    List.fold_left (fun (current, acc) line -> if String.trim line = "" then ([], finish current acc) else (line :: current, acc)) ([], []) lines
  in
  List.rev (finish current acc)

(* Continuation lines are indented, so they cannot be mistaken for a field. *)
let field name stanza =
  let prefix = name ^ ": " in
  List.find_map (fun line -> if String.starts_with ~prefix line then Some (String.sub line (String.length prefix) (String.length line - String.length prefix)) else None) stanza

(* Package and Architecture together, since the same package may be installed
   for more than one architecture. *)
let key stanza = Option.map (fun package -> (package, Option.value ~default:"" (field "Architecture" stanza))) (field "Package" stanza)

(* opam's comparison is Debian's for the version proper -- tildes, letters,
   digit runs -- but opam versions carry no epoch, so that part is split off
   here: a number before the first colon, which outranks the rest. *)
let compare_versions a b =
  let split v =
    match String.index_opt v ':' with
    | Some i when i > 0 && String.for_all (fun c -> c >= '0' && c <= '9') (String.sub v 0 i) -> (int_of_string (String.sub v 0 i), String.sub v (i + 1) (String.length v - i - 1))
    | _ -> (0, v)
  in
  let ea, va = split a and eb, vb = split b in
  match Int.compare ea eb with 0 -> OpamVersionCompare.compare va vb | c -> c

module Keys = Map.Make (struct
  type t = string * string

  let compare = compare
end)

(* The highest version of each package, and the earlier copy when they tie.

   A layer that upgraded a package carries the new stanza; a sibling that never
   touched it still carries the base's old one, and which of them comes first
   is an accident of the dependency order.  The files on disk are the upgraded
   ones whatever that order, since a layer that did not touch the package has
   none of its files to overwrite them with, so the status has to say the same.
   The order of first appearance is kept, so the file reads as the layers'
   copies did. *)
let union contents =
  let newer stanza current =
    match (field "Version" stanza, field "Version" current) with Some v, Some w -> compare_versions v w > 0 | _ -> false
  in
  let best, order =
    List.fold_left
      (fun (best, order) stanza ->
        match key stanza with
        | None -> (best, order)
        | Some k -> (
            match Keys.find_opt k best with
            | None -> (Keys.add k stanza best, k :: order)
            | Some current -> ((if newer stanza current then Keys.add k stanza best else best), order)))
      (Keys.empty, [])
      (List.concat_map stanzas contents)
  in
  List.rev order |> List.map (fun k -> String.concat "\n" (Keys.find k best)) |> String.concat "\n\n" |> fun merged -> merged ^ "\n"
