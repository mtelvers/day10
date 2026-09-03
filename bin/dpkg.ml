(* dpkg keeps one file, /var/lib/dpkg/status, holding a stanza per package:
   RFC822-style fields, stanzas separated by a blank line, continuation lines
   indented.  Merging layers keeps the first copy of any given path, so only one
   layer's status survives and every depext installed by the others looks
   uninstalled. *)

let stanzas content =
  let lines = String.split_on_char '\n' content in
  let finish current acc = if current = [] then acc else List.rev current :: acc in
  let current, acc =
    List.fold_left (fun (current, acc) line -> if String.trim line = "" then ([], finish current acc) else (line :: current, acc)) ([], []) lines
  in
  List.rev (finish current acc)

(* Package and Architecture together, since the same package may be installed
   for more than one architecture.  Continuation lines are indented, so they
   cannot be mistaken for a field. *)
let key stanza =
  let field name =
    let prefix = name ^ ": " in
    List.find_map (fun line -> if String.starts_with ~prefix line then Some (String.sub line (String.length prefix) (String.length line - String.length prefix)) else None) stanza
  in
  Option.map (fun package -> (package, Option.value ~default:"" (field "Architecture"))) (field "Package")

(* Earlier contents win, matching the layer merge itself. *)
let union contents =
  let seen = Hashtbl.create 4096 in
  let keep stanza =
    match key stanza with
    | None -> false
    | Some key -> if Hashtbl.mem seen key then false else (Hashtbl.add seen key (); true)
  in
  List.concat_map stanzas contents |> List.filter keep |> List.map (String.concat "\n") |> String.concat "\n\n" |> fun merged -> merged ^ "\n"
