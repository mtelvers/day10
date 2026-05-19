(* In-memory opam-repository content, indexed by [OpamPackage.t]. Backed by
   either a git revision archived via [git archive] or an on-disk directory. *)

type source =
  | Git of { path : string; rev : string }
  | Dir of string

type reader = unit -> string
type pkg_entry = { opam : reader; extras : (string * reader) list }

type t = {
  source_ids : string list;
  index : (OpamPackage.t, pkg_entry) Hashtbl.t;
  versions : (OpamPackage.Name.t, OpamPackage.Version.t list) Hashtbl.t;
  parsed : (OpamPackage.t, OpamFile.OPAM.t) Hashtbl.t;
}

let parse_source s =
  match String.rindex_opt s ':' with
  | None -> Dir s
  | Some i -> Git { path = String.sub s 0 i; rev = String.sub s (i + 1) (String.length s - i - 1) }

let resolve_git_dir path =
  let candidate = Filename.concat path ".git" in
  if Sys.file_exists candidate then candidate else path

let resolve_commit ~git_dir rev =
  Os.run (Printf.sprintf "git --git-dir=%s rev-parse --verify %s^{commit}" (Filename.quote git_dir) (Filename.quote rev))
  |> String.trim

let git_archive ~git_dir ~commit =
  Os.run (Printf.sprintf "git --git-dir=%s archive --format=tar %s packages" (Filename.quote git_dir) (Filename.quote commit))
  |> Bytes.unsafe_of_string

let classify_tar_path path =
  match String.split_on_char '/' path with
  | [ "packages"; name_dir; name_ver; "opam" ] -> (
      match OpamPackage.of_string_opt name_ver with
      | Some pkg when OpamPackage.Name.to_string (OpamPackage.name pkg) = name_dir -> Some (pkg, `Opam)
      | _ -> None)
  | "packages" :: name_dir :: name_ver :: rest when rest <> [] -> (
      match OpamPackage.of_string_opt name_ver with
      | Some pkg when OpamPackage.Name.to_string (OpamPackage.name pkg) = name_dir -> Some (pkg, `Extra (String.concat "/" rest))
      | _ -> None)
  | _ -> None

let add_version versions_acc pkg =
  let name = OpamPackage.name pkg in
  let prior = try Hashtbl.find versions_acc name with Not_found -> [] in
  Hashtbl.replace versions_acc name (OpamPackage.version pkg :: prior)

let index_buffer ~buf ~index ~versions_acc =
  let pos = ref 0 in
  let f ?global:_ (hdr : Tar.Header.t) () =
    let size = Int64.to_int hdr.Tar.Header.file_size in
    let entry_off = !pos in
    (match hdr.Tar.Header.link_indicator with
    | Tar.Header.Link.Normal -> (
        let read () = Bytes.sub_string buf entry_off size in
        match classify_tar_path hdr.Tar.Header.file_name with
        | Some (pkg, `Opam) ->
            if not (Hashtbl.mem index pkg) then Hashtbl.add index pkg { opam = read; extras = [] };
            add_version versions_acc pkg
        | Some (pkg, `Extra rel) -> (
            match Hashtbl.find_opt index pkg with
            | Some e -> Hashtbl.replace index pkg { e with extras = (rel, read) :: e.extras }
            | None -> ())
        | None -> ())
    | _ -> ());
    let open Tar.Syntax in
    let* () = Tar.seek size in
    Tar.return (Ok ())
  in
  match Tar_bytes.run buf ~pos (Tar.fold f ()) with
  | Ok () -> ()
  | Error e ->
      let msg = Format.asprintf "%a" Tar_bytes.pp_error e in
      failwith (Printf.sprintf "tar fold failed at offset %d: %s" !pos msg)

let rec collect_extras_rec ~pkg_dir ~rel acc =
  let dir = if rel = "" then pkg_dir else Filename.concat pkg_dir rel in
  match Sys.readdir dir with
  | exception Sys_error _ -> acc
  | entries ->
      Array.fold_left
        (fun acc name ->
          if name.[0] = '.' then acc
          else
            let entry_rel = if rel = "" then name else rel ^ "/" ^ name in
            let full = Filename.concat pkg_dir entry_rel in
            if Sys.is_directory full then collect_extras_rec ~pkg_dir ~rel:entry_rel acc
            else (entry_rel, fun () -> Os.read_from_file full) :: acc)
        acc entries

let collect_extras pkg_dir =
  let files_dir = Filename.concat pkg_dir "files" in
  if Sys.file_exists files_dir && Sys.is_directory files_dir then collect_extras_rec ~pkg_dir ~rel:"files" []
  else []

let index_dir ~root ~index ~versions_acc =
  let packages_dir = Filename.concat root "packages" in
  if not (Sys.file_exists packages_dir) then failwith (Printf.sprintf "Repo: %s/packages does not exist" root);
  Sys.readdir packages_dir
  |> Array.iter (fun name ->
         if name.[0] = '.' then ()
         else
           let name_dir = Filename.concat packages_dir name in
           if Sys.is_directory name_dir then
             Sys.readdir name_dir
             |> Array.iter (fun pkg_str ->
                    if pkg_str.[0] = '.' then ()
                    else
                      match OpamPackage.of_string_opt pkg_str with
                      | Some pkg when OpamPackage.Name.to_string (OpamPackage.name pkg) = name ->
                          let pkg_dir = Filename.concat name_dir pkg_str in
                          let opam_path = Filename.concat pkg_dir "opam" in
                          if Sys.file_exists opam_path then begin
                            if not (Hashtbl.mem index pkg) then
                              Hashtbl.add index pkg
                                { opam = (fun () -> Os.read_from_file opam_path); extras = collect_extras pkg_dir };
                            add_version versions_acc pkg
                          end
                      | _ -> ()))

let create sources =
  let index = Hashtbl.create 32768 in
  let versions_acc : (OpamPackage.Name.t, OpamPackage.Version.t list) Hashtbl.t = Hashtbl.create 4096 in
  let source_ids =
    List.map
      (function
        | Git { path; rev } ->
            let git_dir = resolve_git_dir path in
            let commit = resolve_commit ~git_dir rev in
            index_buffer ~buf:(git_archive ~git_dir ~commit) ~index ~versions_acc;
            commit
        | Dir root ->
            index_dir ~root ~index ~versions_acc;
            root)
      sources
  in
  let versions = Hashtbl.create (Hashtbl.length versions_acc) in
  Hashtbl.iter (fun name vs -> Hashtbl.add versions name (List.sort_uniq OpamPackage.Version.compare vs)) versions_acc;
  { source_ids; index; versions; parsed = Hashtbl.create (Hashtbl.length index) }

let source_ids t = t.source_ids
let mem t pkg = Hashtbl.mem t.index pkg
let versions t name = try Hashtbl.find t.versions name with Not_found -> []

let opam_bytes t pkg =
  match Hashtbl.find_opt t.index pkg with
  | None -> None
  | Some entry -> Some (entry.opam ())

let parse_opam ~pkg ~source_id s =
  let filename =
    OpamFile.make
      (OpamFilename.raw
         (Printf.sprintf "%s:packages/%s/%s/opam" source_id
            (OpamPackage.Name.to_string (OpamPackage.name pkg))
            (OpamPackage.to_string pkg)))
  in
  OpamFile.OPAM.read_from_string ~filename s

let opam t pkg =
  match Hashtbl.find_opt t.parsed pkg with
  | Some v -> Some v
  | None ->
      match Hashtbl.find_opt t.index pkg with
      | None -> None
      | Some entry ->
          let source_id = match t.source_ids with c :: _ -> c | [] -> "" in
          let v = parse_opam ~pkg ~source_id (entry.opam ()) in
          Hashtbl.add t.parsed pkg v;
          Some v

let warm t = Hashtbl.iter (fun pkg _ -> ignore (opam t pkg)) t.index

let materialise t pkgs ~dest =
  List.iter
    (fun pkg ->
      match Hashtbl.find_opt t.index pkg with
      | None -> ()
      | Some entry ->
          let pkg_dir =
            Filename.concat dest
              (Printf.sprintf "packages/%s/%s" (OpamPackage.Name.to_string (OpamPackage.name pkg)) (OpamPackage.to_string pkg))
          in
          Os.mkdir ~parents:true pkg_dir;
          Os.write_to_file (Filename.concat pkg_dir "opam") (entry.opam ());
          List.iter
            (fun (rel, read) ->
              let path = Filename.concat pkg_dir rel in
              Os.mkdir ~parents:true (Filename.dirname path);
              Os.write_to_file path (read ()))
            entry.extras)
    pkgs
