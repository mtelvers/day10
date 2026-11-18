(* opam's own Cygwin, which opam init installs under its root: nothing to
   install first and nothing to tell init. *)
let install ~opam_root:_ ~script:_ = []
let init_options ~opam_root:_ = ""

(* Cygwin's setup keeps one file, /etc/setup/installed.db, with a header line
   and then a line per package: the name, the archive it came from, and a
   flag.  Every Windows layer's copy is the whole file, so merging layers leaves
   one of them standing and every depext the others installed looks
   uninstalled.  The union below puts them all back, keeping the highest version
   of each package as Dpkg.union does. *)

let installed_db root = Path.(root / ".cygwin" / "root" / "etc" / "setup" / "installed.db")
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

(* The database is kept beside each layer rather than in it, so that a build
   can merge every layer's copy. *)
let assemble ~layers ~target =
  let db = installed_db target in
  let () = Os.mkdir ~parents:true (Filename.dirname db) in
  Os.write_to_file db (union (List.map (fun layer -> Os.read_from_file Path.(layer / "installed.db")) layers))
let keep ~temp_dir root = Os.cp (installed_db root) Path.(temp_dir / "installed.db")
let discard root = [ installed_db root ]

(* setup --download into opam's own Cygwin cache, from opam's mirror -- the
   cache keeps a directory per site, so it has to be the one opam installs
   from -- a set at a time, and a package at a time when the set fails.

   Whether a build can then install from it with no network is not yet known:
   opam runs setup with --site and without --local-install, which may want
   the mirror's setup.ini.  opam fetching setup-x86_64.exe again first is no
   bar: when that fails it warns and keeps the one it has. *)
let mirror = "https://cygwin.mirror.constant.com/"

let prefetch ~opam_root sets =
  let cygwin = Path.(opam_root / ".cygwin") in
  let fetch names =
    String.concat " "
      [
        Path.(cygwin / "setup-x86_64.exe");
        "--root";
        Path.(cygwin / "root");
        "--arch x86_64 --only-site --site";
        mirror;
        "--local-package-dir";
        Path.(cygwin / "cache");
        "--download --quiet-mode noinput --no-admin --no-desktop --no-shortcuts --no-startmenu --no-write-registry --no-version-check --packages";
        String.concat "," names;
      ]
  in
  List.map
    (fun set ->
      let names = List.map OpamSysPkg.to_string (OpamSysPkg.Set.elements set) in
      match names with [ _ ] -> fetch names | _ -> fetch names ^ " || (" ^ String.concat " & " (List.map (fun n -> fetch [ n ]) names) ^ ")")
    sets
