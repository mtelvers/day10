let std_env ?(ocaml_native = true) ?opam_version ~arch ~os ~os_distribution ~os_family ~os_version ~ocaml_version () = function
  | "arch" -> Some (OpamTypes.S arch)
  | "os" -> Some (OpamTypes.S os)
  | "os-distribution" -> Some (OpamTypes.S os_distribution)
  | "os-version" -> Some (OpamTypes.S os_version)
  | "os-family" -> Some (OpamTypes.S os_family)
  | "opam-version" -> Some (OpamVariable.S (Option.value ~default:OpamVersion.(to_string current) opam_version))
  (* There is no system compliler *)
  | "sys-ocaml-arch"
  | "sys-ocaml-cc"
  | "sys-ocaml-libc"
  | "sys-ocaml-system"
  | "sys-ocaml-version" ->
      Some (OpamTypes.S "")
  | "ocaml:native" -> Some (OpamTypes.B ocaml_native)
  | "ocaml:version" -> Some (OpamTypes.S (OpamPackage.version_to_string ocaml_version))
  | "enable-ocaml-beta-repository" -> None (* Fake variable? *)
  | v ->
      OpamConsole.warning "Unknown variable %S" v;
      None

(* The variables a depext filter is written against.  Across the whole
   repository they reach only for these five; deliberately narrower than
   std_env, so that anything else is left unanswered and depexts_for keeps the
   entry rather than guessing, and so that a variable with no answer outside the
   container -- npm-version, which forty packages use -- costs no warning on
   every hash of every package. *)
let platform_vars ~arch ~os ~os_distribution ~os_family ~os_version v =
  match OpamVariable.Full.to_string v with
  | "arch" -> Some (OpamTypes.S arch)
  | "os" -> Some (OpamTypes.S os)
  | "os-distribution" -> Some (OpamTypes.S os_distribution)
  | "os-family" -> Some (OpamTypes.S os_family)
  | "os-version" -> Some (OpamTypes.S os_version)
  | _ -> None

(* The platforms on which the maintainer has declared a failure expected, from
   the x-ci-accept-failures extension field.  Entries name a distribution and
   version ("debian-11") or, where the distribution has no meaningful version,
   just the distribution ("archlinux"). *)
let accept_failures opam =
  let open OpamParserTypes.FullPos in
  OpamFile.OPAM.extended opam "x-ci-accept-failures" (fun v ->
      match v.pelem with
      | List l -> List.filter_map (fun e -> match e.pelem with String s -> Some s | _ -> None) l.pelem
      | String s -> [ s ]
      | _ -> [])
  |> Option.value ~default:[]

(* For a line in a log that is appended to over the life of a base image, so
   that the entries can be told apart and read in order. *)
let timestamp () =
  let t = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02dZ" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec

let save_layer_info name pkg deps hashes rc =
  Yojson.Safe.to_file name
    (`Assoc
       [
         ("package", `String (OpamPackage.to_string pkg));
         ("exit_status", `Int rc);
         ("deps", `List (List.map (fun p -> `String (OpamPackage.to_string p)) deps));
         ("hashes", `List (List.map (fun h -> `String h) hashes));
         ("created", `Float (Unix.time ()));
       ])

(* A layer records its own size so that reporting on the cache does not mean
   walking all of it again.  The file sits beside layer.json rather than in it:
   layer.json's mtime is when the layer was last used, which is what prune
   sorts on, and rewriting that file would reset it. *)
let size_file dir = Path.(dir / "size")

let load_layer_size dir =
  match Os.read_from_file (size_file dir) with
  | s -> int_of_string_opt (String.trim s)
  | exception _ -> None

(* Best effort: a size that cannot be recorded is measured again next time,
   which is slow rather than wrong, and a cache gone read-only should not stop
   a build. *)
let save_layer_size dir size =
  try Os.write_to_file (size_file dir) (string_of_int size) with
  | _ -> ()

let layer_size dir =
  match load_layer_size dir with
  | Some size -> size
  | None ->
      let size = Os.tree_size dir in
      save_layer_size dir size;
      size

(* Measuring a layer means walking it, so measure the ones with no recorded
   size in parallel.  Each child writes its own file, which is how the result
   reaches the parent -- a forked child cannot return anything. *)
let warm_layer_sizes ?np dirs =
  match List.filter (fun dir -> Option.is_none (load_layer_size dir)) dirs with
  | [] -> 0
  | missing ->
      Os.fork ?np (fun dir -> ignore (layer_size dir)) missing;
      List.length missing

let load_layer_info_exit_status name =
  let json = Yojson.Safe.from_file name in
  Yojson.Safe.Util.(json |> member "exit_status" |> to_int)

let load_layer_info_package_name name =
  let json = Yojson.Safe.from_file name in
  Yojson.Safe.Util.(json |> member "package" |> to_string)

let solution_save name pkgs =
  Yojson.Safe.to_file name
    (`Assoc
       (OpamPackage.Map.fold
          (fun pkg deps lst -> (OpamPackage.to_string pkg, `List (OpamPackage.Set.to_list_map (fun p -> `String (OpamPackage.to_string p)) deps)) :: lst)
          pkgs []))

let solution_load name =
  let open Yojson.Safe.Util in
  Yojson.Safe.from_file name |> to_assoc
  |> List.fold_left
       (fun acc (s, l) ->
         let pkg = s |> OpamPackage.of_string in
         let deps = l |> to_list |> List.map (fun s -> s |> to_string |> OpamPackage.of_string) |> OpamPackage.Set.of_list in
         OpamPackage.Map.add pkg deps acc)
       OpamPackage.Map.empty

let create_opam_repository path =
  let path = Path.(path / "opam-repository") in
  let () = Os.mkdir path in
  let () = Os.write_to_file Path.(path / "repo") {|opam-version: "2.0"|} in
  path

let git_sha dir =
  let git_dir = Path.(dir / ".git") in
  if not (Sys.file_exists git_dir) then None
  else
    let head = Os.read_from_file Path.(git_dir / "HEAD") |> String.trim in
    if String.length head >= 5 && String.sub head 0 5 = "ref: " then
      let ref_path = String.sub head 5 (String.length head - 5) in
      let loose = Path.(git_dir / ref_path) in
      if Sys.file_exists loose then Some (Os.read_from_file loose |> String.trim)
      else
        (* ref may be packed *)
        let packed_refs = Path.(git_dir / "packed-refs") in
        if not (Sys.file_exists packed_refs) then None
        else
          let packed = Os.read_from_file packed_refs in
          let lines = String.split_on_char '\n' packed in
          let rec find = function
            | [] -> None
            | line :: rest -> (
                match String.split_on_char ' ' line with
                | [ sha; r ] when String.equal r ref_path -> Some sha
                | _ -> find rest)
          in
          find lines
    else Some head

let opam_repo_sha opam_repositories =
  List.filter_map git_sha opam_repositories |> String.concat ""
  |> function "" -> None | s -> Some s

(* A source is identified by its checksum, so where it is served from is not
   part of the layer, and a second checksum for the same file says the same
   thing again.  With none, the url has to stand in for it. *)
let effective_url u =
  match OpamFile.URL.checksum u with
  | [] -> OpamFile.URL.create (OpamFile.URL.url u)
  | checksum :: _ -> OpamFile.URL.with_checksum [ checksum ] OpamFile.URL.empty

(* The system packages this platform installs for a package.  Resolved rather
   than hashed whole, so an edit to one distribution's depexts leaves the others
   alone.  A filter that will not evaluate counts as applying: over-keying costs
   a rebuild, under-keying reuses a layer built without the package. *)
let depexts_for ~vars opam =
  OpamFile.OPAM.depexts opam
  |> List.filter (fun (_, filter) -> OpamFilter.eval_to_bool ~default:true vars filter)
  |> List.fold_left (fun acc (names, _) -> OpamSysPkg.Set.union acc names) OpamSysPkg.Set.empty

(* What day10 takes a layer to be.  Not opam's effective_part, which answers
   whether two opam files yield the same package in a switch and so discards
   depexts -- day10 installs those as part of the layer.  Built from empty so
   that a field opam adds later stays out until someone decides it belongs, and
   an opam upgrade cannot re-key every cache. *)
let effective_part ~vars opam =
  let open OpamFile.OPAM in
  let depexts = depexts_for ~vars opam in
  (* In a repository both come from the path, so the file itself carries
     neither, and asserting them makes a value that will not print. *)
  empty |> with_name_opt (name_opt opam) |> with_version_opt (version_opt opam)
  (* What is solved for.  A package's own constraints count even when the
     solution already satisfies them. *)
  |> with_depends (depends opam)
  |> with_depopts (depopts opam)
  |> with_conflicts (conflicts opam)
  |> with_conflict_class (conflict_class opam)
  |> with_available (available opam)
  (* What runs, and what it runs against. *)
  |> with_build (build opam)
  |> with_install (install opam)
  |> with_remove (remove opam)
  (* build-test was the old spelling, so moving a command between them is not a
     change. *)
  |> with_run_test (deprecated_build_test opam @ run_test opam)
  |> with_deprecated_build_doc (deprecated_build_doc opam)
  |> with_substs (substs opam)
  |> with_patches (patches opam)
  |> with_env (env opam)
  |> with_build_env (build_env opam)
  |> with_features (features opam)
  (* Where the sources come from. *)
  |> with_url_opt (Option.map effective_url (url opam))
  |> with_extra_sources (List.map (fun (basename, u) -> (basename, effective_url u)) (extra_sources opam))
  |> with_extra_files_opt (extra_files opam)
  (* A plugin installs elsewhere, so it changes the layer.  The other flags only
     steer the solver, whose answer is hashed as the closure. *)
  |> with_flags (List.filter (function OpamTypes.Pkgflag_Plugin -> true | _ -> false) (flags opam))
  (* The one extension field that alters the build environment.  The rest are
     read from the repository, not from the layer. *)
  |> with_extensions (OpamStd.String.Map.filter (fun k _ -> String.equal k "x-env-path-rewrite") (extensions opam))
  |> with_depexts (if OpamSysPkg.Set.is_empty depexts then [] else [ (depexts, OpamTypes.FBool true) ])

(* Whether asking for [variable] could make this package do anything different.
   There are two ways it can, and only the first was checked: a filter on one of
   its commands names the variable, or it has commands opam runs solely because
   the flag is set.  run-test is the second kind -- opam appends it on a plain
   "if test then", consulting no filter -- so a package written with a run-test
   field rather than a {with-test} filter kept the hash it had without tests,
   and its test job was answered by the layer a plain build left behind.

   A filter counts without being resolved, since it can depend on variables
   known only inside the container: concluding it would not fire risks
   answering a tested run from an untested layer, where the error the other way
   is a rebuild. *)
let can_act_on ~variable opam =
  let only_when_set =
    match variable with
    | "with-test" -> OpamFile.OPAM.run_test opam @ OpamFile.OPAM.deprecated_build_test opam
    | "with-doc" -> OpamFile.OPAM.deprecated_build_doc opam
    | _ -> []
  in
  only_when_set <> []
  || [ OpamFile.OPAM.build opam; OpamFile.OPAM.install opam; OpamFile.OPAM.run_test opam; OpamFile.OPAM.deprecated_build_test opam ]
     |> List.concat_map OpamFilter.commands_variables
     |> List.exists (fun v -> String.equal (OpamVariable.Full.to_string v) variable)

(* [with_test] and [with_doc] change what gets run rather than what gets
   solved, so a package with no test-only dependencies hashes the same either
   way and a tested run would be answered from an untested layer.  Only append
   when set, so a layer built without them keeps the hash it already had. *)
let layer_hash ?(with_test = false) ?(with_doc = false) ~vars opams =
  let hashes =
    List.map
      (fun opam -> opam |> effective_part ~vars |> OpamFile.OPAM.write_to_string |> OpamHash.compute_from_string |> OpamHash.to_string)
      opams
  in
  let flags = (if with_test then [ "with-test" ] else []) @ if with_doc then [ "with-doc" ] else [] in
  String.concat " " (hashes @ flags) |> Digest.string |> Digest.to_hex
