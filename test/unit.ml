open Day10

(* Assertions over the parts of day10 that need neither a container nor a
   repository: the layer lock, tree sizes, size parsing and formatting, platform
   selection, the layer hash, the accept-failures field and the dpkg merge.

   Several of these guard a bug that has already been fixed once, and say so
   where it is not obvious what the assertion is for. *)

let scratch () = Filename.temp_dir ~temp_dir:(Filename.get_temp_dir_name ()) "day10-unit-" ""
let opam_of_string s = OpamFile.OPAM.read_from_string s

(* create_directory_exclusively has to say whether this call was the one that
   wrote the directory.  A job that waited for another to build a layer used to
   be told it had built it, and so that it had already streamed the log, which
   left a failure reported with no explanation at all. *)
let lock_reports_the_writer () =
  let dir = Filename.concat (scratch ()) "layer" in
  let wrote = Os.create_directory_exclusively dir (fun d -> Unix.mkdir d 0o755) in
  let again = Os.create_directory_exclusively dir (fun d -> Unix.mkdir d 0o755) in
  wrote && not again

(* The lock is released by closing the descriptor, and the lock file removed,
   however the call returns.  Leaving either behind blocked every other job
   wanting that layer for the life of the process. *)
let lock_cleaned_up_after_failure () =
  let dir = Filename.concat (scratch ()) "layer" in
  let raised =
    match Os.create_directory_exclusively dir (fun _ -> failwith "as a build would") with
    | _ -> false
    | exception Failure _ -> true
  in
  raised && not (Sys.file_exists (dir ^ ".lock"))

let tree_size_allocates_by_block () =
  let root = scratch () in
  let sub = Filename.concat root "sub" in
  let () = Unix.mkdir sub 0o755 in
  let () = Out_channel.with_open_bin (Filename.concat sub "file") (fun oc -> Out_channel.output_string oc (String.make 5000 'x')) in
  (* Two directories at a block each, and 5000 bytes taking two more. *)
  Os.tree_size root = 4096 + 4096 + 8192

let tree_size_counts_a_hardlink_once () =
  let root = scratch () in
  let () = Out_channel.with_open_bin (Filename.concat root "file") (fun oc -> Out_channel.output_string oc (String.make 100 'x')) in
  let () = Unix.link (Filename.concat root "file") (Filename.concat root "same") in
  Os.tree_size root = 4096 + 4096

(* A unit is required, so that --max-size 40 is refused rather than read as
   forty bytes on a command that deletes without asking. *)
let sizes_parse () =
  let t = 1024 * 1024 * 1024 * 1024 in
  [
    ("40G", Some (40 * 1024 * 1024 * 1024));
    ("40GB", Some (40 * 1024 * 1024 * 1024));
    ("500M", Some (500 * 1024 * 1024));
    ("1K", Some 1024);
    ("1.5T", Some (t + (t / 2)));
    ("40", None);
    ("40B", None);
    ("banana", None);
    ("", None);
  ]
  |> List.for_all (fun (given, expected) -> Cache.size_of_string given = expected)

let sizes_format () =
  [ (0, "0.0B"); (1024, "1.0K"); (1024 * 1024, "1.0M"); (1024 * 1024 * 1024, "1.0G") ] |> List.for_all (fun (bytes, expected) -> Cache.human bytes = expected)

(* Hours below a day and whole days above, never weeks. *)
let buckets_scale_to_the_data () =
  Cache.bucket_width 1.0 = 6 && Cache.bucket_width 46.0 = 6 && Cache.bucket_width 3360.0 = 336

(* Every platform unless a component was given, and never a temp directory. *)
let platforms_are_selected () =
  let root = scratch () in
  let () = List.iter (fun d -> Unix.mkdir (Filename.concat root d) 0o755) [ "ubuntu-24.04-x86_64"; "debian-13-riscv64"; "temp-abc123" ] in
  let selected ?distribution ?version ?arch () = Cache.platforms ~distribution ~version ~arch root |> List.map fst in
  selected () = [ "debian-13-riscv64"; "ubuntu-24.04-x86_64" ]
  && selected ~arch:"riscv64" () = [ "debian-13-riscv64" ]
  && selected ~distribution:"ubuntu" () = [ "ubuntu-24.04-x86_64" ]
  && selected ~version:"24.04" () = [ "ubuntu-24.04-x86_64" ]
  && selected ~distribution:"ubuntu" ~arch:"riscv64" () = []
  && selected ~arch:"ppc64" () = []

let base = {|opam-version: "2.0"
build: [ "make" ]
depends: [ "b" ]
|}

let with_field field = Util.layer_hash [ opam_of_string (base ^ field ^ "\n") ]

(* The hash covers the effective part of the opam file: what decides how a
   package builds, and not what does not.  That is what lets a cached layer
   survive an opam-repository edit to a synopsis or a maintainer, of which there
   are a great many -- hashing the whole file would orphan the layer on every
   one of them.  Note that flags and x- fields are outside it too, so marking a
   version avoid-version, or adding x-ci-accept-failures, costs nothing. *)
let hash_ignores_metadata () =
  let plain = Util.layer_hash [ opam_of_string base ] in
  [
    {|synopsis: "x"|};
    {|description: "x"|};
    {|maintainer: "a@b.c"|};
    {|authors: [ "A" ]|};
    {|homepage: "http://x"|};
    {|bug-reports: "http://x"|};
    {|dev-repo: "git+http://x"|};
    {|license: "MIT"|};
    {|tags: [ "x" ]|};
    {|doc: "http://x"|};
    {|flags: [ avoid-version ]|};
    {|x-ci-accept-failures: [ "debian-11" ]|};
  ]
  |> List.for_all (fun field -> with_field field = plain)

(* And the other way: anything that changes what the build does has to move it,
   or a layer built from different sources answers for these ones. *)
let hash_follows_the_build () =
  let plain = Util.layer_hash [ opam_of_string base ] in
  [
    {|install: [ "make" "install" ]|};
    {|patches: [ "p.diff" ]|};
    {|substs: [ "s.in" ]|};
    {|depopts: [ "z" ]|};
    {|run-test: [ "make" "test" ]|};
    {|available: [ os != "win32" ]|};
    {|url { src: "http://x/y.tbz" checksum: [ "md5=0123456789abcdef0123456789abcdef" ] }|};
  ]
  |> List.for_all (fun field -> with_field field <> plain)
  && Util.layer_hash [ opam_of_string {|opam-version: "2.0"
build: [ "gmake" ]
depends: [ "b" ]
|} ] <> plain
  && Util.layer_hash [ opam_of_string {|opam-version: "2.0"
build: [ "make" ]
depends: [ "c" ]
|} ] <> plain

(* Asking for tests has to reach the hash, or a run with them is answered from a
   layer built without them.  Not asking has to leave the hash alone, or every
   layer already in every cache is orphaned. *)
let hash_separates_the_flags () =
  let opam = opam_of_string base in
  let plain = Util.layer_hash [ opam ] in
  plain = Util.layer_hash [ opam ]
  && plain = Util.layer_hash ~with_test:false ~with_doc:false [ opam ]
  && plain <> Util.layer_hash ~with_test:true [ opam ]
  && plain <> Util.layer_hash ~with_doc:true [ opam ]
  && Util.layer_hash ~with_test:true [ opam ] <> Util.layer_hash ~with_doc:true [ opam ]

let accept_failures_is_read () =
  Util.accept_failures (opam_of_string {|opam-version: "2.0"
x-ci-accept-failures: ["debian-11" "ubuntu-24.04"]
|}) = [ "debian-11"; "ubuntu-24.04" ]
  && Util.accept_failures (opam_of_string {|opam-version: "2.0"
x-ci-accept-failures: "archlinux"
|}) = [ "archlinux" ]
  && Util.accept_failures (opam_of_string {|opam-version: "2.0"|}) = []

(* Merging layers keeps the first copy of a path, so without this only one
   layer's dpkg status survived and every depext the others installed looked
   uninstalled. *)
let dpkg_status_merges () =
  let lower = "Package: curl\nArchitecture: amd64\nStatus: install ok installed\n" in
  let upper = "Package: curl\nArchitecture: amd64\nStatus: deinstall\n\nPackage: git\nArchitecture: amd64\nStatus: install ok installed\n" in
  let merged = Dpkg.union [ lower; upper ] in
  let stanzas = Dpkg.stanzas merged in
  (* Both packages present, and the earlier curl wins. *)
  List.length stanzas = 2
  && List.filter_map Dpkg.key stanzas = [ ("curl", "amd64"); ("git", "amd64") ]
  && List.exists (fun line -> String.equal line "Status: install ok installed") (List.concat stanzas)
  && not (List.exists (fun line -> String.equal line "Status: deinstall") (List.concat stanzas))

let checks =
  [
    ("lock reports the writer", lock_reports_the_writer);
    ("lock cleaned up after failure", lock_cleaned_up_after_failure);
    ("tree size allocates by block", tree_size_allocates_by_block);
    ("tree size counts a hardlink once", tree_size_counts_a_hardlink_once);
    ("sizes parse", sizes_parse);
    ("sizes format", sizes_format);
    ("buckets scale to the data", buckets_scale_to_the_data);
    ("platforms are selected", platforms_are_selected);
    ("hash ignores metadata", hash_ignores_metadata);
    ("hash follows the build", hash_follows_the_build);
    ("hash separates the flags", hash_separates_the_flags);
    ("accept-failures is read", accept_failures_is_read);
    ("dpkg status merges", dpkg_status_merges);
  ]

let () =
  let failed =
    List.filter
      (fun (name, check) ->
        match check () with
        | true -> false
        | false ->
            Printf.printf "FAIL %s\n" name;
            true
        | exception e ->
            Printf.printf "FAIL %s: %s\n" name (Printexc.to_string e);
            true)
      checks
  in
  match failed with
  | [] -> Printf.printf "%d checks passed\n" (List.length checks)
  | failed ->
      Printf.printf "%d of %d checks failed\n" (List.length failed) (List.length checks);
      exit 1
