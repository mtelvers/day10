open Day10

(* Assertions over the parts of day10 that need no container: the layer lock,
   tree sizes, size parsing and formatting, platform selection, the layer hash,
   the command each backend runs, reading a git-backed repository, the
   accept-failures field and the dpkg merge.

   Several of these guard a bug that has already been fixed once, and say so
   where it is not obvious what the assertion is for. *)

let scratch () = Filename.temp_dir ~temp_dir:(Filename.get_temp_dir_name ()) "day10-unit-" ""
(* Read with a filename, the way Repo.parse_opam reads one: in a repository the
   name and version come from the path rather than from the file, and both are
   part of the layer hash.  Given no filename an opam file parses as a package
   with neither, which is not a package day10 ever builds. *)
let opam_of_string ?(pkg = "a.1.0") s =
  let pkg = OpamPackage.of_string pkg in
  let path = Printf.sprintf "packages/%s/%s/opam" (OpamPackage.Name.to_string (OpamPackage.name pkg)) (OpamPackage.to_string pkg) in
  OpamFile.OPAM.read_from_string ~filename:(OpamFile.make (OpamFilename.raw path)) s

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

(* A platform's name comes apart into the three parts os_key built it from, so
   something can act on the platform and not merely select it.  The awkward ones
   are versions that are words rather than numbers. *)
let platform_names_come_apart () =
  [
    ("ubuntu-24.04-x86_64", Some ("ubuntu", "24.04", "x86_64"));
    ("debian-13-riscv64", Some ("debian", "13", "riscv64"));
    ("debian-testing-x86_64", Some ("debian", "testing", "x86_64"));
    ("opensuse-tumbleweed-x86_64", Some ("opensuse", "tumbleweed", "x86_64"));
    ("nonsense", None);
  ]
  |> List.for_all (fun (key, expected) -> Cache.components key = expected)

(* Every platform unless a component was given, and never a temp directory. *)
let platforms_are_selected () =
  let root = scratch () in
  (* A platform is a directory with a base image under it.  temp-abc123 is a
     run in progress and lost+found belongs to the filesystem; neither has one,
     which is what keeps them out however they are named. *)
  let () = List.iter (fun d -> Os.mkdir ~parents:true Path.(root / d / "base" / "fs")) [ "ubuntu-24.04-x86_64"; "debian-13-riscv64" ] in
  let () = List.iter (fun d -> Unix.mkdir (Filename.concat root d) 0o755) [ "temp-abc123"; "lost+found" ] in
  let selected ?distribution ?version ?arch () = Cache.platforms ~distribution ~version ~arch root |> List.map fst in
  selected () = [ "debian-13-riscv64"; "ubuntu-24.04-x86_64" ]
  && selected ~arch:"riscv64" () = [ "debian-13-riscv64" ]
  && selected ~distribution:"ubuntu" () = [ "ubuntu-24.04-x86_64" ]
  && selected ~version:"24.04" () = [ "ubuntu-24.04-x86_64" ]
  && selected ~distribution:"ubuntu" ~arch:"riscv64" () = []
  && selected ~arch:"ppc64" () = []

(* CentOS keeps most of the -devel and -static packages opam names as depexts
   in CodeReady Builder, and ships it disabled, so a base image that does not
   turn it on cannot install them: conf-zlib wants zlib-static on 9 and
   zlib-ng-compat-static on 10, and neither is anywhere else.  Nothing else in
   the matrix has a repository held back this way. *)
let centos_enables_codeready_builder () =
  let enabled ~distribution ~version ~os_family =
    match Dist.of_config ~os_family ~distribution ~version with None -> None | Some dist -> dist.enable_repos
  in
  enabled ~distribution:"centos" ~version:"9" ~os_family:"rhel" = Some "dnf config-manager --set-enabled crb"
  && enabled ~distribution:"centos" ~version:"10" ~os_family:"rhel" = Some "dnf config-manager --set-enabled crb"
  (* It was called something else before 9, and did not exist before 8. *)
  && enabled ~distribution:"centos" ~version:"8" ~os_family:"rhel" = Some "dnf config-manager --set-enabled powertools"
  && enabled ~distribution:"centos" ~version:"7" ~os_family:"rhel" = None
  (* Fedora shares the package manager but carries these in a repository that
     is already on, so it must not inherit the command. *)
  && enabled ~distribution:"fedora" ~version:"42" ~os_family:"fedora" = None
  && enabled ~distribution:"debian" ~version:"13" ~os_family:"debian" = None

(* The arch a caller gives is opam's name for it, which is what opam-repo-ci
   normalises to, and Docker's platform vocabulary is a different one.  Where
   the two coincide the fallthrough was right and nobody noticed the ones where
   they do not: ppc64 became linux/ppc64, the big-endian platform, which no
   image publishes a manifest for. *)
let platforms_name_the_docker_arch () =
  [
    (* opam's names, which is what actually arrives *)
    ("x86_64", "linux/amd64");
    ("x86_32", "linux/386");
    ("arm64", "linux/arm64");
    ("arm32", "linux/arm/v7");
    ("ppc64", "linux/ppc64le");
    ("s390x", "linux/s390x");
    ("riscv64", "linux/riscv64");
    (* and the uname ones, for a caller who typed what the machine calls it *)
    ("amd64", "linux/amd64");
    ("i686", "linux/386");
    ("aarch64", "linux/arm64");
    ("armv7l", "linux/arm/v7");
    ("armv6l", "linux/arm/v6");
    ("ppc64le", "linux/ppc64le");
  ]
  |> List.for_all (fun (arch, expected) -> String.equal (Dockerfile_gen.platform arch) expected)

let base = {|opam-version: "2.0"
build: [ "make" ]
depends: [ "b" ]
|}

(* The platform the depexts are resolved against, which is day10's own function
   with a default platform filled in rather than a second copy of it. *)
let vars ?(os_distribution = "debian") ?(os_family = "debian") ?(os_version = "13") () =
  Util.platform_vars ~arch:"x86_64" ~os:"linux" ~os_distribution ~os_family ~os_version

let hash ?(vars = vars ()) ?pkg file = Util.layer_hash ~vars [ opam_of_string ?pkg file ]
let with_field field = hash (base ^ field ^ "\n")

(* The hash covers the effective part of the opam file: what decides how a
   package builds, and not what does not.  That is what lets a cached layer
   survive an opam-repository edit to a synopsis or a maintainer, of which there
   are a great many -- hashing the whole file would orphan the layer on every
   one of them.  Note that flags and x- fields are outside it too, so marking a
   version avoid-version, or adding x-ci-accept-failures, costs nothing. *)
let hash_ignores_metadata () =
  let plain = hash base in
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
  let plain = hash base in
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
  && hash {|opam-version: "2.0"
build: [ "gmake" ]
depends: [ "b" ]
|} <> plain
  && hash {|opam-version: "2.0"
build: [ "make" ]
depends: [ "c" ]
|} <> plain

(* opam's own effective_part drops depexts, since they do not change what ends
   up in a switch.  For day10 they are installed as part of the layer, so they
   have to count: opam-repository PR #30785 adds gmp-static to conf-gmp.5 and
   edits nothing else, and under opam's answer the key does not move, so the
   week-old layer answers for it -- a green tick for a job that ran no part of
   the change, and a failure for the revdep the change was meant to fix. *)
let hash_follows_the_depexts () =
  with_field {|depexts: [ ["libgmp-dev"] {os-family = "debian"} ]|} <> hash base

(* But only where the depext applies.  Resolving against the platform first is
   what keeps an edit to one distribution's depexts from re-keying the other
   eighteen, which for a package as deep in the graph as conf-gmp would be most
   of every cache. *)
let depexts_are_scoped_to_the_platform () =
  let fedora = vars ~os_distribution:"fedora" ~os_family:"fedora" ~os_version:"42" () in
  let debian = vars () in
  let with_depexts gmp =
    base ^ Printf.sprintf {|depexts: [ [%s] {os-distribution = "fedora"} ["libgmp-dev"] {os-family = "debian"} ]|} gmp ^ "\n"
  in
  let before = with_depexts {|"gmp-devel"|} and after = with_depexts {|"gmp-devel" "gmp-static"|} in
  hash ~vars:fedora before <> hash ~vars:fedora after && hash ~vars:debian before = hash ~vars:debian after

(* A source is identified by its checksum, so where it is served from is not
   part of the layer, and a second checksum for the same file says the same
   thing twice rather than something new.  Hashing every checksum instead would
   have moved sixteen and a half thousand packages -- dune and ocaml-config
   among them, which are in nearly every closure -- on a change that cannot
   affect a build. *)
let hash_identifies_a_source_by_its_checksum () =
  let sha = "sha256=" ^ String.make 64 '0' in
  let url src checksums = Printf.sprintf {|url { src: "%s" checksum: [ %s ] }|} src (String.concat " " (List.map (Printf.sprintf {|"%s"|}) checksums)) in
  with_field (url "http://x/y.tbz" [ sha ]) = with_field (url "http://moved/y.tbz" [ sha ])
  && with_field (url "http://x/y.tbz" [ sha ]) = with_field (url "http://x/y.tbz" [ sha; "md5=0123456789abcdef0123456789abcdef" ])
  (* The checksum itself still counts: different bytes are a different build. *)
  && with_field (url "http://x/y.tbz" [ sha ]) <> with_field (url "http://x/y.tbz" [ "sha256=" ^ String.make 64 '1' ])

(* Which package this is counts, even though it is the only part of the layer
   that comes from the path rather than from the file.  Two packages whose opam
   files are byte for byte the same still install different things, so they
   cannot share a layer, and neither can two versions of one package. *)
let hash_follows_the_package () =
  hash ~pkg:"a.1.0" base <> hash ~pkg:"b.1.0" base && hash ~pkg:"a.1.0" base <> hash ~pkg:"a.2.0" base

(* day10 owns this hash rather than borrowing opam's, so it is worth pinning.
   Any change to it orphans every layer in every cache on every builder, which
   is a thing to decide and then to do, not to discover afterwards.  If this
   check fails, either the change was not meant, or the literal wants updating
   and the caches want rebuilding. *)
let hash_is_stable () = hash base = "29ddeddd71e75c2ceadac7c2c76c6477"

(* Whether asking for tests could change what a package does, which decides
   whether the flag reaches the hash at all.  Sharing the layer when it cannot
   is what stops a package built as someone else's dependency being rebuilt for
   every test job, and opam-repo-ci asks for tests every time -- so this has to
   say no for the eighteen thousand packages with no tests.

   It has to say yes for both ways of writing them.  Only the {with-test}
   filter was recognised, and the two packages the check was built against both
   used it; 188 of the 537 packages with tests use the run-test field instead,
   name the variable nowhere, and so were answered from the layer a plain build
   left behind -- reporting success for a test job that ran no test. *)
let acting_on_the_flag () =
  let acts ~variable file = Util.can_act_on ~variable (opam_of_string file) in
  let plain = {|opam-version: "2.0"
build: [ "make" ]
|} in
  (* Nothing to run differently, so the build and test jobs share a layer. *)
  (not (acts ~variable:"with-test" plain))
  && (not (acts ~variable:"with-doc" plain))
  (* The field opam appends on "if test then", naming no variable. *)
  && acts ~variable:"with-test" (plain ^ {|run-test: [ "dune" "runtest" ]|} ^ "\n")
  && acts ~variable:"with-test" (plain ^ {|build-test: [ "dune" "runtest" ]|} ^ "\n")
  (* The older idiom: a filter on a build command. *)
  && acts ~variable:"with-test" {|opam-version: "2.0"
build: [
  [ "make" ]
  [ "dune" "runtest" ] {with-test}
]
|}
  (* And the same both ways for documentation. *)
  && acts ~variable:"with-doc" (plain ^ {|build-doc: [ "odoc" ]|} ^ "\n")
  && acts ~variable:"with-doc" {|opam-version: "2.0"
build: [
  [ "make" ]
  [ "odoc" ] {with-doc}
]
|}
  (* Tests must not make the doc flag matter, or either would re-key both. *)
  && not (acts ~variable:"with-doc" (plain ^ {|run-test: [ "dune" "runtest" ]|} ^ "\n"))

(* Asking for tests has to reach the hash, or a run with them is answered from a
   layer built without them.  Not asking has to leave the hash alone, or every
   layer already in every cache is orphaned. *)
let hash_separates_the_flags () =
  let opam = opam_of_string base in
  let layer_hash ?with_test ?with_doc () = Util.layer_hash ?with_test ?with_doc ~vars:(vars ()) [ opam ] in
  let plain = layer_hash () in
  plain = layer_hash ()
  && plain = layer_hash ~with_test:false ~with_doc:false ()
  && plain <> layer_hash ~with_test:true ()
  && plain <> layer_hash ~with_doc:true ()
  && layer_hash ~with_test:true () <> layer_hash ~with_doc:true ()

(* A Config.t with only the fields these checks turn on set away from their
   defaults. *)
let config ?(with_test = false) ?(package = "a.1.0") ?(local_packages = []) () =
  {
    Config.dir = "";
    ocaml_version = OpamPackage.of_string "ocaml.5.4.1";
    opam_repositories = [];
    package;
    arch = "x86_64";
    os = "linux";
    os_distribution = "debian";
    os_family = "debian";
    os_version = "13";
    directory = None;
    md = None;
    json = None;
    dot = None;
    with_test;
    with_doc = false;
    tag = None;
    oci = None;
    log = false;
    dry_run = false;
    fork = None;
    build_command = None;
    local_packages;
    prefer_oldest = false;
    update_invariant = false;
    opam_jobs = None;
  }

let commands config pkg = Build_command.for_package ~config (OpamPackage.of_string pkg)

(* Asking for tests applies to the package under test and to nothing else.  This
   went wrong in both directions at once: the flag never reached the Linux build
   at all, while the Windows backend passed it for every package in the
   solution, dependencies included. *)
let with_test_reaches_only_the_target () =
  let asked = config ~with_test:true () in
  commands asked "a.1.0" = [ "day10-install -v --with-test a.1.0" ]
  && commands asked "b.2.0" = [ "day10-install -v b.2.0" ]
  && commands (config ()) "a.1.0" = [ "day10-install -v a.1.0" ]

(* A package of the project's own is pinned to the sources first, and counts as
   a target even though the version is not the one named on the command line. *)
let a_local_package_is_pinned () =
  let mine = config ~with_test:true ~package:"mine" ~local_packages:[ "mine" ] () in
  commands mine "mine.dev" = [ "opam pin -yn mine.dev $HOME/src/"; "cd src"; "day10-install -v --with-test mine.dev" ]
  && commands mine "b.2.0" = [ "day10-install -v b.2.0" ]

(* Naming the packages has to reach dune, not just the solver: dune builds every
   package in the workspace otherwise, including ones that were left out and so
   never solved for. *)
let only_packages_reaches_dune () =
  Build_command.dune ~only_packages:[] [ "@runtest" ] = "opam exec -- dune build '@runtest'"
  && Build_command.dune ~only_packages:[ "a" ] [] = "opam exec -- dune build --only-packages a"
  && Build_command.dune ~only_packages:[ "a"; "b" ] [ "@install" ] = "opam exec -- dune build --only-packages a,b '@install'"

let accept_failures_is_read () =
  Util.accept_failures (opam_of_string {|opam-version: "2.0"
x-ci-accept-failures: ["debian-11" "ubuntu-24.04"]
|}) = [ "debian-11"; "ubuntu-24.04" ]
  && Util.accept_failures (opam_of_string {|opam-version: "2.0"
x-ci-accept-failures: "archlinux"
|}) = [ "archlinux" ]
  && Util.accept_failures (opam_of_string {|opam-version: "2.0"|}) = []

let git dir args = ignore (Os.capture "git" ([ "-C"; dir ] @ args))
let rev_parse dir name = String.trim (Os.capture "git" [ "-C"; dir; "rev-parse"; name ])

(* A one-package repository in git, the package carrying a patch in files/. *)
let git_fixture () =
  let dir = Filename.concat (scratch ()) "repo" in
  let pkg_dir = Filename.concat dir "packages/p/p.1.0" in
  let () = Os.mkdir ~parents:true (Filename.concat pkg_dir "files") in
  let () = Os.write_to_file (Filename.concat dir "repo") "opam-version: \"2.0\"\n" in
  let () = Os.write_to_file (Filename.concat pkg_dir "opam") "opam-version: \"2.0\"\npatches: [ \"fix.patch\" ]\n" in
  let () = Os.write_to_file (Filename.concat pkg_dir "files/fix.patch") "the patch\n" in
  (* The identity comes through the environment rather than git -c so the
     fixture does not depend on what is configured, and the commit skips hooks:
     a throwaway repository under /tmp is not what a policy on authorship is
     for, and a global hooksPath would otherwise reach it. *)
  let () = List.iter (fun (k, v) -> Unix.putenv k v) [ ("GIT_AUTHOR_NAME", "day10 test"); ("GIT_AUTHOR_EMAIL", "test@day10.invalid"); ("GIT_COMMITTER_NAME", "day10 test"); ("GIT_COMMITTER_EMAIL", "test@day10.invalid") ] in
  let () = git dir [ "init"; "-q" ] in
  let () = git dir [ "add"; "-A" ] in
  let () = git dir [ "commit"; "-q"; "--no-verify"; "-m"; "fixture" ] in
  let () = git dir [ "tag"; "fixture-tag" ] in
  dir

(* A git-backed repository is read out of the tree rather than the working copy,
   and a package's files/ has to come with it: patches named in the opam file
   are applied from there, so losing them fails the build in a way that reads as
   a fault in the package. *)
let git_repo_keeps_files () =
  let dir = git_fixture () in
  let repo = Repo.create [ Repo.parse_source (dir ^ ":" ^ rev_parse dir "HEAD") ] in
  let dest = scratch () in
  let () = Repo.materialise repo [ OpamPackage.of_string "p.1.0" ] ~dest in
  let patch = Filename.concat dest "packages/p/p.1.0/files/fix.patch" in
  Sys.file_exists patch && String.equal (Os.read_from_file patch) "the patch\n"

(* A revision may be a commit, a tag or a tree, since a caller hands over
   whichever of those it happens to hold. *)
let any_treeish_resolves () =
  let dir = git_fixture () in
  let pkg = OpamPackage.of_string "p.1.0" in
  [ rev_parse dir "HEAD"; "fixture-tag"; rev_parse dir "HEAD^{tree}" ]
  |> List.for_all (fun rev -> Repo.opam (Repo.create [ Repo.parse_source (dir ^ ":" ^ rev) ]) pkg <> None)

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
    ("platform names come apart", platform_names_come_apart);
    ("centos enables codeready builder", centos_enables_codeready_builder);
    ("platforms name the docker arch", platforms_name_the_docker_arch);
    ("hash ignores metadata", hash_ignores_metadata);
    ("hash follows the build", hash_follows_the_build);
    ("acting on the flag", acting_on_the_flag);
    ("hash separates the flags", hash_separates_the_flags);
    ("hash follows the depexts", hash_follows_the_depexts);
    ("depexts are scoped to the platform", depexts_are_scoped_to_the_platform);
    ("hash identifies a source by its checksum", hash_identifies_a_source_by_its_checksum);
    ("hash follows the package", hash_follows_the_package);
    ("hash is stable", hash_is_stable);
    ("with-test reaches only the target", with_test_reaches_only_the_target);
    ("a local package is pinned", a_local_package_is_pinned);
    ("only-packages reaches dune", only_packages_reaches_dune);
    ("accept-failures is read", accept_failures_is_read);
    ("git repo keeps files", git_repo_keeps_files);
    ("any treeish resolves", any_treeish_resolves);
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
