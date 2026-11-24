(* A package's sources, fetched by day10 on the host and staged in the build's
   own copy of the repository, so that the container needs no network to get
   them and none of the package's own commands runs with any.

   They go in the repository's cache/ directory, laid out as opam's download
   cache (<kind>/<first two of hash>/<hash>), and the repository's repo file
   names that directory in archive-mirrors.  opam looks a source up there by
   its checksum before going upstream, exactly as it does with
   opam.ocaml.org's cache.  The repository already reaches the container on
   every platform, mounted read-only or copied in, so the sources ride in with
   it: nothing new is mounted, and nothing in the container can write to them.

   An archive with a checksum is fetched once per host, into a download cache
   in the same layout, which every build on the host shares and nothing
   prunes, and hard linked from there into the build's repository.  A build
   wanting an archive that another is fetching waits for that fetch rather
   than starting its own, so a hundred builds of one release ask upstream for
   it once.

   A source with no checksum -- an archive of a branch, ocaml-compiler.5.5 and
   the +trunk variants -- is fetched for every build, since what is at its URL
   changes, and given the checksum of what was fetched, in the build's copy of
   its opam file, so opam finds that too.  A VCS source is checked out on the
   host, also for every build, and packed into a tar, which the build's copy
   of the opam file then names in its place. *)

(* Where opam's archive-mirrors fallback points, so a source that has gone from
   upstream is found here as it would be by opam. *)
let mirror = "https://opam.ocaml.org/cache"

(* A source's place in a cache laid out as opam's, under [root]: a directory
   here or a URL such as [mirror]. *)
let path root hash = String.concat "/" (root :: OpamHash.to_path hash)

let is_vcs url = match (OpamFile.URL.url url).OpamUrl.backend with #OpamUrl.version_control -> true | `http | `rsync -> false

(* Downloads go through opam's own downloader, so day10 fetches the way opam
   would on the same machine: curl where there is one, otherwise fetch on
   FreeBSD, ftp on OpenBSD and wget elsewhere.  OPAMFETCH overrides it as it
   does opam -- a command with %{url}% and %{out}% in it, fetcher download for
   one -- read here because day10 does not load opam's environment table. *)
let configure =
  lazy
    (match Sys.getenv_opt "OPAMFETCH" with
    | None | Some "" -> ()
    | Some s ->
        let args = List.map (fun a -> (OpamTypes.CString a, None)) (OpamStd.String.split s ' ') in
        OpamRepositoryConfig.update ~download_tool:(lazy (args, `Default)) ())

(* [note] says why a download failed. *)
let download ~note url dst =
  Lazy.force configure;
  try
    OpamDownload.download_as ~quiet:true ~overwrite:true url (OpamFilename.of_string dst) |> OpamProcess.Job.run;
    true
  with
  | OpamDownload.Download_fail (s, l) ->
      note (Printf.sprintf "%s: %s" (OpamUrl.to_string url) (Option.value ~default:l s));
      false
  | Failure m | Sys_error m ->
      note (Printf.sprintf "%s: %s" (OpamUrl.to_string url) m);
      false

(* [f] given the place for [hash] under [cache], unless something is there
   already.  Another build may put the same archive there first, under a lock
   on a different checksum of it. *)
let put cache hash f =
  let e = path cache hash in
  if not (Sys.file_exists e) then begin
    Os.mkdir ~parents:true (Filename.dirname e);
    try f e with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

(* The entry under the first checksum linked under each of the others.  opam
   asks a mirror for the first one only, but which comes first is the opam
   file's business. *)
let alias ~cache = function [] -> () | first :: rest -> List.iter (fun h -> put cache h (Unix.link (path cache first))) rest

(* Store [file] under every checksum. *)
let store ~cache file hashes =
  (match hashes with first :: _ -> put cache first (Unix.rename file) | [] -> ());
  alias ~cache hashes

(* Keep a file just fetched under its checksums, or under its sha256 when it
   came with none, and return the checksums it is kept under. *)
let keep ~cache tmp hashes =
  Unix.chmod tmp 0o644;
  let hashes = if hashes = [] then [ OpamHash.compute ~kind:`SHA256 tmp ] else hashes in
  store ~cache tmp hashes;
  (* Already there under its first checksum, from another source of the same
     package: [store] left the copy just fetched. *)
  (try Sys.remove tmp with Sys_error _ -> ());
  hashes

(* [url] fetched into a new file in [dir] from upstream, its mirrors or
   opam.ocaml.org's cache, whichever first gives what [hashes] say. *)
let fetch ~note ~dir url hashes =
  let urls = (OpamFile.URL.url url :: OpamFile.URL.mirrors url) @ List.filter_map (fun h -> OpamUrl.parse_opt (path mirror h)) hashes in
  let tmp = Filename.temp_file ~temp_dir:dir ".fetch-" "" in
  let attempt u = download ~note u tmp && List.for_all (OpamHash.check_file tmp) hashes in
  if List.exists attempt urls then Some tmp
  else begin
    (try Sys.remove tmp with Sys_error _ -> ());
    None
  end

(* [f] holding a lock on [file].  The file is left behind: removing it would
   let a build that opens it afterwards lock a new file while one waiting on
   the old file is given that. *)
let with_lock file f =
  let fd = Unix.openfile file [ O_CREAT; O_WRONLY ] 0o644 in
  Fun.protect ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ()) @@ fun () ->
  Unix.lockf fd F_LOCK 0;
  f ()

(* An archive with [hashes] in the host's download cache [downloads], fetched
   unless it is there already, under a lock on its first checksum, which a
   build wanting the same archive waits on.  How it got there, or None if it
   could not be fetched.  An archive there under another of its checksums only
   is fetched again: packages rarely list one archive's checksums in different
   orders. *)
let cached ~downloads ~note url hashes =
  let first = path downloads (List.hd hashes) in
  Os.mkdir ~parents:true (Filename.dirname first);
  with_lock (first ^ ".lock") @@ fun () ->
  if Sys.file_exists first then begin
    alias ~cache:downloads hashes;
    Some "already downloaded"
  end
  else
    match fetch ~note ~dir:downloads url hashes with
    | None -> None
    | Some tmp ->
        Fun.protect ~finally:(fun () -> try Sys.remove tmp with Sys_error _ -> ()) @@ fun () ->
        let _ = keep ~cache:downloads tmp hashes in
        Some "downloaded"

(* The container cannot write to the build's repository, so a hard link to the
   download cache is safe.  A copy where there can be no link. *)
let place src dst = try Unix.link src dst with Unix.Unix_error _ -> Os.cp src dst

let stage_archive ~downloads ~cache ~note url =
  let src = OpamUrl.to_string (OpamFile.URL.url url) in
  match OpamFile.URL.checksum url with
  | [] -> (
      match fetch ~note ~dir:cache url [] with
      | None -> None
      | Some tmp ->
          let kept = keep ~cache tmp [] in
          Some (OpamFile.URL.with_checksum kept url, Printf.sprintf "with no checksum, as (%s)" (OpamHash.to_string (List.hd kept))))
  | hashes -> (
      match cached ~downloads ~note url hashes with
      | None -> None
      | Some how -> (
          try
            List.iter (fun h -> put cache h (place (path downloads h))) hashes;
            Some (url, Printf.sprintf "as (%s), %s" (OpamHash.to_string (List.hd hashes)) how)
          with
          | Unix.Unix_error (e, _, _) ->
              note (Printf.sprintf "%s: %s" src (Unix.error_message e));
              None
          | Os.Copy_error m | Sys_error m ->
              note (Printf.sprintf "%s: %s" src m);
              None))

(* Git, run in [work], a bare repository.  Failure when it fails. *)
let git ~work ?stdin args = String.trim (Os.capture ?stdin "git" ([ "-C"; work ] @ args))

let split0 s = List.filter (fun x -> x <> "") (String.split_on_char '\000' s)

(* A submodule's URL from the .gitmodules of [commit], made absolute: a
   relative one is relative to the [remote] it was fetched from, as git takes
   it. *)
let submodule_url ~work ~remote commit path =
  let blob = commit ^ ":.gitmodules" in
  let name =
    split0 (git ~work [ "config"; "--blob"; blob; "--null"; "--get-regexp"; {|^submodule\..*\.path$|} ])
    |> List.find_map (fun kv ->
           match String.index_opt kv '\n' with
           | Some i when String.sub kv (i + 1) (String.length kv - i - 1) = path ->
               let key = String.sub kv 0 i in
               Some (String.sub key 10 (String.length key - 10 - 5))
           | _ -> None)
  in
  match name with
  | None -> failwith (Printf.sprintf "no .gitmodules entry for %s" path)
  | Some name ->
      let url = git ~work [ "config"; "--blob"; blob; "--get"; "submodule." ^ name ^ ".url" ] in
      let rec up base rel =
        if String.starts_with ~prefix:"../" rel then up (Filename.dirname base) (String.sub rel 3 (String.length rel - 3))
        else if String.starts_with ~prefix:"./" rel then up base (String.sub rel 2 (String.length rel - 2))
        else base ^ "/" ^ rel
      in
      if String.starts_with ~prefix:"../" url || String.starts_with ~prefix:"./" url then up remote url else url

(* [tree] with the entry at [path] -- a submodule's gitlink -- replaced by
   [subtree], rebuilding each tree above it.  Built from objects with mktree:
   the index would want a work tree, which a bare repository has not. *)
let rec graft ~work tree path subtree =
  let name, rest = match String.index_opt path '/' with None -> (path, None) | Some i -> (String.sub path 0 i, Some (String.sub path (i + 1) (String.length path - i - 1))) in
  let entries =
    split0 (git ~work [ "ls-tree"; "-z"; tree ])
    |> List.map (fun line ->
           match String.index_opt line '\t' with
           | Some i when String.sub line (i + 1) (String.length line - i - 1) = name -> (
               match rest with
               | None -> "040000 tree " ^ subtree ^ "\t" ^ name
               | Some rest ->
                   let child = List.nth (String.split_on_char ' ' (String.sub line 0 i)) 2 in
                   "040000 tree " ^ graft ~work child rest subtree ^ "\t" ^ name)
           | _ -> line)
  in
  let input = Filename.temp_file "day10-mktree-" "" in
  Fun.protect ~finally:(fun () -> try Sys.remove input with Sys_error _ -> ()) @@ fun () ->
  Os.write_to_file input (String.concat "" (List.map (fun e -> e ^ "\000") entries));
  git ~work ~stdin:input [ "mktree"; "-z" ]

(* The tree of [commit] with every submodule in it, at the commit its gitlink
   pins, fetched one deep from the URL its .gitmodules gives, and so on down. *)
let rec tree_with_submodules ~work ~remote commit =
  let gitlinks =
    split0 (git ~work [ "ls-tree"; "-r"; "-z"; commit ])
    |> List.filter_map (fun line ->
           match (String.split_on_char ' ' line, String.index_opt line '\t') with
           | "160000" :: _ :: sha :: _, Some i -> Some (String.sub sha 0 (String.index sha '\t'), String.sub line (i + 1) (String.length line - i - 1))
           | _ -> None)
  in
  List.fold_left
    (fun tree (sha, path) ->
      let url = submodule_url ~work ~remote commit path in
      let _ = git ~work [ "fetch"; "--quiet"; "--depth"; "1"; url; sha ] in
      graft ~work tree path (tree_with_submodules ~work ~remote:url sha))
    (git ~work [ "rev-parse"; commit ^ "^{tree}" ])
    gitlinks

(* [refspec] of [remote] -- a branch, a tag or a commit, or HEAD -- fetched one
   commit deep into [work], a bare repository.  Returns the commit, not an
   annotated tag's own object.  Failure when git fails. *)
let fetch_commit ~work ~remote ~refspec =
  let _ = git ~work [ "fetch"; "--quiet"; "--depth"; "1"; remote; refspec ] in
  git ~work [ "rev-parse"; "FETCH_HEAD^{commit}" ]

(* A git source is fetched, one commit deep, into a bare repository -- a
   branch, a tag or a commit, or HEAD when the URL names none -- with its
   submodules grafted in where opam would have checked them out, and git
   archive writes it as a tar.gz straight from the objects, so nothing is
   checked out and .git is never in it.  gzip, not plain tar: the cache entry
   has no extension, and opam tells an archive's type from its first bytes,
   which a plain tar does not identify.  The URL becomes a placeholder with the
   tar's checksum: opam never fetches it, since it finds the tar in the cache
   by that checksum first.  The subpath, if any, is kept, and still applies,
   since the tar holds the whole tree.  hg and darcs are not staged: nothing in
   opam-repository uses them. *)
let stage_vcs ~cache ~note ~name url =
  let u = OpamFile.URL.url url in
  let fail m =
    note (Printf.sprintf "%s: %s" (OpamUrl.to_string u) m);
    None
  in
  match u.OpamUrl.backend with
  | `hg | `darcs -> fail "only git sources can be staged"
  | `http | `rsync -> fail "not a VCS source"
  | `git -> (
      let work = Filename.temp_dir "day10-git-" "" in
      Fun.protect ~finally:(fun () -> OpamFilename.rmdir (OpamFilename.Dir.of_string work)) @@ fun () ->
      let remote = OpamUrl.base_url u and refspec = Option.value ~default:"HEAD" u.OpamUrl.hash in
      let tmp = Filename.temp_file ~temp_dir:cache ".fetch-" ".tar.gz" in
      try
        let _ = git ~work [ "init"; "--bare"; "--quiet" ] in
        (* Ignore the tree's export-ignore and export-subst attributes, which
           would make the tar differ from the checkout opam would have used. *)
        Os.write_to_file Path.(work / "info" / "attributes") "* -export-ignore -export-subst\n";
        let revision = fetch_commit ~work ~remote ~refspec in
        let tree = tree_with_submodules ~work ~remote revision in
        (* Every entry dated the commit's own time: always in the past, even
           to a container whose clock is behind the host's, and the same tar
           every time.  An archive of the commit gets that by itself; one of a
           tree with submodules grafted in would be dated now without
           --mtime. *)
        let source =
          if tree = git ~work [ "rev-parse"; revision ^ "^{tree}" ] then [ revision ]
          else [ "--mtime=@" ^ git ~work [ "show"; "--no-patch"; "--format=%ct"; revision ]; tree ]
        in
        let _ = git ~work ([ "archive"; "--format=tar.gz"; "--prefix=" ^ name ^ "/"; "--output=" ^ tmp ] @ source) in
        let kept = keep ~cache tmp [] in
        let placeholder = OpamUrl.parse ("https://day10.invalid/" ^ name ^ ".tar.gz") in
        Some
          ( url |> OpamFile.URL.with_url placeholder |> OpamFile.URL.with_checksum kept,
            Printf.sprintf "as git archive of %s (%s)" revision (OpamHash.to_string (List.hd kept)) )
      with Failure m | Sys_error m ->
        (try Sys.remove tmp with Sys_error _ -> ());
        fail m)

(* Stage one source, returning the URL the build's opam file should give for
   it -- unchanged, given a checksum, or a placeholder for a packed checkout --
   and how it was staged.  None when it could not be. *)
let stage_url ~downloads ~cache ~note ~name url =
  if is_vcs url then stage_vcs ~cache ~note ~name url else stage_archive ~downloads ~cache ~note url

(* A source could not be staged.  The build could only fail on it, offline,
   with a message about a network the container does not have, so it is not
   run.  Carries the log, which says why. *)
exception Not_staged of string

(* [repo] is the build's copy of the repository and [in_container] the URL it
   is reached by inside the container.  Stages every source of [pkg], noting
   each in [log], and on the console under --log, and rewrites the build's
   copy of the opam file when a source needed a checksum or was packed from a
   checkout.  Not_staged when a source could not be staged. *)
let stage ~(config : Config.t) ~repo ~in_container ~log pkg =
  (* Into the log, and under --log onto the console, as opam reports, at
     [level]: a source staged is a note, a download that failed a warning,
     since a mirror may yet have it, and a source not staged at all an
     error. *)
  let say level s =
    let tag = match level with `Note -> "NOTE" | `Warning -> "WARNING" | `Error -> "ERROR" in
    Os.append_to_file log (Printf.sprintf "[%s] %s\n" tag s);
    if config.log then
      match level with `Note -> OpamConsole.note "%s" s | `Warning -> OpamConsole.warning "%s" s | `Error -> OpamConsole.error "%s" s
  in
  let note level fmt = Printf.ksprintf (say level) fmt in
  let name = OpamPackage.Name.to_string (OpamPackage.name pkg) and nv = OpamPackage.to_string pkg in
  let file = Path.(repo / "packages" / name / nv / "opam") in
  if Sys.file_exists file then begin
    let cache = Path.(repo / "cache") and downloads = Path.(config.dir / "download-cache") in
    Os.mkdir ~parents:true cache;
    Os.write_to_file Path.(repo / "repo") (Printf.sprintf "opam-version: \"2.0\"\narchive-mirrors: %S\n" in_container);
    let filename = OpamFile.make (OpamFilename.of_string file) in
    let opam = OpamFile.OPAM.read_from_string ~filename (Os.read_from_file file) in
    (* Each source paired with what the build's opam file should give for it,
       None if it could not be staged. *)
    let stage_one label url =
      let src = OpamUrl.to_string (OpamFile.URL.url url) in
      match stage_url ~downloads ~cache ~note:(say `Warning) ~name:(nv ^ "-" ^ label) url with
      | None ->
          note `Error "Could not stage %s %s" label src;
          (url, None)
      | Some (staged, how) ->
          note `Note "Staged %s %s %s" label src how;
          (url, Some staged)
    in
    let url = Option.map (stage_one "url") (OpamFile.OPAM.url opam) in
    let extra = List.map (fun (base, u) -> (base, stage_one (OpamFilename.Base.to_string base) u)) (OpamFile.OPAM.extra_sources opam) in
    let sources = Option.to_list url @ List.map snd extra in
    if List.exists (fun (_, staged) -> Option.is_none staged) sources then raise (Not_staged (Os.read_from_file log));
    if List.exists (fun (u, staged) -> staged <> Some u) sources then begin
      let staged (u, s) = Option.value ~default:u s in
      let opam =
        opam
        |> OpamFile.OPAM.with_url_opt (Option.map staged url)
        |> OpamFile.OPAM.with_extra_sources (List.map (fun (base, source) -> (base, staged source)) extra)
      in
      Os.write_to_file file (OpamFile.OPAM.write_to_string ~filename opam)
    end
  end
