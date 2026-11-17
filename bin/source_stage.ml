(* A package's sources, fetched by day10 on the host and staged in the build's
   own copy of the repository, so that the container needs no network to get
   them and none of the package's own commands runs with any.

   They go in the repository's cache/ directory, laid out as opam's download
   cache (<kind>/<first two of hash>/<hash>), and the repository's repo file
   names that directory in archive-mirrors.  opam looks a source up there by
   its checksum before going upstream, exactly as it does with
   opam.ocaml.org's cache.  The repository already reaches the container on
   every platform, so the sources ride in with it: nothing new is mounted and
   nothing outlives the build.

   A source with no checksum -- an archive of a branch, ocaml-compiler.5.5 and
   the +trunk variants -- is fetched all the same and given the checksum of
   what was fetched, in the build's copy of its opam file, so opam finds that
   too.  A VCS source is checked out on the host and packed into a tar, which
   the build's copy of the opam file then names in its place. *)

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

let download ~log url dst =
  Lazy.force configure;
  try
    OpamDownload.download_as ~quiet:true ~overwrite:true url (OpamFilename.of_string dst) |> OpamProcess.Job.run;
    true
  with
  | OpamDownload.Download_fail (s, l) ->
      Os.append_to_file log (Printf.sprintf "day10: %s: %s\n" (OpamUrl.to_string url) (Option.value ~default:l s));
      false
  | Failure m | Sys_error m ->
      Os.append_to_file log (Printf.sprintf "day10: %s: %s\n" (OpamUrl.to_string url) m);
      false

(* Store [file] under every checksum.  opam asks a mirror for the first one
   only, but which comes first is the opam file's business. *)
let store ~cache file hashes =
  List.iteri
    (fun i h ->
      let e = path cache h in
      if not (Sys.file_exists e) then begin
        Os.mkdir ~parents:true (Filename.dirname e);
        if i = 0 then Unix.rename file e else Unix.link (path cache (List.hd hashes)) e
      end)
    hashes

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

let stage_archive ~cache ~log url =
  let hashes = OpamFile.URL.checksum url in
  let urls = (OpamFile.URL.url url :: OpamFile.URL.mirrors url) @ List.filter_map (fun h -> OpamUrl.parse_opt (path mirror h)) hashes in
  let tmp = Filename.temp_file ~temp_dir:cache ".fetch-" "" in
  let attempt u = download ~log u tmp && List.for_all (OpamHash.check_file tmp) hashes in
  if List.exists attempt urls then
    let kept = keep ~cache tmp hashes in
    Some ((if hashes = [] then OpamFile.URL.with_checksum kept url else url), Printf.sprintf "%s (%s)" (if hashes = [] then "with no checksum, as" else "as") (OpamHash.to_string (List.hd kept)))
  else begin
    (try Sys.remove tmp with Sys_error _ -> ());
    None
  end

(* A git source is fetched, one commit deep, into a bare repository -- a
   branch, a tag or a commit, or HEAD when the URL names none -- and git
   archive writes it as a tar.gz straight from the objects, so nothing is
   checked out and .git is never in it.  gzip, not plain tar: the cache entry
   has no extension, and opam tells an archive's type from its first bytes,
   which a plain tar does not identify.  The URL becomes a placeholder with the
   tar's checksum: opam never fetches it, since it finds the tar in the cache
   by that checksum first.  The subpath, if any, is kept, and still applies,
   since the tar holds the whole tree.  hg and darcs are not staged: nothing in
   opam-repository uses them. *)
let stage_vcs ~cache ~log ~name url =
  let u = OpamFile.URL.url url in
  let fail m =
    Os.append_to_file log (Printf.sprintf "day10: %s: %s\n" (OpamUrl.to_string u) m);
    None
  in
  match u.OpamUrl.backend with
  | `hg | `darcs -> fail "only git sources can be staged"
  | `http | `rsync -> fail "not a VCS source"
  | `git -> (
      (* A bare repository: git archive writes the tar from the objects, so
         nothing is checked out. *)
      let work = Filename.temp_dir "day10-git-" "" in
      Fun.protect ~finally:(fun () -> OpamFilename.rmdir (OpamFilename.Dir.of_string work)) @@ fun () ->
      let git args = fst (Os.spawn ~stdout:log ~stderr:log ~append:true "git" ([ "-C"; work ] @ args)) = 0 in
      (* Ignore the tree's export-ignore and export-subst attributes, which
         would make the tar differ from the checkout opam would have used. *)
      let refspec = Option.value ~default:"HEAD" u.OpamUrl.hash in
      if not (git [ "init"; "--bare"; "--quiet" ]) then fail "git init failed"
      else
        let () = Os.write_to_file Path.(work / "info" / "attributes") "* -export-ignore -export-subst\n" in
        if not (git [ "fetch"; "--quiet"; "--depth"; "1"; OpamUrl.base_url u; refspec ]) then fail ("git fetch of " ^ refspec ^ " failed")
        else
          (* The commit, not an annotated tag's own object. *)
          let revision = String.trim (Os.capture "git" [ "-C"; work; "rev-parse"; "FETCH_HEAD^{commit}" ]) in
          (* git archive leaves submodules out, where opam would have checked
             them out: refuse rather than build from part of the source. *)
          if fst (Os.spawn ~stdout:Os.null ~stderr:Os.null "git" [ "-C"; work; "cat-file"; "-e"; revision ^ ":.gitmodules" ]) = 0 then fail (revision ^ " has submodules, which git archive leaves out")
          else
            let tmp = Filename.temp_file ~temp_dir:cache ".fetch-" ".tar.gz" in
            if not (git [ "archive"; "--format=tar.gz"; "--prefix=" ^ name ^ "/"; "--output=" ^ tmp; revision ]) then begin
              (try Sys.remove tmp with Sys_error _ -> ());
              fail "git archive failed"
            end
            else
              let kept = keep ~cache tmp [] in
              let placeholder = OpamUrl.parse ("https://day10.invalid/" ^ name ^ ".tar.gz") in
              Some
                ( url |> OpamFile.URL.with_url placeholder |> OpamFile.URL.with_checksum kept,
                  Printf.sprintf "as git archive of %s (%s)" revision (OpamHash.to_string (List.hd kept)) ))

(* Stage one source, returning the URL the build's opam file should give for
   it -- unchanged, given a checksum, or a placeholder for a packed checkout --
   and how it was staged.  None when it could not be. *)
let stage_url ~cache ~log ~name url = if is_vcs url then stage_vcs ~cache ~log ~name url else stage_archive ~cache ~log url

(* [repo] is the build's copy of the repository and [in_container] the URL it
   is reached by inside the container.  Stages every source of [pkg], noting
   each in [log], and rewrites the build's copy of the opam file when a source
   needed a checksum or was packed from a checkout.  A source that cannot be staged is noted and left: the
   build then fails on it, offline, with opam's own message. *)
let stage ~repo ~in_container ~log pkg =
  let note fmt = Printf.ksprintf (fun s -> Os.append_to_file log ("day10: " ^ s ^ "\n")) fmt in
  let name = OpamPackage.Name.to_string (OpamPackage.name pkg) and nv = OpamPackage.to_string pkg in
  let file = Path.(repo / "packages" / name / nv / "opam") in
  if Sys.file_exists file then begin
    let cache = Path.(repo / "cache") in
    Os.mkdir ~parents:true cache;
    Os.write_to_file Path.(repo / "repo") (Printf.sprintf "opam-version: \"2.0\"\narchive-mirrors: %S\n" in_container);
    let filename = OpamFile.make (OpamFilename.of_string file) in
    let opam = OpamFile.OPAM.read_from_string ~filename (Os.read_from_file file) in
    let changed = ref false in
    let stage_one label url =
      let src = OpamUrl.to_string (OpamFile.URL.url url) in
      match stage_url ~cache ~log ~name:(nv ^ "-" ^ label) url with
      | None ->
          note "could not stage %s %s" label src;
          url
      | Some (staged, how) ->
          note "staged %s %s %s" label src how;
          if staged <> url then changed := true;
          staged
    in
    let opam = OpamFile.OPAM.with_url_opt (Option.map (stage_one "url") (OpamFile.OPAM.url opam)) opam in
    let opam =
      OpamFile.OPAM.with_extra_sources
        (List.map (fun (base, url) -> (base, stage_one (OpamFilename.Base.to_string base) url)) (OpamFile.OPAM.extra_sources opam))
        opam
    in
    if !changed then Os.write_to_file file (OpamFile.OPAM.write_to_string ~filename opam)
  end
