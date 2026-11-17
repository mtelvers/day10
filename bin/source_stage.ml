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
   too.  A VCS source is left for opam, which fails on it without the
   network. *)

(* Where opam's archive-mirrors fallback points, so a source that has gone from
   upstream is found here as it would be by opam. *)
let mirror = "https://opam.ocaml.org/cache"

let entry ~cache hash = String.concat "/" (cache :: OpamHash.to_path hash)

let is_vcs url = match (OpamFile.URL.url url).OpamUrl.backend with #OpamUrl.version_control -> true | `http | `rsync -> false

let curl ~log ~output url =
  fst (Os.spawn ~stdout:log ~stderr:log ~append:true "curl" [ "--fail"; "--silent"; "--show-error"; "--location"; "--max-time"; "900"; "--output"; output; url ]) = 0

(* Store [file] under every checksum.  opam asks a mirror for the first one
   only, but which comes first is the opam file's business. *)
let store ~cache file hashes =
  List.iteri
    (fun i h ->
      let e = entry ~cache h in
      if not (Sys.file_exists e) then begin
        Os.mkdir ~parents:true (Filename.dirname e);
        if i = 0 then Unix.rename file e else Unix.link (entry ~cache (List.hd hashes)) e
      end)
    hashes

(* Fetch one source into the cache, returning the checksums opam will look it
   up by: its own, or for one that has none, the sha256 of what was fetched.
   None when it could not be fetched, or is a VCS source. *)
let stage_url ~cache ~log url =
  if is_vcs url then None
  else
    let urls = List.map OpamUrl.to_string (OpamFile.URL.url url :: OpamFile.URL.mirrors url) in
    let hashes = OpamFile.URL.checksum url in
    let tmp = Filename.temp_file ~temp_dir:cache ".fetch-" "" in
    let mirrors = List.map (fun h -> String.concat "/" (mirror :: OpamHash.to_path h)) hashes in
    let attempt u = curl ~log ~output:tmp u && List.for_all (OpamHash.check_file tmp) hashes in
    if List.exists attempt (urls @ mirrors) then begin
      Unix.chmod tmp 0o644;
      let hashes = if hashes = [] then [ OpamHash.compute ~kind:`SHA256 tmp ] else hashes in
      store ~cache tmp hashes;
      (* Already there under its first checksum, from another source of the
         same package: [store] left the copy just fetched. *)
      (try Sys.remove tmp with Sys_error _ -> ());
      Some hashes
    end
    else begin
      (try Sys.remove tmp with Sys_error _ -> ());
      None
    end

(* [repo] is the build's copy of the repository and [in_container] the URL it
   is reached by inside the container.  Stages every source of [pkg], noting
   each in [log], and rewrites the build's copy of the opam file when a source
   needed a checksum.  A source that cannot be staged is noted and left: the
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
      match stage_url ~cache ~log url with
      | None ->
          note "could not stage %s %s%s" label src (if is_vcs url then " (a VCS source)" else "");
          url
      | Some hashes when OpamFile.URL.checksum url = [] ->
          note "staged %s %s, which has no checksum: using %s" label src (OpamHash.to_string (List.hd hashes));
          changed := true;
          OpamFile.URL.with_checksum hashes url
      | Some _ ->
          note "staged %s %s" label src;
          url
    in
    let opam = OpamFile.OPAM.with_url_opt (Option.map (stage_one "url") (OpamFile.OPAM.url opam)) opam in
    let opam =
      OpamFile.OPAM.with_extra_sources
        (List.map (fun (base, url) -> (base, stage_one (OpamFilename.Base.to_string base) url)) (OpamFile.OPAM.extra_sources opam))
        opam
    in
    if !changed then Os.write_to_file file (OpamFile.OPAM.write_to_string ~filename opam)
  end
