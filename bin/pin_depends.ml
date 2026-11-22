(* The packages a project pins with pin-depends, resolved on the host into opam
   files the build's repository can carry like any other.

   Each pinned source is fetched one commit deep, as Source_stage fetches a git
   source, and its opam file is read from that commit where opam would look for
   it.  The opam file is given the pinned name and version, as opam pin gives
   it, and a url naming the commit rather than whatever branch or tag the
   pin-depends gave: the url is part of the layer hash, so a layer built from
   one commit of a branch is never taken for a later one.  From there the
   package is staged and installed offline like any other, on every platform,
   since nothing here runs anywhere but the host.

   Pins are followed into the opam files they fetch, so a pinned package's own
   pin-depends are honoured as opam honours them.  A project's own package is
   never replaced by a pin of the same name, and two pins of one name that
   disagree are refused rather than one of them picked.

   Only git is resolved: it is what pin-depends name in practice, and what
   Source_stage can stage from a URL with no checksum. *)

(* Where opam looks for a package's opam file in a pinned source, in the order
   it looks (OpamPinned.files_in_source). *)
let opam_paths name = [ name ^ ".opam"; "opam"; "opam/" ^ name ^ ".opam"; "opam/opam" ]

(* [pkg]'s opam file from [url], ready to go in a repository, and the URL as
   resolved to a commit. *)
let fetch pkg url =
  let name = OpamPackage.Name.to_string (OpamPackage.name pkg) in
  match url.OpamUrl.backend with
  | `http | `rsync | `hg | `darcs -> Error "day10 resolves only git pin-depends"
  | `git -> (
      let work = Filename.temp_dir "day10-pin-" "" in
      Fun.protect ~finally:(fun () -> OpamFilename.rmdir (OpamFilename.Dir.of_string work)) @@ fun () ->
      try
        let git = Source_stage.git ~work in
        let _ = git [ "init"; "--bare"; "--quiet" ] in
        let commit = Source_stage.fetch_commit ~work ~remote:(OpamUrl.base_url url) ~refspec:(Option.value ~default:"HEAD" url.OpamUrl.hash) in
        (* Blobs only, so a directory called opam is not taken for a file. *)
        let files = Source_stage.split0 (git [ "ls-tree"; "-r"; "-z"; "--name-only"; commit ]) in
        match List.find_opt (fun path -> List.mem path files) (opam_paths name) with
        | None -> Error (Printf.sprintf "no opam file at %s (looked for %s)" commit (String.concat ", " (opam_paths name)))
        | Some path ->
            let filename = OpamFile.make (OpamFilename.raw (OpamUrl.to_string url ^ ":" ^ path)) in
            (* git show writes the blob as it is stored, so a Windows host's
               autocrlf does not get the chance to rewrite it. *)
            let opam = OpamFile.OPAM.read_from_string ~filename (git [ "show"; commit ^ ":" ^ path ]) in
            let url = { url with OpamUrl.hash = Some commit } in
            Ok
              ( opam
                |> OpamFile.OPAM.with_name (OpamPackage.name pkg)
                |> OpamFile.OPAM.with_version (OpamPackage.version pkg)
                |> OpamFile.OPAM.with_url (OpamFile.URL.create url),
                url )
      with
      | Failure m | Sys_error m -> Error m
      | e -> Error (Printexc.to_string e))

(* Every package pinned by the pin-depends of [local], the project's own
   packages as find_local_packages reads them, and of the packages those pins
   fetch in turn.  Error says which pin could not be resolved and who asked for
   it. *)
let resolve ~(local : (string * OpamFile.OPAM.t) list) =
  let is_local name = List.mem_assoc (OpamPackage.Name.to_string name) local in
  (* The pin-depends of [opam], each with who asked for it. *)
  let pins ~by opam = List.map (fun (pkg, url) -> (pkg, url, by)) (OpamFile.OPAM.pin_depends opam) in
  (* [seen] maps each name resolved so far to its package, the URL first given
     for it, that URL resolved to a commit, and who asked for it.  [todo] is
     worked through in order, a fetched package's own pins going on the end. *)
  let rec go seen resolved todo =
    match todo with
    | [] -> Ok (List.rev resolved)
    | (pkg, url, by) :: todo -> (
        let name = OpamPackage.name pkg in
        let prior = OpamPackage.Name.Map.find_opt name seen in
        if is_local name then begin
          OpamConsole.warning "pin-depends: %s in %s is a package of this project, which is used instead" (OpamPackage.to_string pkg) by;
          go seen resolved todo
        end
        else
          match prior with
          | Some (pkg', url', _, _) when OpamPackage.equal pkg pkg' && OpamUrl.equal url url' -> go seen resolved todo
          | _ -> (
              match fetch pkg url with
              | Error m -> Error (Printf.sprintf "pin-depends: cannot resolve %s from %s, asked for by %s: %s" (OpamPackage.to_string pkg) (OpamUrl.to_string url) by m)
              | Ok (opam, commit_url) -> (
                  match prior with
                  | Some (pkg', _, commit_url', _) when OpamPackage.equal pkg pkg' && OpamUrl.equal commit_url commit_url' -> go seen resolved todo
                  | Some (pkg', _, commit_url', by') ->
                      Error
                        (Printf.sprintf "pin-depends disagree: %s pins %s to %s, and %s pins %s to %s" by' (OpamPackage.to_string pkg')
                           (OpamUrl.to_string commit_url') by (OpamPackage.to_string pkg) (OpamUrl.to_string commit_url))
                  | None ->
                      OpamConsole.note "pin-depends: %s -> %s" (OpamPackage.to_string pkg) (OpamUrl.to_string commit_url);
                      let seen = OpamPackage.Name.Map.add name (pkg, url, commit_url, by) seen in
                      (* The repository's copy drops pin-depends: they are followed here. *)
                      let resolved = (pkg, OpamFile.OPAM.with_pin_depends [] opam) :: resolved in
                      go seen resolved (todo @ pins ~by:(OpamPackage.to_string pkg) opam))))
  in
  go OpamPackage.Name.Map.empty [] (List.concat_map (fun (name, opam) -> pins ~by:(name ^ ".opam") opam) local)
