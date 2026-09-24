(* The modules this drives are a library, so that they can be tested without
   being copied into a test executable one at a time. *)
open Day10

module Solver = Opam_0install.Solver.Make (Repo_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let resolve_opam ctx pkg =
  match Repo_context.opam_file ctx pkg with
  | Some o -> o
  | None -> failwith (Printf.sprintf "opam not found for %s" (OpamPackage.to_string pkg))

(* The flags only apply to the package under test: a dependency is built the
   same way whether or not the caller asked for tests. *)
let layer_hash_of ~(config : Config.t) ctx pkg pkgs =
  let target = Config.is_target_package ~config pkg in
  let matters requested variable = requested && target && Util.can_act_on ~variable (resolve_opam ctx pkg) in
  Util.layer_hash ~with_test:(matters config.with_test "with-test") ~with_doc:(matters config.with_doc "with-doc")
    ~vars:(Config.platform_vars ~config) (List.map (resolve_opam ctx) pkgs)

let container =
  match OpamSysPoll.os OpamVariable.Map.empty with
  | Some "linux" -> (module Linux : S.CONTAINER)
  | Some "freebsd" -> (module Freebsd : S.CONTAINER)
  | Some "win32" -> (module Windows : S.CONTAINER)
  | _ -> (module Dummy : S.CONTAINER)

module Container = (val container)

let init t =
  let config = Container.config ~t in
  let os_dir = Path.(config.dir / Config.os_key ~config) in
  let () = Os.mkdir ~parents:true os_dir in
  let root = Path.(os_dir / "base") in
  if not (Sys.file_exists root) then
    ignore @@ Os.create_directory_exclusively root @@ fun target_dir ->
    let temp_dir = Filename.temp_dir ~temp_dir:config.dir ~perms:0o755 "temp-" "" in
    Cleanup.with_resource (Cleanup.Temp_dir temp_dir) @@ fun () ->
    let opam_repository = Util.create_opam_repository temp_dir in
    let build_log = Path.(temp_dir / "build.log") in
    let _ = Container.run ~t ~temp_dir opam_repository build_log in
    Unix.rename temp_dir target_dir

let () = OpamFormatConfig.init ()

(* let root = OpamStateConfig.opamroot ()
let _ = OpamStateConfig.load_defaults root *)
let () = OpamCoreConfig.init ?debug_level:(Some 10) ?debug_sections:(Some (OpamStd.String.Map.singleton "foo" (Some 10))) ()

let opam_env ~(config : Config.t) pkg v =
  (*  if List.mem v OpamPackageVar.predefined_depends_variables then (Some (OpamTypes.B true))
  else *)
  let is_target_pkg = Config.is_target_package ~config pkg in
  match OpamVariable.Full.to_string v with
  | "version" -> Some (OpamTypes.S (OpamPackage.Version.to_string (OpamPackage.version pkg)))
  | "with-test" -> Some (OpamTypes.B (config.with_test && is_target_pkg))
  | "with-doc" -> Some (OpamTypes.B (config.with_doc && is_target_pkg))
  | "with-dev"
  | "with-dev-setup"
  | "dev" ->
      Some (OpamTypes.B false)
  | "build" -> Some (OpamTypes.B true)
  | "post" -> None
  | x -> Config.std_env ~config x

(* Every package the project defines, read.  Reading is part of finding them: a
   file that will not parse is not a package we can do anything with, and a
   project is entitled to keep broken ones around deliberately.  Skipping one is
   worth saying out loud, though, in case it is a package the caller expected us
   to build rather than a fixture. *)
let rec find_local_packages dir =
  try
    Sys.readdir dir |> Array.to_list
    |> List.concat_map (fun name ->
         let path = Filename.concat dir name in
         (* Nothing whose name starts with a dot belongs to the project, file or
            directory.  macOS leaves AppleDouble copies of everything it puts in
            a tarball, so ._foo.opam turns up beside foo.opam and is not an opam
            file at all -- it starts with a binary header. *)
         if String.length name > 0 && name.[0] = '.' then []
         else if Sys.is_directory path then
           (* A directory ending in .t is a cram test by dune's convention.  What
              is inside it is fixture, not project: ocsigen-dune-rules keeps
              empty .opam files there for its tests to generate over. *)
           if name = "_build" || name = "_opam" || Filename.check_suffix name ".t" then []
           else find_local_packages path
         else if Filename.check_suffix name ".opam" then (
           match OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw path)) with
           | opam -> [ (Filename.remove_extension name, opam) ]
           | exception e ->
               OpamConsole.warning "Ignoring %s: %s" path (Printexc.to_string e);
               [])
         else [])
  with
  | Sys_error _ -> []

let make_repo (config : Config.t) =
  Repo.create (List.map Repo.parse_source config.opam_repositories)

(* One go at the solver.  [invariant] holds the ocaml version to the one asked
   for; [allow_all_avoid] lets a package all of whose versions carry
   avoid-version into the solution. *)
type pass = {
  invariant : bool;
  allow_all_avoid : bool;
}

let solve ~repo (config : Config.t) root_packages =
  let constraints ~invariant =
    (if invariant then [ (OpamPackage.name config.ocaml_version, (`Eq, OpamPackage.version config.ocaml_version)) ] else [])
    @ List.map (fun pkg -> (OpamPackage.name pkg, (`Eq, OpamPackage.version pkg))) root_packages
    |> OpamPackage.Name.Map.of_list
  in
  let pins =
    Option.fold ~none:OpamPackage.Name.Map.empty
      ~some:(fun directory ->
        find_local_packages directory
        |> List.fold_left
             (fun acc (name, opam) -> OpamPackage.Name.Map.add (OpamPackage.Name.of_string name) (OpamPackage.Version.of_string "dev", opam) acc)
             OpamPackage.Name.Map.empty)
      config.directory
  in
  let test =
    if config.with_test then List.map OpamPackage.name root_packages |> OpamPackage.Name.Set.of_list
    else OpamPackage.Name.Set.empty
  in
  let doc =
    if config.with_doc then List.map OpamPackage.name root_packages |> OpamPackage.Name.Set.of_list
    else OpamPackage.Name.Set.empty
  in
  let roots = OpamPackage.name config.ocaml_version :: List.map OpamPackage.name root_packages in
  let attempt { invariant; allow_all_avoid } =
    let context =
      Repo_context.create ~prefer_oldest:config.prefer_oldest ~allow_all_avoid ~env:(Config.std_env ~config) ~constraints:(constraints ~invariant) ~pins ~test
        ~doc ~repo ()
    in
    (context, Solver.solve context roots)
  in
  (* Prefer a solution that leaves out any package whose every version carries
     avoid-version, and settle for one that includes it rather than reporting no
     solution at all -- which is what opam does, minimising how many such
     packages a solution contains.

     Under --update-invariant the same pair runs again with the ocaml version
     unconstrained.  Relaxing last keeps the requested version wherever it can
     be honoured and gives it up only where it cannot, which is what a compiler
     package needs: ocaml depends on the compiler rather than the other way
     about, so the version follows from the package under test.

     Each pass falls through to the next only on failure, so the result kept is
     the first that solved or, if none did, the last one's -- the most permissive
     pass, whose diagnostics are the ones worth reporting.  The earlier ones can
     only say a package has no known implementations. *)
  let fallbacks =
    { invariant = true; allow_all_avoid = true }
    :: (if config.update_invariant then [ { invariant = false; allow_all_avoid = false }; { invariant = false; allow_all_avoid = true } ] else [])
  in
  let context, solution =
    List.fold_left (fun acc pass -> match acc with (_, Ok _) -> acc | _ -> attempt pass) (attempt { invariant = true; allow_all_avoid = false }) fallbacks
  in
  match solution with
  | Ok out ->
      let sels = Output.to_map out in
      let depends = Hashtbl.create 100 in
      let classify x =
        match Solver.package_name x with
        | Some pkg -> `Opam pkg
        | None -> `Virtual x
      in
      let () =
        Role_map.iter
          (fun role sel ->
            let impl = Output.unwrap sel in
            Solver.Input.requires role impl |> fst
            |> List.iter (fun dep ->
                   let dep = Input.dep_info dep in
                   let dep_role = dep.dep_role in
                   if dep.dep_importance <> `Restricts then Hashtbl.add depends (classify role) (classify dep_role)))
          sels
      in
      let rec expand role =
        Hashtbl.find_all depends role
        |> List.concat_map (function
             | `Opam dep -> [ dep ]
             | `Virtual _ as role -> expand role)
      in
      let pkgs = Solver.packages_of_result out |> OpamPackage.Set.of_list in
      let pkgnames = OpamPackage.names_of_packages pkgs in
      let deptree =
        OpamPackage.Set.fold
          (fun pkg acc ->
            let opam = Repo_context.load context pkg in
            (* The names this package depends on, as the environment resolves
               them.  [tests:false] asks the same question with with-test turned
               off, whatever the caller requested. *)
            let depends_names ~tests ~post =
              let env v =
                if (not tests) && String.equal (OpamVariable.Full.to_string v) "with-test" then Some (OpamTypes.B false) else opam_env ~config pkg v
              in
              OpamFile.OPAM.depends opam |> OpamFilter.partial_filter_formula env |> OpamFilter.filter_deps ~build:true ~post |> OpamFormula.all_names
            in
            let reached_only_through_post =
              OpamPackage.Name.Set.diff (depends_names ~tests:true ~post:true) (depends_names ~tests:true ~post:false)
            in
            let there_only_for_tests = OpamPackage.Name.Set.diff (depends_names ~tests:true ~post:true) (depends_names ~tests:false ~post:true) in
            (* A post dependency must not become an ordering edge: that is what
               post is for, and what lets a cycle through one resolve at all.
               But with-test trumps post -- a dependency that is there only
               because tests were asked for has to be in the switch that runs
               them, so it is kept.  Written {with-test & post} by melange,
               ocamlformat, printbox-text and re, and by nothing else in
               opam-repository. *)
            let deppost = OpamPackage.Name.Set.diff reached_only_through_post there_only_for_tests in
            let depopts = OpamFile.OPAM.depopts opam |> OpamFormula.all_names in
            let depopts = OpamPackage.Name.Set.inter depopts pkgnames |> OpamPackage.Name.Set.to_list in
            let name = OpamPackage.name pkg in
            let deps =
              expand (`Opam name) @ depopts |> OpamPackage.Name.Set.of_list |> fun x ->
              OpamPackage.Name.Set.diff x deppost |> OpamPackage.packages_of_names pkgs
            in
            OpamPackage.Map.add pkg deps acc)
          pkgs OpamPackage.Map.empty
      in
      let rec dfs map pkg =
        let deps = OpamPackage.Map.find pkg deptree in
        OpamPackage.Set.fold
          (fun p acc ->
            match OpamPackage.Map.mem p acc with
            | true -> acc
            | false -> dfs acc p)
          deps (OpamPackage.Map.add pkg deps map)
      in
      let root_pkgs = OpamPackage.Set.filter (fun p -> List.exists (fun r -> OpamPackage.name r = OpamPackage.name p) root_packages) (Solver.packages_of_result out |> OpamPackage.Set.of_list) in
      Ok (context, OpamPackage.Set.fold (fun pkg acc -> dfs acc pkg) root_pkgs OpamPackage.Map.empty)
  | Error problem -> Error (Solver.diagnostics problem)

let rec topological_sort pkgs =
  match OpamPackage.Map.is_empty pkgs with
  | true -> []
  | false ->
      (* Find all packages which can be installed *)
      let installable, remainder = OpamPackage.Map.partition (fun _ deps -> OpamPackage.Set.is_empty deps) pkgs in
      let () = assert (not (OpamPackage.Map.is_empty installable)) in
      let installable = OpamPackage.Map.to_list installable |> List.map fst in
      (* Remove the dependency on any installable package from the remaining packages *)
      let pkgs = OpamPackage.Map.map (fun deps -> List.fold_left (fun acc pkg -> OpamPackage.Set.remove pkg acc) deps installable) remainder in
      installable @ topological_sort pkgs

let pkg_deps solution =
  List.fold_left
    (fun map pkg ->
      let deps_direct = OpamPackage.Map.find pkg solution in
      let deps_plus_children = OpamPackage.Set.fold (fun pkg acc -> OpamPackage.Set.union acc (OpamPackage.Map.find pkg map)) deps_direct deps_direct in
      OpamPackage.Map.add pkg deps_plus_children map)
    OpamPackage.Map.empty

(*
let reduce dependencies =
  OpamPackage.Map.map (fun u ->
      OpamPackage.Set.filter
        (fun v ->
          let others = OpamPackage.Set.remove v u in
          OpamPackage.Set.fold (fun o acc -> acc || OpamPackage.Set.mem v (OpamPackage.Map.find o dependencies)) others false |> not)
        u)
*)

let extract_dag dag root =
  let rec loop visited to_visit result =
    match to_visit with
    | [] -> result
    | pkg :: rest -> (
        if OpamPackage.Set.mem pkg visited then
          (* OpamPackage already processed, skip it *)
          loop visited rest result
        else
          (* Mark package as visited *)
          let new_visited = OpamPackage.Set.add pkg visited in
          match OpamPackage.Map.find_opt pkg dag with
          | None ->
              (* OpamPackage not found in the original map, skip it *)
              loop new_visited rest result
          | Some deps ->
              (* Add package and its dependencies to result *)
              let new_result = OpamPackage.Map.add pkg deps result in
              (* Add all dependencies to the work list *)
              let deps_list = OpamPackage.Set.fold (fun dep acc -> dep :: acc) deps [] in
              let new_to_visit = deps_list @ rest in
              loop new_visited new_to_visit new_result)
  in
  loop OpamPackage.Set.empty [ root ] OpamPackage.Map.empty

type build_result =
  | Solution of OpamTypes.package_set OpamTypes.package_map
  | No_solution of string
  | Dependency_failed
  | Failure of string
  | Success of string

let build_result_to_string = function
  | Solution _ -> "solution"
  | No_solution _ -> "no_solution"
  | Dependency_failed -> "dependency_failed"
  | Failure _ -> "failure"
  | Success _ -> "success"

let print_build_result = function
  | Solution _ -> OpamConsole.note "solution"
  | No_solution _ -> OpamConsole.warning "no_solution"
  | Dependency_failed -> OpamConsole.warning "dependency_failed"
  | Failure _ -> OpamConsole.error "failure"
  | Success _ -> OpamConsole.note "success"

let build_layer ctx t pkg hash ordered_deps ordered_hashes =
  let config = Container.config ~t in
  let layer_dir = Path.(config.dir / Config.os_key ~config / hash) in
  let layer_json = Path.(layer_dir / "layer.json") in
  let write_layer target_dir =
    let () = OpamConsole.note "Building %s" (OpamPackage.to_string pkg) in
    let temp_dir = Filename.temp_dir ~temp_dir:config.dir ~perms:0o755 "temp-" "" in
    Cleanup.with_resource (Cleanup.Temp_dir temp_dir) @@ fun () ->
    let opam_repo = Util.create_opam_repository temp_dir in
    let () = Repo.materialise (Repo_context.repo ctx) (pkg :: ordered_deps) ~dest:opam_repo in
    let build_log = Path.(temp_dir / "build.log") in
    let r = Container.build ~t ~temp_dir build_log pkg ordered_hashes in
    let () = Unix.rename temp_dir target_dir in
    Util.save_layer_info layer_json pkg ordered_deps ordered_hashes r
  in
  (* A layer built just now streamed its output as it went and announced itself
     as it started.  One taken from the cache was compiled weeks ago by somebody
     building something else, so replaying its log under --log buried the build
     the caller actually asked about -- but it is still a package that went into
     this build, so say which under --log.  Between them the two notes list
     everything used, built or not.  --markdown and --json still record every
     layer's log in full for a post mortem. *)
  (* Whether this process built the layer, which is not the same as whether it
     was missing when we looked: a job wanting a layer another job is already
     building waits inside create_directory_exclusively and returns having
     built nothing. *)
  let built = if Sys.file_exists layer_dir then false else Os.create_directory_exclusively layer_dir write_layer in
  let () = if (not built) && config.log then OpamConsole.note "Using %s" (OpamPackage.to_string pkg) in
  let () = Unix.utimes layer_json 0.0 0.0 in
  let exit_status = Util.load_layer_info_exit_status layer_json in
  match exit_status with
  | 0 -> Success hash
  | _ ->
      (* The log is the whole explanation of a failure, and whatever reads
         day10's output classifies it by matching on that text -- an unavailable
         system package, say -- so it has to be there even when the layer failed
         weeks ago and all we are reporting is the cached verdict.  Streaming
         has already shown it if we built it just now. *)
      let streamed = built && config.log in
      if not streamed then OpamConsole.error "%s failed:\n%s" (OpamPackage.to_string pkg) (Os.read_from_file Path.(layer_dir / "build.log"));
      (* A failure the maintainer has already declared expected here.  day10
         knows which platform was asked for, so it does the matching and emits
         the marker on its own line; the failure above says which package. *)
      let accepted = Util.accept_failures (resolve_opam ctx pkg) in
      if List.exists (fun p -> String.equal p (Config.platform ~config) || String.equal p config.os_distribution) accepted then OpamConsole.note "accept_failures";
      Failure hash

let build ~repo config packages =
  match solve ~repo config packages with
  | Ok (ctx, solution) ->
      let () = if config.log then Dot_solution.to_string solution |> print_endline in
      let () = Option.iter (fun filename -> Dot_solution.save filename solution) config.dot in
      let t = Container.init ~config in
      init t;
      let ordered_installation = topological_sort solution in
      let dependencies = pkg_deps solution ordered_installation in
      let all_layers_exist =
        if config.dry_run then
          let rec check_all prev_success = function
            | [] -> true
            | pkg :: rest ->
                let ordered_deps = extract_dag dependencies pkg |> topological_sort |> List.rev |> List.tl in
                let hash = layer_hash_of ~config ctx pkg (pkg :: ordered_deps) in
                let layer_dir = Path.(config.dir / Config.os_key ~config / hash) in
                let layer_json = Path.(layer_dir / "layer.json") in
                let layer_exists = Sys.file_exists layer_dir in
                if layer_exists then
                  let exit_status = Util.load_layer_info_exit_status layer_json in
                  check_all (prev_success && exit_status = 0) rest
                else if prev_success then false
                else check_all false rest
          in
          check_all true ordered_installation
        else false
      in
      if config.dry_run && not all_layers_exist then (
        Container.deinit ~t;
        [ Solution solution ]
      )
      else
      let results, _ =
        List.fold_left
          (fun (res, m) pkg ->
            if Config.is_local_package ~config pkg then
              (* Skip local packages — they'll be built from the workspace by dune *)
              (res, m)
            else
            let ordered_deps = extract_dag dependencies pkg |> topological_sort |> List.rev |> List.tl in
            let ordered_deps = List.filter (fun p -> not (Config.is_local_package ~config p)) ordered_deps in
            let ordered_hashes =
              List.filter_map
                (fun p ->
                  match OpamPackage.Map.find p m with
                  | Success h
                  | Failure h ->
                      Some h
                  | _ -> None)
                ordered_deps
            in
            let hash = layer_hash_of ~config ctx pkg (pkg :: ordered_deps) in
            match res with
            | [] ->
                let r = build_layer ctx t pkg hash ordered_deps ordered_hashes in
                ([ r ], OpamPackage.Map.add pkg r m)
            | Success _ :: _ ->
                let r = build_layer ctx t pkg hash ordered_deps ordered_hashes in
                (r :: res, OpamPackage.Map.add pkg r m)
            | _ -> (Dependency_failed :: res, OpamPackage.Map.add pkg Dependency_failed m))
          ([], OpamPackage.Map.empty) ordered_installation
      in
      Container.deinit ~t;
      results @ [ Solution solution ]
  | Error s ->
      let () = if config.log then print_endline s in
      [ No_solution s ]

open Cmdliner

let run_list (config : Config.t) all_versions =
  let () = Random.self_init () in
  let repo = make_repo config in
  let all_packages =
    Repo.fold
      (fun pkg set ->
        match Repo.opam repo pkg with
        | None -> set
        | Some opam -> (
            match OpamFilter.eval_to_bool ~default:false (opam_env ~config pkg) (OpamFile.OPAM.available opam) with
            | true -> OpamPackage.Set.add pkg set
            | false -> set))
      repo OpamPackage.Set.empty
  in
  let packages_to_show =
    if all_versions then all_packages
    else
      OpamPackage.Name.Map.fold
        (fun n vset base -> OpamPackage.Set.add (OpamPackage.create n (OpamPackage.Version.Set.max_elt vset)) base)
        (OpamPackage.to_map all_packages) OpamPackage.Set.empty
  in
  let package_list =
    packages_to_show
    |> OpamPackage.Set.to_list_map (fun x -> (Random.bits (), x))
    |> List.sort compare |> List.map snd
    |> List.map OpamPackage.to_string
  in
  List.iter print_endline package_list;
  Option.iter (fun filename -> Json_packages.write_packages filename package_list) config.json

let output (config : Config.t) results =
  let os_key = Config.os_key ~config in
  let opam_repo_sha = Util.opam_repo_sha config.opam_repositories |> Option.value ~default:"unknown" in
  let () =
    Option.iter
      (fun filename ->
        let oc = open_out_bin filename in
        let () = Printf.fprintf oc "---\nstatus: %s\ncommit: %s\npackage: %s\n---\n" (build_result_to_string (List.hd results)) opam_repo_sha config.package in
        let () =
          List.rev results
          |> List.iter (function
               | Solution solution ->
                   Printf.fprintf oc "\n# Solution\n\n";
                   output_string oc (Dot_solution.to_string solution)
               | Success hash
               | Failure hash ->
                   let package = Util.load_layer_info_package_name Path.(config.dir / os_key / hash / "layer.json") in
                   Printf.fprintf oc "\n# %s\n\n" package;
                   let build_log = Os.read_from_file Path.(config.dir / os_key / hash / "build.log") in
                   output_string oc build_log
               | No_solution log -> output_string oc log
               | _ -> ())
        in
        close_out oc)
      config.md
  in
  let () =
    Option.iter
      (fun filename ->
        let hash =
          List.find_map
            (function
              | Success hash
              | Failure hash ->
                  Some hash
              | _ -> None)
            results
        in
        let solution =
          List.find_map
            (function
              | Solution s -> Some (Dot_solution.to_string s)
              | No_solution s -> Some s
              | _ -> None)
            results
        in
        let j =
          `Assoc
            ([ ("name", `String config.package); ("status", `String (build_result_to_string (List.hd results))); ("sha", `String opam_repo_sha) ]
            @ Option.fold ~none:[]
                ~some:(fun hash ->
                  let build_log = Os.read_from_file Path.(config.dir / os_key / hash / "build.log") in
                  [ ("layer", `String hash); ("log", `String build_log) ])
                hash
            @ Option.fold ~none:[] ~some:(fun s -> [ ("solution", `String s) ]) solution)
        in
        Yojson.Safe.to_file filename j)
      config.json
  in
  let () =
    Option.iter
      (fun tag ->
        let layers =
          List.filter_map
            (function
              | Success hash
              | Failure hash ->
                  Some hash
              | _ -> None)
            results
        in
        let () = OpamConsole.note "Importing layers into Docker with tag: %s" tag in
        let temp_dir = Filename.temp_dir ~temp_dir:config.dir ~perms:0o755 "docker-import-" "" in
        let cp s d = [ "cp"; "--update=none"; "--archive"; "--no-dereference"; "--recursive"; "--link"; "--no-target-directory"; s; d ] in
        let () =
          List.iter
            (fun hash ->
              let layer_dir = Path.(config.dir / os_key / hash / "fs") in
              let _ = Os.sudo (cp layer_dir temp_dir) in
              ())
            (layers @ [ "base" ])
        in
        let () =
          match layers with
          | hash :: _ ->
              let opam_repo_src = Path.(config.dir / os_key / hash / "opam-repository") in
              let opam_repo_dst = Path.(temp_dir / "home" / "opam" / ".opam" / "repo" / "default") in
              let _ = Os.sudo (cp opam_repo_src opam_repo_dst) in
              ()
          | _ -> ()
        in
        let () = Os.run (String.concat " " [ "sudo"; "tar"; "-C"; temp_dir; "-c"; "."; "|"; "docker"; "import"; "-"; tag ]) |> print_string in
        let _ = Os.sudo [ "rm"; "-rf"; temp_dir ] in
        ())
      config.tag
  in
  let () =
    Option.iter
      (fun oci_dir ->
        let layers =
          List.filter_map
            (function
              | Success hash
              | Failure hash ->
                  Some hash
              | _ -> None)
            results
        in
        let tag = config.package in
        Oci.generate ~config ~oci_dir ~tag ~layers)
      config.oci
  in
  print_build_result (List.hd results)

let run_build (config : Config.t) =
  let build_command = Option.value ~default:{ Config.run = "dune build"; network = false } config.build_command in
  (* Use build_command = None during dependency layer building *)
  let dep_config = { config with build_command = None } in
  let local_pkgs = List.map (fun name -> OpamPackage.of_string (name ^ ".dev")) config.local_packages in
  if local_pkgs = [] then begin
    let dir = Option.value ~default:"." config.directory in
    OpamConsole.error "No .opam files found in %s. day10 build/exec needs at least one .opam file to determine dependencies." dir;
    exit 1
  end;
  let repo = make_repo dep_config in
  let results = build ~repo dep_config local_pkgs in
  let exit_code =
    match results with
    (* build_layer has already reported whichever layer failed, and its log. *)
    | Dependency_failed :: _ -> 1
    | No_solution s :: _ -> OpamConsole.error "No solution: %s" s; 1
    | _ ->
        (* All dependency layers built. Now run the build command in the container. *)
        let all_hashes =
          List.filter_map (function Success h -> Some h | _ -> None) results
        in
        let build_config = { config with build_command = Some build_command } in
        let t = Container.init ~config:build_config in
        let temp_dir = Filename.temp_dir ~temp_dir:config.dir ~perms:0o755 "temp-" "" in
        let r =
          Cleanup.with_resource (Cleanup.Temp_dir temp_dir) @@ fun () ->
          let _opam_repo = Util.create_opam_repository temp_dir in
          let build_log = Path.(temp_dir / "build.log") in
          let dummy_pkg = List.hd local_pkgs in
          let r = Container.build ~t ~temp_dir build_log dummy_pkg all_hashes in
          (* The output arrived as it was produced, so there is nothing left to
             print but the verdict. *)
          if r <> 0 then OpamConsole.error "build failed with exit code %i" r;
          r
        in
        Container.deinit ~t;
        r
  in
  exit exit_code

(* Bring a base image's package index up to date without rebuilding the base.
   A stale index is what makes a depext install fetch a version the mirror has
   withdrawn, which arrives as a 404 and looks like the package's fault.

   [max_age] makes this safe to call on every idle window: a platform whose
   index is younger than that is left alone.  The caller decides when -- an
   ocluster worker knows when it is idle and can pause -- and day10 knows how. *)
let refresh_one ~dir ~log ~max_age (platform, path) =
  (* The base's own log is the record: it already says what has happened to this
     base, and a refresh is one of those things.  Its mtime therefore answers
     how old the index is without a file of day10's own alongside -- and answers
     it from the start, because an unrefreshed base's log is as old as the base,
     which is exactly how old its index is.  A marker would have read as
     infinitely old instead, and refreshed everything on a first sweep whatever
     --max-age said. *)
  let history = Path.(path / "base" / "build.log") in
  let hours_since_refresh =
    match (Unix.stat history).st_mtime with
    (* Clamped, because a log appended to a moment ago can carry an mtime a
       shade later than Unix.time reads, which would otherwise print as -0. *)
    | mtime -> Float.max 0.0 ((Unix.time () -. mtime) /. 3600.0)
    | exception _ -> infinity
  in
  match max_age with
  | Some hours when hours_since_refresh < float_of_int hours ->
      OpamConsole.note "%s: refreshed %.0f hour(s) ago, within %d" platform hours_since_refresh hours;
      0
  | _ -> (
      match Cache.components platform with
      | None ->
          OpamConsole.warning "%s: cannot tell which distribution this is, so leaving it alone" platform;
          0
      | Some (os_distribution, os_version, arch) ->
          OpamConsole.note "%s: refreshing" platform;
          let config =
            {
              Config.dir;
              ocaml_version = OpamPackage.of_string "ocaml.0.0.0";
              opam_repositories = [];
              package = "";
              arch;
              os = "linux";
              os_distribution;
              (* Only a fallback: Dist looks the distribution and version up
                 first, and the name does not record a family. *)
              os_family = os_distribution;
              os_version;
              directory = None;
              md = None;
              json = None;
              dot = None;
              with_test = false;
              with_doc = false;
              tag = None;
              oci = None;
              log;
              dry_run = false;
              fork = None;
              build_command = None;
              local_packages = [];
              prefer_oldest = false;
              update_invariant = false;
              opam_jobs = None;
            }
          in
          let t = Container.init ~config in
          let temp_dir = Filename.temp_dir ~temp_dir:dir ~perms:0o755 "temp-" "" in
          let code =
            Cleanup.with_resource (Cleanup.Temp_dir temp_dir) @@ fun () ->
            let refresh_log = Path.(temp_dir / "refresh.log") in
            match Container.refresh ~t ~temp_dir refresh_log with
            | 0 ->
                (* Appended only when it worked, so the mtime means the last
                   successful refresh.  Recording a failure would leave the
                   index stale while claiming to be fresh, and --max-age would
                   then skip it. *)
                Os.append_to_file history (Printf.sprintf "\n=== index refreshed %s ===\n%s" (Util.timestamp ()) (Os.read_from_file refresh_log));
                OpamConsole.note "%s: index up to date" platform;
                0
            | code ->
                OpamConsole.error "%s: refreshing the index failed with exit code %d:\n%s" platform code (Os.read_from_file refresh_log);
                code
          in
          Container.deinit ~t;
          code)

(* Every platform in the cache unless one was named, as cache-info and prune do:
   a builder serving the whole matrix has nineteen of them, and the one that
   goes stale unnoticed is the one nobody thought to name. *)
let run_refresh_base ~dir ~distribution ~version ~arch ~log max_age =
  match Cache.platforms ~distribution ~version ~arch dir with
  | [] ->
      OpamConsole.warning "No base image to refresh in %s" dir;
      exit 0
  | platforms ->
      let codes = List.map (refresh_one ~dir ~log ~max_age) platforms in
      exit (if List.exists (fun code -> code <> 0) codes then 1 else 0)

let run_ci ?repo (config : Config.t) =
  let repo = match repo with Some r -> r | None -> make_repo config in
  let package = OpamPackage.of_string (config.package ^ ".dev") in
  let results = build ~repo config [ package ] in
  output config results

let run_health_check ?repo (config : Config.t) =
  let repo = match repo with Some r -> r | None -> make_repo config in
  let package = OpamPackage.of_string config.package in
  let results = build ~repo config [ package ] in
  output config results

let run_health_check_multi (config : Config.t) package_arg =
  match package_arg.[0] = '@' with
  | false ->
      (* Single package: use paths as-is (files, not directories) *)
      let config = { config with package = package_arg } in
      run_health_check config
  | true ->
      let filename = String.sub package_arg 1 (String.length package_arg - 1) in
      let packages = Json_packages.read_packages filename in
      (* Multiple packages: treat paths as directories *)
      let () = Option.iter (fun path -> Os.mkdir ~parents:true path) config.json in
      let () = Option.iter (fun path -> Os.mkdir ~parents:true path) config.md in
      let () = Option.iter (fun path -> Os.mkdir ~parents:true path) config.dot in
      let () = Option.iter (fun path -> Os.mkdir ~parents:true Path.(path / "blobs" / "sha256")) config.oci in
      (* One repo shared across all packages. For [--fork np], warm the
         parsed memo in the parent so children inherit it via CoW. *)
      let repo = make_repo config in
      let run_with_package pkg_name =
        let json = Option.map (fun path -> Path.(path / pkg_name ^ ".json")) config.json in
        let md = Option.map (fun path -> Path.(path / pkg_name ^ ".md")) config.md in
        let dot = Option.map (fun path -> Path.(path / pkg_name ^ ".dot")) config.dot in
        let config = { config with package = pkg_name; json; md; dot } in
        run_health_check ~repo config
      in
      match config.fork with
      | Some 1
      | None -> List.iter run_with_package packages
      | Some np ->
          Repo.warm repo;
          Os.fork ~np run_with_package packages

let cache_dir_term =
  let doc = "Directory to use for caching (required)" in
  let env = Cmd.Env.info "DAY10_CACHE_DIR" in
  Arg.(required & opt (some string) None & info [ "cache-dir" ] ~env ~docv:"DIR" ~doc)

let ocaml_version_term =
  let doc = "OCaml version to use (default 5.4.1)" in
  let env = Cmd.Env.info "DAY10_OCAML_VERSION" in
  Arg.(value & opt string "ocaml.5.4.1" & info [ "ocaml-version" ] ~env ~docv:"VERSION" ~doc)

let opam_repository_term =
  let doc = "Directory containing opam repositories (required, can be specified multiple times)" in
  let env = Cmd.Env.info "DAY10_OPAM_REPOSITORY" in
  let arg = Arg.(non_empty & opt_all string [] & info [ "opam-repository" ] ~env ~docv:"OPAM-REPO" ~doc) in
  Term.(const (List.concat_map (String.split_on_char ',')) $ arg)

let md_term =
  let doc = "Output results in markdown format" in
  Arg.(value & opt (some string) None & info [ "md" ] ~docv:"FILE" ~doc)

let json_term =
  let doc = "Output results in json format" in
  Arg.(value & opt (some string) None & info [ "json" ] ~docv:"FILE" ~doc)

let dot_term =
  let doc = "Save solution in Graphviz DOT format" in
  Arg.(value & opt (some string) None & info [ "dot" ] ~docv:"FILE" ~doc)

let with_test_term =
  let doc = "Enable test dependencies (default false)" in
  let env = Cmd.Env.info "DAY10_WITH_TEST" in
  Arg.(value & flag & info [ "with-test" ] ~env ~doc)

let with_doc_term =
  let doc = "Enable doc dependencies (default false)" in
  let env = Cmd.Env.info "DAY10_WITH_DOC" in
  Arg.(value & flag & info [ "with-doc" ] ~env ~doc)

let log_term =
  let doc = "Print build logs (default false)" in
  Arg.(value & flag & info [ "log" ] ~doc)

let dry_run_term =
  let doc = "Calculate solution and check if layers exist without building (default false)" in
  Arg.(value & flag & info [ "dry-run" ] ~doc)

let all_versions_term =
  let doc = "List all versions instead of just the latest" in
  Arg.(value & flag & info [ "all-versions" ] ~doc)

let tag_term =
  let doc = "Import layers into Docker with specified tag" in
  Arg.(value & opt (some string) None & info [ "tag" ] ~docv:"TAG" ~doc)

let oci_term =
  let doc = "Generate OCI image layout in specified directory" in
  Arg.(value & opt (some string) None & info [ "oci" ] ~docv:"DIR" ~doc)

let arch_term =
  let doc = "Architecture (default: detected from system)" in
  let env = Cmd.Env.info "DAY10_ARCH" in
  let default = (OpamStd.Sys.uname ()).machine in
  Arg.(value & opt string default & info [ "arch" ] ~env ~docv:"ARCH" ~doc)

let os_term =
  let doc = "Operating system (default: detected from system)" in
  let env = Cmd.Env.info "DAY10_OS" in
  let default = OpamSysPoll.os OpamVariable.Map.empty |> Option.value ~default:"linux" in
  Arg.(value & opt string default & info [ "os" ] ~env ~docv:"OS" ~doc)

let os_distribution_term =
  let doc = "OS distribution (default: detected from system)" in
  let env = Cmd.Env.info "DAY10_OS_DISTRIBUTION" in
  let default = OpamSysPoll.os_distribution OpamVariable.Map.empty |> Option.value ~default:"debian" in
  Arg.(value & opt string default & info [ "os-distribution" ] ~env ~docv:"OS_DISTRIBUTION" ~doc)

let os_family_term =
  let doc = "OS family (default: detected from system)" in
  let env = Cmd.Env.info "DAY10_OS_FAMILY" in
  let default = OpamSysPoll.os_family OpamVariable.Map.empty |> Option.value ~default:"debian" in
  Arg.(value & opt string default & info [ "os-family" ] ~env ~docv:"OS_FAMILY" ~doc)

let os_version_term =
  let doc = "OS version (default: detected from system)" in
  let env = Cmd.Env.info "DAY10_OS_VERSION" in
  let default = OpamSysPoll.os_version OpamVariable.Map.empty |> Option.value ~default:"13" in
  Arg.(value & opt string default & info [ "os-version" ] ~env ~docv:"OS_VERSION" ~doc)

(* OCaml-CI names packages the way its solver does, with the version attached:
   sherlodoc.dev rather than sherlodoc.  Take either, since a name is what
   everything downstream compares against -- and appending ".dev" to one that
   already carries a version would ask for sherlodoc.dev.dev. *)
let package_name package =
  match OpamPackage.of_string_opt package with
  | Some package -> OpamPackage.name_to_string package
  | None -> package

let prefer_oldest_term =
  let doc = "Solve for the lowest version of each dependency the constraints allow, rather than the highest" in
  let env = Cmd.Env.info "DAY10_PREFER_OLDEST" in
  Arg.(value & flag & info [ "prefer-oldest" ] ~env ~doc)

let update_invariant_term =
  let doc = "Let the solver pick the OCaml version when --ocaml-version cannot be honoured, as needed to test a compiler package" in
  let env = Cmd.Env.info "DAY10_UPDATE_INVARIANT" in
  Arg.(value & flag & info [ "update-invariant" ] ~env ~doc)

let only_packages_term =
  let doc = "Treat only these packages as local, rather than every .opam file found in DIRECTORY (can be specified multiple times)" in
  let env = Cmd.Env.info "DAY10_ONLY_PACKAGES" in
  let arg = Arg.(value & opt_all string [] & info [ "only-packages" ] ~env ~docv:"PACKAGE" ~doc) in
  Term.(const (fun packages -> List.concat_map (String.split_on_char ',') packages |> List.map package_name) $ arg)

let fork_term =
  let doc = "Process packages in parallel using fork with N parallel jobs" in
  let env = Cmd.Env.info "DAY10_FORK" in
  Arg.(value & opt (some int) None & info [ "fork" ] ~env ~docv:"N" ~doc)

let opam_jobs_term =
  let doc = "How many jobs a package's build may run at once.  Defaults to one per core up to a ceiling, which is right for a machine running one build and too many for a worker running several." in
  let env = Cmd.Env.info "DAY10_OPAM_JOBS" in
  Arg.(value & opt (some int) None & info [ "opam-jobs" ] ~env ~docv:"N" ~doc)

let make_exec_config ~dir ~ocaml_version ~opam_repositories ~directory ~with_test ~with_doc ~log ~arch ~os ~os_distribution ~os_family ~os_version ~only_packages
    ~prefer_oldest ~update_invariant ~opam_jobs ~build_command =
  let ocaml_version = OpamPackage.of_string ocaml_version in
  let directory = Unix.realpath directory in
  let found = find_local_packages directory |> List.map fst in
  (* An empty --only-packages means every .opam file in the directory.  Naming
     them instead is for a caller that has already decided which are buildable,
     so the names have to be a subset: anything else is a typo, and left to the
     solver it would surface as an unrelated-looking failure to resolve. *)
  let local_packages =
    match only_packages with
    | [] -> found
    | packages -> (
        match List.filter (fun p -> not (List.mem p found)) packages with
        | [] -> packages
        | unknown ->
            OpamConsole.error "--only-packages named packages with no .opam file in %s: %s" directory (String.concat ", " unknown);
            exit 1)
  in
  {
    Config.dir;
    ocaml_version;
    opam_repositories;
    package = "";
    arch;
    os;
    os_distribution;
    os_family;
    os_version;
    directory = Some directory;
    md = None;
    json = None;
    dot = None;
    with_test;
    with_doc;
    tag = None;
    oci = None;
    log;
    dry_run = false;
    fork = None;
    build_command;
    local_packages;
    prefer_oldest;
    update_invariant;
    opam_jobs;
  }

let exec_cmd =
  let directory_arg =
    let doc = "Directory containing the project" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DIRECTORY" ~doc)
  in
  let command_args =
    let doc = "Command to run in the container (use -- to separate from options)" in
    Arg.(non_empty & pos_right 0 string [] & info [] ~docv:"CMD" ~doc)
  in
  let exec_term =
    Term.(
      const (fun dir ocaml_version opam_repositories directory cmd with_test with_doc log arch os os_distribution os_family os_version only_packages prefer_oldest update_invariant opam_jobs ->
          run_build
            (make_exec_config ~dir ~ocaml_version ~opam_repositories ~directory ~with_test ~with_doc ~log ~arch ~os ~os_distribution ~os_family ~os_version
               ~only_packages ~prefer_oldest ~update_invariant ~opam_jobs ~build_command:(Some { Config.run = String.concat " " ([ "opam"; "exec"; "--" ] @ List.map Filename.quote cmd); network = true })))
      $ cache_dir_term $ ocaml_version_term $ opam_repository_term $ directory_arg $ command_args $ with_test_term $ with_doc_term $ log_term $ arch_term $ os_term $ os_distribution_term $ os_family_term $ os_version_term $ only_packages_term
      $ prefer_oldest_term $ update_invariant_term $ opam_jobs_term)
  in
  let exec_info = Cmd.info "exec" ~doc:"Run a command in a container with the project's dependencies" in
  Cmd.v exec_info exec_term

let build_cmd =
  let directory_arg =
    let doc = "Directory to build" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DIRECTORY" ~doc)
  in
  let dune_args =
    let doc = "Extra arguments passed to dune build (e.g. @runtest, @install)" in
    Arg.(value & pos_right 0 string [] & info [] ~docv:"ARGS" ~doc)
  in
  let build_term =
    Term.(
      const (fun dir ocaml_version opam_repositories directory dune_extra with_test with_doc log arch os os_distribution os_family os_version only_packages prefer_oldest update_invariant opam_jobs ->
          run_build
            (make_exec_config ~dir ~ocaml_version ~opam_repositories ~directory ~with_test ~with_doc ~log ~arch ~os ~os_distribution ~os_family ~os_version
               ~only_packages ~prefer_oldest ~update_invariant ~opam_jobs ~build_command:(Some { Config.run = Build_command.dune ~only_packages dune_extra; network = false })))
      $ cache_dir_term $ ocaml_version_term $ opam_repository_term $ directory_arg $ dune_args $ with_test_term $ with_doc_term $ log_term $ arch_term $ os_term $ os_distribution_term $ os_family_term $ os_version_term $ only_packages_term
      $ prefer_oldest_term $ update_invariant_term $ opam_jobs_term)
  in
  let build_info = Cmd.info "build" ~doc:"Build a project using cached dependencies (alias for: exec . -- dune build)" in
  Cmd.v build_info build_term

let ci_cmd =
  let directory_arg =
    let doc = "Directory to test" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"DIRECTORY" ~doc)
  in
  let ci_term =
    Term.(
      const (fun dir ocaml_version opam_repositories directory md json dot with_test log dry_run oci arch os os_distribution os_family os_version fork prefer_oldest update_invariant opam_jobs ->
          let ocaml_version = OpamPackage.of_string ocaml_version in
          let package_names = find_local_packages directory |> List.map fst in
          run_ci
            {
              dir;
              ocaml_version;
              opam_repositories;
              package = List.hd package_names;
              arch;
              os;
              os_distribution;
              os_family;
              os_version;
              directory = Some directory;
              md;
              json;
              dot;
              with_test;
              with_doc = false;
              tag = None;
              oci;
              log;
              dry_run;
              fork;
              build_command = None;
              local_packages = package_names;
              prefer_oldest;
              update_invariant;
              opam_jobs;
            })
      $ cache_dir_term $ ocaml_version_term $ opam_repository_term $ directory_arg $ md_term $ json_term $ dot_term $ with_test_term $ log_term $ dry_run_term $ oci_term $ arch_term $ os_term $ os_distribution_term $ os_family_term $ os_version_term $ fork_term
      $ prefer_oldest_term $ update_invariant_term $ opam_jobs_term)
  in
  let ci_info = Cmd.info "ci" ~doc:"Run CI tests on a directory" in
  Cmd.v ci_info ci_term

let health_check_cmd =
  let package_arg =
    let doc = "Package name to test (or @filename to read package list from file)" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"PACKAGE" ~doc)
  in
  let health_check_term =
    Term.(
      const (fun dir ocaml_version opam_repositories package_arg md json dot with_test log dry_run tag oci arch os os_distribution os_family os_version fork prefer_oldest update_invariant opam_jobs ->
          let ocaml_version = OpamPackage.of_string ocaml_version in
          run_health_check_multi
            { dir; ocaml_version; opam_repositories; package = ""; arch; os; os_distribution; os_family; os_version; directory = None; md; json; dot; with_test; with_doc = false; tag;oci; log; dry_run; fork; build_command = None; local_packages = []; prefer_oldest; update_invariant; opam_jobs }
            package_arg)
      $ cache_dir_term $ ocaml_version_term $ opam_repository_term $ package_arg $ md_term $ json_term $ dot_term $ with_test_term $ log_term $ dry_run_term $ tag_term $ oci_term $ arch_term $ os_term $ os_distribution_term $ os_family_term $ os_version_term $ fork_term
      $ prefer_oldest_term $ update_invariant_term $ opam_jobs_term)
  in
  let health_check_info = Cmd.info "health-check" ~doc:"Run health check on a package or list of packages" in
  Cmd.v health_check_info health_check_term

(* The cache-wide commands take these instead of the detected defaults the rest
   of day10 uses: a platform is something to select here, not something to
   assume, and only a component actually given should narrow the search. *)
let platform_filter_terms =
  let distribution =
    let doc = "Restrict to platforms of this OS distribution (default: every platform in the cache)" in
    Arg.(value & opt (some string) None & info [ "os-distribution" ] ~docv:"OS_DISTRIBUTION" ~doc)
  in
  let version =
    let doc = "Restrict to platforms of this OS version (default: every platform in the cache)" in
    Arg.(value & opt (some string) None & info [ "os-version" ] ~docv:"OS_VERSION" ~doc)
  in
  let arch =
    let doc = "Restrict to platforms of this architecture (default: every platform in the cache)" in
    Arg.(value & opt (some string) None & info [ "arch" ] ~docv:"ARCH" ~doc)
  in
  (distribution, version, arch)

let cache_info_cmd =
  let distribution_arg, version_arg, arch_arg = platform_filter_terms in
  let cache_info_term =
    Term.(
      const (fun dir distribution version arch np -> Cache.info ~dir ~distribution ~version ~arch ~np)
      $ cache_dir_term $ distribution_arg $ version_arg $ arch_arg $ fork_term)
  in
  let cache_info_info =
    Cmd.info "cache-info" ~doc:"Report what the cache holds, by platform, age and outcome"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Reports every platform in the cache, broken down by how long ago each layer was last used and by whether it built. Age is time since \
             last use, not since the layer was created, which is what prune sorts on too.";
          `P
            "A layer's size is recorded beside it the first time it is measured, so the first run over an existing cache walks it and later runs do \
             not. Use --fork to measure in parallel. Directories that cannot be read unprivileged are skipped, which undercounts by whatever is \
             inside them.";
        ]
  in
  Cmd.v cache_info_info cache_info_term

let refresh_base_cmd =
  let max_age_arg =
    let doc = "Leave a platform alone if its index was refreshed less than $(docv) hours ago, so this is safe to call on every idle window." in
    Arg.(value & opt (some int) None & info [ "max-age" ] ~docv:"HOURS" ~doc)
  in
  let distribution_arg, version_arg, arch_arg = platform_filter_terms in
  let refresh_base_term =
    Term.(
      const (fun dir distribution version arch log max_age -> run_refresh_base ~dir ~distribution ~version ~arch ~log max_age)
      $ cache_dir_term $ distribution_arg $ version_arg $ arch_arg $ log_term $ max_age_arg)
  in
  let refresh_base_info =
    Cmd.info "refresh-base" ~doc:"Bring base images' package indexes up to date, in place"
      ~man:
        [
          `S Manpage.s_description;
          `P
            "Runs the distribution's index update inside each base image. The index is build-time state rather than something a cached layer was \
             compiled against, so layers built on a base stay valid -- unlike rebuilding it, which would leave them standing on libraries they never \
             saw and cost a rebuild of every one.";
          `P "Every platform in the cache, unless --os-distribution, --os-version or --arch names one.";
          `P
            "Intended to be driven by whatever knows the machine is idle: an ocluster worker can pause, call this, and resume. Nothing else may be \
             building while it runs, since it writes into a base that every build reads.";
        ]
  in
  Cmd.v refresh_base_info refresh_base_term

let prune_cmd =
  let days_arg =
    let doc = "Delete cache entries unused for more than $(docv) days." in
    Arg.(value & opt (some int) None & info [ "days" ] ~docv:"N" ~doc)
  in
  let percent_arg =
    let doc = "Keep the newest $(docv)% of the cache by size; delete least recently used entries beyond that." in
    Arg.(value & opt (some int) None & info [ "percent" ] ~docv:"N" ~doc)
  in
  let max_size_arg =
    let doc = "Delete least recently used entries until the cache is within $(docv), which must carry a unit: 40G, 500M." in
    Arg.(value & opt (some string) None & info [ "max-size" ] ~docv:"SIZE" ~doc)
  in
  let distribution_arg, version_arg, arch_arg = platform_filter_terms in
  let prune_term =
    Term.(
      const (fun dir distribution version arch np days percent max_size ->
          let mode =
            match days, percent, max_size with
            | Some d, None, None when d >= 0 -> Cache.Keep_days d
            | None, Some p, None when p >= 0 && p <= 100 -> Cache.Keep_percent p
            | None, None, Some size -> (
                match Cache.size_of_string size with
                | Some bytes when bytes >= 0 -> Cache.Max_size bytes
                | _ ->
                    OpamConsole.error "--max-size wants a size with a unit, such as 40G or 500M, not %S" size;
                    exit 1)
            | None, None, None ->
                OpamConsole.error "Specify one of --days N, --percent N or --max-size SIZE";
                exit 1
            | Some _, None, None ->
                OpamConsole.error "--days must be >= 0";
                exit 1
            | None, Some _, None ->
                OpamConsole.error "--percent must be between 0 and 100";
                exit 1
            | _ ->
                OpamConsole.error "--days, --percent and --max-size are mutually exclusive";
                exit 1
          in
          Cache.prune ~dir ~distribution ~version ~arch ?np mode)
      $ cache_dir_term $ distribution_arg $ version_arg $ arch_arg $ fork_term $ days_arg $ percent_arg $ max_size_arg)
  in
  let prune_info = Cmd.info "prune" ~doc:"Delete cache entries by age (--days), by count (--percent) or to fit a size (--max-size)" in
  Cmd.v prune_info prune_term

let list_cmd =
  let list_term =
    Term.(
      const (fun ocaml_version opam_repositories all_versions json arch os os_distribution os_family os_version ->
          let ocaml_version = OpamPackage.of_string ocaml_version in
          run_list
            { dir = ""; ocaml_version; opam_repositories; package = ""; arch; os; os_distribution; os_family; os_version; directory = None; md = None; json; dot = None; with_test = false; with_doc = false; tag = None; oci = None; log = false; dry_run = false; fork = None; build_command = None; local_packages = []; prefer_oldest = false; update_invariant = false; opam_jobs = None }
            all_versions)
      $ ocaml_version_term $ opam_repository_term $ all_versions_term $ json_term $ arch_term $ os_term $ os_distribution_term $ os_family_term $ os_version_term)
  in
  let list_info = Cmd.info "list" ~doc:"List packages in opam repositories" in
  Cmd.v list_info list_term

let main_info =
  let doc = "A tool for running CI and health checks" in
  let man =
    [
      `S Manpage.s_description;
      `P "This tool provides CI testing and health checking capabilities.";
      `P "Use '$(mname) ci DIRECTORY' to run CI tests on a directory.";
      `P "Use '$(mname) health-check PACKAGE' to run health checks on a package.";
      `P "Use '$(mname) health-check @FILENAME' to run health checks on multiple packages listed in FILENAME (JSON format: {\"packages\":[...]})";
      `P "Use '$(mname) list' list packages in opam repository.";
      `P "Use '$(mname) prune --days N' to delete cache entries older than N days, or '$(mname) prune --percent N' to keep only the newest N% of entries.";
      `P "Add --md flag to output results in markdown format.";
      `S Manpage.s_examples;
      `P "$(mname) ci --cache-dir /tmp/cache --opam-repository /tmp/opam-repository /path/to/project";
      `P "$(mname) health-check --cache-dir /tmp/cache --opam-repositories /tmp/opam-repository package --md";
      `P "$(mname) health-check --cache-dir /tmp/cache --opam-repositories /tmp/opam-repository @packages.json";
      `P "$(mname) list --opam-repositories /tmp/opam-repository";
      `P "$(mname) prune --cache-dir /tmp/cache --percent 90";
    ]
  in
  Cmd.info "day10" ~version:"0.0.1" ~doc ~man

let load_env_file path =
  if Sys.file_exists path then
    let seen = Hashtbl.create 16 in
    Os.read_from_file path |> String.split_on_char '\n'
    |> List.iter (fun line ->
      match String.split_on_char '=' line with
      | key :: rest when key <> "" && key.[0] <> '#' ->
          let env_key = "DAY10_" ^ key in
          let value = String.concat "=" rest in
          let value =
            if Hashtbl.mem seen env_key then
              (Sys.getenv_opt env_key |> Option.value ~default:"") ^ "," ^ value
            else value
          in
          Hashtbl.replace seen env_key ();
          Unix.putenv env_key value
      | _ -> ())

(* Find the directory positional arg of exec/build/ci in argv so we can load
   that project's .day10 file before cmdliner consumes DAY10_* env vars.
   Heuristic: the first argv entry (after the subcommand, before `--`) that
   points at a directory containing a .day10 file. *)
let find_project_dir_from_argv () =
  match Array.to_list Sys.argv with
  | _ :: ("exec" | "build" | "ci") :: rest ->
      let rec find = function
        | [] | "--" :: _ -> None
        | arg :: _ when Sys.file_exists (Filename.concat arg ".day10") -> Some arg
        | _ :: rest -> find rest
      in
      find rest
  | _ -> None

let () =
  Option.iter (fun home -> load_env_file (Filename.concat home ".day10")) (Sys.getenv_opt "HOME");
  load_env_file ".day10";
  Option.iter (fun dir -> load_env_file (Filename.concat dir ".day10")) (find_project_dir_from_argv ());
  Cleanup.install ();
  let default_term = Term.(ret (const (`Help (`Pager, None)))) in
  let cmd = Cmd.group ~default:default_term main_info [ build_cmd; exec_cmd; ci_cmd; health_check_cmd; list_cmd; cache_info_cmd; refresh_base_cmd; prune_cmd ] in
  exit (Cleanup.main (fun () -> Cmd.eval ~catch:false cmd))
