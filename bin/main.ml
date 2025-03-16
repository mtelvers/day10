module Solver = Opam_0install.Solver.Make (Opam_0install.Dir_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let versions = [ "5.3.0"; "4.14.2"; "4.08.2" ]
let hostname = "builder"
let () = Os.write_to_file (Config.dir [ "hosts" ]) ("127.0.0.1 localhost " ^ hostname)
let () = OpamFormatConfig.init ()
let root = OpamStateConfig.opamroot ()
let _ = OpamStateConfig.load_defaults root
let () = OpamCoreConfig.init ?debug_level:(Some 10) ?debug_sections:(Some (OpamStd.String.Map.singleton "foo" (Some 10))) ()
let std_env = Opam_0install.Dir_context.std_env ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian" ~os_family:"debian" ~os_version:"12" ()
let commit = Os.run ("git -C " ^ Config.opam_repository [] ^ " rev-parse HEAD") |> String.trim

let () =
  if not (Sys.file_exists (Config.dir [ "results"; commit ])) then
    let argv = [ "/usr/bin/env"; "bash"; "-c"; "opam update" ] in
    let mounts =
      [
        { Json_config.ty = "bind"; src = Config.opam_repository []; dst = "/home/opam/opam-repository"; options = [ "rbind"; "rprivate" ] };
        { ty = "bind"; src = Config.download_cache; dst = "/home/opam/.opam/download-cache"; options = [ "rbind"; "rprivate" ] };
        { ty = "bind"; src = Config.dir [ "hosts" ]; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
      ]
    in
    let config = Json_config.make ~root:"rootfs" ~cwd:"/home/opam" ~argv ~hostname ~uid:1000 ~gid:1000 ~env:Config.env ~mounts ~network:true in
    let () = Os.write_to_file (Config.dir [ "config.json" ]) (Yojson.Safe.pretty_to_string config) in
    let r = Os.sudo [ "runc"; "run"; "-b"; Config.dir []; "build" ] in
    assert (r = 0)

let () = List.iter Os.mkdir [ Config.dir []; Config.dir [ "results" ]; Config.dir [ "results"; commit ]; Config.dir [ "temp" ]; Config.dir [ "work" ] ]

let () =
  List.iter
    (fun version ->
      List.iter Os.mkdir
        [
          Config.dir [ "results"; commit; version ];
          Config.dir [ "results"; commit; version; "good" ];
          Config.dir [ "results"; commit; version; "bad" ];
          Config.dir [ "results"; commit; version; "solution" ];
          Config.dir [ "work"; version ];
        ])
    versions

let opam_file pkg =
  let opam_path = Config.opam_repository [ "packages"; OpamPackage.name_to_string pkg; OpamPackage.to_string pkg; "opam" ] in
  OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))

let solve ocaml_version pkg =
  let constraints =
    OpamPackage.Name.Map.of_list
      [ (OpamPackage.name ocaml_version, (`Eq, OpamPackage.version ocaml_version)); (OpamPackage.name pkg, (`Eq, OpamPackage.version pkg)) ]
  in
  let context = Opam_0install.Dir_context.create ~env:std_env ~constraints (Config.opam_repository [ "packages" ]) in
  let r = Solver.solve context [ OpamPackage.name pkg ] in
  match r with
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
      OpamPackage.Set.fold
        (fun pkg acc ->
          let opam = opam_file pkg in
          let depopts = OpamFile.OPAM.depopts opam |> OpamFormula.all_names in
          let depopts = OpamPackage.Name.Set.inter depopts pkgnames |> OpamPackage.Name.Set.to_list in
          let name = OpamPackage.name pkg in
          let deps = expand (`Opam name) @ depopts |> OpamPackage.Name.Set.of_list |> OpamPackage.packages_of_names pkgs in
          OpamPackage.Map.add pkg deps acc)
        pkgs OpamPackage.Map.empty
  | Error problem ->
      OpamConsole.error "No solution";
      print_endline (Solver.diagnostics problem);
      OpamPackage.Map.empty

let all_packages =
  let packages = Config.opam_repository [ "packages" ] in
  Array.fold_left
    (fun acc d -> Filename.concat packages d |> Sys.readdir |> Array.fold_left (fun acc d -> OpamPackage.Set.add (OpamPackage.of_string d) acc) acc)
    OpamPackage.Set.empty (Sys.readdir packages)

let latest =
  OpamPackage.Name.Map.fold
    (fun n vset base -> OpamPackage.Set.add (OpamPackage.create n (OpamPackage.Version.Set.max_elt vset)) base)
    (OpamPackage.to_map all_packages) OpamPackage.Set.empty

let latest_available =
  let pinned_names = OpamPackage.Name.Set.singleton (OpamPackage.Name.of_string "ocaml") in
  OpamPackage.Set.filter
    (fun pkg ->
      let opam = opam_file pkg in
      let avail = OpamFile.OPAM.available opam in
      (not (OpamPackage.Name.Set.mem pkg.name pinned_names)) && OpamFilter.eval_to_bool ~default:false (fun v -> std_env (OpamVariable.Full.to_string v)) avail)
    latest

let rec topological_sort freq pkgs =
  match OpamPackage.Map.is_empty pkgs with
  | true -> []
  | false ->
      (* Find all packages which can be installed *)
      let installable = OpamPackage.Map.filter (fun _ deps -> OpamPackage.Set.is_empty deps) pkgs in
      let () = assert (not (OpamPackage.Map.is_empty installable)) in
      (* find most frequent dep *)
      let i =
        OpamPackage.Map.to_list installable |> List.map fst
        |> List.sort (fun p1 p2 -> compare (OpamPackage.Map.find p2 freq) (OpamPackage.Map.find p1 freq))
        |> List.hd
      in
      (* Remove package i and remove the dependency on i from all other packages *)
      let pkgs = OpamPackage.Map.remove i pkgs |> OpamPackage.Map.map (fun deps -> OpamPackage.Set.remove i deps) in
      i :: topological_sort freq pkgs

let rec find_all_deps solution deps acc =
  OpamPackage.Set.fold
    (fun dep acc ->
      match OpamPackage.Set.mem dep acc with
      | true -> acc
      | false -> find_all_deps solution (OpamPackage.Map.find dep solution) (OpamPackage.Set.add dep acc))
    deps acc

let hash_of_set s = s |> OpamPackage.Set.to_list |> List.map OpamPackage.to_string |> String.concat " " |> Digest.string |> Digest.to_hex

type compiler = {
  version : string;
  ocaml_version : OpamPackage.t;
  solution : OpamPackage.Set.t OpamPackage.Map.t;
}

let compilers =
  List.map
    (fun version ->
      let ocaml_version = OpamPackage.create (OpamPackage.Name.of_string "ocaml") (OpamPackage.Version.of_string version) in
      { version; ocaml_version; solution = solve ocaml_version ocaml_version })
    versions

let munge { ocaml_version; solution; _ } pkg =
  OpamPackage.Map.filter (fun x _ -> not (OpamPackage.Map.mem x solution)) pkg
  |> OpamPackage.Map.map (fun x -> OpamPackage.Set.filter_map (fun y -> if OpamPackage.Map.mem y solution then Some ocaml_version else Some y) x)
  |> OpamPackage.Map.map (fun x -> if OpamPackage.Set.is_empty x then OpamPackage.Set.singleton ocaml_version else x)
  |> OpamPackage.Map.add ocaml_version OpamPackage.Set.empty

(*
let () =
  OpamPackage.Map.iter
    (fun pkg deps ->
      let () = Printf.printf "pkg %s\n" (OpamPackage.to_string pkg) in
      OpamPackage.Set.iter (fun pkg -> Printf.printf "- pkg %s\n" (OpamPackage.to_string pkg)) deps)
    x
   *)

let () =
  List.iter
    (fun compiler ->
      OpamPackage.Set.to_list latest_available
      |> Os.fork (fun package ->
             let filename = Config.dir [ "results"; commit; compiler.version; "solution"; OpamPackage.to_string package ] in
             if not (Sys.file_exists filename) then ignore (solve compiler.ocaml_version package |> munge compiler |> Json_solution.save filename)))
    compilers

let build package compiler =
  let () = OpamConsole.note "Package %s on %s" (OpamPackage.to_string package) compiler.version in
  let solution = Json_solution.load (Config.dir [ "results"; commit; compiler.version; "solution"; OpamPackage.to_string package ]) in
  let chrono = OpamConsole.timer () in
  let dependencies = OpamPackage.Map.mapi (fun pkg deps -> find_all_deps solution deps (OpamPackage.Set.singleton pkg)) solution in
  let frequency =
    OpamPackage.Map.mapi
      (fun pkg _ -> OpamPackage.Map.fold (fun _ deps sum -> if OpamPackage.Set.mem pkg deps then sum + 1 else sum) dependencies 0)
      dependencies
  in
  let () = OpamConsole.note "finding all dependencies took %.3fs" (chrono ()) in
  let chrono = OpamConsole.timer () in
  let ordered_installation = topological_sort frequency solution in
  let () = OpamConsole.note "topological sort took %.3fs" (chrono ()) in
  ignore
    (List.fold_left
       (fun acc pkg ->
         if acc = 0 then
           let () = OpamConsole.note "building pkg %s" (OpamPackage.to_string pkg) in
           let deps = OpamPackage.Map.find pkg solution in
           let () =
             if not (OpamPackage.Set.is_empty deps) then
               OpamConsole.note "deps %s" (OpamPackage.Set.to_list deps |> List.map OpamPackage.to_string |> String.concat ",")
           in
           let alldeps = OpamPackage.Map.find pkg dependencies in
           let bad =
             OpamPackage.Set.fold
               (fun pkg acc -> acc || Sys.file_exists (Config.dir [ "results"; commit; compiler.version; "bad"; OpamPackage.to_string pkg ]))
               alldeps false
           in
           if not bad then
             let hash = hash_of_set alldeps in
             let upperdir = Config.dir [ hash ] in
             if not (Sys.file_exists upperdir) then
               let argv =
                 [ "/usr/bin/env"; "bash"; "-c" ]
                 @
                 if pkg = compiler.ocaml_version then [ "opam switch create default " ^ OpamPackage.to_string pkg ]
                 else
                   [
                     String.concat " && "
                       [
                         "if [ -e $HOME/.opam/default/.opam-switch/switch-state ] ; then opamh.exe make-state \
                          --output=$HOME/.opam/default/.opam-switch/switch-state --quiet; fi";
                         "opam-build -v " ^ OpamPackage.to_string pkg;
                       ];
                   ]
               in
               let workdir = Config.dir [ "work"; compiler.version ] in
               let lowerdir = Config.dir [ "rootfs" ] in
               let chrono = OpamConsole.timer () in
               let () =
                 if OpamPackage.Set.is_empty deps then Os.mkdir upperdir
                 else
                   OpamPackage.Set.iter
                     (fun dep ->
                       assert (
                         0
                         = Os.sudo
                             [
                               "cp";
                               "--no-clobber";
                               "--archive";
                               "--no-dereference";
                               "--recursive";
                               "--reflink=auto";
                               "--no-target-directory";
                               Config.dir [ hash_of_set (find_all_deps solution (OpamPackage.Set.singleton dep) OpamPackage.Set.empty) ];
                               upperdir;
                             ]))
                     deps
               in
               let () = OpamConsole.note "copy took %.3fs" (chrono ()) in
               let chrono = OpamConsole.timer () in
               let mounts =
                 [
                   {
                     Json_config.ty = "overlay";
                     src = "overlay";
                     dst = "/";
                     options = [ "lowerdir=" ^ lowerdir; "upperdir=" ^ upperdir; "workdir=" ^ workdir ];
                   };
                   { ty = "bind"; src = Config.download_cache; dst = "/home/opam/.opam/download-cache"; options = [ "rbind"; "rprivate" ] };
                   { ty = "bind"; src = Config.dir [ "hosts" ]; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
                 ]
               in
               let temp_dir = Filename.temp_dir ~temp_dir:(Config.dir [ "temp" ]) ~perms:0o755 "runc-" ("-" ^ compiler.version) in
               let () = Os.mkdir (Filename.concat temp_dir "dummy") in
               let config = Json_config.make ~root:"dummy" ~cwd:"/home/opam" ~argv ~hostname ~uid:1000 ~gid:1000 ~env:Config.env ~mounts ~network:true in
               let () = Os.write_to_file (Filename.concat temp_dir "config.json") (Yojson.Safe.pretty_to_string config) in
               let () = OpamConsole.note "configuration files created in %.3fs" (chrono ()) in
               let chrono = OpamConsole.timer () in
               let build_log = Filename.concat upperdir "build.log" in
               let r = Os.sudo ~stdout:build_log ~stderr:build_log [ "runc"; "run"; "-b"; temp_dir; compiler.version ] in
               let () = OpamConsole.note "runc ran for %.3fs" (chrono ()) in
               let chrono = OpamConsole.timer () in
               let _ =
                 if r = 0 then
                   let () = Os.append_to_file (Config.dir [ "results"; commit; compiler.version; "good"; OpamPackage.to_string pkg ]) "b" in
                   Os.sudo [ "rm"; "-rf"; Filename.concat upperdir "tmp"; temp_dir ]
                 else
                   let () = Sys.rename build_log (Config.dir [ "results"; commit; compiler.version; "bad"; OpamPackage.to_string pkg ]) in
                   Os.sudo [ "rm"; "-rf"; upperdir; temp_dir ]
               in
               let () = OpamConsole.note "tidy up took %.3fs" (chrono ()) in
               r
             else
               let () = Os.append_to_file (Config.dir [ "results"; commit; compiler.version; "good"; OpamPackage.to_string pkg ]) "-" in
               acc
           else
             let () = OpamConsole.warning "bad package or dependency" in
             1
         else acc)
       0 ordered_installation)

let () =
  OpamPackage.Set.to_list_map (fun package -> compilers |> List.map (fun c -> (package, c))) latest_available
  |> List.flatten
  |> Os.fork ~np:4 (fun (package, compiler) -> build package compiler)

let () = exit 0

let emit_page name page =
  let open Tyxml.Html in
  Printf.printf "Generating: %s\n" name;
  let file_handle = open_out name in
  let fmt = Format.formatter_of_out_channel file_handle in
  Format.fprintf fmt "%a@." (pp ~indent:true ()) page;
  close_out file_handle

let () =
  let open Tyxml.Html in
  List.iter
    (fun compiler ->
      OpamPackage.Set.iter
        (fun package ->
          let name = OpamPackage.to_string package in
          let () = OpamConsole.note "Package %s" name in
          let solution = Json_solution.load (Config.dir [ "results"; commit; compiler.version; "solution"; OpamPackage.to_string package ]) in
          let alldeps = OpamPackage.Map.mapi (fun pkg deps -> find_all_deps solution deps (OpamPackage.Set.singleton pkg)) solution in
          let frequency =
            OpamPackage.Map.mapi (fun pkg _ -> OpamPackage.Map.fold (fun _ deps sum -> if OpamPackage.Set.mem pkg deps then sum + 1 else sum) alldeps 0) alldeps
          in
          let ordered_installation = topological_sort frequency solution in
          let style, result =
            if OpamPackage.Map.is_empty solution then ("skipped", txt "no solution")
            else if Sys.file_exists (Config.dir [ "results"; commit; compiler.version; "good"; name ]) then ("good", txt "good")
            else if Sys.file_exists (Config.dir [ "results"; commit; compiler.version; "bad"; name ]) then
              ("bad", a ~a:[ a_href ("results/" ^ commit ^ "/bad/" ^ name) ] [ txt "bad" ])
            else ("bad", txt "bad dependency")
          in
          html
            (head (title (txt name)) [ link ~rel:[ `Stylesheet ] ~href:"stylesheet.css" () ])
            (body
               (List.map
                  (fun pkg ->
                    let name = OpamPackage.to_string pkg in
                    let content =
                      let deps = OpamPackage.Map.find pkg solution in
                      let alldeps = find_all_deps solution deps (OpamPackage.Set.singleton pkg) in
                      let hash = hash_of_set alldeps in
                      let build_log = Config.dir [ hash; "build.log" ] in
                      if Sys.file_exists build_log then pre [ txt (Os.read_from_file build_log) ]
                      else
                        let bad_log = Config.dir [ "results"; commit; compiler.version; "bad"; OpamPackage.to_string pkg ] in
                        if Sys.file_exists bad_log then pre [ txt (Os.read_from_file bad_log) ] else pre []
                    in
                    details (summary ~a:[ a_class [ style ] ] [ txt name ]) [ content ])
                  ordered_installation))
          |> emit_page (Config.dir [ "html"; name ^ ".html" ]))
        latest_available)
    compilers

(* Ordered by date *)
(* let commits = Sys.readdir (Config.dir [ "results" ]) |> Array.to_list *)

let commits =
  [
    "64a9d673ccf21203b08de3ef29ca06ad97d5bc3c";
    "94a68d7e968e714ae17a21d84962e93eadbb0ffb";
    "8707d628f2beb80e7f60b89d60c33bdf2ffd9026";
    "862a7640b194b6ef60dc2d24341920e48dd021fe";
  ]

let set_of_dir commit compiler dir =
  Sys.readdir (Config.dir [ "results"; commit; compiler.version; dir ])
  |> Array.fold_left (fun acc file -> OpamPackage.Set.add (OpamPackage.of_string file) acc) OpamPackage.Set.empty

type results = {
  commit : string;
  solution : OpamPackage.Set.t;
  good : OpamPackage.Set.t;
  bad : OpamPackage.Set.t;
}

let results =
  List.map
    (fun commit ->
      List.map
        (fun compiler ->
          { commit; solution = set_of_dir commit compiler "solution"; good = set_of_dir commit compiler "good"; bad = set_of_dir commit compiler "bad" })
        compilers)
    commits
  |> List.flatten

let html_list_of_set s =
  let open Tyxml.Html in
  ul
    (OpamPackage.Set.to_list_map
       (fun s ->
         let name = OpamPackage.to_string s in
         li [ a ~a:[ a_href (name ^ ".html") ] [ txt name ] ])
       s)

let rec loop = function
  | g0 :: g1 :: tl ->
      let open Tyxml.Html in
      let acc = [ h1 [ txt (g0.commit ^ " - " ^ g1.commit) ] ] in
      let removed = OpamPackage.Set.diff g0.solution g1.solution in
      let added = OpamPackage.Set.diff g1.solution g0.solution in
      let acc = acc @ [ h2 [ txt "removed solution" ]; html_list_of_set removed ] in
      let acc = acc @ [ h2 [ txt "added solution" ]; html_list_of_set added ] in
      let added = OpamPackage.Set.diff g1.good g0.good in
      let acc = acc @ [ h2 [ txt "added good" ]; html_list_of_set added ] in
      let removed = OpamPackage.Set.diff g0.bad g1.bad in
      let added = OpamPackage.Set.diff g1.bad g0.bad in
      let acc = acc @ [ h2 [ txt "removed bad" ]; html_list_of_set removed ] in
      let acc = acc @ [ h2 [ txt "added bad" ]; html_list_of_set added ] in
      acc @ loop (g1 :: tl)
  | _ -> []

let () =
  let open Tyxml.Html in
  html (head (title (txt "index")) [ link ~rel:[ `Stylesheet ] ~href:"stylesheet.css" () ]) (body (loop results))
  |> emit_page (Config.dir [ "html"; "index.html" ])
