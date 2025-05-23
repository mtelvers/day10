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

let () =
  List.iter Os.mkdir
    [ Config.dir []; Config.dir [ "results" ]; Config.dir [ "results"; commit ]; Config.dir [ "temp" ]; Config.dir [ "work" ]; Config.dir [ "html" ] ]

let () =
  List.iter
    (fun version ->
      List.iter Os.mkdir
        [
          Config.dir [ "results"; commit; version ];
          Config.dir [ "results"; commit; version; "good" ];
          Config.dir [ "results"; commit; version; "bad" ];
          Config.dir [ "results"; commit; version; "solution" ];
          Config.dir [ "results"; commit; version; "status" ];
          Config.dir [ "results"; commit; version; "dot" ];
          Config.dir [ "work"; version ];
          Config.dir [ "html"; version ];
        ])
    versions

let opam_file pkg =
  let opam_path = Config.opam_repository [ "packages"; OpamPackage.name_to_string pkg; OpamPackage.to_string pkg; "opam" ] in
  OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))

let env pkg v =
  let () = Printf.printf "Var name %s\n%!" (OpamVariable.Full.to_string v) in
  (*  if List.mem v OpamPackageVar.predefined_depends_variables then (Some (OpamTypes.B true))
  else *)
  match OpamVariable.Full.to_string v with
  | "version" -> Some (OpamTypes.S (OpamPackage.Version.to_string (OpamPackage.version pkg)))
  | "with-test"
  | "with-dev-setup"
  | "dev"
  | "with-doc" ->
      Some (OpamTypes.B false)
  | "build" -> Some (OpamTypes.B true)
  | x -> std_env x

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
      let deptree =
        OpamPackage.Set.fold
          (fun pkg acc ->
            let opam = opam_file pkg in
            let deps = OpamFile.OPAM.depends opam |> OpamFilter.partial_filter_formula (env pkg) in
            let with_post = OpamFilter.filter_deps ~build:true ~post:true deps |> OpamFormula.all_names in
            let without_post = OpamFilter.filter_deps ~build:true ~post:false deps |> OpamFormula.all_names in
            let deppost = OpamPackage.Name.Set.diff with_post without_post in
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
      dfs OpamPackage.Map.empty pkg
  | Error problem ->
      OpamConsole.error "No solution";
      print_endline (Solver.diagnostics problem);
      OpamPackage.Map.empty

let all_packages =
  let packages = Config.opam_repository [ "packages" ] in
  Array.fold_left
    (fun acc d -> Filename.concat packages d |> Sys.readdir |> Array.fold_left (fun acc d -> OpamPackage.Set.add (OpamPackage.of_string d) acc) acc)
    OpamPackage.Set.empty (Sys.readdir packages)

(*
let all_packages =
  OpamPackage.Set.(
    empty |> add (OpamPackage.of_string "0install.2.18") |> add (OpamPackage.of_string "alcotest.1.8.0") |> add (OpamPackage.of_string "base.v0.17.1"))
   *)

let all_packages = OpamPackage.Set.filter (fun pkg -> String.starts_with ~prefix:"a" (OpamPackage.to_string pkg)) all_packages

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
}

let compilers =
  List.map
    (fun version ->
      let ocaml_version = OpamPackage.create (OpamPackage.Name.of_string "ocaml") (OpamPackage.Version.of_string version) in
      { version; ocaml_version })
    versions

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
             if not (Sys.file_exists filename) then
               ignore
                 (solve compiler.ocaml_version package |> Json_solution.save filename
                 |> Dot_solution.save (Config.dir [ "results"; commit; compiler.version; "dot"; OpamPackage.to_string package ]))))
    compilers

type layer = {
  package : string;
  deps : string;
  hash : string;
  copy : float;
  build : float;
  tidy : float;
  result : int;
}
[@@deriving yojson]

type result =
  [ `Success
  | `Failed
  | `No_solution
  ]
[@@deriving yojson]

type status = {
  package : string;
  compiler : string;
  find_deps : float;
  sort : float;
  layers : layer list;
  result : result;
}
[@@deriving yojson]

let build_layer compiler solution dependencies pkg =
  let layer = { package = OpamPackage.to_string pkg; deps = ""; hash = ""; copy = 0.; build = 0.; tidy = 0.; result = 0 } in
  let deps = OpamPackage.Map.find pkg solution in
  let layer = { layer with deps = OpamPackage.Set.to_string deps } in
  let alldeps = OpamPackage.Map.find pkg dependencies in
  let bad =
    OpamPackage.Set.fold
      (fun pkg acc -> acc || Sys.file_exists (Config.dir [ "results"; commit; compiler.version; "bad"; OpamPackage.to_string pkg ]))
      alldeps false
  in
  if bad then { layer with result = 1 }
  else
    let hash = hash_of_set alldeps in
    let layer = { layer with hash } in
    let upperdir = Config.dir [ hash ] in
    if not (Sys.file_exists upperdir) then
      let argv =
        [
          "/usr/bin/env";
          "bash";
          "-c";
          String.concat " && "
            [
              "if [ -e $HOME/.opam/default/.opam-switch/switch-state ] ; then opamh.exe make-state --output=$HOME/.opam/default/.opam-switch/switch-state \
               --quiet; fi";
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
                      (* "--no-clobber"; *)
                      "--update=none";
                      "--archive";
                      "--no-dereference";
                      "--recursive";
                      "--link";
                      "--no-target-directory";
                      Config.dir [ hash_of_set (find_all_deps solution (OpamPackage.Set.singleton dep) OpamPackage.Set.empty) ];
                      upperdir;
                    ]))
            deps
      in
      let layer = { layer with copy = chrono () } in
      let chrono = OpamConsole.timer () in
      let mounts =
        [
          { Json_config.ty = "overlay"; src = "overlay"; dst = "/"; options = [ "lowerdir=" ^ lowerdir; "upperdir=" ^ upperdir; "workdir=" ^ workdir ] };
          { ty = "bind"; src = Config.download_cache; dst = "/home/opam/.opam/download-cache"; options = [ "rbind"; "rprivate" ] };
          { ty = "bind"; src = Config.dir [ "hosts" ]; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
        ]
      in
      let temp_dir = Filename.temp_dir ~temp_dir:(Config.dir [ "temp" ]) ~perms:0o755 "runc-" ("-" ^ compiler.version) in
      let () = Os.mkdir (Filename.concat temp_dir "dummy") in
      let config = Json_config.make ~root:"dummy" ~cwd:"/home/opam" ~argv ~hostname ~uid:1000 ~gid:1000 ~env:Config.env ~mounts ~network:true in
      let () = Os.write_to_file (Filename.concat temp_dir "config.json") (Yojson.Safe.pretty_to_string config) in
      let build_log = Filename.concat upperdir "build.log" in
      let result = Os.sudo ~stdout:build_log ~stderr:build_log [ "runc"; "run"; "-b"; temp_dir; OpamPackage.to_string pkg ^ "-" ^ compiler.version ] in
      let layer = { layer with build = chrono () } in
      let chrono = OpamConsole.timer () in
      let _ =
        if result = 0 then
          let () = Os.append_to_file (Config.dir [ "results"; commit; compiler.version; "good"; OpamPackage.to_string pkg ]) "b" in
          Os.sudo [ "rm"; "-rf"; Filename.concat upperdir "tmp"; temp_dir ]
        else
          let () = Sys.rename build_log (Config.dir [ "results"; commit; compiler.version; "bad"; OpamPackage.to_string pkg ]) in
          Os.sudo [ "rm"; "-rf"; upperdir; temp_dir ]
      in
      let layer = { layer with tidy = chrono () } in
      { layer with result }
    else
      let () = Os.append_to_file (Config.dir [ "results"; commit; compiler.version; "good"; OpamPackage.to_string pkg ]) "-" in
      { layer with result = 0 }

let build compiler package =
  let status = { package = OpamPackage.to_string package; compiler = compiler.version; find_deps = 0.; sort = 0.; layers = []; result = `Success } in
  let solution = Json_solution.load (Config.dir [ "results"; commit; compiler.version; "solution"; status.package ]) in
  let chrono = OpamConsole.timer () in
  let dependencies = OpamPackage.Map.mapi (fun pkg deps -> find_all_deps solution deps (OpamPackage.Set.singleton pkg)) solution in
  let status = { status with find_deps = chrono () } in
  let chrono = OpamConsole.timer () in
  let ordered_installation = topological_sort solution in
  let status = { status with sort = chrono () } in
  let layers =
    List.fold_left
      (fun (lst : layer list) pkg ->
        match lst with
        | [] -> [ build_layer compiler solution dependencies pkg ]
        | acc :: _ when acc.result = 0 -> build_layer compiler solution dependencies pkg :: lst
        | _ -> lst)
      [] ordered_installation
  in
  let result =
    match layers with
    | [] -> `No_solution
    | acc :: _ when acc.result = 0 -> `Success
    | _ -> `Failed
  in
  { status with layers; result } |> status_to_yojson |> Yojson.Safe.to_file (Config.dir [ "results"; commit; compiler.version; "status"; status.package ])

let () =
  Os.fork
    (fun compiler ->
      OpamPackage.Set.iter
        (fun package ->
          if not (Sys.file_exists (Config.dir [ "results"; commit; compiler.version; "status"; OpamPackage.to_string package ])) then build compiler package)
        latest_available)
    compilers

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
          let status = Yojson.Safe.from_file (Config.dir [ "results"; commit; compiler.version; "status"; name ]) |> status_of_yojson |> Result.get_ok in
          html
            (head (title (txt name)) [ link ~rel:[ `Stylesheet ] ~href:"/stylesheet.css" () ])
            (body
               (List.map
                  (fun (layer : layer) ->
                    let style = if layer.result = 0 then "good" else "bad" in
                    let content =
                      let build_log = Config.dir [ layer.hash; "build.log" ] in
                      if Sys.file_exists build_log then pre [ txt (Os.read_from_file build_log) ]
                      else
                        let bad_log = Config.dir [ "results"; commit; compiler.version; "bad"; layer.package ] in
                        if Sys.file_exists bad_log then pre [ txt (Os.read_from_file bad_log) ] else pre []
                    in
                    details (summary ~a:[ a_class [ style ] ] [ txt layer.package ]) [ content ])
                  status.layers))
          |> emit_page (Config.dir [ "html"; compiler.version; name ^ ".html" ]))
        latest_available)
    compilers

(* Ordered by date *)
(* let commits = Sys.readdir (Config.dir [ "results" ]) |> Array.to_list *)

let commits = [ "4592800a5b7fe3cf52f4afb459b13d9138a079d4" ]

let set_of_dir commit compiler dir =
  Sys.readdir (Config.dir [ "results"; commit; compiler.version; dir ])
  |> Array.fold_left (fun acc file -> OpamPackage.Set.add (OpamPackage.of_string file) acc) OpamPackage.Set.empty

type details = {
  compiler : compiler;
  solution : OpamPackage.Set.t;
  good : OpamPackage.Set.t;
  bad : OpamPackage.Set.t;
  status : OpamPackage.Set.t;
}

type results = {
  commit : string;
  details : details list;
}

let results =
  List.map
    (fun commit ->
      {
        commit;
        details =
          List.map
            (fun compiler ->
              {
                compiler;
                solution = set_of_dir commit compiler "solution";
                good = set_of_dir commit compiler "good";
                bad = set_of_dir commit compiler "bad";
                status = set_of_dir commit compiler "status";
              })
            compilers;
      })
    commits

(*
          let style =
            match status.result with
            | `Sucess -> "good"
            | `No_solution -> "skipped"
            | `Failed -> "bad"
          in
   *)

let html_list_of_set compiler s =
  let open Tyxml.Html in
  ul
    (OpamPackage.Set.to_list_map
       (fun s ->
         let name = OpamPackage.to_string s in
         li [ a ~a:[ a_href (compiler.version ^ "/" ^ name ^ ".html") ] [ txt name ] ])
       s)

let rec loop = function
  | g0 :: g1 :: tl ->
      let open Tyxml.Html in
      [ h1 [ txt (g0.commit ^ " - " ^ g1.commit) ] ]
      @ (List.map2
           (fun d0 d1 ->
             let removed = OpamPackage.Set.diff d0.solution d1.solution in
             let added = OpamPackage.Set.diff d1.solution d0.solution in
             [ h2 [ txt "removed solution" ]; html_list_of_set d0.compiler removed; h2 [ txt "added solution" ]; html_list_of_set d0.compiler added ])
           g0.details g1.details
        |> List.flatten)
      @ (List.map2
           (fun d0 d1 ->
             let added = OpamPackage.Set.diff d1.good d0.good in
             [ h2 [ txt "added good" ]; html_list_of_set d0.compiler added ])
           g0.details g1.details
        |> List.flatten)
      @ (List.map2
           (fun d0 d1 ->
             let removed = OpamPackage.Set.diff d0.bad d1.bad in
             let added = OpamPackage.Set.diff d1.bad d0.bad in
             [ h2 [ txt "removed bad" ]; html_list_of_set d0.compiler removed; h2 [ txt "added bad" ]; html_list_of_set d0.compiler added ])
           g0.details g1.details
        |> List.flatten)
      @ loop (g1 :: tl)
  | [ g0 ] ->
      let open Tyxml.Html in
      [
        h1 [ txt g0.commit ];
        table
          [
            tr (List.map (fun d0 -> td [ h2 [ txt "Solutions" ]; html_list_of_set d0.compiler d0.solution ]) g0.details);
            tr (List.map (fun d0 -> td [ h2 [ txt "Good" ]; html_list_of_set d0.compiler d0.good ]) g0.details);
            tr (List.map (fun d0 -> td [ h2 [ txt "Bad" ]; html_list_of_set d0.compiler d0.bad ]) g0.details);
          ];
      ]
  | _ -> []

let () =
  let open Tyxml.Html in
  html (head (title (txt "index")) [ link ~rel:[ `Stylesheet ] ~href:"/stylesheet.css" () ]) (body (loop results))
  |> emit_page (Config.dir [ "html"; "index.html" ])
