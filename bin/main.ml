module Solver = Opam_0install.Solver.Make (Opam_0install.Dir_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let () = OpamFormatConfig.init ()
let root = OpamStateConfig.opamroot ()
let _ = OpamStateConfig.load_defaults root
let () = OpamCoreConfig.init ?debug_level:(Some 10) ?debug_sections:(Some (OpamStd.String.Map.singleton "foo" (Some 10))) ()
let std_env = Opam_0install.Dir_context.std_env ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian" ~os_family:"debian" ~os_version:"12" ()
let opam_repository = "/home/mtelvers/opam-repository/packages"

(* let commit = "c940a5f1a97ea0abf72b5f8a3319f15551c41337" *)
(* let commit = "a263eb47bd23b8c5e3555052b1f086fe933eebde" *)
let commit = "64a9d673ccf21203b08de3ef29ca06ad97d5bc3c"

let opam_file pkg =
  let opam_path = List.fold_left Filename.concat opam_repository [ OpamPackage.name_to_string pkg; OpamPackage.to_string pkg; "opam" ] in
  OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))

let solve pkg =
  let constraints =
    OpamPackage.Name.Map.of_list
      [ (OpamPackage.Name.of_string "ocaml", (`Eq, OpamPackage.Version.of_string "5.3.0")); (OpamPackage.name pkg, (`Eq, OpamPackage.version pkg)) ]
  in
  let context = Opam_0install.Dir_context.create ~env:std_env ~constraints opam_repository in
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
  Array.fold_left
    (fun acc d -> Filename.concat opam_repository d |> Sys.readdir |> Array.fold_left (fun acc d -> OpamPackage.Set.add (OpamPackage.of_string d) acc) acc)
    OpamPackage.Set.empty (Sys.readdir opam_repository)

let latest =
  OpamPackage.Name.Map.fold
    (fun n vset base -> OpamPackage.Set.add (OpamPackage.create n (OpamPackage.Version.Set.max_elt vset)) base)
    (OpamPackage.to_map all_packages) OpamPackage.Set.empty

let latest_available =
  let pinned = OpamPackage.Set.singleton (OpamPackage.of_string "ocaml.5.3.0") in
  let pinned_names = OpamPackage.names_of_packages pinned in
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

let write_to_file filename str = Out_channel.with_open_text filename @@ fun oc -> Out_channel.output_string oc str
let append_to_file filename str = Out_channel.with_open_gen [ Open_text; Open_append; Open_creat ] 0o644 filename @@ fun oc -> Out_channel.output_string oc str
let config_dir = List.fold_left (fun acc f -> Filename.concat acc f) "/home/mtelvers/day29"
let hostname = "builder"
let () = write_to_file (config_dir [ "hosts" ]) ("127.0.0.1 localhost " ^ hostname)

let env =
  [
    ("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin");
    ("HOME", "/home/opam");
    ("OPAMYES", "1");
    ("OPAMCONFIRMLEVEL", "unsafe-yes");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
  ]

let sudo ?stdout ?stderr cmd =
  let () = OpamConsole.note "%s" (String.concat " " cmd) in
  Sys.command (Filename.quote_command ?stdout ?stderr "sudo" cmd)

let rec find_all_deps solution deps acc =
  OpamPackage.Set.fold
    (fun dep acc ->
      match OpamPackage.Set.mem dep acc with
      | true -> acc
      | false -> find_all_deps solution (OpamPackage.Map.find dep solution) (OpamPackage.Set.add dep acc))
    deps acc

let hash_of_set s = s |> OpamPackage.Set.to_list |> List.map OpamPackage.to_string |> String.concat " " |> Digest.string |> Digest.to_hex
let ocaml = solve (OpamPackage.of_string "ocaml.5.3.0")

module IntSet = Set.Make (Int)

let _ =
  OpamPackage.Set.fold
    (fun package acc ->
      let acc =
        let rec loop acc =
          if IntSet.cardinal acc <= 32 then acc
          else
            let running, finished =
              IntSet.partition
                (fun pid ->
                  let c, _ = Unix.waitpid [ WNOHANG ] pid in
                  pid <> c)
                acc
            in
            let () = if IntSet.is_empty finished then Unix.sleepf 0.1 in
            loop running
        in
        loop acc
      in
      match Unix.fork () with
      | 0 ->
          let filename = config_dir [ "results"; commit; "solution"; OpamPackage.to_string package ] in
          if not (Sys.file_exists filename) then
            ignore
              (solve package
              |> OpamPackage.Map.filter (fun x _ -> not (OpamPackage.Map.mem x ocaml))
              |> OpamPackage.Map.map (fun x -> OpamPackage.Set.filter (fun y -> not (OpamPackage.Map.mem y ocaml)) x)
              |> Json_solution.save filename);
          exit 0
      | child -> IntSet.add child acc)
    latest_available IntSet.empty

let () =
  OpamPackage.Set.iter
    (fun package ->
      let chrono = OpamConsole.timer () in
      let solution =
        let filename = config_dir [ "results"; commit; "solution"; OpamPackage.to_string package ] in
        if Sys.file_exists filename then Json_solution.load filename
        else
          solve package
          |> OpamPackage.Map.filter (fun x _ -> not (OpamPackage.Map.mem x ocaml))
          |> OpamPackage.Map.map (fun x -> OpamPackage.Set.filter (fun y -> not (OpamPackage.Map.mem y ocaml)) x)
          |> Json_solution.save filename
      in
      let () = OpamConsole.note "solve for %s took %.3fs" (OpamPackage.to_string package) (chrono ()) in
      let chrono = OpamConsole.timer () in
      let dependencies = OpamPackage.Map.mapi (fun pkg deps -> find_all_deps solution deps (OpamPackage.Set.singleton pkg)) solution in
      let frequency =
        OpamPackage.Map.mapi (fun pkg _ -> OpamPackage.Map.fold (fun _ deps sum -> if OpamPackage.Set.mem pkg deps then sum + 1 else sum) dependencies 0) dependencies
      in
      let () = OpamConsole.note "finding all dependencies took %.3fs" (chrono ()) in
      let chrono = OpamConsole.timer () in
      let ordered_installation = topological_sort frequency solution in
      let () = OpamConsole.note "topological sort took %.3fs" (chrono ()) in
      ignore
        (List.fold_left
           (fun acc pkg ->
             if acc = 0 then
               let () = OpamConsole.note "pkg %s" (OpamPackage.to_string pkg) in
               let deps = OpamPackage.Map.find pkg solution in
               let () =
                 if not (OpamPackage.Set.is_empty deps) then
                   OpamConsole.note "deps %s" (OpamPackage.Set.to_list deps |> List.map OpamPackage.to_string |> String.concat ",")
               in
               let alldeps = OpamPackage.Map.find pkg dependencies in
               let bad =
                 OpamPackage.Set.fold (fun pkg acc -> acc || Sys.file_exists (config_dir [ "results"; commit; "bad"; OpamPackage.to_string pkg ])) alldeps false
               in
               if not bad then
                 let hash = hash_of_set alldeps in
                 let upperdir = config_dir [ hash ] in
                 if not (Sys.file_exists upperdir) then
                   let argv =
                     [
                       "/usr/bin/env";
                       "bash";
                       "-c";
                       "opamh.exe make-state --output=$HOME/.opam/default/.opam-switch/switch-state --quiet && opam-build -v " ^ OpamPackage.to_string pkg;
                     ]
                   in
                   let workdir = config_dir [ "work" ] in
                   let lowerdir = config_dir [ "rootfs" ] in
                   let chrono = OpamConsole.timer () in
                   let () =
                     if OpamPackage.Set.is_empty deps then Sys.mkdir upperdir 0o755
                     else
                       OpamPackage.Set.iter
                         (fun dep ->
                           let () = OpamConsole.note "%s" (OpamPackage.to_string dep) in
                           let () = OpamConsole.note "built with %s" (find_all_deps solution (OpamPackage.Set.singleton dep) OpamPackage.Set.empty |> OpamPackage.Set.to_list |> List.map OpamPackage.to_string |> String.concat " ") in
                           assert (
                             0
                             = sudo
                                 [
                                   "cp";
                                   "--no-clobber";
                                   "--archive";
                                   "--no-dereference";
                                   "--recursive";
                                   "--reflink=auto";
                                   "--no-target-directory";
                                   config_dir [ hash_of_set (find_all_deps solution (OpamPackage.Set.singleton dep) OpamPackage.Set.empty) ];
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
                       { ty = "bind"; src = config_dir [ "download-cache" ]; dst = "/home/opam/.opam/download-cache"; options = [ "rbind"; "rprivate" ] };
                       { ty = "bind"; src = config_dir [ "hosts" ]; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
                     ]
                   in
                   let config = Json_config.make ~cwd:"/home/opam" ~argv ~hostname ~uid:1000 ~gid:1000 ~env ~mounts ~network:true in
                   let () = write_to_file (config_dir [ "config.json" ]) (Yojson.Safe.pretty_to_string config) in
                   let () = OpamConsole.note "configuration files created in %.3fs" (chrono ()) in
                   let chrono = OpamConsole.timer () in
                   let temp = Filename.temp_file ~temp_dir:(config_dir [ "temp" ]) "build-" (OpamPackage.to_string pkg) in
                   let r = sudo ~stdout:temp ~stderr:temp [ "runc"; "run"; "-b"; config_dir []; "build" ] in
                   let () = OpamConsole.note "runc ran for %.3fs" (chrono ()) in
                   let chrono = OpamConsole.timer () in
                   let _ =
                     if r = 0 then
                       let () = append_to_file (config_dir [ "results"; commit; "good"; OpamPackage.to_string pkg ]) "b" in
                       let () = Sys.rename temp (Filename.concat upperdir "build.log") in
                       sudo [ "rm"; "-rf"; Filename.concat upperdir "tmp" ]
                     else
                       let () = Sys.rename temp (config_dir [ "results"; commit; "bad"; OpamPackage.to_string pkg ]) in
                       sudo [ "rm"; "-rf"; upperdir ]
                   in
                   let () = OpamConsole.note "tidy up took %.3fs" (chrono ()) in
                   r
                 else acc
               else
                 let () = OpamConsole.warning "bad package or dependency" in
                 1
             else acc)
           0 ordered_installation))
    latest_available

let emit_page name page =
  let open Tyxml.Html in
  Printf.printf "Generating: %s\n" name;
  let file_handle = open_out name in
  let fmt = Format.formatter_of_out_channel file_handle in
  Format.fprintf fmt "%a@." (pp ~indent:true ()) page;
  close_out file_handle

let log_to_pre textfile =
  let open Tyxml.Html in
  let input = In_channel.with_open_text textfile @@ fun ic -> In_channel.input_all ic in
  html (head (title (txt textfile)) []) (body [ pre [ txt input ] ])

let index_html =
  let open Tyxml.Html in
  html
    (head (title (txt "day10")) [ link ~rel:[ `Stylesheet ] ~href:"stylesheet.css" () ])
    (body
       [
         table
           (OpamPackage.Set.to_list_map
              (fun package ->
                let () = OpamConsole.note "Package %s" (OpamPackage.to_string package) in
                let solution = Json_solution.load (config_dir [ "results"; commit; "solution"; OpamPackage.to_string package ]) in
                let alldeps = OpamPackage.Map.mapi (fun pkg deps -> find_all_deps solution deps (OpamPackage.Set.singleton pkg)) solution in
                let frequency =
                  OpamPackage.Map.mapi
                    (fun pkg _ -> OpamPackage.Map.fold (fun _ deps sum -> if OpamPackage.Set.mem pkg deps then sum + 1 else sum) alldeps 0)
                    alldeps
                in
                let ordered_installation = topological_sort frequency solution in
                let name = OpamPackage.to_string package in
                let style, result =
                  if OpamPackage.Map.is_empty solution then ("skipped", txt "no solution")
                  else if Sys.file_exists (config_dir [ "results"; commit; "good"; name ]) then ("good", txt "good")
                  else if Sys.file_exists (config_dir [ "results"; commit; "bad"; name ]) then
                    ("bad", a ~a:[ a_href ("results/" ^ commit ^ "/bad/" ^ name) ] [ txt "bad" ])
                  else ("bad", txt "bad dependency")
                in
                tr ~a:[]
                  [
                    td [ txt (OpamPackage.to_string package) ];
                    td
                      [
                        details
                          (summary ~a:[ a_class [ style ] ] [ result ])
                          [
                            ol
                              (List.map
                                 (fun pkg ->
                                   let style, content =
                                     let () = OpamConsole.note "pkg %s" (OpamPackage.to_string pkg) in
                                     let deps = OpamPackage.Map.find pkg solution in
                                     let alldeps = find_all_deps solution deps (OpamPackage.Set.singleton pkg) in
                                     let hash = hash_of_set alldeps in
                                     let () = OpamConsole.note "hash %s" hash in
                                     let build_log = config_dir [ hash; "build.log" ] in
                                     if Sys.file_exists build_log then
                                       let () = emit_page (config_dir [ hash ^ ".html" ]) (log_to_pre build_log) in
                                       ("good", a ~a:[ a_href (hash ^ ".html") ] [ txt (OpamPackage.to_string pkg) ])
                                     else
                                       let bad_log = config_dir [ "results"; commit; "bad"; OpamPackage.to_string pkg ] in
                                       if Sys.file_exists bad_log then
                                         let () = emit_page (config_dir [ hash ^ ".html" ]) (log_to_pre bad_log) in
                                         ("bad", a ~a:[ a_href (hash ^ ".html") ] [ txt (OpamPackage.to_string pkg) ])
                                       else ("skipped", txt (OpamPackage.to_string pkg))
                                   in
                                   li ~a:[ a_class [ style ] ] [ content ])
                                 ordered_installation);
                          ];
                      ];
                  ])
              latest_available
           |> List.rev);
       ])

let () = emit_page (config_dir [ "index.html" ]) index_html
