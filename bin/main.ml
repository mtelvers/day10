module Solver = Opam_0install.Solver.Make (Opam_0install.Switch_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let () = OpamFormatConfig.init ()
let root = OpamStateConfig.opamroot ()
let _ = OpamStateConfig.load_defaults root
let () = OpamCoreConfig.init ?debug_level:(Some 10) ?debug_sections:(Some (OpamStd.String.Map.singleton "foo" (Some 10))) ()
let constraints = OpamPackage.Name.Map.of_list [ (OpamPackage.Name.of_string "ocaml", (`Eq, OpamPackage.Version.of_string "5.3.0")) ]

let solve pkg st =
  let context = Opam_0install.Switch_context.create ~constraints st in
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
          let depopts = OpamFile.OPAM.depopts (OpamSwitchState.opam st pkg) |> OpamFormula.all_names in
          let depopts = OpamPackage.Name.Set.inter depopts pkgnames |> OpamPackage.Name.Set.to_list in
          let name = OpamPackage.name pkg in
          let deps = expand (`Opam name) @ depopts |> OpamPackage.Name.Set.of_list |> OpamPackage.packages_of_names pkgs in
          OpamPackage.Map.add pkg deps acc)
        pkgs OpamPackage.Map.empty
  | Error problem ->
      OpamConsole.error "No solution";
      print_endline (Solver.diagnostics problem);
      OpamPackage.Map.empty

let available =
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none gt @@ fun st ->
  OpamSwitchState.compute_available_packages gt st.switch st.switch_config ~pinned:OpamPackage.Set.empty ~opams:st.opams

let latest =
  OpamPackage.Name.Map.fold
    (fun n vset base -> OpamPackage.Set.add (OpamPackage.create n (OpamPackage.Version.Set.max_elt vset)) base)
    (OpamPackage.to_map available) OpamPackage.Set.empty

let rec topological_sort pkgs =
  match OpamPackage.Map.is_empty pkgs with
  | true -> []
  | false ->
      (* Find any package which can be installed - could sort by frequency *)
      let i = OpamPackage.Map.filter (fun _ deps -> OpamPackage.Set.is_empty deps) pkgs |> OpamPackage.Map.choose |> fun (i, _) -> i in
      (* Remove package i and remove the dependency on i from all other packages *)
      let pkgs = OpamPackage.Map.remove i pkgs |> OpamPackage.Map.map (fun deps -> OpamPackage.Set.remove i deps) in
      i :: topological_sort pkgs

let write_to_file filename str = Out_channel.with_open_text filename @@ fun oc -> Out_channel.output_string oc str
let append_to_file filename str = Out_channel.with_open_gen [ Open_text; Open_append; Open_creat ] 0o644 filename @@ fun oc -> Out_channel.output_string oc str
let config_dir = List.fold_left (fun acc f -> Filename.concat acc f) "/home/mtelvers/day28"
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

let sudo cmd =
  let () = OpamConsole.note "%s" (String.concat " " cmd) in
  Sys.command (Filename.quote_command "sudo" cmd)

let ocaml =
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none gt @@ fun st -> solve (OpamPackage.of_string "ocaml.5.3.0") st

let () =
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none gt @@ fun st ->
  latest
  |> OpamPackage.Set.iter (fun package ->
         let chrono = OpamConsole.timer () in
         let solution =
           let filename = config_dir [ "results"; "solution"; OpamPackage.to_string package ] in
           if Sys.file_exists filename then Json_solution.load filename
           else
             solve package st
             |> OpamPackage.Map.filter (fun x _ -> not (OpamPackage.Map.mem x ocaml))
             |> OpamPackage.Map.map (fun x -> OpamPackage.Set.filter (fun y -> not (OpamPackage.Map.mem y ocaml)) x)
             |> Json_solution.save filename
         in
         let () = OpamConsole.note "solve for %s took %.3fs" (OpamPackage.to_string package) (chrono ()) in
         let chrono = OpamConsole.timer () in
         let ordered_installation = topological_sort solution in
         let () = OpamConsole.note "topological sort took %.3fs" (chrono ()) in
         ignore
           (List.fold_left
              (fun acc pkg ->
                if acc = 0 && not (Sys.file_exists (config_dir [ "results"; "bad"; OpamPackage.to_string pkg ])) then
                  let () = OpamConsole.note "Pkg %s" (OpamPackage.to_string pkg) in
                  let deps = OpamPackage.Map.find pkg solution in
                  let () =
                    if not (OpamPackage.Set.is_empty deps) then
                      OpamConsole.note "deps %s" (OpamPackage.Set.to_list deps |> List.map OpamPackage.to_string |> String.concat ",")
                  in
                  let rec loop deps acc =
                    OpamPackage.Set.fold (fun dep acc -> loop (OpamPackage.Map.find dep solution) (OpamPackage.Set.add dep acc)) deps acc
                  in
                  let alldeps = loop deps OpamPackage.Set.empty in
                  let hash_of_set s = s |> OpamPackage.Set.to_list |> List.map OpamPackage.to_string |> String.concat " " |> Digest.string |> Digest.to_hex in
                  let hash = hash_of_set (OpamPackage.Set.add pkg alldeps) in
                  let upperdir = config_dir [ hash ] in
                  if not (Sys.file_exists upperdir) then
                    let argv =
                      [
                        "/usr/bin/env";
                        "bash";
                        "-c";
                        "opamh.exe make-state --output=$HOME/.opam/5.3/.opam-switch/switch-state --quiet && opam-build -v " ^ OpamPackage.to_string pkg;
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
                            ignore
                              (sudo
                                 [
                                   "cp";
                                   "--no-clobber";
                                   "--archive";
                                   "--no-dereference";
                                   "--recursive";
                                   "--reflink=auto";
                                   "--no-target-directory";
                                   config_dir [ hash_of_set (loop (OpamPackage.Set.singleton dep) OpamPackage.Set.empty) ];
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
                    let r = sudo [ "runc"; "run"; "-b"; config_dir []; "build" ] in
                    let () = OpamConsole.note "runc ran for %.3fs" (chrono ()) in
                    let chrono = OpamConsole.timer () in
                    let _ =
                      if r = 0 then
                        let () = append_to_file (config_dir [ "results"; "good"; OpamPackage.to_string pkg ]) "b" in
                        sudo [ "rm"; "-rf"; Filename.concat upperdir "tmp" ]
                      else
                        let () = append_to_file (config_dir [ "results"; "bad"; OpamPackage.to_string pkg ]) "b" in
                        sudo [ "rm"; "-rf"; upperdir ]
                    in
                    let () = OpamConsole.note "tidy up took %.3fs" (chrono ()) in
                    r
                  else
                    let () = OpamConsole.warning "layer exist" in
                    acc
                else acc)
              0 ordered_installation))
