module Solver = Opam_0install.Solver.Make (Opam_0install.Switch_context)
module Input = Solver.Input
module Output = Solver.Solver.Output
module Role = Solver.Input.Role
module Role_map = Output.RoleMap

let constraints = OpamPackage.Name.Map.of_list [ (OpamPackage.Name.of_string "ocaml", (`Eq, OpamPackage.Version.of_string "5.3.0")) ]

let solve pkg =
  let root = OpamStateConfig.opamroot () in
  OpamFormatConfig.init ();
  ignore (OpamStateConfig.load_defaults root);
  OpamCoreConfig.init ();
  OpamStateConfig.init ();
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_none gt @@ fun st ->
  let context = Opam_0install.Switch_context.create ~constraints st in
  let r = Solver.solve context [ OpamPackage.Name.of_string pkg ] in
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

let ocaml = solve "ocaml"

let obuilder =
  solve "obuilder"
  |> OpamPackage.Map.filter (fun x _ -> not (OpamPackage.Map.mem x ocaml))
  |> OpamPackage.Map.map (fun x -> OpamPackage.Set.filter (fun y -> not (OpamPackage.Map.mem y ocaml)) x)

let () =
  OpamPackage.Map.iter
    (fun k v ->
      Printf.printf "Pkg %s\n" (OpamPackage.to_string k);
      OpamPackage.Set.iter (fun p -> Printf.printf "- %s\n" (OpamPackage.to_string p)) v)
    obuilder

let rec topological_sort pkgs =
  match OpamPackage.Map.is_empty pkgs with
  | true -> []
  | false ->
      (* Find any package which can be installed - could sort by frequency *)
      let i = OpamPackage.Map.filter (fun _ deps -> OpamPackage.Set.is_empty deps) pkgs |> OpamPackage.Map.choose |> fun (i, _) -> i in
      (* Remove package i and remove the dependency on i from all other packages *)
      let pkgs = OpamPackage.Map.remove i pkgs |> OpamPackage.Map.map (fun deps -> OpamPackage.Set.remove i deps) in
      i :: topological_sort pkgs

let ordered_installation = topological_sort obuilder
let () = List.iter (fun pkg -> Printf.printf "%s\n" (OpamPackage.to_string pkg)) ordered_installation
let write_to_file filename str = Out_channel.with_open_text filename @@ fun oc -> Out_channel.output_string oc str
let config_dir = Filename.concat "/home/mtelvers/day28"
let hostname = "builder"

let env =
  [
    ("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin");
    ("HOME", "/home/opam");
    ("OPAMYES", "1");
    ("OPAMCONFIRMLEVEL", "unsafe-yes");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
  ]

let _ =
  List.fold_left
    (fun acc pkg ->
      let upperdir = config_dir (OpamPackage.to_string pkg) in
      if acc = 0 && not (Sys.file_exists upperdir) then
        let () = OpamConsole.note "Pkg %s\n%!" (OpamPackage.to_string pkg) in
        let () = Sys.mkdir upperdir 0o755 in
        let deps = OpamPackage.Map.find pkg obuilder in
        let argv =
          [ "/usr/bin/env"; "bash"; "-c"; "opamh.exe make-state --output=$HOME/.opam/5.3/.opam-switch/switch-state --quiet && opam-build " ^ OpamPackage.to_string pkg ]
        in
        let rec loop deps acc =
          OpamPackage.Set.fold (fun dep acc ->
              loop (OpamPackage.Map.find dep obuilder) (OpamPackage.Set.add dep acc)
            ) deps acc
        in
        let lowerdir =
          loop deps OpamPackage.Set.empty |>
          OpamPackage.Set.to_list |> List.map (fun d -> config_dir (OpamPackage.to_string d)) |> fun l -> String.concat ":" (l @ [ config_dir "rootfs" ])
        in
        let workdir = config_dir "work" in
        let mounts =
          [
            { Json_config.ty = "overlay"; src = "overlay"; dst = "/"; options = [ "lowerdir=" ^ lowerdir; "upperdir=" ^ upperdir; "workdir=" ^ workdir ] };
            { ty = "bind"; src = config_dir "download-cache"; dst = "/home/opam/.opam/download-cache"; options = [ "rbind"; "rprivate" ] };
            { ty = "bind"; src = config_dir "hosts"; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
          ]
        in
        let config = Json_config.make ~cwd:"/home/opam" ~argv ~hostname ~uid:1000 ~gid:1000 ~env ~mounts ~network:true in
        let () = write_to_file (config_dir "config.json") (Yojson.Safe.pretty_to_string config) in
        let () = write_to_file (config_dir "hosts") ("127.0.0.1 localhost " ^ hostname) in
        let r = Sys.command (Filename.quote_command "sudo" [ "runc"; "run"; "-b"; config_dir "/"; "build" ]) in
        let _ = if r <> 0 then Sys.command (Filename.quote_command "sudo" [ "rm"; "-rf"; upperdir ]) else 0 in
        r
      else acc)
    0 ordered_installation
