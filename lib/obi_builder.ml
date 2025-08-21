type compiler = {
  version : string;
  ocaml_version : OpamPackage.t;
}

let make_compiler version =
  let ocaml_version = OpamPackage.create (OpamPackage.Name.of_string "ocaml") (OpamPackage.Version.of_string version) in
  { version; ocaml_version }

let get_compilers config =
  List.map make_compiler config.Obi_config.ocaml_versions

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

let build_layer config commit compiler solution dependencies pkg =
  let layer = { Obi_solution.package = OpamPackage.to_string pkg; deps = ""; hash = ""; copy = 0.; build = 0.; tidy = 0.; result = 0 } in
  let deps = OpamPackage.Map.find pkg solution in
  let layer = { layer with deps = OpamPackage.Set.to_string deps } in
  let alldeps = OpamPackage.Map.find pkg dependencies in
  let bad =
    OpamPackage.Set.fold
      (fun pkg acc -> acc || Sys.file_exists (Obi_config.work_dir_path config [ "results"; commit; compiler.version; "bad"; OpamPackage.to_string pkg ]))
      alldeps false
  in
  if bad then { layer with result = 1 }
  else
    let hash = hash_of_set alldeps in
    let layer = { layer with hash } in
    let upperdir = Obi_config.work_dir_path config [ hash ] in
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
      let workdir = Obi_config.work_dir_path config [ "work"; compiler.version ] in
      let lowerdir = Obi_config.work_dir_path config [ "rootfs" ] in
      let chrono = OpamConsole.timer () in
      let () =
        if OpamPackage.Set.is_empty deps then Obi_os.mkdir upperdir
        else
          OpamPackage.Set.iter
            (fun dep ->
              assert (
                0
                = Obi_os.sudo
                    [
                      "cp";
                      "--update=none";
                      "--archive";
                      "--no-dereference";
                      "--recursive";
                      "--link";
                      "--no-target-directory";
                      Obi_config.work_dir_path config [ hash_of_set (find_all_deps solution (OpamPackage.Set.singleton dep) OpamPackage.Set.empty) ];
                      upperdir;
                    ]))
            deps
      in
      let layer = { layer with copy = chrono () } in
      let chrono = OpamConsole.timer () in
      let mounts =
        [
          { Obi_container.ty = "overlay"; src = "overlay"; dst = "/"; options = [ "lowerdir=" ^ lowerdir; "upperdir=" ^ upperdir; "workdir=" ^ workdir ] };
          { ty = "bind"; src = Obi_config.download_cache_path config; dst = "/home/opam/.opam/download-cache"; options = [ "rbind"; "rprivate" ] };
          { ty = "bind"; src = Obi_config.work_dir_path config [ "hosts" ]; dst = "/etc/hosts"; options = [ "ro"; "rbind"; "rprivate" ] };
        ]
      in
      let temp_dir = Filename.temp_dir ~temp_dir:(Obi_config.work_dir_path config [ "temp" ]) ~perms:0o755 "runc-" ("-" ^ compiler.version) in
      let () = Obi_os.mkdir (Filename.concat temp_dir "dummy") in
      let runtime_config = Obi_container.make_runtime_config ~root:"dummy" ~cwd:"/home/opam" ~argv ~hostname:config.hostname ~uid:1000 ~gid:1000 ~env:config.env ~mounts ~network:true in
      let config_file = Filename.concat temp_dir "config.json" in
      let () = Obi_os.write_to_file config_file (Yojson.Safe.pretty_to_string runtime_config) in
      let build_log = Filename.concat upperdir "build.log" in
      let result = Obi_os.sudo ~stdout:build_log ~stderr:build_log [ "runc"; "run"; "-b"; temp_dir; OpamPackage.to_string pkg ^ "-" ^ compiler.version ] in
      let layer = { layer with build = chrono () } in
      let chrono = OpamConsole.timer () in
      let _ =
        if result = 0 then
          let () = Obi_os.append_to_file (Obi_config.work_dir_path config [ "results"; commit; compiler.version; "good"; OpamPackage.to_string pkg ]) "b" in
          Obi_os.sudo [ "rm"; "-rf"; Filename.concat upperdir "tmp"; temp_dir ]
        else
          let () = Sys.rename build_log (Obi_config.work_dir_path config [ "results"; commit; compiler.version; "bad"; OpamPackage.to_string pkg ]) in
          Obi_os.sudo [ "rm"; "-rf"; upperdir; temp_dir ]
      in
      let layer = { layer with tidy = chrono () } in
      { layer with result }
    else
      let () = Obi_os.append_to_file (Obi_config.work_dir_path config [ "results"; commit; compiler.version; "good"; OpamPackage.to_string pkg ]) "-" in
      { layer with result = 0 }

let build_package config commit compiler package =
  let status = { Obi_solution.package = OpamPackage.to_string package; compiler = compiler.version; find_deps = 0.; sort = 0.; layers = []; result = `Success } in
  let solution = Obi_solution.load_json (Obi_config.work_dir_path config [ "results"; commit; compiler.version; "solution"; status.package ]) in
  let chrono = OpamConsole.timer () in
  let dependencies = OpamPackage.Map.mapi (fun pkg deps -> find_all_deps solution deps (OpamPackage.Set.singleton pkg)) solution in
  let status = { status with find_deps = chrono () } in
  let chrono = OpamConsole.timer () in
  let ordered_installation = topological_sort solution in
  let status = { status with sort = chrono () } in
  let layers =
    List.fold_left
      (fun (lst : Obi_solution.layer list) pkg ->
        match lst with
        | [] -> [ build_layer config commit compiler solution dependencies pkg ]
        | acc :: _ when acc.result = 0 -> build_layer config commit compiler solution dependencies pkg :: lst
        | _ -> lst)
      [] ordered_installation
  in
  let result =
    match layers with
    | [] -> `No_solution
    | acc :: _ when acc.result = 0 -> `Success
    | _ -> `Failed
  in
  { status with layers; result } |> Obi_solution.status_to_yojson |> Yojson.Safe.to_file (Obi_config.work_dir_path config [ "results"; commit; compiler.version; "status"; status.package ])