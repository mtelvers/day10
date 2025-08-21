module Config = Obi_config
module Os = Obi_os
module Container = Obi_container
module Solver = Obi_solver
module Solution = Obi_solution
module Builder = Obi_builder
module Report = Obi_report

let setup_directories (config : Config.t) =
  let () = OpamFormatConfig.init () in
  let root = OpamStateConfig.opamroot () in
  let _state = OpamStateConfig.load_defaults root in
  let () = OpamCoreConfig.init ?debug_level:(Some 10) ?debug_sections:(Some (OpamStd.String.Map.singleton "foo" (Some 10))) () in
  
  (* Create opam repository path relative to work dir if it's not absolute *)
  let opam_repo_path = 
    if Filename.is_relative config.opam_repository then
      Config.work_dir_path config ["opam-repository"]
    else
      config.opam_repository
  in
  
  (* Create download cache path relative to work dir if it's not absolute *)
  let download_cache_path = 
    if Filename.is_relative config.download_cache then
      Config.work_dir_path config ["download-cache"]
    else
      config.download_cache
  in
  
  (* Create the main work directory *)
  let _ = Os.mkdir (Config.work_dir_path config []) in
  
  (* Set up git safe directory to avoid ownership issues *)
  let _ = Sys.command ("git config --global --add safe.directory " ^ opam_repo_path) in
  
  (* Check if opam repository exists *)
  if not (Sys.file_exists opam_repo_path) then (
    Printf.printf "Cloning opam repository to %s...\n%!" opam_repo_path;
    let result = Sys.command ("git clone https://github.com/ocaml/opam-repository " ^ opam_repo_path) in
    if result <> 0 then failwith "Failed to clone opam repository"
  ) else (
    Printf.printf "Updating opam repository at %s...\n%!" opam_repo_path;
    let result = Sys.command ("git -C " ^ opam_repo_path ^ " pull origin master") in
    if result <> 0 then Printf.eprintf "Warning: Failed to update opam repository\n%!"
  );
  
  let commit = Os.run ("git -C " ^ opam_repo_path ^ " rev-parse HEAD") |> String.trim in
  Printf.printf "Using opam repository commit: %s\n%!" commit;
  
  (* Set up directory structure *)
  let () =
    List.iter Os.mkdir
      [ Config.work_dir_path config []; Config.work_dir_path config [ "results" ]; Config.work_dir_path config [ "results"; commit ]; 
        Config.work_dir_path config [ "temp" ]; Config.work_dir_path config [ "work" ]; Config.work_dir_path config [ "html" ];
        Config.work_dir_path config [ "dummy" ]; download_cache_path ]
  in
  
  (* Set up per-compiler directories *)
  let () =
    List.iter
      (fun version ->
        List.iter Os.mkdir
          [
            Config.work_dir_path config [ "results"; commit; version ];
            Config.work_dir_path config [ "results"; commit; version; "good" ];
            Config.work_dir_path config [ "results"; commit; version; "bad" ];
            Config.work_dir_path config [ "results"; commit; version; "solution" ];
            Config.work_dir_path config [ "results"; commit; version; "status" ];
            Config.work_dir_path config [ "results"; commit; version; "dot" ];
            Config.work_dir_path config [ "work"; version ];
            Config.work_dir_path config [ "html"; version ];
          ])
      config.ocaml_versions
  in
  
  (* Set up container rootfs if it doesn't exist *)
  let rootfs_path = Config.work_dir_path config ["rootfs"] in
  if not (Sys.file_exists rootfs_path) then (
    Printf.printf "Setting up container rootfs at %s...\n%!" rootfs_path;
    let _ = Os.sudo ["mkdir"; "-p"; rootfs_path] in
    
    (* Create Debian 12 rootfs *)
    Printf.printf "Creating Debian 12 container base...\n%!";
    (* First check if docker is available and user has permissions *)
    let docker_check = Sys.command "docker version >/dev/null 2>&1" in
    if docker_check <> 0 then (
      Printf.printf "Docker not available or permission denied.\n";
      Printf.printf "Skipping container rootfs creation. Please run:\n";
      Printf.printf "  sudo usermod -aG docker $USER\n";
      Printf.printf "Then log out and back in, and run setup again.\n%!";
      Printf.printf "Container rootfs setup skipped\n%!";
    ) else (
      let container_id = Os.run "docker run -d debian:12" |> String.trim in
      if container_id = "" then failwith "Failed to start container";
      let export_cmd = Printf.sprintf "docker export %s | sudo tar -C %s -x" container_id rootfs_path in
      let result = Sys.command export_cmd in
      let _ = Sys.command (Printf.sprintf "docker rm -f %s" container_id) in
      if result <> 0 then failwith "Failed to create container rootfs";
      
      (* Install opam *)
      Printf.printf "Installing opam in container...\n%!";
      let opam_url = "https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-x86_64-linux" in
      let opam_path = Filename.concat rootfs_path "usr/local/bin/opam" in
      let result = Os.sudo ["curl"; "-L"; opam_url; "-o"; opam_path] in
      if result <> 0 then failwith "Failed to download opam";
      let _ = Os.sudo ["chmod"; "+x"; opam_path] in
      
      (* Note: Original Makefile also installed opamh.exe and opam-build binaries *)
      (* These would need to be built separately and made available *)
      
      (* Set up sudoers for opam user *)
      let sudoers_dir = Filename.concat rootfs_path "etc/sudoers.d" in
      let _ = Os.sudo ["mkdir"; "-p"; sudoers_dir] in
      let sudoers_content = "opam ALL=(ALL:ALL) NOPASSWD:ALL\n" in
      let sudoers_file = Filename.concat sudoers_dir "opam" in
      Os.write_to_file "/tmp/obi-sudoers" sudoers_content;
      let _ = Os.sudo ["cp"; "/tmp/obi-sudoers"; sudoers_file] in
      let _ = Os.sudo ["rm"; "/tmp/obi-sudoers"] in
      
      Printf.printf "Container rootfs setup completed\n%!";
    );
  ) else (
    Printf.printf "Container rootfs already exists at %s\n%!" rootfs_path;
  );
  
  (* Set up ownership and permissions *)
  let _ = Os.sudo ["chown"; "-R"; "1000:1000"; download_cache_path] in
  let _ = Os.sudo ["chown"; "-R"; "1000:1000"; opam_repo_path] in
  
  (* Create hosts file *)
  let () = Os.write_to_file (Config.work_dir_path config [ "hosts" ]) ("127.0.0.1 localhost " ^ config.hostname) in
  
  (* Run initial container setup if needed *)
  let setup_marker = Config.work_dir_path config ["setup-complete"] in
  if not (Sys.file_exists setup_marker) then (
    Printf.printf "Running initial container setup...\n%!";
    let setup_cmd = [
      "/usr/bin/env"; "bash"; "-c";
      "apt update && apt upgrade -y && apt install build-essential unzip bubblewrap git sudo curl rsync -y && " ^
      "adduser --disabled-password --gecos 'opam' --no-create-home --home /home/opam opam && " ^
      "chown -R $(id -u opam):$(id -g opam) /home/opam && " ^
      "su - opam -c 'opam init -k local -a /home/opam/opam-repository --bare -y' && " ^
      "su - opam -c 'opam switch create default --empty'"
    ] in
    let mounts = [
      { Container.ty = "bind"; src = (if Filename.is_relative opam_repo_path then Filename.concat (Sys.getcwd ()) opam_repo_path else opam_repo_path); dst = "/home/opam/opam-repository"; options = ["rbind"; "rprivate"] };
      { ty = "bind"; src = (if Filename.is_relative download_cache_path then Filename.concat (Sys.getcwd ()) download_cache_path else download_cache_path); dst = "/home/opam/.opam/download-cache"; options = ["rbind"; "rprivate"] };
      { ty = "bind"; src = (let hosts_path = Config.work_dir_path config ["hosts"] in if Filename.is_relative hosts_path then Filename.concat (Sys.getcwd ()) hosts_path else hosts_path); dst = "/etc/hosts"; options = ["ro"; "rbind"; "rprivate"] };
    ] in
    let container_config = Container.make_runtime_config 
      ~root:"rootfs" ~cwd:"/home/opam" ~argv:setup_cmd 
      ~hostname:config.hostname ~uid:0 ~gid:0 ~env:config.env ~mounts ~network:true in
    let config_path = Config.work_dir_path config ["config.json"] in
    Os.write_to_file config_path (Yojson.Safe.pretty_to_string container_config);
    let result = Os.sudo ["runc"; "run"; "-b"; Config.work_dir_path config []; "setup"] in
    if result = 0 then (
      Os.write_to_file setup_marker "setup completed";
      Printf.printf "Initial container setup completed successfully\n%!";
    ) else (
      Printf.eprintf "Container setup failed with exit code %d\n%!" result;
      failwith "Container setup failed"
    )
  ) else (
    Printf.printf "Container setup already completed\n%!";
  );
  
  commit

let solve_packages config commit =
  let compilers = Builder.get_compilers config in
  let all_packages = Solver.get_all_packages config in
  Printf.printf "Found %d total packages\n%!" (OpamPackage.Set.cardinal all_packages);
  let latest = Solver.get_latest_packages all_packages in
  Printf.printf "Found %d latest versions\n%!" (OpamPackage.Set.cardinal latest);
  let latest_available = Solver.get_available_packages config latest in
  Printf.printf "Found %d available packages\n%!" (OpamPackage.Set.cardinal latest_available);
  let packages_to_solve = 
    match config.Config.limit with
    | None -> latest_available
    | Some n -> 
        Printf.printf "Limiting to %d packages for trial run\n%!" n;
        let package_list = OpamPackage.Set.to_list latest_available in
        let limited_list = 
          if List.length package_list <= n then package_list
          else List.filteri (fun i _ -> i < n) package_list
        in
        OpamPackage.Set.of_list limited_list
  in
  Printf.printf "Processing %d packages\n%!" (OpamPackage.Set.cardinal packages_to_solve);
  List.iter
    (fun compiler ->
      OpamPackage.Set.to_list packages_to_solve
      |> Os.fork (fun package ->
             let filename = Config.work_dir_path config [ "results"; commit; compiler.Builder.version; "solution"; OpamPackage.to_string package ] in
             if not (Sys.file_exists filename) then
               ignore
                 (Solver.solve config compiler.ocaml_version package |> Solution.save_json filename
                 |> Solution.save_dot (Config.work_dir_path config [ "results"; commit; compiler.version; "dot"; OpamPackage.to_string package ]))))
    compilers

let build_packages config commit =
  let compilers = Builder.get_compilers config in
  let all_packages = Solver.get_all_packages config in
  let latest = Solver.get_latest_packages all_packages in
  let latest_available = Solver.get_available_packages config latest in
  let packages_to_build = 
    match config.Config.limit with
    | None -> latest_available
    | Some n -> 
        let package_list = OpamPackage.Set.to_list latest_available in
        let limited_list = 
          if List.length package_list <= n then package_list
          else List.filteri (fun i _ -> i < n) package_list
        in
        OpamPackage.Set.of_list limited_list
  in
  Os.fork
    (fun compiler ->
      OpamPackage.Set.iter
        (fun package ->
          if not (Sys.file_exists (Config.work_dir_path config [ "results"; commit; compiler.Builder.version; "status"; OpamPackage.to_string package ])) then 
            Builder.build_package config commit compiler package)
        packages_to_build)
    compilers

let generate_reports config commit =
  let compilers = Builder.get_compilers config in
  let all_packages = Solver.get_all_packages config in
  let latest = Solver.get_latest_packages all_packages in
  let latest_available = Solver.get_available_packages config latest in
  let packages_to_report = 
    match config.Config.limit with
    | None -> latest_available
    | Some n -> 
        let package_list = OpamPackage.Set.to_list latest_available in
        let limited_list = 
          if List.length package_list <= n then package_list
          else List.filteri (fun i _ -> i < n) package_list
        in
        OpamPackage.Set.of_list limited_list
  in
  List.iter
    (fun compiler ->
      OpamPackage.Set.iter
        (fun package ->
          Report.generate_package_report config commit compiler package)
        packages_to_report)
    compilers;
  Report.generate_index_report config [commit] compilers

let run config =
  let commit = setup_directories config in
  solve_packages config commit;
  build_packages config commit;
  generate_reports config commit