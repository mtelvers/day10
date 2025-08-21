open Cmdliner

let setup_cmd =
  let doc = "Set up the container environment and initialize directories" in
  let info = Cmd.info "setup" ~doc in
  Cmd.v info Term.(const (fun config ->
    let commit = Obi.setup_directories config in
    Printf.printf "Setup completed. Current commit: %s\n" commit
  ) $ Obi.Config.config_term)

let solve_cmd =
  let doc = "Solve package dependencies for all packages" in
  let info = Cmd.info "solve" ~doc in
  Cmd.v info Term.(const (fun config ->
    let commit = Obi.setup_directories config in
    Obi.solve_packages config commit;
    Printf.printf "Dependency solving completed.\n"
  ) $ Obi.Config.config_term)

let build_cmd =
  let doc = "Build all packages using container isolation" in
  let info = Cmd.info "build" ~doc in
  Cmd.v info Term.(const (fun config ->
    let commit = Obi.setup_directories config in
    Obi.build_packages config commit;
    Printf.printf "Building completed.\n"
  ) $ Obi.Config.config_term)

let report_cmd =
  let doc = "Generate HTML reports from build results" in
  let info = Cmd.info "report" ~doc in
  Cmd.v info Term.(const (fun config ->
    let commit = Obi.setup_directories config in
    Obi.generate_reports config commit;
    Printf.printf "Reports generated.\n"
  ) $ Obi.Config.config_term)

let run_cmd =
  let doc = "Run the complete pipeline: solve, build, and report" in
  let info = Cmd.info "run" ~doc in
  Cmd.v info Term.(const (fun config ->
    Printf.printf "Starting OBI (OCaml Build Infrastructure)...\n";
    Obi.run config;
    Printf.printf "Pipeline completed.\n"
  ) $ Obi.Config.config_term)

let serve_cmd =
  let doc = "Serve HTML reports on localhost:8080" in
  let info = Cmd.info "serve" ~doc in
  Cmd.v info Term.(const (fun config ->
    let html_dir = Obi.Config.work_dir_path config ["html"] in
    Printf.printf "Serving reports from %s on http://localhost:8080\n" html_dir;
    let cmd = Printf.sprintf "python3 -m http.server --directory %s --bind 0.0.0.0 8080" html_dir in
    ignore (Sys.command cmd)
  ) $ Obi.Config.config_term)

let config_cmd =
  let doc = "Manage configuration files" in
  let info = Cmd.info "config" ~doc in
  let action_arg =
    let doc = "Action to perform: show, init, save" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ACTION" ~doc)
  in
  let output_file_arg =
    let doc = "Output file path for save/init actions" in
    Arg.(value & opt (some string) None & info ["output"; "o"] ~docv:"FILE" ~doc)
  in
  Cmd.v info Term.(const (fun action output_file config ->
    match action with
    | "show" ->
        Printf.printf "Current configuration:\n";
        Printf.printf "- Opam repository: %s\n" config.Obi.Config.opam_repository;
        Printf.printf "- Download cache: %s\n" config.download_cache;
        Printf.printf "- Work directory: %s\n" config.work_dir;
        Printf.printf "- OCaml versions: %s\n" (String.concat ", " config.ocaml_versions);
        Printf.printf "- Hostname: %s\n" config.hostname;
        Printf.printf "- Limit: %s\n" (match config.limit with None -> "unlimited" | Some n -> string_of_int n);
    | "init" ->
        let output_path = Option.value output_file ~default:".obi.toml" in
        if Obi.Config.save_config_file output_path Obi.Config.default then
          Printf.printf "Initialized config file: %s\n" output_path
        else
          Printf.eprintf "Failed to create config file: %s\n" output_path
    | "save" ->
        let output_path = Option.value output_file ~default:".obi.toml" in
        if Obi.Config.save_config_file output_path config then
          Printf.printf "Saved current config to: %s\n" output_path
        else
          Printf.eprintf "Failed to save config file: %s\n" output_path
    | _ ->
        Printf.eprintf "Unknown action: %s\nValid actions: show, init, save\n" action
  ) $ action_arg $ output_file_arg $ Obi.Config.config_term)

let default_cmd =
  let doc = "OBI (OCaml Build Infrastructure) - comprehensive build testing for OCaml packages" in
  let man = [
    `S Manpage.s_description;
    `P "OBI provides automated health checks for OCaml packages by building them from the opam repository across multiple compiler versions using container isolation.";
    `P "Use 'obi COMMAND --help' for more information on a specific command.";
    `S Manpage.s_examples;
    `P "obi run --work-dir /tmp/obi-work";
    `P "obi solve --ocaml-versions 5.3.0,4.14.2";
    `P "obi serve";
  ] in
  let info = Cmd.info "obi" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.group info [
    setup_cmd; solve_cmd; build_cmd; report_cmd; run_cmd; serve_cmd; config_cmd
  ]

let () = exit (Cmd.eval default_cmd)