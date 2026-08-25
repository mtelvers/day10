let build ~(config : Config.t) ~(dist : Dist.t) ~temp_dir _opam_repository build_log uid gid =
  let base_image = Dist.base_image ~arch:config.arch ~distribution:config.os_distribution ~version:config.os_version in
  let dockerfile = Dockerfile_gen.dockerfile ~dist ~arch:config.arch ~base_image ~uid ~gid in
  let dockerfile_path = Path.(temp_dir / "Dockerfile") in
  let () = Os.write_to_file dockerfile_path (Dockerfile.string_of_t dockerfile) in
  let tag = Printf.sprintf "day10-%s:%s" config.os_distribution config.os_version in
  let build_result = Os.exec ~stdout:build_log ~stderr:build_log [ "docker"; "build"; "-t"; tag; temp_dir ] in
  match build_result with
  | 0 ->
      let rootfs = Path.(temp_dir / "fs") in
      let container = Filename.basename temp_dir in
      let () = Os.mkdir rootfs in
      Cleanup.with_resource (Cleanup.Docker_container container) @@ fun () ->
      let _ = Os.sudo [ "docker"; "create"; "--name"; container; tag ] in
      let () = Os.run (String.concat " " [ "sudo"; "docker"; "export"; container; "|"; "sudo"; "tar"; "-xf"; "-"; "-C"; rootfs ]) |> print_string in
      let _ = Os.sudo [ "docker"; "rm"; container ] in
      let _ = Os.sudo [ "sh"; "-c"; ("rm -f " ^ Path.(rootfs / "home" / "opam" / ".opam" / "repo" / "state-*.cache")) ] in
      0
  | build_result -> build_result
