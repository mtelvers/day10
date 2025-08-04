type t = { config : Config.t }

let std_env ~(config : Config.t) =
  Util.std_env ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian" ~os_family:"debian" ~os_version:"12" ~ocaml_version:config.ocaml_version ()

let init ~(config : Config.t) = { config }
let deinit ~t:_ = ()
let config ~t = t.config

let layer_hash ~t deps =
  let os =
    List.map
      (fun v -> std_env ~config:t.config v |> Option.map OpamVariable.string_of_variable_contents |> Option.value ~default:"unknown")
      [ "os-family"; "os-version"; "arch" ]
  in
  os @ List.map OpamPackage.to_string deps |> String.concat " " |> Digest.string |> Digest.to_hex

let run ~t:_ ~temp_dir opam_repository build_log = 0

let build ~t ~temp_dir build_log pkg ordered_hashes =
  let config = t.config in
  let () =
    List.iter
      (fun hash ->
        let path = Os.path [ config.dir; hash ] in
        let e = if Sys.file_exists path then "ok" else "not found" in
        Printf.printf "%s: %s\n" path e)
      ordered_hashes
  in
  let rootfs = Os.path [ temp_dir; "fs" ] in
  0
