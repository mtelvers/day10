type t = { config : Config.t }

let std_env ~(config : Config.t) =
  Util.std_env ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian" ~os_family:"debian" ~os_version:"12" ~ocaml_version:config.ocaml_version ()

let init ~(config : Config.t) = { config }
let deinit ~t:_ = ()
let config ~t = t.config

let os_key ~config =
  let os =
    List.map
      (fun v -> std_env ~config v |> Option.map OpamVariable.string_of_variable_contents |> Option.value ~default:"unknown")
      [ "os-family"; "os-version"; "arch" ]
  in
  String.concat "-" os

let layer_hash ~t deps =
  let hashes =
    List.map
      (fun opam ->
        opam |> Util.opam_file t.config.opam_repositories |> Option.get |> OpamFile.OPAM.effective_part |> OpamFile.OPAM.write_to_string
        |> OpamHash.compute_from_string |> OpamHash.to_string)
      deps
  in
  String.concat " " hashes |> Digest.string |> Digest.to_hex

let run ~t:_ ~temp_dir:_ _opam_repository _build_log = 0

let build ~t ~temp_dir _build_log _pkg ordered_hashes =
  let config = t.config in
  let () =
    List.iter
      (fun hash ->
        let path = Path.(config.dir / hash) in
        let e = if Sys.file_exists path then "ok" else "not found" in
        Printf.printf "%s: %s\n" path e)
      ordered_hashes
  in
  let _rootfs = Path.(temp_dir / "fs") in
  0
