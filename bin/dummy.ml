type t = { config : Config.t }

let init ~(config : Config.t) = { config }
let deinit ~t:_ = ()
let config ~t = t.config

let arch = Os.arch
let fork = Os.fork

(* Nothing here builds as root, so what there is to remove is the user's own. *)
let remove = List.iter (Os.rm ~recursive:true)
let temp_dir path () = if Sys.file_exists path then try Os.rm ~recursive:true path with Unix.Unix_error _ | Sys_error _ -> ()

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

