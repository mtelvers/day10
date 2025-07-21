let read_from_file filename = In_channel.with_open_text filename @@ fun ic -> In_channel.input_all ic
let write_to_file filename str = Out_channel.with_open_text filename @@ fun oc -> Out_channel.output_string oc str
let append_to_file filename str = Out_channel.with_open_gen [ Open_text; Open_append; Open_creat ] 0o644 filename @@ fun oc -> Out_channel.output_string oc str
let path = List.fold_left Filename.concat ""

let sudo ?stdout ?stderr cmd =
  (*  let () = OpamConsole.note "%s" (String.concat " " cmd) in *)
  Sys.command (Filename.quote_command ?stdout ?stderr "sudo" cmd)

let exec ?stdout ?stderr cmd =
  let () = OpamConsole.note "%s" (String.concat " " cmd) in
  Sys.command (Filename.quote_command ?stdout ?stderr (List.hd cmd) (List.tl cmd))

let retry_exec ?stdout ?stderr ?(tries = 10) cmd =
  let rec loop n =
    match (exec ?stdout ?stderr cmd, n) with
    | 0, _ -> 0
    | r, 0 -> r
    | _, n ->
        OpamConsole.note "retry %i: %s" (tries - n + 1) (String.concat " " cmd);
        Unix.sleepf (Random.float 2.0);
        loop (n - 1)
  in
  loop tries

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  r

let nproc () = run "nproc" |> String.trim |> int_of_string
let mkdir dir = if not (Sys.file_exists dir) then Sys.mkdir dir 0o755

let rec rm ?(recursive = false) path =
  try
    let stat = Unix.lstat path in
    match stat.st_kind with
    | S_REG
    | S_LNK
    | S_CHR
    | S_BLK
    | S_FIFO
    | S_SOCK -> (
        try Unix.unlink path with
        | Unix.Unix_error (Unix.EACCES, _, _) ->
            Unix.chmod path (stat.st_perm lor 0o222);
            Unix.unlink path)
    | S_DIR ->
        if recursive then Sys.readdir path |> Array.iter (fun f -> rm ~recursive (Filename.concat path f));
        Unix.rmdir path
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> ()

module IntSet = Set.Make (Int)

let fork ?np f lst =
  let nproc = Option.value ~default:(nproc ()) np in
  List.fold_left
    (fun acc x ->
      let acc =
        let rec loop acc =
          if IntSet.cardinal acc <= nproc then acc
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
          f x;
          exit 0
      | child -> IntSet.add child acc)
    IntSet.empty lst
  |> IntSet.iter (fun pid -> ignore (Unix.waitpid [] pid))

let create_directory_exclusively dir_name write_function =
  let lock_file = dir_name ^ ".lock" in
  let lock_fd = Unix.openfile lock_file [ O_CREAT; O_WRONLY ] 0o644 in
  Unix.lockf lock_fd F_LOCK 0;
  if not (Sys.file_exists dir_name) then write_function dir_name;
  Unix.close lock_fd;
  try Unix.unlink lock_file with
  | _ -> ()

exception Copy_error of string

let cp ?(buffer_size = 65536) ?(preserve_permissions = true) ?(preserve_times = true) src dst =
  let safe_close fd =
    try Unix.close fd with
    | _ -> ()
  in
  let src_stats =
    try Unix.stat src with
    | Unix.Unix_error (err, _, _) -> raise (Copy_error (Printf.sprintf "Cannot stat source file '%s': %s" src (Unix.error_message err)))
  in
  if src_stats.st_kind <> S_REG then raise (Copy_error (Printf.sprintf "Source '%s' is not a regular file" src));
  let src_fd =
    try Unix.openfile src [ O_RDONLY ] 0 with
    | Unix.Unix_error (err, _, _) -> raise (Copy_error (Printf.sprintf "Cannot open source file '%s': %s" src (Unix.error_message err)))
  in
  let dst_fd =
    try Unix.openfile dst [ O_WRONLY; O_CREAT; O_TRUNC ] src_stats.st_perm with
    | Unix.Unix_error (err, _, _) ->
        safe_close src_fd;
        raise (Copy_error (Printf.sprintf "Cannot open destination file '%s': %s" dst (Unix.error_message err)))
  in
  let buffer = Bytes.create buffer_size in
  let rec copy_loop () =
    try
      match Unix.read src_fd buffer 0 buffer_size with
      | 0 -> ()
      | bytes_read ->
          let rec write_all pos remaining =
            if remaining > 0 then
              let bytes_written = Unix.write dst_fd buffer pos remaining in
              write_all (pos + bytes_written) (remaining - bytes_written)
          in
          write_all 0 bytes_read;
          copy_loop ()
    with
    | Unix.Unix_error (err, _, _) ->
        safe_close src_fd;
        safe_close dst_fd;
        raise (Copy_error (Printf.sprintf "Error during copy: %s" (Unix.error_message err)))
  in
  copy_loop ();
  safe_close src_fd;
  safe_close dst_fd;
  (if preserve_permissions then
     try Unix.chmod dst src_stats.st_perm with
     | Unix.Unix_error (err, _, _) -> Printf.eprintf "Warning: Could not preserve permissions: %s\n" (Unix.error_message err));
  if preserve_times then
    try Unix.utimes dst src_stats.st_atime src_stats.st_mtime with
    | Unix.Unix_error (err, _, _) -> Printf.eprintf "Warning: Could not preserve timestamps: %s\n" (Unix.error_message err)

let hardlink_tree ~source ~target =
  let rec process_directory current_source current_target =
    let entries = Sys.readdir current_source in
    Array.iter
      (fun entry ->
        let source = Filename.concat current_source entry in
        let target = Filename.concat current_target entry in
        try
          let stat = Unix.lstat source in
          match stat.st_kind with
          | S_LNK -> if not (Sys.file_exists target) then Unix.symlink (Unix.readlink source) target
          | S_REG -> if not (Sys.file_exists target) then Unix.link source target
          | S_DIR ->
              mkdir target;
              process_directory source target
          | S_CHR
          | S_BLK
          | S_FIFO
          | S_SOCK ->
              ()
        with
        | Unix.Unix_error (Unix.EMLINK, _, _) -> cp source target
        | Unix.Unix_error (err, _, _) -> Printf.eprintf "Warning: %s -> %s = %s\n" source target (Unix.error_message err))
      entries
  in
  process_directory source target
