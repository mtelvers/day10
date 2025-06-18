let read_from_file filename = In_channel.with_open_text filename @@ fun ic -> In_channel.input_all ic
let write_to_file filename str = Out_channel.with_open_text filename @@ fun oc -> Out_channel.output_string oc str
let append_to_file filename str = Out_channel.with_open_gen [ Open_text; Open_append; Open_creat ] 0o644 filename @@ fun oc -> Out_channel.output_string oc str
let path = List.fold_left Filename.concat ""

let sudo ?stdout ?stderr cmd =
  (*  let () = OpamConsole.note "%s" (String.concat " " cmd) in *)
  Sys.command (Filename.quote_command ?stdout ?stderr "sudo" cmd)

let exec ?stdout ?stderr cmd =
  (*  let () = OpamConsole.note "%s" (String.concat " " cmd) in *)
  Sys.command (Filename.quote_command ?stdout ?stderr (List.hd cmd) (List.tl cmd))

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  r

let nproc () = run "nproc" |> String.trim |> int_of_string
let mkdir dir = if not (Sys.file_exists dir) then Sys.mkdir dir 0o755
let rm path = if (Sys.file_exists path) then Unix.unlink path

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
  try
    Unix.lockf lock_fd F_LOCK 0;
    if Sys.file_exists dir_name then Unix.close lock_fd
    else (
      write_function dir_name;
      Unix.close lock_fd;
      Unix.unlink lock_file)
  with
  | e ->
      Unix.close lock_fd;
      (try Unix.unlink lock_file with
      | _ -> ());
      raise e

let hardlink_tree ~source ~target =
  let rec process_directory current_source current_target =
    let entries = Sys.readdir current_source in
    Array.iter (fun entry ->
      let source = Filename.concat current_source entry in
      let target = Filename.concat current_target entry in
      try
        let stat = Unix.stat source in
        match stat.st_kind with
        | Unix.S_LNK
        | Unix.S_REG ->
          if not (Sys.file_exists target) then
            Unix.link source target
        | Unix.S_DIR ->
          mkdir target;
          process_directory source target
        | _ ->
          ()
      with
      | Unix.Unix_error _ ->
          ()
    ) entries
  in
  process_directory source target
