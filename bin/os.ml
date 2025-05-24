let read_from_file filename = In_channel.with_open_text filename @@ fun ic -> In_channel.input_all ic
let write_to_file filename str = Out_channel.with_open_text filename @@ fun oc -> Out_channel.output_string oc str
let append_to_file filename str = Out_channel.with_open_gen [ Open_text; Open_append; Open_creat ] 0o644 filename @@ fun oc -> Out_channel.output_string oc str
let path = List.fold_left Filename.concat ""

let sudo ?stdout ?stderr cmd =
  (*  let () = OpamConsole.note "%s" (String.concat " " cmd) in *)
  Sys.command (Filename.quote_command ?stdout ?stderr "sudo" cmd)

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  r

let nproc = run "nproc" |> String.trim |> int_of_string
let mkdir dir = if not (Sys.file_exists dir) then Sys.mkdir dir 0o755

let mkdir_p path =
  String.split_on_char '/' path
  |> List.filter (fun s -> String.length s > 0)
  |> List.fold_left
       (fun acc el ->
         let acc = acc ^ "/" ^ el in
         if Sys.file_exists acc then acc
         else
           let () = Sys.mkdir acc 0o777 in
           acc)
       ""

module IntSet = Set.Make (Int)

let fork ?np f lst =
  let nproc = Option.value ~default:nproc np in
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
