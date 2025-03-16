let read_from_file filename = In_channel.with_open_text filename @@ fun ic -> In_channel.input_all ic
let write_to_file filename str = Out_channel.with_open_text filename @@ fun oc -> Out_channel.output_string oc str
let append_to_file filename str = Out_channel.with_open_gen [ Open_text; Open_append; Open_creat ] 0o644 filename @@ fun oc -> Out_channel.output_string oc str

let sudo ?stdout ?stderr cmd =
  let () = OpamConsole.note "%s" (String.concat " " cmd) in
  Sys.command (Filename.quote_command ?stdout ?stderr "sudo" cmd)

let run cmd =
  let inp = Unix.open_process_in cmd in
  let r = In_channel.input_all inp in
  In_channel.close inp;
  r

let nproc = run "nproc" |> String.trim |> int_of_string
let mkdir dir = if not (Sys.file_exists dir) then Sys.mkdir dir 0o755

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
