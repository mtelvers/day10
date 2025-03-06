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
