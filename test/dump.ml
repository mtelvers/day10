(* Print the Dockerfile day10 would build for a distribution, so that changes to
   the generator show up as a diff in the expected output rather than silently
   altering the images. *)

let () =
  let distribution = Sys.argv.(1) in
  let version = Sys.argv.(2) in
  print_string (Dockerfile.string_of_t (Dockerfile_gen.debian ~arch:"x86_64" ~distribution ~version ~uid:1000 ~gid:1000))
