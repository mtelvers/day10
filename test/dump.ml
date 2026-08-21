(* Print the Dockerfile day10 would build for a distribution, so that changes to
   the generator show up as a diff in the expected output rather than silently
   altering the images. *)

let () =
  let distribution = Sys.argv.(1) in
  let version = Sys.argv.(2) in
  let base_image = Printf.sprintf "%s:%s" distribution version in
  print_string (Dockerfile.string_of_t (Dockerfile_gen.dockerfile ~dist:Dist.apt ~arch:"x86_64" ~base_image ~uid:1000 ~gid:1000))
