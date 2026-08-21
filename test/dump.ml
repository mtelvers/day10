(* Print the Dockerfile day10 would build for a distribution, so that changes to
   the generator show up as a diff in the expected output rather than silently
   altering the images.  The architecture defaults to x86_64 so that the checked
   in expected output does not depend on the machine running the tests. *)

let () =
  let distribution = Sys.argv.(1) in
  let version = Sys.argv.(2) in
  let os_family = Sys.argv.(3) in
  let arch = if Array.length Sys.argv > 4 then Sys.argv.(4) else "x86_64" in
  match Dist.of_config ~os_family ~distribution ~version with
  | None -> failwith (Printf.sprintf "No distribution rules for %s %s (os-family %s)" distribution version os_family)
  | Some dist ->
      let base_image = Dist.base_image ~arch ~distribution ~version in
      print_string (Dockerfile.string_of_t (Dockerfile_gen.dockerfile ~dist ~arch ~base_image ~uid:1000 ~gid:1000))
