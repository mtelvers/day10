(* The commands day10 builds to run inside the container.

   Shared by the backends because the decisions here were previously made in
   three places, and a change to them was once applied to only one: --with-test
   reached the Linux build but not FreeBSD's, and Windows passed it for every
   package rather than the one under test. *)

(* Tests are asked for the package under test, not for the things it depends on:
   a dependency is built the same way either way. *)
let tests_requested ~(config : Config.t) pkg = config.with_test && Config.is_target_package ~config pkg

(* [with_test] overrides that decision, for a caller splitting the work into a
   run that installs and a run that tests.  [reinstall] makes the driver remove
   the package first, which the second of those runs needs: opam resolves a
   build command against switch state, so a package that publishes a variable in
   its own .config changes what its own command means once installed.
   conf-libclang.22 builds with "configure.sh version", which is 22 until it is
   installed and the detected llvm version afterwards, so building it a second
   time failed.  opam reinstall removes first for the same reason. *)
let for_package ~(config : Config.t) ?with_test ?(reinstall = false) pkg =
  let name = OpamPackage.to_string pkg in
  (* A package built from the project's own sources has to be pinned to them
     first, and the build run from there. *)
  let pin = if Config.is_local_package ~config pkg then [ "opam pin -yn " ^ name ^ " $HOME/src/"; "cd src" ] else [] in
  let with_test = match with_test with Some requested -> requested | None -> tests_requested ~config pkg in
  pin @ [ "day10-install -v " ^ (if reinstall then "--reinstall " else "") ^ (if with_test then "--with-test " else "") ^ name ]

(* The dune invocation that [day10 build] stands for.  Packages the caller named
   are passed on to dune, not merely used to decide what to solve for: dune
   builds every package in the workspace unless told otherwise, and the ones
   left out were never solved for, so it would build them anyway and fail for
   want of their dependencies. *)
let dune ~only_packages args =
  let only = match only_packages with [] -> [] | packages -> [ "--only-packages"; String.concat "," packages ] in
  String.concat " " ([ "opam"; "exec"; "--"; "dune"; "build" ] @ only @ List.map Filename.quote args)
