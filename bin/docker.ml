open Dockerfile

let get_arch = function
  | "i386" | "i486" | "i586" | "i686" -> Some `I386
  | "armv7l" | "armv6l" -> Some `Aarch32
  | _ -> None

let get_platform = function
  | `I386 -> Some "386"
  | `Aarch32 -> Some "arm"
  | _ -> None

let dockerfile (distro : Dockerfile_opam.Distro.distro) arch =
  let arch = get_arch arch in
  let dockerfile =
    Dockerfile_opam.gen_distro
      ~opam:("6693-2.4.1", Some "https://github.com/dra27/opam.git")
      ?arch
      ~alias:"base"
      (distro :> Dockerfile_opam.Distro.t)
  in
  let open Dockerfile in
  let setup_and_build =
    heredoc {|
  set -e
  opam init --confirm-level=unsafe-yes --auto-setup --disable-sandboxing --bare
  git clone https://github.com/mtelvers/opam-build.git
  cd opam-build
  opam switch create . --empty
  opam install . --yes|}
  in
  dockerfile
  @@ from ?platform:(Option.bind arch get_platform) ~alias:"opam-build" "base"
  @@ (match distro with
        (* Temporary hack: enable static compilation, but opam-build doesn't
           need to be static anymore *)
      | `Fedora _ | `CentOS _ | `OracleLinux _ ->
          let enable_crb =
            let crb =
              match distro with
              | `OracleLinux `V8 -> Some "ol8_codeready_builder"
              | `OracleLinux `V9 -> Some "ol9_codeready_builder"
              | `OracleLinux `V10 -> Some "ol10_codeready_builder"
              | _ -> None
            in
            Option.fold ~none:empty ~some:(run "dnf config-manager --set-enabled %s") crb
          in
          user "root"
          @@ enable_crb
          @@ Dockerfile_opam.Linux.RPM.install "glibc-static libstdc++-static"
          @@ user "opam"
      | `OpenSUSE _ ->
          user "root"
          @@ Dockerfile_opam.Linux.Zypper.install "glibc-devel-static libstdc++-devel"
          @@ user "opam"
      | _ ->
          empty)
  @@ run_heredoc [setup_and_build, None]
  @@ from ?platform:(Option.bind arch get_platform) "base"
  @@ copy ~from:"opam-build" ~src:["/home/opam/opam-build/_opam/bin/opam-build"] ~dst:"/usr/bin/opam-build" ()

let build_base (distro : Dockerfile_opam.Distro.distro) arch ~temp_dir build_log =
  let dockerfile_path = Path.(temp_dir / "Dockerfile") in
  let () = Os.write_to_file dockerfile_path (Dockerfile.string_of_t (dockerfile distro arch)) in
  let tag =
    let tag =
      Dockerfile_opam.Distro.tag_of_distro (distro :> Dockerfile_opam.Distro.t)
    in
    let arch = Option.bind (get_arch arch) get_platform in
    "day10-" ^ tag ^ Option.fold ~none:"" ~some:(Printf.sprintf "-%s") arch
  in
  if Os.exec ~stdout:build_log ~stderr:build_log [ "docker"; "build"; "-t"; tag; temp_dir ] = 0 then
    Some tag
  else
    None
