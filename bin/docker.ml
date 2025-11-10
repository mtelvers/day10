let platform = function
  | "x86_64"
  | "amd64" ->
      "linux/amd64"
  | "i386"
  | "i686" ->
      "linux/386"
  | "aarch64"
  | "arm64" ->
      "linux/arm64"
  | "armv7l" -> "linux/arm/v7"
  | arch -> "linux/" ^ arch

let debian ~(config : Config.t) ~temp_dir _opam_repository build_log uid gid =
  let base_image = Printf.sprintf "%s:%s" config.os_distribution config.os_version in
  let dockerfile =
    let open Dockerfile in
    from ~platform:(platform config.arch) base_image
    @@ run "apt update && apt upgrade -y"
    @@ run "apt install build-essential unzip bubblewrap git sudo curl rsync -y"
    @@ run "curl -L https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-%s-linux -o /usr/local/bin/opam && chmod +x /usr/local/bin/opam"
         config.arch
    @@ run
         "curl -L https://github.com/mtelvers/opam-build/releases/download/1.3.0/opam-build-1.3.0-%s-linux -o /usr/local/bin/opam-build && chmod +x \
          /usr/local/bin/opam-build"
         config.arch
    @@ run "echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections"
    @@ run "if getent passwd %i; then userdel -r $(id -nu %i); fi" uid uid
    @@ run "groupadd --gid %i opam" gid
    @@ run "adduser --disabled-password --gecos '@opam' --no-create-home --uid %i --gid %i --home /home/opam opam" uid gid
    @@ run "mkdir -p /home/opam && chown -R %i:%i /home/opam" uid gid
    @@ run "echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam"
    @@ run "chmod 440 /etc/sudoers.d/opam" @@ run "chown root:root /etc/sudoers.d/opam"
    @@ copy ~chown:(string_of_int uid ^ ":" ^ string_of_int gid) ~src:[ "opam-repository" ] ~dst:"/home/opam/opam-repository" ()
    @@ user "%i:%i" uid gid @@ workdir "/home/opam"
    @@ run "opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing -y"
    @@ run "opam switch create default --empty"
  in
  let dockerfile_path = Path.(temp_dir / "Dockerfile") in
  let () = Os.write_to_file dockerfile_path (Dockerfile.string_of_t dockerfile) in
  let tag = Printf.sprintf "day10-%s:%s" config.os_distribution config.os_version in
  let build_result = Os.exec ~stdout:build_log ~stderr:build_log [ "docker"; "build"; "-t"; tag; temp_dir ] in
  match build_result with
  | 0 ->
      let rootfs = Path.(temp_dir / "fs") in
      let container = Filename.basename temp_dir in
      let () = Os.mkdir rootfs in
      let _ = Os.sudo [ "docker"; "create"; "--name"; container; tag ] in
      let () = Os.run (String.concat " " [ "sudo"; "docker"; "export"; container; "|"; "sudo"; "tar"; "-xf"; "-"; "-C"; rootfs ]) |> print_string in
      let _ = Os.sudo [ "docker"; "rm"; container ] in
      let _ = Os.sudo [ "sh"; "-c"; ("rm -f " ^ Path.(rootfs / "home" / "opam" / ".opam" / "repo" / "state-*.cache")) ] in
      0
  | build_result -> build_result
