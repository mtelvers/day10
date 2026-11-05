(* MSYS2 under opam's root, as --os-distribution msys2 asks: pacman for the
   depexts, a real awk.exe, and copies where Cygwin would make symlinks.
   Unpacked into the root so that the layers carry it, and opam init is
   pointed at it. *)

let url = "https://github.com/msys2/msys2-installer/releases/download/2026-09-27/msys2-base-x86_64-20260927.tar.xz"
let root ~opam_root = Path.(opam_root / "msys64")

(* MSYS2 is set up on the container's own filesystem and copied into opam's
   root afterwards: on the bind mount pacman can neither sign its keys nor
   replace the binaries it is running from.  What needs a shell runs from a
   script, as the commands may carry no quotes.  The first login populates
   the keyring, and the script closes the gpg-agent that would otherwise
   outlive the shell and hold the container's output open.  pacman itself is
   run from cmd, since a runtime update ends every MSYS2 process, and twice,
   as the second pass gets what the first left.  Upgrading ca-certificates
   leaves the CA bundle empty, a copy of a file the hook makes afterwards,
   so it is remade before anything else is fetched.  unzip is required by
   opam, rsync by a local repository, make by most packages, and git as
   opam's Cygwin gets it.  The
   downloaded packages are dropped before the copy. *)
let staging = {|C:\msys64|}

let scripts =
  [
    ("msys2-keyring.sh", "set -e\npacman-key --init\npacman-key --populate msys2\ngpgconf --homedir /etc/pacman.d/gnupg --kill all\n");
    ("msys2-certs.sh", "update-ca-trust\n");
  ]

let install ~opam_root ~rootfs =
  let tarball = Path.(opam_root / "msys2.tar.xz") in
  let () = List.iter (fun (name, contents) -> Os.write_to_file Path.(rootfs / name) contents) scripts in
  let pacman = Path.(staging / "usr" / "bin" / "pacman.exe") in
  let bash name = Path.(staging / "usr" / "bin" / "bash.exe") ^ " -l " ^ Path.(opam_root / name) in
  [
    "curl.exe --fail -L -o " ^ tarball ^ " " ^ url;
    "tar.exe -xf " ^ tarball ^ " -C " ^ {|C:\|};
    "del " ^ tarball;
    bash "msys2-keyring.sh";
    pacman ^ " -Syuu --noconfirm";
    pacman ^ " -Syuu --noconfirm";
    bash "msys2-certs.sh";
    pacman ^ " -S --noconfirm --needed unzip make git rsync";
    pacman ^ " -Scc --noconfirm";
    "xcopy " ^ staging ^ " " ^ root ~opam_root ^ {|\|} ^ " /E /H /K /Q /Y";
  ]

let init_options ~opam_root = " --cygwin-location " ^ root ~opam_root

(* pacman keeps a directory per package, which the layers merge as they are. *)
let assemble ~layers:_ ~target:_ = ()
let keep ~temp_dir:_ _ = ()
let discard root = List.filter Sys.file_exists (List.map (fun (name, _) -> Path.(root / name)) scripts)
