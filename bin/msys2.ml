(* MSYS2 under opam's root, as --os-distribution msys2 asks: pacman for the
   depexts, a real awk.exe, and copies where Cygwin would make symlinks.
   Unpacked into the root so that the layers carry it, and opam init is
   pointed at it. *)

let url = "https://github.com/msys2/msys2-installer/releases/download/2026-09-27/msys2-base-x86_64-20260927.tar.xz"
let root ~opam_root = Path.(opam_root / "msys64")

(* What needs a shell runs from a script, as the commands may carry no
   quotes.  The first login populates and signs pacman's keyring on its own,
   and the script closes the gpg-agent that this leaves running, which would
   otherwise outlive the shell and hold the container's output open.  pacman itself is run from cmd, since a runtime update ends
   every MSYS2 process, and twice, as the second pass gets what the first
   left.  Upgrading ca-certificates leaves the CA bundle empty, a copy of a
   file the hook makes afterwards, so it is remade before anything else is
   fetched.  unzip is required by opam, rsync by a local repository, make by
   most packages, git as opam's Cygwin gets it, and diffutils as Cygwin's base
   category and every Linux base have it.  The downloaded packages are
   dropped so that the layer does not carry them.

   The mirrors answer a burst with 429 Too Many Requests, and a base fetches
   some 160 files: pacman fetches one at a time rather than five, and a step
   that fails is tried again after a minute and again after five, rather than
   failing the base for the next job to try at once. *)
let install ~opam_root ~script =
  let tarball = Path.(opam_root / "msys2.tar.xz") in
  let bin = Path.(root ~opam_root / "usr" / "bin") in
  let pacman = Path.(bin / "pacman.exe") in
  let bash name contents = Path.(bin / "bash.exe") ^ " -l " ^ script name contents in
  let sleep seconds = Printf.sprintf "powershell -NoProfile -NonInteractive -Command Start-Sleep %d" seconds in
  let retry command = Printf.sprintf "(%s || (%s && %s) || (%s && %s))" command (sleep 60) command (sleep 300) command in
  [
    "curl.exe --fail -L -o " ^ tarball ^ " " ^ url;
    "tar.exe -xf " ^ tarball ^ " -C " ^ opam_root;
    "del " ^ tarball;
    bash "msys2-setup.sh"
      "set -e\n\
       gpgconf --homedir /etc/pacman.d/gnupg --kill all\n\
       sed -i 's/^ParallelDownloads.*/ParallelDownloads = 1/' /etc/pacman.conf\n";
    retry (pacman ^ " -Syuu --noconfirm");
    retry (pacman ^ " -Syuu --noconfirm");
    bash "msys2-certs.sh" "update-ca-trust\n";
    retry (pacman ^ " -S --noconfirm --needed unzip make git rsync diffutils");
    pacman ^ " -Scc --noconfirm";
  ]

let init_options ~opam_root = " --cygwin-location " ^ root ~opam_root

(* pacman keeps a directory per package, which the layers merge as they are. *)
let assemble ~layers:_ ~target:_ = ()
let keep ~temp_dir:_ _ = ()
let discard _ = []

(* pacman -Sw into its own cache, which opam's pacman -Su then installs from
   without refreshing: a set at a time, and a package at a time when the set
   fails. *)
let prefetch ~opam_root sets =
  let fetch names = Path.(root ~opam_root / "usr" / "bin" / "pacman.exe") ^ " -Sw --noconfirm " ^ String.concat " " names in
  List.map
    (fun set ->
      let names = List.map OpamSysPkg.to_string (OpamSysPkg.Set.elements set) in
      match names with [ _ ] -> fetch names | _ -> fetch names ^ " || (" ^ String.concat " & " (List.map (fun n -> fetch [ n ]) names) ^ ")")
    sets
