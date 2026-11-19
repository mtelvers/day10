type t = {
  config : Config.t;
  uid : int;
  gid : int;
}

let env ~(config : Config.t) =
  [
    ("HOME", "/home/opam");
    ("CI", "true");
    ("OPAM_REPO_CI", "true");
    ("OPAMYES", "1");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
    ("OPAMJOBS", string_of_int (Option.value ~default:Os.default_jobs config.opam_jobs));
  ]

(* The release --os-version names, as opam names it: by osreldate, where
   1501000 is 15.1.  None for anything else, which init refuses, since a base
   of some other release would be filed under this one. *)
let release os_version =
  match int_of_string_opt os_version with
  | Some v when v >= 1000000 -> Some (Printf.sprintf "%d.%d-RELEASE" (v / 100000) (v / 1000 mod 100))
  | _ -> None

let init ~(config : Config.t) =
  (* Before anything is made in the cache under the version's name. *)
  let () =
    if Option.is_none (release config.os_version) then
      OpamConsole.error_and_exit `Bad_arguments "--os-version %s: FreeBSD's is an osreldate, such as 1501000 for 15.1" config.os_version
  in
  let uid, gid =
    match (Unix.getuid (), Unix.getgid ()) with
    | 0, _ -> (1000, 1000)
    | uid, gid -> (uid, gid)
  in
  { config; uid; gid }

let deinit ~t:_ = ()
let config ~t = t.config

let fork = Os.fork

(* As opam names it, since the layer key resolves filters against it: uname
   says amd64 where opam in the jail says x86_64. *)
let arch () = OpamSysPoll.arch OpamVariable.Map.empty |> Option.value ~default:(Os.arch ())

(* A layer is root-owned, since the jail ran as root to install depexts, so
   only root can delete it. *)
let remove = Os.sudo_rm
let temp_dir path () = if Sys.file_exists path then Os.quiet "sudo" [ "rm"; "-rf"; path ]

(* A killed jail leaves the nullfs mounts it took from its fstab and the devfs
   under its rootfs ([temp/fs]); unmount both before removing the tree, or rm
   walks into a still-mounted filesystem. *)
let reclaim_temp path =
  let fstab = Path.(path / "fstab") in
  if Sys.file_exists fstab then Os.quiet "sudo" [ "umount"; "-a"; "-f"; "-F"; fstab ];
  Os.quiet "sudo" [ "umount"; "-f"; Path.(path / "fs" / "dev") ];
  temp_dir path ()

let jail ~temp_dir ~rootfs ~mounts ~env ~argv ~network ~username =
  let jailname = Filename.basename temp_dir in
  let fstab = Path.(temp_dir / "fstab") in
  let () =
    List.map
      (fun (m : Mount.t) ->
        let full = Path.(temp_dir / m.dst) in
        let () = if not (Sys.file_exists full) then ignore (Os.sudo [ "mkdir"; "-p"; full ]) in
        String.concat " " [ m.src; full; m.ty; (if List.mem "ro" m.options then "ro" else "rw"); "0"; "0" ])
      mounts
    |> String.concat "\n" |> Os.write_to_file fstab
  in
  let quote_conf s =
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '"';
    String.iter
      (fun c ->
        (match c with
        | '"'
        | '\\' ->
            Buffer.add_char buf '\\'
        | _ -> ());
        Buffer.add_char buf c)
      s;
    Buffer.add_char buf '"';
    Buffer.contents buf
  in
  let sh_cmd =
    let env_prefix =
      if env = [] then ""
      else
        let pairs = List.map (fun (k, v) -> k ^ "=" ^ Filename.quote v) env in
        "export " ^ String.concat " " pairs ^ "; "
    in
    env_prefix ^ String.concat " && " argv
  in
  let command_value =
    match username with
    | None -> sh_cmd
    | Some u -> "/usr/bin/su -l " ^ u ^ " -c " ^ Filename.quote sh_cmd
  in
  let network_lines =
    if network then [ "ip4 = inherit;"; "ip6 = inherit;"; "host = inherit;" ]
    else [ "vnet;"; "exec.start = " ^ quote_conf "/sbin/ifconfig lo0 127.0.0.1/8" ^ ";" ]
  in
  let conf =
    let indent = List.map (fun l -> "  " ^ l) in
    String.concat "\n"
      ([ jailname ^ " {" ]
      @ indent
          ([ "path = " ^ quote_conf rootfs ^ ";"; "mount.devfs;"; "mount.fstab = " ^ quote_conf fstab ^ ";" ]
          @ network_lines
          @ [ "command = " ^ quote_conf command_value ^ ";" ])
      @ [ "}" ])
    ^ "\n"
  in
  let conf_file = Path.(temp_dir / "jail.conf") in
  let () = Os.write_to_file conf_file conf in
  [ "jail"; "-f"; conf_file; "-c"; jailname ]

let run ~t ~temp_dir ~depexts opam_repository build_log =
  let config = t.config in
  let rootfs = Path.(temp_dir / "fs") in
  let () = Os.mkdir rootfs in
  (* The release is the one the platform is keyed on, and freebsd-update is told
     so rather than left to read the running kernel's, so that a jail of an
     older release than the host is patched as what it is.  The mirror lays
     releases out by uname -m and then uname -p, which differ on arm64:
     releases/arm64/aarch64.  Fetched and unpacked directly: bsdinstall
     script, which used to do this, partitions a disk first and on 15.1 waits
     forever for one to be named. *)
  let release = Option.get (release config.os_version) in
  let distsite =
    let machine = Os.capture "uname" [ "-m" ] |> String.trim and processor = Os.capture "uname" [ "-p" ] |> String.trim in
    Printf.sprintf "https://download.freebsd.org/ftp/releases/%s/%s/%s" machine processor release
  in
  (* Every step adds to the one log, and shows it as it happens under --log. *)
  let fetched = Os.exec ~stdout:build_log ~stderr:build_log ~append:true ~tee:config.log in
  let logged = Os.sudo ~stdout:build_log ~stderr:build_log ~append:true ~tee:config.log in
  let manifest = Path.(temp_dir / "MANIFEST") and archive = Path.(temp_dir / "base.txz") in
  (* The mirror carries the supported releases; one that has gone from it is
     not one to build a base of, since freebsd-update has nothing for it
     either. *)
  let fetch file path =
    match fetched [ "curl"; "--fail"; "-L"; "-o"; path; distsite ^ "/" ^ file ] with
    | 0 -> ()
    | _ -> failwith (Printf.sprintf "%s/%s could not be fetched: is %s a supported release?" distsite file release)
  in
  let () = fetch "MANIFEST" manifest in
  let () = fetch "base.txz" archive in
  (* The manifest's line for the archive: its name, its sha256, and the rest. *)
  let () =
    let expected =
      Os.read_from_file manifest |> String.split_on_char '\n'
      |> List.find_map (fun line -> match String.split_on_char '\t' line with "base.txz" :: sum :: _ -> Some sum | _ -> None)
    in
    let actual = Os.capture "sha256" [ "-q"; archive ] |> String.trim in
    if expected <> Some actual then failwith (Printf.sprintf "%s does not match the checksum in %s" archive manifest)
  in
  let _ = logged [ "tar"; "-xpf"; archive; "-C"; rootfs ] in
  let () = List.iter Os.rm [ archive; manifest ] in
  let _ = Os.sudo [ "cp"; "/etc/resolv.conf"; Path.(rootfs / "etc" / "resolv.conf") ] in
  let _ = logged [ "freebsd-update"; "--not-running-from-cron"; "--currently-running"; release; "-b"; rootfs; "fetch"; "install" ] in
  let _ = logged [ "pkg"; "--chroot"; rootfs; "install"; "-y"; "pkg" ] in
  let _ = logged [ "pkg"; "--chroot"; rootfs; "upgrade"; "-y"; "-f" ] in
  let opam = Path.(rootfs / "usr" / "bin" / "opam") in
  let arch = if config.arch = "amd64" then "x86_64" else config.arch in
  (* --fail, or a missing release is downloaded as a page of HTML and fails
     later, as something else. *)
  let _ = logged [ "curl"; "--fail"; "-L"; "https://github.com/ocaml/opam/releases/download/2.5.1/opam-2.5.1-" ^ arch ^ "-freebsd"; "-o"; opam ] in
  let _ = Os.sudo [ "chmod"; "+x"; opam ] in
  let day10_install = Path.(rootfs / "usr" / "bin" / "day10-install") in
  let _ = logged [ "curl"; "--fail"; "-L"; "https://github.com/mtelvers/day10-install/releases/download/1.4.0/day10-install-1.4.0-" ^ arch ^ "-freebsd"; "-o"; day10_install ] in
  let _ = Os.sudo [ "chmod"; "+x"; day10_install ] in
  (* Every depext into pkg's cache, so a build installs from it with no
     network: a set at a time, as an install asks for it, and a package at a
     time when the set fails.  A script in the jail, rather than in the jail's
     command, where jail.conf would take a $ for one of its own variables.
     pkg is then told not to refresh its catalogue, which the base keeps, so
     opam's pkg install finds what was fetched against it. *)
  let prefetch = "/tmp/day10-prefetch.sh" in
  let () =
    let fetch names = "pkg fetch -y -d " ^ String.concat " " names in
    let lines =
      List.map
        (fun set ->
          let names = List.map OpamSysPkg.to_string (OpamSysPkg.Set.elements set) in
          match names with [ _ ] -> fetch names | _ -> fetch names ^ " || { " ^ String.concat "; " (List.map (fun n -> fetch [ n ]) names) ^ "; }")
        depexts
    in
    let script = Path.(temp_dir / "prefetch.sh") in
    Os.write_to_file script (String.concat "\n" (lines @ [ "exit 0"; "" ]));
    ignore (Os.sudo [ "cp"; script; rootfs ^ prefetch ])
  in
  let argv =
    [
      "pw groupadd opam -g " ^ string_of_int t.gid;
      "pw useradd -m -n opam -g opam -u " ^ string_of_int t.uid ^ " -h - -c opam";
      "pkg install -y sudo gmake git patch rsync bash zstd pkgconf";
      {|echo "opam ALL=(ALL:ALL) NOPASSWD:ALL" > /usr/local/etc/sudoers.d/opam|};
      "sh " ^ prefetch;
      "rm " ^ prefetch;
      "echo 'REPO_AUTOUPDATE = false;' >> /usr/local/etc/pkg.conf";
    ]
  in
  let result = logged (jail ~temp_dir ~rootfs ~mounts:[] ~env:[] ~argv ~network:true ~username:None) in
  let () = if result = 0 then Os.quiet "sudo" [ "umount"; Path.(rootfs / "dev") ] in
  let _ = Os.sudo [ "chflags"; "-R"; "0"; rootfs ] in
  (* Every build logs in as opam with su -l, and the skeleton's .profile and
     .login run fortune, which put a random tip in every build's log.  Commented
     out as freebsd-infra's base image does; .hushlogin, below, keeps the motd
     out too. *)
  let _ =
    Os.sudo
      [ "sed"; "-i"; ""; "-e"; "s/^[^#]*fortune/# &/"; Path.(rootfs / "home" / "opam" / ".profile"); Path.(rootfs / "home" / "opam" / ".login") ]
  in
  let argv =
    [ "touch /home/opam/.hushlogin"; "opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing -y"; "opam switch create default --empty" ]
  in
  let mounts = [ { Mount.ty = "nullfs"; src = opam_repository; dst = Path.("fs" / "home" / "opam" / "opam-repository"); options = [ "ro" ] } ] in
  let result = logged (jail ~temp_dir ~rootfs ~mounts ~env:(env ~config) ~argv ~network:true ~username:(Some "opam")) in
  let () =
    if result = 0 then (
      Os.quiet "sudo" [ "umount"; Path.(rootfs / "dev") ];
      Os.quiet "sudo" [ "umount"; "-a"; "-f"; "-F"; Path.(temp_dir / "fstab") ])
  in
  let _ = Os.sudo [ "rm"; "-rf"; Path.(rootfs / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "environment") ] in
  let _ = Os.sudo [ "sh"; "-c"; ("rm -f " ^ Path.(rootfs / "home" / "opam" / ".opam" / "repo" / "state-*.cache")) ] in
  let () = Os.write_to_file Path.(temp_dir / "status") (string_of_int result) in
  result

(* What a build writes that no layer keeps: scratch space, opam's sources,
   build trees and caches, and the repository, which is the solution's.
   Relative to the root, as find prints them there. *)
let excluded =
  let switch = Path.("." / "home" / "opam" / ".opam" / "default" / ".opam-switch") in
  [
    Path.("." / "dev");
    Path.("." / "tmp");
    Path.("." / "var" / "tmp");
    (* pkg's download cache and repository catalogues: what it fetched, not
       what it installed. *)
    Path.("." / "var" / "cache" / "pkg");
    Path.("." / "var" / "db" / "pkg" / "repos");
    Path.("." / "home" / "opam" / ".opam" / "repo" / "default");
    Path.("." / "home" / "opam" / ".opam" / "repo" / "state-*.cache");
    Path.("." / "home" / "opam" / ".opam" / "log");
    Path.("." / "home" / "opam" / ".opam" / "download-cache");
    Path.("." / "home" / "opam" / "src");
    Path.(switch / "sources");
    Path.(switch / "build");
    Path.(switch / "packages" / "cache");
    Path.(switch / "environment");
  ]

(* pkg keeps one database, /var/db/pkg/local.sqlite, and every layer that
   installed system packages carries the whole of it, so putting layers
   together leaves one layer's database standing and every package the others
   installed unknown to pkg, which then installs them again.  As Cygwin's
   installed.db is on Windows, the database is put back together on the host's
   side: each layer keeps, beside its fs, a manifest of every package its build
   added, and a build registers the manifests of its layers that the database
   it was given does not list.  pkg register records a package whose files are
   in place without touching them, and runs its scripts, which put back the
   users and groups the other layers' packages made.  It runs in a jail on the
   tree: with -r, pkg reads files and runs scripts on the host. *)
let pkg_dir = "pkg"

(* Name, version and whether pkg installed it as a dependency, as the tree's
   database lists them. *)
let pkg_installed root =
  Os.capture ~check:false "sudo" [ "pkg"; "-o"; "PKG_DBDIR=" ^ Path.(root / "var" / "db" / "pkg"); "query"; "%n %v %a" ]
  |> String.split_on_char '\n'
  |> List.filter_map (fun line -> match String.split_on_char ' ' line with [ name; version; automatic ] -> Some (name, (version, automatic = "1")) | _ -> None)

(* Every package the build added or changed: what the layer has to give a
   database that does not list it. *)
let save_manifests ~root ~before dir =
  let db = "PKG_DBDIR=" ^ Path.(root / "var" / "db" / "pkg") in
  let added = List.filter (fun (name, (version, _)) -> match List.assoc_opt name before with Some (v, _) -> v <> version | None -> true) (pkg_installed root) in
  if added <> [] then begin
    Os.mkdir dir;
    List.iter
      (fun (name, (_, automatic)) ->
        (* An array of one, as pkg info writes it, which register does not read. *)
        let manifest = match Yojson.Safe.from_string (Os.capture "sudo" [ "pkg"; "-o"; db; "info"; "-R"; "--raw-format"; "json"; name ]) with `List [ m ] -> m | m -> m in
        Os.write_to_file Path.(dir / (name ^ ".json")) (Yojson.Safe.to_string manifest);
        if automatic then Os.write_to_file Path.(dir / (name ^ ".automatic")) "")
      added
  end

(* The manifests of the build's layers that the tree's database does not list,
   topmost first so that, as for a file, the first layer to give a package has
   it; registered in a jail as root, with what pkg says in the layer's
   pkg-register.log. *)
let register_manifests ~t ~temp_dir ~root ordered_hashes =
  let config = t.config in
  let os_key = Config.os_key ~config in
  let known = pkg_installed root in
  let manifests =
    List.fold_left
      (fun acc hash ->
        let dir = Path.(config.dir / os_key / hash / pkg_dir) in
        if not (Sys.file_exists dir) then acc
        else
          Sys.readdir dir |> Array.to_list |> List.sort String.compare
          |> List.fold_left
               (fun acc file ->
                 if not (Filename.check_suffix file ".json") then acc
                 else
                   let name = Filename.chop_suffix file ".json" in
                   if List.mem_assoc name acc || List.mem_assoc name known then acc else (name, (dir, Sys.file_exists Path.(dir / (name ^ ".automatic")))) :: acc)
               acc)
      [] ordered_hashes
    |> List.rev
  in
  if manifests <> [] then begin
    let staging = Path.("tmp" / "day10-pkg") in
    let () = ignore (Os.sudo [ "mkdir"; "-p"; Path.(root / staging) ]) in
    let () = List.iter (fun (name, (dir, _)) -> ignore (Os.sudo [ "cp"; Path.(dir / (name ^ ".json")); Path.(root / staging) ])) manifests in
    let script =
      List.map
        (fun (name, (_, automatic)) -> Printf.sprintf "pkg register %s-M /%s/%s.json || echo 'day10: pkg register %s failed'" (if automatic then "-d " else "") staging name name)
        manifests
      |> String.concat "; "
    in
    let log = Path.(temp_dir / "pkg-register.log") in
    let _ = Os.sudo ~stdout:log ~stderr:log (jail ~temp_dir ~rootfs:root ~mounts:[] ~env:[] ~argv:[ script ] ~network:true ~username:None) in
    Os.quiet "sudo" [ "umount"; Path.(root / "dev") ];
    ignore (Os.sudo [ "rm"; "-rf"; Path.(root / staging) ])
  end

(* A build runs on a real directory, not a union: FreeBSD's unionfs will not
   rename a directory over an empty one in its upper layer (EINVAL), which pkg
   does when two packages in one transaction share a directory, and a union
   mount often stays busy once the jail is gone.  As on macOS, the layers are
   copied into one tree, a marker is touched, and the layer is what changed
   after it.  Copied and not hard linked: pkg updates its SQLite database in
   place, and through a link that is the cached layer's, and a link's change
   time moves whenever any other build links or unlinks the same inode. *)
(* As on Linux, a package is built with no network: its sources are fetched on
   the host and staged in the build's copy of the repository (Source_stage).
   A package with depexts first has them installed in a jail of its own with
   the network, by opam and pkg alone, before anything of the package's own
   has run; one with none never has the network.  Both jails run on the same
   tree, after the marker, so the layer has what each wrote.  The tests run in
   the build's jail: it has no network either. *)
let build ~t ~temp_dir ~depexts build_log pkg ordered_hashes =
  let config = t.config in
  let os_key = Config.os_key ~config in
  let root = Path.(temp_dir / "root") in
  let layer = Path.(temp_dir / "fs") in
  let marker = Path.(temp_dir / "marker") in
  let () = List.iter Os.mkdir [ root; layer ] in
  (* Each a jail: whether it has the network, and its commands. *)
  let phases =
    match config.build_command with
    | Some { run; network } -> [ (network, [ "cd src"; run ]) ]
    | None ->
        let depexts =
          if OpamSysPkg.Set.is_empty depexts then [] else [ (true, Build_command.for_package ~config ~depexts_only:true ~with_test:false pkg) ]
        in
        depexts @ [ (false, Build_command.for_package ~config pkg) ]
  in
  let () =
    if Option.is_none config.build_command then
      Source_stage.stage ~repo:Path.(temp_dir / "opam-repository") ~in_container:"file:///home/opam/.opam/repo/default/cache" ~log:build_log pkg
  in
  (* Topmost first: tar -k leaves a path as the first layer to give it has it.
     tar rather than cp, which would copy each of a file's hard links on its
     own: /rescue alone would be some 3GB. *)
  let () =
    List.iter
      (fun hash ->
        let dir = Path.(config.dir / os_key / hash / "fs") in
        ignore (Os.sudo [ "sh"; "-c"; Printf.sprintf "tar -C %s -cf - . | tar -xpkf - -C %s" (Filename.quote dir) (Filename.quote root) ]))
      (ordered_hashes @ [ "base" ])
  in
  (* No one layer's switch state lists what the others installed.  Given to
     opam, since day10 writes it as itself. *)
  let () =
    let packages_dir = Path.(root / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "packages") in
    let state_file = Path.(root / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "switch-state") in
    if Sys.file_exists packages_dir then begin
      (* Not in temp_dir, which becomes the layer. *)
      let scratch = Filename.temp_file "switch-state" "" in
      Opamh.dump_state packages_dir scratch;
      ignore (Os.sudo [ "install"; "-o"; string_of_int t.uid; "-g"; string_of_int t.gid; "-m"; "644"; scratch; state_file ]);
      Sys.remove scratch
    end
  in
  (* Before the marker: what registering writes -- the database, the password
     files -- is the layers' and not this build's. *)
  let () = register_manifests ~t ~temp_dir ~root ordered_hashes in
  let before = pkg_installed root in
  let mounts = [ { Mount.ty = "nullfs"; src = Path.(temp_dir / "opam-repository"); dst = Path.("root" / "home" / "opam" / ".opam" / "repo" / "default"); options = [ "ro" ] } ] in
  let mounts =
    match config.directory with
    | None -> mounts
    | Some src -> mounts @ [ { ty = "nullfs"; src; dst = Path.("root" / "home" / "opam" / "src"); options = [ "rw" ] } ]
  in
  (* After the tree is made, so that everything in it is older: tar sets a
     file's modification time from the archive, but its change time is when it
     landed.  A second apart, since a change in the same second as the marker
     would otherwise be lost to a coarse clock. *)
  let () = Unix.sleep 1 in
  let () = Os.write_to_file marker "" in
  let () = Unix.sleep 1 in
  (* Shown as it happens for the command the user asked for, as on Linux. *)
  let tee = Option.is_some config.build_command || config.log in
  (* Appended, since staging may have started the log; stopping at the first
     jail that fails. *)
  let run_phase (network, argv) =
    let result = Os.sudo ~stdout:build_log ~stderr:build_log ~tee ~append:true (jail ~temp_dir ~rootfs:root ~mounts ~env:(env ~config) ~argv ~network ~username:(Some "opam")) in
    (* Quietly, here and wherever a jail has run: jail(8) unmounts its devfs and
       fstab mounts as it removes the jail, so these normally find nothing there
       and say so in every build's log.  They are for a jail that left one. *)
    Os.quiet "sudo" [ "umount"; Path.(root / "dev") ];
    Os.quiet "sudo" [ "umount"; "-a"; "-f"; "-F"; Path.(temp_dir / "fstab") ];
    result
  in
  let result = List.fold_left (fun result phase -> if result = 0 then run_phase phase else result) 0 phases in
  let prune = String.concat " -o " (List.map (fun path -> "-path " ^ Filename.quote path) excluded) in
  (* Every directory above each path goes too, each on its own, so that it keeps
     the owner and mode the build gave it; tar would make a missing one
     root's.  In C order a directory comes before what is in it. *)
  let parents = {|awk '{ n = split($0, part, "/"); path = part[1]; for (i = 2; i <= n; i++) { path = path "/" part[i]; print path } }'|} in
  let collect =
    Printf.sprintf "cd %s && find . \\( %s \\) -prune -o -newercm %s -print | %s | LC_ALL=C sort -u | tar --no-recursion -T - -cf - | tar -xpf - -C %s" (Filename.quote root) prune
      (Filename.quote marker) parents (Filename.quote layer)
  in
  let _ = Os.sudo [ "sh"; "-c"; collect ] in
  let () = save_manifests ~root ~before Path.(temp_dir / pkg_dir) in
  let _ = Os.sudo [ "rm"; "-rf"; root; marker ] in
  result

