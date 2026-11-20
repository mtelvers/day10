(* macOS: each build cold-boots an APFS clone of a macOS image, through
   day10-vm, which starts VMs from the console user's login session where
   Apple allows them.  Nothing is shared with a running guest: the layers are
   copied onto the clone's disk before it boots, with the job that builds the
   package, and what the build wrote is read back after it stops.  See
   day10-vm.

   A layer holds two trees, the opam user's home and Homebrew's prefix, kept at
   their paths on the guest's Data volume -- Users/opam and opt/homebrew -- so
   that a layer's fs is copied onto a mounted clone as it stands. *)

type t = {
  config : Config.t;
  (* day10-vm, by full path: sudo does not keep PATH. *)
  vm : string;
  (* The image every build is a clone of: plain macOS, the command line tools
     and an opam user, made with day10-vm install and its setup job.  Beside
     the base and the layers, and on the same volume, since a clone is only
     free within one. *)
  image : string;
}

let home = Path.("Users" / "opam")
let brew = Path.("opt" / "homebrew")

(* What the host gives a build besides the layers: the opam repository, and
   for the base the depext sets.  Outside the two trees, so never part of a
   layer. *)
let inputs = Path.("Users" / "Shared" / "day10")

(* Touched once everything is copied in.  What the build writes is newer, and
   what was copied in is not: the copy sets a file's change time when it lands,
   before the marker. *)
let marker = Path.("private" / "var" / "day10-marker")

let opam_root = Path.(home / ".opam")
let switch = Path.(opam_root / "default" / ".opam-switch")

(* opam 2.6 bumped the root format, which the opam libraries in day10-install
   do not read yet, as on Windows. *)
let opam_url = "https://github.com/ocaml/opam/releases/download/2.5.2/opam-2.5.2-arm64-macos"
let day10_install_url = "https://github.com/mtelvers/day10-install/releases/download/1.5.0/day10-install-1.5.0-arm64-macos"

(* The guest's uid and gid for opam, as the image's setup job made it. *)
let owner = "501:20"

let find_in_path name =
  Option.bind (Sys.getenv_opt "PATH") @@ fun path ->
  String.split_on_char ':' path |> List.find_map (fun dir -> let file = Path.(dir / name) in if Sys.file_exists file then Some file else None)

(* The images builds boot, one per macOS version, as day10-vm install makes
   them: images/macos-15.6.1 and so on, in the cache directory.  The guest's
   version, not the host's, is what a build sees and what keys its layers, so
   updating the host leaves the cache where it was. *)
let images ~dir = Path.(dir / "images")
let image_dir ~dir version = Path.(images ~dir / ("macos-" ^ version))

let image_versions ~dir =
  match Sys.readdir (images ~dir) with
  | entries ->
      Array.to_list entries
      |> List.filter_map (fun entry ->
             match String.split_on_char '-' entry with
             | [ "macos"; version ] when Sys.file_exists Path.(images ~dir / entry / "config.json") -> Some version
             | _ -> None)
  | exception Sys_error _ -> []

let newest_image_version ~dir = List.fold_left (fun newest version -> match newest with Some v when OpamVersionCompare.compare v version >= 0 -> newest | _ -> Some version) None (image_versions ~dir)

let init ~(config : Config.t) =
  let os_key = Config.os_key ~config in
  (* An image kept in the platform's own directory is where one was kept
     before images had a directory of their own. *)
  let image =
    let shared = image_dir ~dir:config.dir config.os_version in
    if Sys.file_exists Path.(shared / "config.json") then shared else Path.(config.dir / os_key / "image")
  in
  let vm =
    match Option.fold ~none:(find_in_path "day10-vm") ~some:Option.some (Sys.getenv_opt "DAY10_VM") with
    | Some vm -> vm
    | None -> OpamConsole.error_and_exit `Configuration_error "day10-vm is not on PATH: install it, or name it with DAY10_VM"
  in
  if not (Sys.file_exists Path.(image / "config.json")) then
    OpamConsole.error_and_exit `Configuration_error
      "There is no macOS image for %s at %s: make one with day10-vm install and image/setup.sh, and put it there" os_key
      (image_dir ~dir:config.dir config.os_version);
  { config; vm; image }

let deinit ~t:_ = ()
let config ~t = t.config

(* opam says arm64, as --arch is given on the Macs. *)
let arch () = OpamSysPoll.arch OpamVariable.Map.empty |> Option.value ~default:(Os.arch ())

(* Apple licenses two macOS VMs at once on a Mac, and the framework refuses a
   third. *)
let fork ?np f xs = Os.fork ~np:(min 2 (Option.value ~default:2 np)) f xs

(* A layer read back from a guest keeps the guest's owners, so only root can
   delete all of one. *)
let remove = Os.sudo_rm
let temp_dir path () = if Sys.file_exists path then Os.quiet "sudo" [ "rm"; "-rf"; path ]

(* A killed build may leave the clone's disk mounted at [temp/mnt]; stop anything
   still writing there and detach it before the delete, or rm walks into the
   guest's disk (see [mounted]). Best effort: [mounted] normally unmounts through
   the VM helper, which reclaim does not have here, so if a disk is still
   attached this falls back to diskutil. *)
let reclaim_temp path =
  let mnt = Path.(path / "mnt") in
  Os.quiet "sudo" [ "pkill"; "-f"; "[" ^ String.sub mnt 0 1 ^ "]" ^ String.sub mnt 1 (String.length mnt - 1) ];
  Os.quiet "sudo" [ "diskutil"; "unmount"; "force"; mnt ];
  temp_dir path ()

let vm ~t args = Os.sudo (t.vm :: args)

let vm_exn ~t args =
  match vm ~t args with
  | 0 -> ()
  | status -> failwith (Printf.sprintf "day10-vm %s exited with status %d" (String.concat " " args) status)

(* A clone's disk, mounted for [f] and let go of however [f] ends: see
   Cleanup.  A copy into or out of the mount is a process of its own, which an
   interrupt to day10 does not reach, so whatever is still at work there is
   stopped first: left to run, it went on writing into the directory the disk
   had been mounted on, after the temp directory around it had been deleted.
   The bracket keeps pkill from matching its own command line. *)
let mounted ~t ~dir ~mnt f =
  let () = vm_exn ~t [ "mount"; dir; mnt ] in
  let release () =
    Os.quiet "sudo" [ "pkill"; "-f"; "[" ^ String.sub mnt 0 1 ^ "]" ^ String.sub mnt 1 (String.length mnt - 1) ];
    Os.quiet "sudo" [ t.vm; "unmount"; dir; mnt ]
  in
  Cleanup.with_resource release f

let sh cmd =
  match Os.sudo [ "sh"; "-c"; cmd ] with
  | 0 -> ()
  | status -> failwith (Printf.sprintf "%s exited with status %d" cmd status)

(* The environment a build runs in, as opam on the other platforms sees it, and
   Homebrew held to the index the base fetched. *)
let env ~(config : Config.t) =
  [
    ("HOME", "/Users/opam");
    ("USER", "opam");
    ("LOGNAME", "opam");
    ("PATH", "/Users/opam/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/bin:/bin:/usr/sbin:/sbin");
    ("LANG", "en_US.UTF-8");
    ("CI", "true");
    ("OPAM_REPO_CI", "true");
    ("OPAMYES", "1");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
    ("OPAMJOBS", string_of_int (Option.value ~default:Os.default_jobs config.opam_jobs));
    ("HOMEBREW_NO_AUTO_UPDATE", "1");
    ("HOMEBREW_NO_INSTALL_CLEANUP", "1");
    ("HOMEBREW_NO_ANALYTICS", "1");
    ("HOMEBREW_NO_ENV_HINTS", "1");
  ]

(* The job day10-vm runs once at boot, as root: wait for what the build needs,
   then run [command] as opam in its home.  A guest has no address for the
   first moments after boot, which a download would fail on. *)
let job_script ~config ~network ~src command =
  String.concat "\n"
    ([ "#!/bin/sh" ]
    @ (if network then [ "for i in $(seq 1 60); do curl -s -o /dev/null --max-time 3 https://opam.ocaml.org/ && break; sleep 1; done" ] else [])
    @ (if src then
         [
           {|for i in $(seq 1 60); do [ -d "/Volumes/My Shared Files/src" ] && break; sleep 1; done|};
           {|ln -sfn "/Volumes/My Shared Files/src" /Users/opam/src|};
         ]
       else [])
    @ [
        "exec sudo -u opam -H /usr/bin/env -i "
        ^ String.concat " " (List.map (fun (k, v) -> k ^ "=" ^ Filename.quote v) (env ~config))
        ^ " /bin/bash -s <<'DAY10'";
        "cd \"$HOME\"";
        command;
        "DAY10";
        "";
      ])

(* Write the job onto the clone, which [mnt] must have mounted: done while the
   clone is mounted to copy its inputs in, it costs no mount of its own. *)
let write_job ~t ~temp_dir ~mnt ~network ~src command =
  let script = Path.(temp_dir / "job.sh") in
  let () = Os.write_to_file script (job_script ~config:t.config ~network ~src:(Option.is_some src) command) in
  vm_exn ~t [ "job"; mnt; script ]

(* Boot the clone, whose disk must not be mounted, on the job written there,
   the job's output going to the build log, and to the terminal when [tee].
   The run's status is the job's, or 125 when the VM failed rather than the
   job. *)
let run_vm ~hard_stop ~t ~build_log ~tee ~append ~name ~network ~src ~vm_dir =
  let shares = match src with None -> [] | Some dir -> [ "--share"; "src:" ^ dir ] in
  (* The VM is stopped however this scope ends, as Linux deletes its runc
     container.  The client stops it too when it goes, but nothing makes it go
     when day10 is interrupted: writing to the build log rather than through
     day10, it outlived day10 and kept the VM running on a disk that day10 had
     just deleted.  Stopping the VM ends the client's run, so it exits. *)
  Cleanup.with_resource (fun () -> Os.quiet "sudo" [ t.vm; "stop"; name ]) @@ fun () ->
  Os.sudo ~stdout:build_log ~stderr:build_log ~tee ~append
    ([ t.vm; "run"; "--dir"; vm_dir; "--name"; name ] @ (if hard_stop then [ "--hard-stop" ] else []) @ (if network then [] else [ "--no-network" ]) @ shares)

(* A later boot of a clone that is already set up, which mounts it only to
   write the job. *)
let boot ~t ~temp_dir ~build_log ~tee ~append ~name ~network ~src ~vm_dir ~mnt command =
  let () = mounted ~t ~dir:vm_dir ~mnt @@ fun () -> write_job ~t ~temp_dir ~mnt ~network ~src command in
  run_vm ~hard_stop:true ~t ~build_log ~tee ~append ~name ~network ~src ~vm_dir

(* What a guest leaves in opam's root that is not part of a layer, as on the
   other platforms: locks, the environment and repository caches opam
   rebuilds, and its logs. *)
let untidy =
  [
    Path.(opam_root / "lock");
    Path.(opam_root / "conf.lock");
    Path.(opam_root / "log");
    Path.(opam_root / "repo" / "conf.lock");
    Path.(switch / "lock");
    Path.(switch / "environment");
    Path.(brew / "var" / "homebrew" / "locks");
  ]

let tidy fs =
  sh (String.concat " " ([ "rm -rf" ] @ List.map (fun path -> Filename.quote Path.(fs / path)) untidy @ [ Filename.quote Path.(fs / opam_root / "repo") ^ "/*.cache" ]))

let run ~t ~temp_dir ~depexts opam_repository build_log =
  let config = t.config in
  let vm_dir = Path.(temp_dir / "vm") and mnt = Path.(temp_dir / "mnt") and fs = Path.(temp_dir / "fs") in
  let () = Os.mkdir fs in
  let () = Util.write_depexts Path.(temp_dir / "depexts") depexts in
  let () = vm_exn ~t [ "clone"; t.image; vm_dir ] in
  let input = "/" ^ inputs in
  let command =
    String.concat "\n"
      [
        "set -ex";
        "mkdir -p ~/bin";
        Printf.sprintf "curl --fail -sSL -o ~/bin/opam %s && chmod +x ~/bin/opam" opam_url;
        (* From the release, as the FreeBSD and Windows bases have it, over any
           the guest image carries. *)
        Printf.sprintf "curl --fail -sSL -o ~/bin/day10-install %s && chmod +x ~/bin/day10-install" day10_install_url;
        {|NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"|};
        "brew --version";
        (* Every depext into Homebrew's cache, which the base keeps, so a build
           installs from it with no network.  A set at a time, as an install
           asks for it, and a formula at a time when the set fails, one of its
           names being unknown, so that the rest are still fetched. *)
        Printf.sprintf
          {|while read -r s; do [ -n "$s" ] || continue; brew fetch --deps $s || for p in $s; do brew fetch --deps $p || true; done; done < %s/depexts|}
          input;
        Printf.sprintf "opam init -k local -a %s/opam-repository --bare --disable-sandboxing -y" input;
        (* Fall back to opam's cache when a package's upstream sources are gone
           or have changed, as the other bases do. *)
        {|opam option --global 'archive-mirrors="https://opam.ocaml.org/cache"'|};
        "opam switch create default --empty";
      ]
  in
  let () =
    mounted ~t ~dir:vm_dir ~mnt @@ fun () ->
    let dst = Path.(mnt / inputs) in
    let () = sh (Printf.sprintf "mkdir -p %s && ditto %s %s && cp %s %s/depexts" (Filename.quote dst) (Filename.quote opam_repository)
                   (Filename.quote Path.(dst / "opam-repository")) (Filename.quote Path.(temp_dir / "depexts")) (Filename.quote dst)) in
    write_job ~t ~temp_dir ~mnt ~network:true ~src:None command
  in
  (* Shut down by the guest, not stopped once the job reports as a build is: a
     build's disk is thrown away, but this one is kept and every build boots a
     clone of it, so its volumes are left as macOS leaves them. *)
  let result = run_vm ~hard_stop:false ~t ~build_log ~tee:config.log ~append:false ~name:(Filename.basename temp_dir) ~network:true ~src:None ~vm_dir in
  (* The base is the disk the job ran on, as a Linux base is the container's
     whole filesystem: every build boots a clone of it, so what the base
     installed and fetched is in place without being copied.  Its two trees are
     kept in fs as well, as a layer's are. *)
  let () =
    if result = 0 then
      mounted ~t ~dir:vm_dir ~mnt @@ fun () ->
      let () = sh (Printf.sprintf "cd %s && tar -cf - %s %s | tar -xpf - -C %s" (Filename.quote mnt) home brew (Filename.quote fs)) in
      let () = tidy mnt in
      sh ("rm -rf " ^ Filename.quote Path.(mnt / inputs))
  in
  let () = if result = 0 then tidy fs else sh ("rm -rf " ^ Filename.quote vm_dir) in
  result

(* A package is built with no network, as on Linux: its sources are staged on
   the host in the build's copy of the repository (Source_stage).  A package
   with depexts first boots with the network to have opam install them and
   nothing else; the tests, when asked for, boot again after the build.  Each
   boot is of the same clone, so it finds what the one before installed. *)
type phase = { suffix : string; network : bool; command : string }

(* Where a build's copy of the repository is put on the guest, below. *)
let repo_in_guest = "/" ^ Path.(opam_root / "repo" / "default")

(* What a build writes that no layer keeps: sources and build trees, caches and
   logs, and the repository, which is the solution's. *)
let excluded =
  [
    Path.(opam_root / "repo");
    Path.(opam_root / "log");
    Path.(opam_root / "download-cache");
    Path.(switch / "sources");
    Path.(switch / "build");
    Path.(switch / "packages" / "cache");
    Path.(home / "Library" / "Caches");
    Path.(home / "Library" / "Logs");
    Path.(home / "src");
  ]

let build ~t ~temp_dir ~depexts build_log pkg ordered_hashes =
  let config = t.config in
  let os_key = Config.os_key ~config in
  let vm_dir = Path.(temp_dir / "vm") and mnt = Path.(temp_dir / "mnt") and fs = Path.(temp_dir / "fs") in
  let () = Os.mkdir fs in
  let phases =
    match config.build_command with
    | Some { run; network } -> [ { suffix = ""; network; command = "cd src && " ^ run } ]
    | None ->
        let command ?reinstall ?depexts_only ~with_test () =
          String.concat " && " (Build_command.for_package ~config ?reinstall ?depexts_only ~with_test pkg)
        in
        let depexts =
          if OpamSysPkg.Set.is_empty depexts then []
          else [ { suffix = "-depexts"; network = true; command = command ~depexts_only:true ~with_test:false () } ]
        and tests =
          if Build_command.tests_requested ~config pkg then
            [ { suffix = "-test"; network = false; command = command ~reinstall:true ~with_test:true () } ]
          else []
        in
        depexts @ [ { suffix = ""; network = false; command = command ~with_test:false () } ] @ tests
  in
  let () =
    if Option.is_none config.build_command then
      Source_stage.stage ~repo:Path.(temp_dir / "opam-repository") ~in_container:("file://" ^ repo_in_guest ^ "/cache") ~log:build_log pkg
  in
  (* A clone of the base's disk, which has the base in it already.  A base
     built before bases kept their disk has only its trees, which go in first
     over a clone of the image, as a layer's do. *)
  let base_vm = Path.(config.dir / os_key / "base" / "vm") in
  let from_base = Sys.file_exists Path.(base_vm / "config.json") in
  let () = vm_exn ~t [ "clone"; (if from_base then base_vm else t.image); vm_dir ] in
  (* The layers outwards from the base: ordered_hashes runs outwards from the
     package.  Each is merged over the one before, so the topmost layer has the
     last word on a path, by Merge's rules, as on Linux: a path that is a
     symlink beneath and a directory above becomes the directory, which ditto
     refused to do.  Tree by tree, so that each tree's own directory keeps the
     owner the guest gave it. *)
  let sources = (if from_base then [] else [ "base" ]) @ List.rev ordered_hashes in
  let () =
    mounted ~t ~dir:vm_dir ~mnt @@ fun () ->
    let () =
      List.iter
        (fun tree ->
          let layers = List.filter Sys.file_exists (List.map (fun hash -> Path.(config.dir / os_key / hash / "fs" / tree)) sources) in
          Os.merge_layers ~copy:true ~markers:false ~layers ~target:Path.(mnt / tree) ())
        [ home; brew ]
    in
    (* No one layer's switch state lists what the others installed: write one
       that lists them all. *)
    let () =
      let packages hash = Path.(config.dir / os_key / hash / "fs" / switch / "packages") in
      match List.concat_map (fun hash -> if Sys.file_exists (packages hash) then Array.to_list (Sys.readdir (packages hash)) else []) sources with
      | [] -> ()
      | names ->
          let state = Path.(temp_dir / "switch-state") in
          let () = Opamh.dump_state_of (List.sort_uniq String.compare names) state in
          sh (Printf.sprintf "cp %s %s && chown %s %s" (Filename.quote state) (Filename.quote Path.(mnt / switch / "switch-state")) owner
                (Filename.quote Path.(mnt / switch / "switch-state")))
    in
    let repo = Path.(mnt / opam_root / "repo" / "default") in
    let () =
      sh (Printf.sprintf "rm -rf %s && ditto %s %s && chown -R %s %s" (Filename.quote repo) (Filename.quote Path.(temp_dir / "opam-repository"))
            (Filename.quote repo) owner (Filename.quote repo))
    in
    (* The first phase's job, while the clone is mounted anyway.  Outside the
       two trees, so the marker does not care when it is written. *)
    let () =
      let { network; command; _ } = List.hd phases in
      write_job ~t ~temp_dir ~mnt ~network ~src:config.directory command
    in
    sh ("touch " ^ Filename.quote Path.(mnt / marker))
  in
  (* Shown as it happens for the command the user asked for, as on Linux. *)
  let tee = Option.is_some config.build_command || config.log in
  let result =
    phases
    |> List.mapi (fun index phase -> (index, phase))
    |> List.find_map (fun (index, { suffix; network; command }) ->
           let name = Filename.basename temp_dir ^ suffix and append = index > 0 || Sys.file_exists build_log and src = config.directory in
           let status =
             (* The first phase's job is on the disk already; a later phase
                mounts the clone to write its own. *)
             if index = 0 then run_vm ~hard_stop:true ~t ~build_log ~tee ~append ~name ~network ~src ~vm_dir
             else boot ~t ~temp_dir ~build_log ~tee ~append ~name ~network ~src ~vm_dir ~mnt command
           in
           match status with 0 -> None | code -> Some code)
    |> Option.value ~default:0
  in
  (* Everything in the two trees newer than the marker, but for what no layer
     keeps, and every directory above it.  The directories go too, each on its
     own, so that they keep the guest's owner and mode: tar would make a missing
     one root's, and the next build, copying it in, would find opam unable to
     write there.  In C order a directory comes before what is in it.  A file
     the build deleted is not recorded, as on Windows. *)
  let () =
    mounted ~t ~dir:vm_dir ~mnt @@ fun () ->
    let prune = String.concat " -o " (List.map (fun path -> "-path " ^ Filename.quote path) excluded) in
    let parents = {|awk '{ n = split($0, part, "/"); path = part[1]; print path; for (i = 2; i <= n; i++) { path = path "/" part[i]; print path } }'|} in
    sh
      (Printf.sprintf "cd %s && find %s %s \\( %s \\) -prune -o -newercm %s -print | %s | LC_ALL=C sort -u | tar --no-recursion -T - -cf - | tar -xpf - -C %s"
         (Filename.quote mnt) home brew prune marker parents (Filename.quote fs))
  in
  let () = tidy fs in
  let () = sh ("rm -rf " ^ Filename.quote vm_dir) in
  result
