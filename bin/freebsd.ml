type t = {
  config : Config.t;
  uid : int;
  gid : int;
}

let env = [ ("HOME", "/home/opam"); ("OPAMYES", "1"); ("OPAMCONFIRMLEVEL", "unsafe-yes"); ("OPAMERRLOGLEN", "0"); ("OPAMPRECISETRACKING", "1") ]

let std_env ~(config : Config.t) =
  Util.std_env ~arch:config.arch ~os:"freebsd" ~os_distribution:"freebsd" ~os_family:"bsd" ~os_version:"1402000" ~ocaml_version:config.ocaml_version ()

let install_script =
  {|#!/bin/sh
#-
# Copyright (c) 2011 Nathan Whitehorn
# Copyright (c) 2013-2015 Devin Teske
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#
# $FreeBSD$
#
############################################################ INCLUDES

BSDCFG_SHARE="/usr/share/bsdconfig"
. $BSDCFG_SHARE/common.subr || exit 1

############################################################ MAIN

f_dprintf "Began Installation at %s" "$( date )"
f_dprintf "BSDINSTALL_CHROOT %s" "$1"
export BSDINSTALL_CHROOT=$1

error() {
	local msg
	if [ -n "$1" ]; then
		f_dprintf "error %s" "$1"
	fi
	exit
}


rm -rf $BSDINSTALL_TMPETC
mkdir $BSDINSTALL_TMPETC
mkdir -p $1 || error "mkdir failed for $1"

test ! -d $BSDINSTALL_DISTDIR && mkdir -p $BSDINSTALL_DISTDIR

if [ ! -f $BSDINSTALL_DISTDIR/MANIFEST -a -z "$BSDINSTALL_DISTSITE" ]; then
	export BSDINSTALL_DISTSITE="https://download.freebsd.org/ftp/releases/amd64/amd64/14.2-RELEASE"
	fetch -o $BSDINSTALL_DISTDIR/MANIFEST $BSDINSTALL_DISTSITE/MANIFEST || error "Could not download $BSDINSTALL_DISTSITE/MANIFEST"
fi

export DISTRIBUTIONS="base.txz"

FETCH_DISTRIBUTIONS=""
for dist in $DISTRIBUTIONS; do
	if [ ! -f $BSDINSTALL_DISTDIR/$dist ]; then
		FETCH_DISTRIBUTIONS="$FETCH_DISTRIBUTIONS $dist"
	fi
done
FETCH_DISTRIBUTIONS=`echo $FETCH_DISTRIBUTIONS`	# Trim white space

if [ -n "$FETCH_DISTRIBUTIONS" -a -z "$BSDINSTALL_DISTSITE" ]; then
	exec 3>&1
	BSDINSTALL_DISTSITE=`bsdinstall mirrorselect 2>&1 1>&3`
	MIRROR_BUTTON=$?
	exec 3>&-
	test $MIRROR_BUTTON -eq 0 || error "No mirror selected"
	export BSDINSTALL_DISTSITE
fi

if [ ! -z "$FETCH_DISTRIBUTIONS" ]; then
	bsdinstall distfetch || error "Failed to fetch distribution"
fi

bsdinstall checksum || error "Distribution checksum failed"
bsdinstall distextract || error "Distribution extract failed"

bsdinstall config  || error "Failed to save config"
cp /etc/resolv.conf $1/etc

bsdinstall entropy

f_dprintf "Installation Completed at %s" "$(date)"
exit $SUCCESS

################################################################################
# END
################################################################################|}

let init ~(config : Config.t) =
  let uid, gid =
    match (Unix.getuid (), Unix.getgid ()) with
    | 0, _ -> (1000, 1000)
    | uid, gid -> (uid, gid)
  in
  { config; uid; gid }

let deinit ~t:_ = ()
let config ~t = t.config

let os_key ~config =
  let os =
    List.map
      (fun v -> std_env ~config v |> Option.map OpamVariable.string_of_variable_contents |> Option.value ~default:"unknown")
      [ "os-family"; "os-version"; "arch" ]
  in
  String.concat "-" os

let layer_hash ~t deps =
  let hashes =
    List.map
      (fun opam ->
        opam |> Util.opam_file t.config.opam_repositories |> Option.get |> OpamFile.OPAM.effective_part |> OpamFile.OPAM.write_to_string
        |> OpamHash.compute_from_string |> OpamHash.to_string)
      deps
  in
  String.concat " " hashes |> Digest.string |> Digest.to_hex

let jail ~temp_dir ~rootfs ~mounts ~env ~argv ~network ~username =
  let mounts =
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
    [ "mount.fstab=" ^ fstab ]
  in
  let env = List.map (fun (k, v) -> k ^ "='" ^ v ^ "'") env in
  let params = String.concat " " [ (if List.is_empty env then "" else String.concat " " ("env" :: env)); String.concat " && " argv ] in
  let network = if network then [ "ip4=inherit"; "ip6=inherit"; "host=inherit" ] else [ "exec.start=/sbin/ifconfig lo0 127.0.0.1/8"; "vnet" ] in
  let cmd = Option.fold ~none:[ "command=/bin/sh" ] ~some:(fun u -> [ "command=/usr/bin/su"; "-l"; u ]) username in
  [ "jail"; "-c"; "name=" ^ Filename.basename temp_dir; "path=" ^ rootfs; "mount.devfs" ] @ mounts @ network @ cmd @ [ "-c"; params ]

let run ~t ~temp_dir opam_repository build_log =
  let config = t.config in
  let rootfs = Path.(temp_dir / "fs") in
  let () = Os.mkdir rootfs in
  let script = Path.(temp_dir / "install_script") in
  let () = Os.write_to_file script install_script in
  let _ = Os.sudo ~stdout:"/dev/null" [ "bsdinstall"; "-D"; build_log; "script"; script; rootfs ] in
  let _ = Os.sudo [ "chmod"; "777"; build_log ] in
  let _ = Os.sudo ~stdout:build_log [ "freebsd-update"; "-b"; rootfs; "fetch"; "install" ] in
  let _ = Os.sudo ~stdout:build_log [ "pkg"; "--chroot"; rootfs; "install"; "-y"; "pkg" ] in
  let _ = Os.sudo ~stdout:build_log [ "pkg"; "--chroot"; rootfs; "upgrade"; "-y"; "-f" ] in
  let opam = Path.(rootfs / "usr" / "bin" / "opam") in
  let _ = Os.sudo [ "curl"; "-L"; "https://github.com/ocaml/opam/releases/download/2.3.0/opam-2.3.0-" ^ config.arch ^ "-freebsd"; "-o"; opam ] in
  let _ = Os.sudo [ "sudo"; "chmod"; "+x"; opam ] in
  let opam_build = Path.(rootfs / "usr" / "bin" / "opam-build") in
  let _ = Os.sudo [ "curl"; "-L"; "https://github.com/mtelvers/opam-build/releases/download/1.2.0/opam-build-1.2.0-" ^ config.arch ^ "-freebsd"; "-o"; opam_build ] in
  let _ = Os.sudo [ "sudo"; "chmod"; "+x"; opam_build ] in
  let argv =
    [
      "pw groupadd opam -g " ^ string_of_int t.gid;
      "pw useradd -m -n opam -g opam -u " ^ string_of_int t.uid ^ " -h - -c opam";
      "pkg install -y sudo gmake git patch rsync bash zstd pkgconf";
      {|echo "opam ALL=(ALL:ALL) NOPASSWD:ALL" > /usr/local/etc/sudoers.d/opam|};
    ]
  in
  let result = Os.sudo ~stdout:build_log (jail ~temp_dir ~rootfs ~mounts:[] ~env:[] ~argv ~network:true ~username:None) in
  let () = if result = 0 then ignore (Os.sudo [ "umount"; Path.(rootfs / "dev") ]) in
  let _ = Os.sudo [ "chflags"; "-R"; "0"; rootfs ] in
  let argv =
    [ "touch /home/opam/.hushlogin"; "opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing -y"; "opam switch create default --empty" ]
  in
  let mounts = [ { Mount.ty = "nullfs"; src = opam_repository; dst = Path.("fs" / "home" / "opam" / "opam-repository"); options = [ "ro" ] } ] in
  let result = Os.sudo ~stdout:build_log (jail ~temp_dir ~rootfs ~mounts ~env ~argv ~network:true ~username:(Some "opam")) in
  let () =
    if result = 0 then (
      ignore (Os.sudo [ "umount"; Path.(rootfs / "dev") ]);
      ignore (Os.sudo [ "umount"; "-a"; "-f"; "-F"; Path.(temp_dir / "fstab") ]))
  in
  let _ =
    Os.sudo
      [
        "rm";
        "-rf";
        Path.(rootfs / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "environment");
        Path.(rootfs / "home" / "opam" / ".opam" / "repo" / "state-33BF9E46.cache");
      ]
  in
  let () = Os.write_to_file Path.(temp_dir / "status") (string_of_int result) in
  result

let build ~t ~temp_dir build_log pkg ordered_hashes =
  let config = t.config in
  let os_key = os_key ~config in
  let lowerdir = Path.(temp_dir / "lower") in
  let upperdir = Path.(temp_dir / "fs") in
  let workdir = Path.(temp_dir / "work") in
  let () = List.iter Os.mkdir [ lowerdir; upperdir; workdir ] in
  let pin = if OpamPackage.name_to_string pkg = config.package then [ "opam pin -yn " ^ OpamPackage.to_string pkg ^ " $HOME/src/"; "cd src" ] else [] in
  let with_test = if config.with_test then "--with-test " else "" in
  let argv = pin @ [ "opam-build -v " ^ with_test ^ OpamPackage.to_string pkg ] in
  let () =
    List.iter
      (fun hash ->
        (* no directory target option on FreeBSD cp *)
        let dir = Path.(config.dir / os_key / hash / "fs") in
        let dirs = Sys.readdir dir |> Array.to_list |> List.map (fun d -> Path.(dir / d)) in
        ignore (Os.sudo ([ "cp"; "-n"; "-a"; "-R"; "-l" ] @ dirs @ [ lowerdir ])))
      (ordered_hashes @ [ "base" ])
  in
  let () =
    let packages_dir = Path.(lowerdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "packages") in
    let state_file = Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "switch-state") in
    if Sys.file_exists packages_dir then Opamh.dump_state packages_dir state_file
  in
  let mounts =
    [
      { Mount.ty = "nullfs"; src = lowerdir; dst = "work"; options = [ "ro" ] };
      { Mount.ty = "unionfs"; src = upperdir; dst = "work"; options = [ "rw" ] };
      { ty = "nullfs"; src = Path.(temp_dir / "opam-repository"); dst = Path.("work" / "home" / "opam" / ".opam" / "repo" / "default"); options = [ "ro" ] };
    ]
  in
  let mounts =
    match config.directory with
    | None -> mounts
    | Some src -> mounts @ [ { ty = "nullfs"; src; dst = Path.("work" / "home" / "opam" / "src"); options = [ "rw" ] } ]
  in
  let result = Os.sudo ~stdout:build_log (jail ~temp_dir ~rootfs:workdir ~mounts ~env ~argv ~network:true ~username:(Some "opam")) in
  let () =
    if result = 0 then (
      ignore (Os.sudo [ "umount"; Path.(workdir / "dev") ]);
      ignore (Os.sudo [ "umount"; "-a"; "-f"; "-F"; Path.(temp_dir / "fstab") ]))
  in
  let _ =
    Os.sudo
      [
        "rm";
        "-rf";
        lowerdir;
        workdir;
        Path.(upperdir / "tmp");
        Path.(upperdir / "home" / "opam" / "default" / ".opam-switch" / "sources");
        Path.(upperdir / "home" / "opam" / "default" / ".opam-switch" / "build");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "packages" / "cache");
        Path.(upperdir / "home" / "opam" / ".opam" / "default" / ".opam-switch" / "environment");
        Path.(upperdir / "home" / "opam" / ".opam" / "repo" / "state-33BF9E46.cache");
      ]
  in
  result
