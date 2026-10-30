(* Flatten a stack of overlay layers into one directory, base-most first, each
   layer written over what the ones below it put there.

   A layer is an overlayfs upper directory, in which a path the build removed
   is not absent but recorded as a 0:0 character device.  Flattened, that
   marker means what the kernel would have made of it: the path is deleted
   from what the layers below put in the merge, and the marker stays to hide
   the path in whatever is mounted beneath the merge, which is the base.  A
   later layer that provides the path writes over the marker like anything
   else.  Without [markers] the deletion is made and no marker left, for a
   merge that is going to be a docker image, which has nothing beneath it and
   cannot carry a device node.

   cp did this well, until the markers: it hardlinks a device node like any
   other file and will not write a directory over one, and the rules that grew
   around that ran as the user, who cannot look into every root-owned
   directory of a layer.  So this is a walk of our own, run as root under sudo
   as day10 cp, that knows what a marker is.

   Hardlinked rather than copied, so a layer costs inodes and not bytes; a
   symlink is hardlinked too, which link(2) does on Linux.  A directory the
   layer removed and made again, which overlayfs records as an xattr on the
   new directory rather than as markers, is not recognised yet: the old
   contents would merge into the new. *)

let is_marker _path (st : Unix.stats) = st.st_kind = Unix.S_CHR && st.st_rdev = 0

let rec apply ~is_marker ~markers ~source ~target =
  let st = Unix.lstat source in
  let existing = match Unix.lstat target with st -> Some st | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None in
  let clear () = Option.iter (fun _ -> Os.rm ~recursive:true target) existing in
  if is_marker source st then begin
    clear ();
    if markers then Unix.link ~follow:false source target
  end
  else
    match st.st_kind with
    | S_DIR ->
        (match existing with
        | Some { Unix.st_kind = S_DIR; _ } -> ()
        | Some _ ->
            clear ();
            Unix.mkdir target 0o755
        | None -> Unix.mkdir target 0o755);
        Array.iter
          (fun entry -> apply ~is_marker ~markers ~source:(Filename.concat source entry) ~target:(Filename.concat target entry))
          (Sys.readdir source);
        (* After the contents, as cp does: writing them moves the mtime, and
           the mode could have kept them out. *)
        Unix.chown target st.st_uid st.st_gid;
        Unix.chmod target st.st_perm;
        Unix.utimes target st.st_atime st.st_mtime
    | S_REG -> (
        clear ();
        try Unix.link ~follow:false source target
        with Unix.Unix_error (Unix.EMLINK, _, _) ->
          Os.cp source target;
          Unix.chown target st.st_uid st.st_gid)
    | S_LNK | S_CHR | S_BLK | S_FIFO | S_SOCK ->
        clear ();
        Unix.link ~follow:false source target

(* [is_marker] can be given for a test to stand in markers made of ordinary
   files, since a device node takes root to make. *)
let layers ?(is_marker = is_marker) ~markers ~target layers =
  Os.mkdir ~parents:true target;
  List.iter (fun source -> apply ~is_marker ~markers ~source ~target) layers
