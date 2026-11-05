(* CopyFileW, which keeps what a byte copy drops: the file's attributes, among
   them the SYSTEM attribute that marks a Cygwin symlink of the kind an
   unprivileged user gets, and on ReFS the copy is a block clone.  Bound when
   first used, and only on Windows, so a build that happens to have
   ctypes-foreign elsewhere is unaffected. *)
open Ctypes

let copy_file_w = lazy (Foreign.foreign "CopyFileW" (ptr char @-> ptr char @-> int @-> returning int))
let get_last_error = lazy (Foreign.foreign "GetLastError" (void @-> returning uint32_t))

(* A path as the W functions take it: UTF-16LE, ending in a two-byte NUL. *)
let wide s =
  let b = Buffer.create ((2 * String.length s) + 2) in
  let rec go i =
    if i < String.length s then (
      let d = String.get_utf_8_uchar s i in
      Buffer.add_utf_16le_uchar b (Uchar.utf_decode_uchar d);
      go (i + Uchar.utf_decode_length d))
  in
  go 0;
  Buffer.add_string b "\000\000";
  CArray.start (CArray.of_string (Buffer.contents b))

let copy =
  if Sys.win32 then
    Some
      (fun src dst ->
        if Lazy.force copy_file_w (wide src) (wide dst) 0 <> 0 then Ok ()
        else Error (Printf.sprintf "CopyFileW failed with error %s" (Unsigned.UInt32.to_string (Lazy.force get_last_error ()))))
  else None
