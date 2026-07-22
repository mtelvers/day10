(* A pure-OCaml runner for [Tar.t] computations against an in-memory [bytes]
   buffer. Same shape as [Tar_unix.run] but seeks the cursor in [pos]
   instead of [lseek]ing a file descriptor — so [Tar.fold] works without
   the [pipe_fold]-style read-and-discard hack. Depends only on [tar]; no
   [tar-unix], [lwt], or [eio]. *)

module High : sig
  type t
  type 'a s = 'a

  external inj : 'a s -> ('a, t) Tar.io = "%identity"
  external prj : ('a, t) Tar.io -> 'a s = "%identity"
end = struct
  type t
  type 'a s = 'a

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

type t = High.t
type error = [ `Fatal of Tar.error | `Unexpected_end_of_file | `Msg of string ]

let pp_error ppf = function
  | `Fatal e -> Tar.pp_error ppf e
  | `Unexpected_end_of_file -> Format.fprintf ppf "Unexpected end of file"
  | `Msg s -> Format.fprintf ppf "Error %s" s

let value v = Tar.High (High.inj v)

let run buf ~pos t =
  let len = Bytes.length buf in
  let rec run : type a. (a, _ as 'err, t) Tar.t -> (a, 'err) result = function
    | Tar.Read n ->
        if Int64.of_int Sys.max_string_length < n then Error (`Msg "Tar_bytes: read length exceeds maximum string length")
        else
          let n = Int64.to_int n in
          let avail = len - !pos in
          if avail = 0 then Error `Unexpected_end_of_file
          else
            let take = if n < avail then n else avail in
            let s = Bytes.sub_string buf !pos take in
            pos := !pos + take;
            Ok s
    | Tar.Really_read n ->
        if Int64.of_int Sys.max_string_length < n then Error (`Msg "Tar_bytes: read length exceeds maximum string length")
        else
          let n = Int64.to_int n in
          if len - !pos < n then Error `Unexpected_end_of_file
          else
            let s = Bytes.sub_string buf !pos n in
            pos := !pos + n;
            Ok s
    | Tar.Seek n ->
        if Int64.of_int max_int < n then Error (`Msg "Tar_bytes: seek offset exceeds maximum integer")
        else
          let p = !pos + Int64.to_int n in
          if p < 0 || p > len then Error `Unexpected_end_of_file
          else (
            pos := p;
            Ok ())
    | Tar.Write _ -> Error (`Msg "Tar_bytes: archive is read-only")
    | Tar.Return v -> v
    | Tar.High v -> High.prj v
    | Tar.Bind (x, f) -> (
        match run x with
        | Ok v -> run (f v)
        | Error _ as e -> e)
  in
  run t
