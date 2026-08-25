(* Resources that have to be released if day10 stops before it is finished.

   A run that is killed -- because the answer is no longer wanted, or because
   the terminal went away -- would otherwise leave an overlay still mounted and
   a root-owned temp directory behind in the cache, which is why the cache
   accumulates temp-* directories that nothing ever removes.

   Holding a resource for the duration of a scope means it is released whether
   that scope ends by returning, by raising, or by being killed: the signal
   handler raises, so the finalisers run on the way out. *)

type t =
  | Temp_dir of string
  | Mount of string
  | Docker_container of string
  | Runc_container of string

exception Killed of int

(* Sys.sigint and friends are OCaml's own numbering, which is negative and no
   use for reporting, so pair each with the number the shell knows it by. *)
let signals = [ (Sys.sigint, 2); (Sys.sighup, 1); (Sys.sigterm, 15) ]

(* Releasing must not itself be interrupted.  It waits for children of its own,
   which is a safe point where a pending signal would raise, and a finaliser
   that raises becomes Fun.Finally_raised, losing the original reason and
   skipping whatever cleanup was still to come.  A second signal is therefore
   ignored until we are done; SIGKILL remains available to anyone who means
   it. *)
let uninterruptible f =
  let previous = List.map (fun (signal, _) -> (signal, Sys.signal signal Sys.Signal_ignore)) signals in
  Fun.protect ~finally:(fun () -> List.iter (fun (signal, behaviour) -> Sys.set_signal signal behaviour) previous) f

(* Best effort, and quiet: the normal path has usually released the resource
   already, and a failure here must not mask whatever we are unwinding from. *)
let release r =
  let quiet cmd = ignore (Os.sudo ~stdout:"/dev/null" ~stderr:"/dev/null" cmd) in
  let mounted path = Os.sudo ~stdout:"/dev/null" ~stderr:"/dev/null" [ "mountpoint"; "-q"; path ] = 0 in
  match r with
  | Mount path -> if mounted path then quiet [ "umount"; path ]
  | Docker_container name -> quiet [ "docker"; "rm"; "-f"; name ]
  | Runc_container name -> quiet [ "runc"; "delete"; "-f"; name ]
  | Temp_dir path -> if Sys.file_exists path then quiet [ "rm"; "-rf"; path ]

let with_resource r f = Fun.protect ~finally:(fun () -> uninterruptible (fun () -> release r)) f

(* Raising from the handler rather than exiting is what lets the finalisers run.
   The exception surfaces at the next safe point, which is as soon as the signal
   arrives now that Os waits for its children itself rather than leaving it to
   system(). *)
let install () =
  List.iter
    (fun (signal, number) ->
      Sys.set_signal signal
        (Sys.Signal_handle
           (fun _ ->
             List.iter (fun (signal, _) -> Sys.set_signal signal Sys.Signal_ignore) signals;
             raise (Killed number))))
    signals

(* The signal is already pending by the time a finaliser starts, so it fires at
   the first safe point inside it -- masking is itself a safe point, so this
   cannot be prevented, only tolerated.  Fun.protect reports a finaliser that
   raises as Finally_raised, wrapped once per scope we unwind through, but the
   outer finalisers still run, so the resources are released either way and only
   the reason needs digging out. *)
let rec reason = function
  | Fun.Finally_raised e -> reason e
  | e -> e

(* Cmdliner would otherwise catch Killed itself and report it as an internal
   error, so the caller passes ~catch:false and we do the reporting, keeping
   cmdliner's exit code for a genuine uncaught exception.  Death by signal
   conventionally reports 128 + the signal number. *)
let main f =
  match f () with
  | code -> code
  | exception e -> (
      match reason e with
      | Killed n ->
          Printf.eprintf "day10: interrupted by signal %i, released outstanding resources\n%!" n;
          128 + n
      | e ->
          Printf.eprintf "day10: internal error, uncaught exception:\n       %s\n%!" (Printexc.to_string e);
          125)
