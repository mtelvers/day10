(* The platform day10 is running on, chosen once from the host's OS.  A cache
   holds layers for the host's own container backend, so this is also the
   platform whose services the cache-wide commands use. *)

include
  (val match OpamSysPoll.os OpamVariable.Map.empty with
       | Some "linux" -> (module Linux : S.PLATFORM)
       | Some "freebsd" -> (module Freebsd : S.PLATFORM)
       | Some "win32" -> (module Windows : S.PLATFORM)
       | _ -> (module Dummy : S.PLATFORM)
      : S.PLATFORM)
