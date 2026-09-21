(* Operations over a whole cache rather than a single build: reporting what is
   in it, and deleting what is no longer worth keeping.

   A cache holds one directory per platform, named distribution-version-arch,
   beside the temp directories of runs in progress.  Both operations work across
   every platform they are given, because a layer that outlives its platform's
   usefulness is invisible to anything that only looks at the platform in front
   of it -- which is how a stale distribution survives untouched while a busy
   one is trimmed. *)

type layer = {
  dir : string;
  platform : string;
  (* From layer.json's mtime, which day10 touches whenever a layer takes part in
     a build.  So this is when the layer was last wanted, not when it was
     built, and both operations here are about use rather than age. *)
  last_used : float;
  ok : bool;
}

type mode =
  | Keep_days of int
  | Keep_percent of int
  | Max_size of int

(* A platform is a directory holding a base image: base/fs is what every layer
   for it is built on, so a directory without one is not a platform whatever it
   is called.  That is a surer test than the name, and it keeps out whatever
   else happens to be in the cache -- a cache of its own filesystem has a
   lost+found, which by name alone reads as a platform.

   A component that was given is matched against the part of the name it
   occupies rather than by splitting the name: only its two ends are reliable
   delimiters, since a version may be 24.04 or unstable or tumbleweed. *)
let platforms ~distribution ~version ~arch dir =
  let contains needle key =
    let n = String.length needle and k = String.length key in
    let rec go i = i + n <= k && (String.equal (String.sub key i n) needle || go (i + 1)) in
    go 0
  in
  let matches key =
    Option.fold ~none:true ~some:(fun d -> String.starts_with ~prefix:(d ^ "-") key) distribution
    && Option.fold ~none:true ~some:(fun v -> contains ("-" ^ v ^ "-") key) version
    && Option.fold ~none:true ~some:(fun a -> String.ends_with ~suffix:("-" ^ a) key) arch
  in
  Os.ls dir
  |> List.filter (fun path -> Sys.file_exists Path.(path / "base" / "fs"))
  |> List.filter_map (fun path ->
         let key = Filename.basename path in
         if matches key then Some (key, path) else None)
  |> List.sort compare

(* Only a directory holding a layer.json counts, which is what keeps base/ and
   any half-written layer out of every total and out of prune's reach. *)
let layers_of (platform, path) =
  Os.ls path
  |> List.filter_map (fun dir ->
         let layer_json = Path.(dir / "layer.json") in
         if not (Sys.file_exists layer_json) then None
         else
           match ((Unix.stat layer_json).st_mtime, Util.load_layer_info_exit_status layer_json) with
           | last_used, exit_status -> Some { dir; platform; last_used; ok = exit_status = 0 }
           | exception _ -> None)

(* The three parts of a platform directory's name, for when something has to
   act on the platform rather than merely select it.  Config.os_key writes it as
   distribution-version-arch, so the arch comes off the last dash and the
   distribution off the first, leaving the version between: debian-testing and
   opensuse-tumbleweed both come apart correctly.  A version containing a dash
   would not, and nothing writes one. *)
let components key =
  match String.rindex_opt key '-' with
  | None -> None
  | Some last -> (
      let arch = String.sub key (last + 1) (String.length key - last - 1) in
      let rest = String.sub key 0 last in
      match String.index_opt rest '-' with
      | None -> None
      | Some first -> Some (String.sub rest 0 first, String.sub rest (first + 1) (String.length rest - first - 1), arch))

let by_platform ~distribution ~version ~arch dir = platforms ~distribution ~version ~arch dir |> List.map (fun p -> (fst p, layers_of p))

let human bytes =
  let rec go n = function
    | [ unit ] -> Printf.sprintf "%.1f%s" n unit
    | unit :: rest -> if n < 1024.0 then Printf.sprintf "%.1f%s" n unit else go (n /. 1024.0) rest
    | [] -> Printf.sprintf "%.0f" n
  in
  go (float_of_int bytes) [ "B"; "K"; "M"; "G"; "T" ]

(* The inverse, for --max-size: 40G, 40GB, 500M, 1.5T.  A unit is required, so
   that --max-size 40 is refused rather than read as forty bytes and taken as
   licence to empty the cache -- which is a plausible slip for 40G in a cron
   entry, on a command that deletes without asking. *)
let size_of_string s =
  let s = String.trim s in
  let s = if String.length s > 1 && Char.uppercase_ascii s.[String.length s - 1] = 'B' then String.sub s 0 (String.length s - 1) else s in
  let n = String.length s in
  if n = 0 then None
  else
    let scale =
      match Char.uppercase_ascii s.[n - 1] with
      | 'K' -> Some 1024
      | 'M' -> Some (1024 * 1024)
      | 'G' -> Some (1024 * 1024 * 1024)
      | 'T' -> Some (1024 * 1024 * 1024 * 1024)
      | _ -> None
    in
    match scale with
    | Some multiplier -> Option.map (fun v -> int_of_float (v *. float_of_int multiplier)) (float_of_string_opt (String.sub s 0 (n - 1)))
    | None -> None

let age_hours now layer = (now -. layer.last_used) /. 3600.0
let bar_width = 18
let sum sized = List.fold_left (fun acc (_, size) -> acc + size) 0 sized
let tally sized = (List.length sized, sum sized)

(* Bucket width for the histogram, taken from the range the layers actually
   cover rather than fixed: a worker two days old and a cache four months old
   want different scales, and one set of bands says nothing useful about both.
   Aim for ten or so buckets, in hours below a day and whole days above.  Not
   weeks or months: a boundary is worth reading off the axis only if it
   converts to what --days takes without having to work it out, which 48 hours
   does and 15 weeks does not.  --percent is not served by an age axis at all,
   since it works on cumulative size and will cut anywhere, including inside
   one of these buckets; that is what the footer is for. *)
let bucket_width hours =
  let ladder = [ 6; 12; 24; 48; 72; 168; 336; 504; 720; 1440; 2160; 4320; 8760 ] in
  let target = hours /. 10.0 in
  match List.find_opt (fun w -> float_of_int w >= target) ladder with
  | Some w -> w
  | None -> List.nth ladder (List.length ladder - 1)

let histogram now sized =
  let oldest = List.fold_left (fun acc (layer, _) -> Float.max acc (age_hours now layer)) 0.0 sized in
  let width = bucket_width oldest in
  let count = max 1 (int_of_float (ceil (oldest /. float_of_int width))) in
  let buckets = Array.make count (0, 0) in
  let () =
    List.iter
      (fun (layer, size) ->
        let i = min (count - 1) (int_of_float (age_hours now layer) / width) in
        let n, bytes = buckets.(i) in
        buckets.(i) <- (n + 1, bytes + size))
      sized
  in
  let largest = Array.fold_left (fun acc (_, bytes) -> max acc bytes) 0 buckets in
  let per_unit, unit = if width < 24 then (1, "hours") else (24, "days") in
  (* Past a couple of months a figure in days stops meaning much on sight, so
     say how far back the axis reaches in a unit that does. *)
  let gloss = if oldest > 1344.0 then Printf.sprintf "  (oldest %.0f weeks)" (oldest /. 168.0) else "" in
  Printf.printf "  %-28s %8s %8s%s\n" (unit ^ " since last used") "size" "layers" gloss;
  Array.iteri
    (fun i (n, bytes) ->
      (* Any bucket holding something gets at least one mark, so a bucket with
         a handful of small layers is not shown as empty. *)
      let marks = if bytes = 0 then 0 else max 1 (bytes * bar_width / largest) in
      let label = Printf.sprintf "%3d-%3d" (i * width / per_unit) ((i + 1) * width / per_unit) in
      Printf.printf "  %8s  %-18s %8s %8d\n" label (String.make marks '#') (human bytes) n)
    buckets

(* What has to go for the rest to fit within [limit]: the least recently used,
   until it does.  Both size-driven modes reduce to this -- a percentage is a
   ceiling expressed as a fraction of what is there rather than an absolute. *)
let fit_to limit sized =
  let rec take doomed keeping = function
    | [] -> List.rev doomed
    | (((_, size) as entry) :: rest) -> if keeping <= limit then List.rev doomed else take (entry :: doomed) (keeping - size) rest
  in
  take [] (sum sized) (List.sort (fun (a, _) (b, _) -> compare a.last_used b.last_used) sized)

let freed_by_days now sized days =
  let cutoff = float_of_int days *. 86400.0 in
  tally (List.filter (fun (layer, _) -> now -. layer.last_used > cutoff) sized)

let freed_by_percent sized pct = tally (fit_to (sum sized * pct / 100) sized)

(* Sizes come from each layer's own record, measuring whatever has none, so the
   first run over an existing cache walks it and later runs do not. *)
let info ~dir ~distribution ~version ~arch ~np =
  if not (Sys.file_exists dir) then OpamConsole.warning "Cache directory %s does not exist" dir
  else
    let found = by_platform ~distribution ~version ~arch dir in
    let all = List.concat_map snd found in
    let () =
      match Util.warm_layer_sizes ?np (List.map (fun l -> l.dir) all) with
      | 0 -> ()
      | n -> OpamConsole.note "Measuring %d layer(s) with no recorded size; later runs read the recorded ones" n
    in
    let now = Unix.time () in
    (* Sized once: every layer's size is wanted by the histogram and again by
       each cutoff in the footer. *)
    let sized layers = List.map (fun layer -> (layer, Util.layer_size layer.dir)) layers in
    let header name layers =
      let sized = sized layers in
      let count, bytes = tally sized in
      let failed = List.length (List.filter (fun (layer, _) -> not layer.ok) sized) in
      Printf.printf "\n%-21s %6d layers  %8s%s\n" name count (human bytes)
        (if failed = 0 then "" else Printf.sprintf "   %d failed" failed);
      sized
    in
    let () = List.iter (fun (platform, layers) -> if layers <> [] then histogram now (header platform layers)) found in
    match all with
    | [] -> Printf.printf "\nNothing in %s\n%!" dir
    | _ ->
        let sized =
          match found with
          | [ _ ] -> sized all (* the platform's own line has already said this *)
          | _ -> header (Printf.sprintf "total, %d platforms" (List.length found)) all
        in
        (* Against every platform in scope, because that is what prune deletes
           from: a figure for one platform would understate the command. *)
        let () = Printf.printf "\n" in
        List.iteri
          (fun i (flag, n, (count, bytes)) ->
            Printf.printf "  %-12s %-14s %8s %6d %s\n%!"
              (if i = 0 then "would free" else "")
              (Printf.sprintf "%s %d" flag n) (human bytes) count
              (if count = 1 then "layer" else "layers"))
          [
            ("--days", 90, freed_by_days now sized 90);
            ("--days", 30, freed_by_days now sized 30);
            ("--percent", 90, freed_by_percent sized 90);
            ("--percent", 50, freed_by_percent sized 50);
          ]

let prune ~dir ~distribution ~version ~arch ?np mode =
  if not (Sys.file_exists dir) then OpamConsole.warning "Cache directory %s does not exist" dir
  else
    let entries = by_platform ~distribution ~version ~arch dir |> List.concat_map snd in
    (* Sizes are wanted by the two modes that talk about size, so measure
       whatever has no record.  A periodic call pays for that once and reads the
       records afterwards.  --days is about time alone and never measures. *)
    let sized () =
      let () =
        match Util.warm_layer_sizes ?np (List.map (fun l -> l.dir) entries) with
        | 0 -> ()
        | n -> OpamConsole.note "Measuring %d layer(s) with no recorded size" n
      in
      List.map (fun l -> (l, Util.layer_size l.dir)) entries
    in
    (* Ranked across every platform in scope rather than within each: a layer
       nothing has wanted for months should go before a busy platform's recent
       ones, wherever it happens to live. *)
    let to_delete, summary =
      match mode with
      | Keep_days days ->
          let cutoff = Unix.time () -. (float_of_int days *. 86400.0) in
          (List.filter (fun l -> l.last_used < cutoff) entries, Printf.sprintf "unused for more than %d day(s)" days)
      | Keep_percent pct ->
          let sized = sized () in
          let total = sum sized in
          let limit = total * pct / 100 in
          (List.map fst (fit_to limit sized), Printf.sprintf "keeping the newest %d%% of %s, so %s" pct (human total) (human limit))
      | Max_size limit ->
          let sized = sized () in
          let total = sum sized in
          ( List.map fst (fit_to limit sized),
            if total <= limit then Printf.sprintf "%s already within %s" (human total) (human limit)
            else Printf.sprintf "%s over %s" (human (total - limit)) (human limit) )
    in
    match to_delete with
    | [] -> OpamConsole.note "No cache entries to prune (%s)" summary
    | _ ->
        let counted =
          List.sort_uniq compare (List.map (fun l -> l.platform) to_delete)
          |> List.map (fun p -> Printf.sprintf "%s %d" p (List.length (List.filter (fun l -> String.equal l.platform p) to_delete)))
        in
        OpamConsole.note "Pruning %d cache entries (%s): %s" (List.length to_delete) summary (String.concat ", " counted);
        (* A batch at a time rather than piped into xargs, which was only ever
           keeping each command line inside the kernel's limit. *)
        let rec remove = function
          | [] -> ()
          | paths ->
              let batch = List.filteri (fun i _ -> i < 500) paths in
              let rest = List.filteri (fun i _ -> i >= 500) paths in
              (match Os.sudo ("rm" :: "-rf" :: batch) with
              | 0 -> ()
              | code -> OpamConsole.error "rm -rf exited with status %d" code);
              remove rest
        in
        (* Read before removing, and only what was already recorded: measuring
           the rest would walk trees that rm is about to walk anyway.  Where a
           layer has no record the total is a floor rather than the figure. *)
        let recorded = List.filter_map (fun l -> Util.load_layer_size l.dir) to_delete in
        remove (List.map (fun l -> l.dir) to_delete);
        (* Silence rather than a floor of zero when nothing was recorded at all,
           which is what --days on a cache no report has run over looks like. *)
        if recorded <> [] then
          OpamConsole.note "Freed %s%s"
            (if List.length recorded = List.length to_delete then "" else "at least ")
            (human (List.fold_left ( + ) 0 recorded))
