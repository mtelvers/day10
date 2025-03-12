let opam_repository = List.fold_left Filename.concat "/home/mtelvers/opam-repository"
let download_cache = "/home/mtelvers/download-cache"
let dir = List.fold_left Filename.concat "/home/mtelvers/day29"

let env =
  [
    ("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin");
    ("HOME", "/home/opam");
    ("OPAMYES", "1");
    ("OPAMCONFIRMLEVEL", "unsafe-yes");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
  ]
