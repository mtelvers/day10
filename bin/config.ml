let opam_repository = "/home/mtelvers/opam-repository"
let download_cache = "/home/mtelvers/download-cache"
let dir = "/home/mtelvers/day29"
let hostname = "builder"

let env =
  [
    ("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin");
    ("HOME", "/home/opam");
    ("OPAMYES", "1");
    ("OPAMCONFIRMLEVEL", "unsafe-yes");
    ("OPAMERRLOGLEN", "0");
    ("OPAMPRECISETRACKING", "1");
  ]
