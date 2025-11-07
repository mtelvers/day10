type package_list = {
  packages: string list;
} [@@deriving yojson]

let read_packages filename =
  let json = Yojson.Safe.from_file filename in
  match package_list_of_yojson json with
  | Ok { packages } -> packages
  | Error msg -> failwith (Printf.sprintf "Failed to parse package list from %s: %s" filename msg)

let write_packages filename packages =
  let package_list = { packages } in
  let json = package_list_to_yojson package_list in
  Yojson.Safe.to_file filename json
