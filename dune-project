(lang dune 3.17)

(name obi)

(generate_opam_files true)

(source
 (github ocurrent/obi))

(authors "OCaml Contributors")

(maintainers "OCaml Contributors")

(license ISC)

(documentation https://github.com/ocurrent/obi)

(package
 (name obi)
 (synopsis "OCaml Build Infrastructure")
 (description "OBI (OCaml Build Infrastructure) is a comprehensive build and testing tool for OCaml packages. It provides automated health checks by building packages from the opam repository across multiple compiler versions using container isolation.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.17))
  opam-0install
  yojson
  tyxml
  ppx_deriving_yojson
  cmdliner
  toml
  (odoc :with-doc))
 (tags
  (build testing opam containers ci)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
