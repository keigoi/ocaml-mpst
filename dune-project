(lang dune 2.8)

(name mpst)

(version 0.0.1)

(authors "Keigo Imai")

(maintainers "keigo.imai@gmail.com")

(source
 (github keigoi/ocaml-mpst))

(documentation "https://github.com/keigoi/ocaml-mpst")

(generate_opam_files true)

(package
 (name mpst)
 (synopsis "Re-implementation of OCaml-MPST [WIP]")
 (description "Re-implementation of OCaml-MPST [WIP].")
 (depends stdlib-shims hlist rows))
