(lang dune 2.8)

(name mpst2)

(version 0.0.1)

(authors "Keigo Imai")

(maintainers "keigo.imai@gmail.com")

(source
 (github keigoi/mpst2))

(documentation "https://github.com/keigoi/mpst2")

(generate_opam_files true)

(package
 (name mpst2)
 (synopsis "Re-implementation of OCaml-MPST [WIP]")
 (description "Re-implementation of OCaml-MPST [WIP].")
 (depends stdlib-shims hlist rows))
