(lang dune 1.12)

(authors "Keigo Imai")

(maintainers "keigo.imai@gmail.com")

(source
 (github keigoi/ocaml-mpst))

(documentation "https://github.com/keigoi/ocaml-mpst")

(generate_opam_files true)

(package
 (name benchmark)
 (synopsis "Dummy package context for benchmark")
 (description "Internal package used to partition benchmark.")
 (depends core core_bench))

(package
 (name concur-shims)
 (synopsis "A thin switching layer for concurrency")
 (description
  "Cocnur_shims provide a thin layer for switching concurrency libraries -- between threads and lwt.")
 (depends
  (ocaml
   (>= 4.08))))

(package
 (name examples)
 (synopsis "Dummy package context for examples")
 (description "Internal package used to partition examples.")
 (depends dns-lwt-unix lwt_log lwt_ssl))

(package
 (name linocaml-light)
 (synopsis
  "Lightweight version of LinOCaml: Linear types in OCaml via a parameterised monad")
 (description
  "Lightweight version of LinOCaml: Linear types in OCaml via a parameterised monad.")
 (depends
  (ocaml
   (>= 4.08))
  (concur-shims
   (= :version))
  stdlib-shims
  ppx_tools_versioned))

(package
 (name ocaml-mpst-lwt)
 (synopsis "Concurrency library based on Multiparty Session Types")
 (description
  "OCaml-MPST is a library implementation of Multiparty Session Types.")
 (depends
  (ocaml-mpst
   (= :version))
  lwt))

(package
 (name ocaml-mpst-plug-http)
 (synopsis "OCaml-MPST on top of HTTP (very tentative)")
 (description
  "OCaml-MPST is a library implementation of Multiparty Session Types.")
 (depends
  (ocaml-mpst-plug
   (= :version))
  conduit-lwt-unix
  cohttp-lwt-unix
  uri
  sexplib))

(package
 (name ocaml-mpst-plug)
 (synopsis "A pluggable variant of OCaml-MPST (very tentative)")
 (description
  "OCaml-MPST is a library implementation of Multiparty Session Types.")
 (depends
  (ocaml
   (>= 4.08))
  lwt))

(package
 (name ocaml-mpst)
 (synopsis "Concurrency library based on Multiparty Session Types")
 (description
  "OCaml-MPST is a library implementation of Multiparty Session Types.")
 (depends
  (concur-shims
   (= :version))
  (linocaml-light
   (= :version))))
