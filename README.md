# OCaml-MPST

## Build

* Install OPAM

```sh
    sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
    opam switch 4.06.1
    eval $(opam env)
```

* Download and build

```sh
    git clone https://github.com/keigoi/ocaml-mpst.git
    cd ocaml-mpst
    
    # all
    opam install dune lwt_ssl lwt_log cohttp-lwt-unix
    dune build
    
    # minimal
    opam install dune lwt
    dune build example/ex1.exe
    ./_build/default/example/ex1.exe
```

## Examples

* See [Examples](examples/)

