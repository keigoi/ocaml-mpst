# OCaml-MPST

## Lightweight implementation

* A lightweight implementation, [ocaml-mpst-light](https://github.com/keigoi/ocaml-mpst-light/), is available at:
  * https://github.com/keigoi/ocaml-mpst-light/


## Try OCaml-MPST [Online](https://keigoi.github.io/ocaml-mpst-light/index.html)!

* An interactive programming interface is available at:
  * https://keigoi.github.io/ocaml-mpst-light/index.html

![Try OCaml-MPST Screenshot](https://keigoi.github.io/ocaml-mpst-light/screenshot.png)

[Try OCaml-MPST Online](https://keigoi.github.io/ocaml-mpst-light/index.html)


## Presentation

* [Slides presented at PLAS Group Seminar at University of Kent](https://www.slideshare.net/keigoi/ocamlmpst-global-protocol-combinators-175519214)


## Build (For experienced users)

(We recommend a lightweight implementation [ocaml-mpst-light](https://github.com/keigoi/ocaml-mpst-light/))

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

