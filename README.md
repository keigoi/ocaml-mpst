# OCaml-MPST

## Lightweight implementation

* A lightweight implementation [ocaml-mpst-light](https://github.com/keigoi/ocaml-mpst-light/) is available.

## What is `ocaml-mpst`?

`ocaml-mpst` is  a communication library powered by  __Multiparty Session Types__ (abbreviated as MPST) in OCaml. Thus it ensures: 

* __Dadlock-freedom__, 
* __Protocol fidelity__ (communication will take place according to a prescribed protocol) and 
* __Communication safety__ (you do not get any type-mismatch errors)

--- under the assumption that all communication channels are used _linearly_. Linearity is checked either _dynamically_ (default) or _statically_, via another library `linocaml`.

## Try OCaml-MPST [Online](https://keigoi.github.io/ocaml-mpst-light/index.html)!

* An interactive programming interface is available at:
  * https://keigoi.github.io/ocaml-mpst-light/index.html

![Try OCaml-MPST Screenshot](https://keigoi.github.io/ocaml-mpst-light/screenshot.png)

[Try OCaml-MPST Online](https://keigoi.github.io/ocaml-mpst-light/index.html)

## `ocaml-mpst` in a nutshell

1. Write down a **protocol** using  **Global Combinators**. 

```ocaml
let ring = (a --> b) msg @@ (b --> c) msg @@ (c --> a) finish
```

--- This is a simple three-party ring-based protocol with _participants_ `A`, `B` and `C`, circulating message with _label_ `msg` in this order.

2. Extract channels (here `sa`, `sb`, and `sc`) from the protocol:

```ocaml
let sa = get_ch a ring
let sb = get_ch b ring
let sc = get_ch c ring
```

3. Run the participants in parallel!

```ocaml
(* Participant A *)
Thread.create (fun () -> 
  let sa = send sa#role_B#msg "Hello, " in
  let `msg(str, sa) = receive sa#role_C in
  print_endline str;
  close sa
) ();;

(* Participant B *)
Thread.create (fun () ->
  let `msg(str,sb) = receive sb#role_A in
  let sb = send sb#role_C#msg (str ^ "MPST") in
  close sb
) ();;

(* Participant C *)
let `msg(str, sc) = receive sc#role_C in
let sc = send sc#role_A#msg (str ^ " World!") in
close sc
```

It will start two threads behaving as the participant `A` and `B`, then runs `C` in the main thread. 

* Primitive `send s#role_X#msg value` outputs  on channel `s`  to role `X`, with a message label `msg` and  _payload_ value `value`.  It returns a _continuation channel_ which should be re-bound to the same variable s, which will result in `let s = send s#roleX .. in `.
* Primitive `receive s#role_W` inputs the message from role `W`. The received message will have form ```msg(val, s)`` packed inside a OCaml's _polymorphic variant_ constructor ``msg`, with payload value `val` and continuation channel `s`.
* Primitive `close` terminates a communication channel.

The above code is _session-typed_, as prescribed in the protocol `ring`  above. The all communications are deadlock-free, faithful to the protocol, and (OCaml-) type-error free!


Some basic notes:

* In a protocol `(x --> y) msg @@ cont`, `-->` is a 4-ary operator taking an output role  `x`, input role `y`, message label `msg` and continuation `cont`, where `@@` is a function application operator (equivalent to `$` in Haskell).
* Output expression  `send s#role_X#msgY valZ`  is parsed as `((send (s#role_X#msgY)) valZ)` where s#role_X#msgY is a standard _method invocation_ syntax of OCaml, chained twice in a row. 


More examples including branching, loops and delegation will come soon!


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

