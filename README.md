# OCaml-MPST

## New version is coming soon

We are striving to develop the new generation of OCaml-MPST which equips with _timeouts_ (in particular, _mixed choices_ in session-type/process-calculus terminology) and is supporting _OCaml Multicore_.

The older ECOOP 2020 version is still accessible at [mpst1](https://github.com/keigoi/ocaml-mpst/tree/mpst1) branch.

## What is `ocaml-mpst`?

`ocaml-mpst` is a communication library powered by **Multiparty Session Types**
(abbreviated as MPST) in OCaml. Thus it ensures:

- **Deadlock-freedom**,
- **Protocol fidelity** (communication will take place according to a prescribed
  protocol) and
- **Communication safety** (you do not get any type-mismatch errors)

--- under the assumption that all communication channels are used _linearly_.
Linearity is checked either _dynamically_ (default) or _statically_, via another
library `linocaml`.

## Install

Install [OPAM](https://opam.ocaml.org/). then the following command will install
OCaml-MPST in your OPAM switch:

```bash
opam pin -y https://github.com/keigoi/ocaml-mpst.git
```

It will install several packages from this and other repositories. To remove them, type:
`opam pin remove ocaml-mpst rows hlist`
.

## Try OCaml-MPST [Online](https://keigoi.github.io/ocaml-mpst-light/index.html)!

- An interactive web interface (though with an older version) is available at:
  - https://keigoi.github.io/ocaml-mpst-light/index.html

## Try OCaml-MPST on your PC

To run a example, after installation, type the script below in the terminal:

```
git clone --recurse-submodules https://github.com/keigoi/ocaml-mpst.git
cd ocaml-mpst/

dune exec examples/mpst/calc.exe
```

## ocaml-mpst in 5 minutes

0. Create and go into `examples/my_example/` and prepare your own `dune` file and `my_example.ml`.

```
(executable
 (name my_example)
 (modules my_example)
 (preprocess
  (staged_pps mpst.ppx.ty))
 (libraries mpst))
```

1. Declare the name of participants (__roles__) and __labels__:

```
open Mpst.BasicCombinators
open Mpst.Unicast

[%%declare_roles_prefixed a, b, c] (* note that the order matters *)
[%%declare_labels msg]
```

2. Write down a **protocol** using **Global Combinators**.

```ocaml
let ring = (a --> b) msg @@ (b --> c) msg @@ (c --> a) finish
```

--- This is a simple three-party ring-based protocol with _participants_ `A`,
`B` and `C`, circulating messages with _label_ `msg` in this order.

- Protocol `(a --> b) msg` specifies a message with label `msg` is sent from `a`
  to `b`. Protocols are composed by applying combinator `-->` to existing
  protocols (possibly via OCaml's function application operator `@@`, as above).
- Combinator `finish` denotes termination of a protocol.

(More combinators will be explained later.)

3. Extract channels for each participants (here `sa` for `A`, `sb` for `B`, and
   `sc` for `C`) from the protocol:

```ocaml
let (`cons(sa, `cons(sb, `cons(sc, _)))) = extract ring;;
```

4. Run threads in parallel, one for each participant!

```ocaml
(* Participant A *)
Thread.create (fun () ->
  let sa = select sa#role_B#msg in
  let `msg sa = branch sa#role_C in
  close sa
) ();;

(* Participant B *)
Thread.create (fun () ->
  let `msg sb = branch sb#role_A in
  let sb = select sb#role_C#msg in
  close sb
) ();;

(* Participant C *)
let `msg sc = branch sc#role_B in
let sc = select sc#role_A#msg in
close sc;
print_endline "Ring-based communication finished successfully!"
```

5. Run it!

```sh
dune exec ./my_example.exe
```

It will start two threads behaving as the participant `A` and `B`, then runs `C`
in the main thread.

- Primitive `select s#role_X#msg value` outputs on channel `s` to role `X`, with a
  message label `msg`. Expression `s#role_X#msg` is a standard _method invocation_ 
  syntax of OCaml, chained twice in a row. It  returns a _continuation channel_
  which should be re-bound to the same variable `s` ensuring linearity, which is
  why sending is written as `let s = select s#roleX .. in `.
- Primitive `branch s#role_W` inputs the message from role `W`. The received
  message will have form
  `` msg s` packed inside a OCaml's _polymorphic variant_ constructor  ``msg`, 
  with continuation channel`s`(again, re-binding existing channel variable `s`).
- Primitive `close` terminates a communication channel.

- To communicate OCaml values, use `(==>)` combinator and `send` and `receive`
  primitives (see [the calculator example](examples/calc), for details).

###

The above code is _session-typed_, as prescribed in the protocol `ring` above.
The all communications are deadlock-free, faithful to the protocol, and
type-error free!

Some basic notes:

- In a protocol `(x --> y) msg @@ cont`, `-->` is a 4-ary operator taking an
  output role `x`, input role `y`, message label `msg` and continuation `cont`,
  where `@@` is a function application operator (equivalent to `$` in Haskell).
- Output expression `select s#role_X#msg` is parsed as
  `(select (s#role_X#msg))`.

More examples including branching, loops and delegation will come soon!

## Presentation

- [Presentation video at ECOOP 2020](https://2020.ecoop.org/details/ecoop-2020-papers/9/Multiparty-Session-Programming-with-Global-Protocol-Combinators)
- [Slides presented at PLAS Group Seminar at University of Kent](https://www.slideshare.net/keigoi/ocamlmpst-global-protocol-combinators-175519214)

## Examples

- See [Examples](examples/)
