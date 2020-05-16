# OCaml-MPST

## What is `ocaml-mpst`?

`ocaml-mpst` is  a communication library powered by  __Multiparty Session Types__ (abbreviated as MPST) in OCaml. Thus it ensures: 

* __Deadlock-freedom__, 
* __Protocol fidelity__ (communication will take place according to a prescribed protocol) and 
* __Communication safety__ (you do not get any type-mismatch errors)

--- under the assumption that all communication channels are used _linearly_. Linearity is checked either _dynamically_ (default) or _statically_, via another library `linocaml`.


## Install

Install [OPAM](https://opam.ocaml.org/). then the following command will install OCaml-MPST in your OPAM switch:

```bash
opam pin -y https://github.com/keigoi/ocaml-mpst.git
```

It will install several packages from this repository. To remove them, type:  `opam pin remove concur-shims linocaml-light ocaml-mpst ocaml-mpst-lwt ocaml-mpst-plug ocaml-mpst-plug-http` .

For benchmarks and some examples, you might need additional dependencies, which can be installed by:

```bash
opam install dns-lwt-unix cmdliner lwt_log lwt_ssl core_bench conf-libev
```


## Try OCaml-MPST [Online](https://keigoi.github.io/ocaml-mpst-light/index.html)!

* An interactive web interface is available at:
  * https://keigoi.github.io/ocaml-mpst-light/index.html


## Try OCaml-MPST on your PC

To run a example, after installation, type the script below in the terminal:
```
git clone https://github.com/keigoi/ocaml-mpst.git
cd ocaml-mpst/

# compile it
dune build examples/mpst/ring.exe

# run it
dune exec examples/mpst/ring.exe
```

Or alternatively, you can also use `ocamlfind` to compile things:

```bash
ocamlfind ocamlopt -thread -linkpkg -package ocaml-mpst ring.ml -o ring.exe
./ring.exe
```


## ocaml-mpst in 5 minutes

1. Write down a **protocol** using  **Global Combinators**. 

```ocaml
open Mpst
let ring = gen @@ (a --> b) msg @@ (b --> c) msg @@ (c --> a) finish
```

  --- This is a simple three-party ring-based protocol with _participants_ `A`, `B` and `C`, circulating messages with _label_ `msg` in this order. 

  * Protocol  `(a --> b) msg` specifies a message with label `msg` is sent from `a` to `b`. Protocols are composed by applying combinator `-->` to existing protocols (possibly via OCaml's function application operator `@@`, as above).
  * Combinator `finish` denotes termination of a protocol.

(More combinators will be explained later.)

2. Extract channels for each participants (here `sa` for `A`, `sb` for `B`, and `sc` for `C`) from the protocol:

```ocaml
let sa = get_ch a ring
let sb = get_ch b ring
let sc = get_ch c ring
```

3. Run threads in parallel, one for each participant!

NB: The following uses [concur-shims](packages/concur-shims) offers an `IO` monad parameterised over direct style and LWT.

```
open Concur_shims
let (let*) = IO.bind
```

```ocaml
(* Participant A *)
Thread.create (fun () -> 
  let* sa = send sa#role_B#msg "Hello, " in
  let* `msg(str, sa) = receive sa#role_C in
  print_endline str;
  close sa
) ();;

(* Participant B *)
Thread.create (fun () ->
  let* `msg(str,sb) = receive sb#role_A in
  let* sb = send sb#role_C#msg (str ^ "MPST") in
  close sb
) ();;

(* Participant C *)
let* `msg(str, sc) = receive sc#role_C in
let* sc = send sc#role_A#msg (str ^ " World!") in
close sc
```

It will start two threads behaving as the participant `A` and `B`, then runs `C` in the main thread. 

* Primitive `send s#role_X#msg value` outputs  on channel `s`  to role `X`, with a message label `msg` and  _payload_ value `value`.  Expression `s#role_X#msg` is a standard _method invocation_ syntax of OCaml, chained twice in a row. It returns a _continuation channel_ which should be re-bound to the same variable `s` ensuring linearity, which is why sending is written as `let s = send s#roleX .. in `.
* Primitive `receive s#role_W` inputs the message from role `W`. The received message will have form ```msg(val, s)`` packed inside a OCaml's _polymorphic variant_ constructor ``msg`, with payload value `val` and continuation channel `s` (again, re-binding existing channel variable `s`).
* Primitive `close` terminates a communication channel.

### 
The above code is _session-typed_, as prescribed in the protocol `ring`  above. The all communications are deadlock-free, faithful to the protocol, and type-error free!


Some basic notes:

* In a protocol `(x --> y) msg @@ cont`, `-->` is a 4-ary operator taking an output role  `x`, input role `y`, message label `msg` and continuation `cont`, where `@@` is a function application operator (equivalent to `$` in Haskell).
* Output expression  `send s#role_X#msg value`  is parsed as `((send (s#role_X#msg)) value)`.


More examples including branching, loops and delegation will come soon!


## Presentation

* [Slides presented at PLAS Group Seminar at University of Kent](https://www.slideshare.net/keigoi/ocamlmpst-global-protocol-combinators-175519214)


## Examples

* See [Examples](examples/)

## Benchmark Scripts

Benchmark Scripts [run_all.sh](benchmark/run_all.sh) depends on fixed OPAM switches: `ocaml-mpst-ev` and `ocaml-mpst-lwt`. Please install the corresponding packages, for example:

```bash
opam switch create ocaml-mpst-lwt 4.09.1+flambda
opam pin -y https://github.com/keigoi/ocaml-mpst.git
# install additional dependencies
opam install dns-lwt-unix cmdliner lwt_log lwt_ssl core_bench conf-libev

opam switch create ocaml-mpst-ev 4.09.1+flambda
git clone https://github.com/keigoi/ocaml-mpst.git
cd ocaml-mpst
opam pin -n -y .
cd packages/ocaml-mpst-ev
opam pin -y
```


# Notes on optional library dependencies

## LWT vs. Threads

OCaml-MPST depends on [concur-shims](packages/concur-shims/), which provides a thin layer for switching between standard `threads` library and [LWT](https://github.com/ocsigen/lwt).

Thus, __the programming interface and behaviour significantly changes if you (un)install LWT__.   

* Output is non-blocking in  LWT version of OCaml-MPST.
* Output is __blocking__ in `threads` version (it uses `Event.channel`).

Normally, you can just use `lwt` version of the OCaml-MPST. 

Also note that the above Quick Start automatically installs LWT, as `ocaml-mpst-plug-http` indirectly depends on `lwt`.

(The reasons for using `concur_shims` are (1) to ease readability and maintainability of the implementation code (2) running benchmark using both `threads` and `lwt` and (3) to provide an easy interface for users who are not fluent with recent OCaml based on `lwt`.)

### Monadic vs. Direct Style

Some programming idioms in direct style is not available in lwt.
While you can use [let rebinding in OCaml 4.08.0 or later](https://github.com/ocaml/ocaml/pull/1947), like

```ocaml
let (let*) = Lwt.bind (* IO.bind in concur-shims will also work *)

  ...
  let* `msg(x, s) = receive s#role_A in ... (* legal *)
```

You can't do the same thing in `match`-construct:

```ocaml
  match* receive s#role_A with (* illegal *)
  | `left(x, s) -> ...
  | `right(x, s) -> ...
```

In that case, you must bind the received result once and match on it, like the following:

```ocaml
  let* var = receive s#role_A in
  match var with
  | `left(x, s) -> ...
  | `right(x, s) -> ...
```

Another solution would be to use [ppx_let](https://github.com/janestreet/ppx_let).


## Nano_mutex in Janestreet Core

`Nano_mutex` in Jane Street's Core makes better performance in dynamic linearity checking.  If you install `core`, OPAM will automatically recompiles everything. Otherwise, OCaml-MPST uses either `Mutex` or `Lwt_mutex` module (indirectly via `concur_shims`).  


## Libraries used in examples and benchmarks

Some examples and benchmarks has additional dependencies. To install them at once, type:

```bash
opam install dns-lwt-unix cmdliner lwt_log
```

* DNS example: `dns-lwt-unix`
* OAuth exapmle: `cmdliner lwt_log lwt_ssl`
* Benchmarks: `core_bench conf-libev`


