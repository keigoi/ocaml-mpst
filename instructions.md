
## Getting started

For the ECOOP'20 artifact evaluation, please use the VM prepared: 

1. Download our [modified VM](https://drive.google.com/open?id=198S910WCd8y4Ow14WRg_9e3rrs-qsK3Q).
2. Load it in [Virtual Box](https://www.virtualbox.org/) and boot it.
3. Open a terminal and navigate to ~/ocaml-mpst-lwt.
4. Follow the instructions below.

In the following, we assume that you are in the ocaml-mpst-lwt directory.
## Artifact layout

In addition to the source code of the library, which is a git clone of [mpst-ocaml](https://github.com/keigoi/ocaml-mpst/), 
the artifact also contains
* a [benchmark](benchmark/) folder, which includes the source of the benchmarks and the scripts for producing the graphs (Section 6.1, Figure 15)
* an [examples](examples/) folder, which includes various examples, including [examples/mpst/toy_oauth.ml file](examples/mpst/toy_oauth.ml) with the running example from the paper (Section 2). 
* an [examples/protocols](examples/protocols) folder, which includes the global combinators from Figure 16 (Section 6.2).   
* a script, [examples/run_oauth.sh](examples/run_oauth.sh), for running the OAuth use case (Section 6.3) 
* a tutorial that guides you through implementing and testing your own examples 


## Step 0: Build all examples 

## STEP 1: Run mpst-ocaml benchmarks (Section 6.1)

* Run the benchmarks script (it should take approximately 10-15 min): 

```
sudo ./benchmark/run_all.sh
``` 

* Display the graphs from the paper using jupyter-notebook
```
jupyter-notebook benchmark/graphs/Graphs.ipynb
```

The jupyter script will open in a new chrome tab. Click Run to run the script and display the graphs. 
At the bottom of the page, you will see a summary of the results. The graphs correspond to Figure 15 (Section 6.1) 

More information about the source of the benchmarks is available [here](benchmark/).

## STEP 3: Run applications, written with mpst-ocaml 
### STEP 3.1: Run an oAuth use case (Section 6.3)
* run the run_oAuth script 
```
./examples/run_oauth.sh
```

This will trigger a facebook authentication (a tab in chrome will open). 
You can either use your own facebook account to login, or use our test account. 
If you use your own account, a message displaying that no access is allows will be displayed. 
The test account is:
username: ecoop.91.2020@gmail.com
password: ecoop2020

* the source code of the example is in examples/oAuth.ml

### STEP 3.2: Run a dns server (Section 6.2)

* run the run_dns script 
```
./examples/run_dns.sh
```

Follow the instructions 

* the source code of the example is in examples/oAuth.ml

### STEP 3.3: Compile the global protocol combinators (Section 6.2)
All global protocol combinators from the paper are in the examples/protocols folder. 
To build all examples, run: 

```
dune build @examples/protocols/all
```

* You can modify and test some of these protocols to ensure that an error is detected.  
To test if a global protocol compiles after you have modify it, run: 

```dune build examples/protocols/the_name_of_the_protocol.exe```

where you have to replace the_name_of_the_protocol with the name of the file that you are modifying. 

Note that this folder contains only the protocols. 
The easiest way to explore the channel vectors inferred by running the global combinators is to open VSCode, 
choose some of the files from the protocol folder, and hover over the type of the global combinator. 
To configure VS code run in the terminal: 
```
dune build @example/all
```

### Step 4: Other example and implementing your own protocols (Optional)
1. Check if protocols are correct 
* open the [ring.ml](examples/mpst/ring.ml) file
* uncomment the various global combinators at the bottom of the file and check the error messages

2. Experiment with a simple calculator  
* open the [examples/mpst/calc.ml](examples/mpst/calc.ml) file

```
dune build examples/mpst/calc.ml
dune exec ./examples/mpst/calc.exe
```

3. Create a simple ring protocol 
* open VSCode and ... 
* follow the short tutorial [here](https://github.com/keigoi/ocaml-mpst/wiki/Ocaml-mpst-in-5-minutes) to implement the protocol 

__Hint:__ If you are struggling, the [examples/mpst/ring.ml file](examples/mpst/ring.ml) contains the full implementation, you can use it for reference. 

#### Compile/run a new example
Use the following commands. The commands assumes the newly created file is /examples/mpst/ring_protocol.ml. 

* Open the dune file (/examples/mpst/dune) and add a new executable entry to it: 
```
(executable
 (name ring_protocol)
 (modules ring_protocol)
 (libraries mpst))
```

* to build the example use:
```
dune build examples/mpst/ring_protocol.ml
``` 
* to execute the example use:
```
dune exec ./examples/mpst/ring_protocol.exe
``` 

#### __Note__ on syntax discrepancies:

There are small syntax discrepancies between ocaml-mpst-lwt and the paper. 
The running example of the paper uses the simplest in-built communication transport in Ocaml (Event), which is also avoidable in our [ocaml-mpst-light](https://keigoi.github.io/ocaml-mpst-light/index.html) implementation.

The full ocaml-mpst library of the artifact is parametric on the underlying transport. To enable this parametricity, we have created a wrapper that uses the [lwt](https://ocsigen.org/lwt/5.2.0/manual/manual) transport when installed, and switches to the in-built [Event] (https://caml.inria.fr/pub/docs/manual-ocaml/libref/Event.html) module if lwt is not available. 
To accommodate the lwt requirements, the wrapper requires some syntactic changes as explained below. 

Primitives `send`, `recv` and `close` are monadic in lwt, and
you must first declare monadic `let*` binding and use it, as follows:

```ocaml
open Concur_shims
let (let*) = IO.bind

let thread_A ch =
  let* ch = send ch#role_B#left "hello" in
  let* `msg(_,ch) = receive ch#role_C in ..
```

Pattern-matching `match` is not available for monads. Thus, when you make an external choice,
you must first bind the received variant to a variable, then match on it, as follows:

```ocaml
let thread_B ch =
  let* var = receive ch#role_A in
  match var with
  | `left(x, ch) -> ...
  | `right(x, ch) -> ...
```

For details, see the [notes on library dependencies](README.md#notes-on-optional-library-dependencies), implemented on top of [concur-shims](packages/concur-shims/)


Note that the light version of mpst-ocaml, which is not parametric on the transport but uses only the in-build [Event module](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Event.html) of ocaml, matches the exact syntax from the paper and is available to try [here](https://keigoi.github.io/ocaml-mpst-light/index.html) follows the syntax of the paper. 


## Additional details
* All examples are already compiled and the executables are ```in_build/default/examples/**.exe.```
* The file [examples/mpst/toy_oauth.ml file](examples/mpst/toy_oauth.ml) contains the running example from the paper (Section 2). You can compile and run it with:

```
dune build examples/mpst/toy_oauth.exe
``` 

* If you wish to modify the existing example (e.g. examples/mpst/calc.ml) and
recompile it, type
```
dune clean; dune @examples/mpst/all
```
If compilation fails, also try
```
opam switch ocaml-mpst-lwt; eval $(opam env)
```
## Additional information
* For instructions for compiling the ocaml-mpst on your own machine, see [README.md](https://github.com/keigoi/ocaml-mpst/blob/master/README.md). Note that some of the large examples (dns, oAuth)  need [additional dependencies](https://github.com/keigoi/ocaml-mpst/blob/master/README.md#libraries-used-in-examples-and-benchmarks). 
* Try OCaml-MPST [Online](https://keigoi.github.io/ocaml-mpst-light/index.html)!
An interactive web interface is available at: https://keigoi.github.io/ocaml-mpst-light/index.html
