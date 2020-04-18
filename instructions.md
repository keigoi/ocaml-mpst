
## Getting started
For instructions for compiling the ocaml-mpst on your own machine, see [README.md](https://github.com/keigoi/ocaml-mpst/edit/master/readme.md).

Alternatively, you may use the VM prepared for the ECOOP'20 artifact evaluation.

1. Download our [modified VM](https://drive.google.com/open?id=198S910WCd8y4Ow14WRg_9e3rrs-qsK3Q).
2. Load it in [Virtual Box](https://www.virtualbox.org/) and boot it.
3. Open a terminal and navigate to ~/ocaml-mpst-lwt.
4. Follow the instructions below.

In the following, we assume that you are in the ocaml-mpst-lwt directory.
## Artifact layout

In addition to the source code of the library, which is a git clone of [mpst-ocaml](https://github.com/keigoi/ocaml-mpst/), 
the artifact also contains
* a [benchmark](benchmark/) folder, which includes the source of the benchmarks and the scripts for producing the graphs (Section 6.1, Figure 15)
* an [examples](examples/) folder, which includes various examples
* an [examples/protocol](examples/protocol) folder, which includes the global combinators from Figure 16 (Section 6.2).   
* a script, examples/run_oauth.sh, for running the OAuth use case (Section 6.3) 
* a tutorial that guides you through implementing and testing your own examples 

## Quick check before you start
To ensure that your environemnt is working, open a terminal and run the following commands. 

Make sure that there are no errors, failures or timeouts.

 * Build all examples: 
``` 
dune build @examples/all 
```

* Run the benchmarks script from the paper and display the graphs: 

Run all benchmarks for a limited time.:
```
./benchmarks/run_all.sh 0.5s
``` 

Display the graphs from the paper using jupyter-notebook
```
jupyter-notebook benchmark/graphs/Graph.ipynb
```

## STEP 1: Getting to know the library:


## STEP 2: Run mpst-ocaml benchmarks (Section 6.1):

* Run the benchmarks script (it should take approximately 10-15 min): 

```
./benchmarks/run_all.sh
``` 

* Display the graphs from the paper using jupyter-notebook
```
jupyter-notebook benchmark/graphs/Graph.ipynb
```

The jupyter script will open in a new chrome tab. Click Run to run the scipt and display the graphs. 
At the bottom of the page, you will see a summary of the results. The graphs correspond to Figure 15 (Section 6.1) 

More information about the source of the benchmarks is available [here] (benchmark/).

## STEP 3: Run applications, written with mpst-ocaml 
### STEP 3.1: Run an oAuth use case (Section 6.3)
* run the run_oAuth script 
```
./examples/run_oauth.sh
```

This will trigger a facebook authentication (a tab in chrome will open). 
You can either use your own facebook account to login, or use our test account. 
If you use your own account, a message dispalying that no access is allowd will be displayed. 

* the source code of the example is in examples/oAuth.ml

### STEP 3.2: Run a dns server 

* run the run_dns script 
```
./examples/run_oauth.sh
```

Follow the instructions 

* the source code of the example is in examples/oAuth.ml

### STEP 3.3: Run the global protocol combinators (Section 6.2)
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
The easiest way to explore the channle vectors inferred by running the global combinators is to open VSCode, 
choose some of the files from the protocol folder, and hover over the type of the global combinator. 

(TODO: Maybe put a picture here?)

## Additional information 
*  All examples are already compiled and the executables are ```in_build/default/examples/**.exe.```

* Other useful commands:  

If you wish to modify the existing example (e.g. examples/mpst/calc.ml) and
recompile it, type
```
dune clean; dune @examples/mpst/all
```
If compilation fails, also try
```
opam switch ocaml-mpst-lwt; eval $(opam env)
```

* Try OCaml-MPST [Online](https://keigoi.github.io/ocaml-mpst-light/index.html)!
An interactive web interface is available at: https://keigoi.github.io/ocaml-mpst-light/index.html
