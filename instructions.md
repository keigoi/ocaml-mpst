
## Getting started
For instructions for compiling the ocaml-mpst on your own machine, see README.md.

Alternatively, you may use the VM prepared for the ECOOP'20 artifact evaluation.

1. Download our modified VM.
2. Load it in Virtual Box and boot it.
3. Open a terminal and navigate to ~/ocaml-mpst-lwt.
4. Follow the instructions below.

In the following, we assume that you are in the ocaml-mpst-lwt directory.
## Artifact layout

In addition to the source code of the library, which is a git clone of the githunb repository of the libary,
the artifact also contains
* a [benchmark](benchmark/) folder, which includes the source of the benchmarks and the scripts for producing the graphs (Section 6.1, Figure 15)
* an [examples](examples/) folder, which includes various examples, all global combinators from Figure 16 (Section 6.2).   
* a script, examples/run_oauth.sh for running the OAuth use case (Section 6.3) 
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

* Run the benchmarks script from the paper and display the graphs: 
```./benchmarks/run_all.sh 10s``` 

* Display the graphs from the paper using jupyter-notebook
```jupyter-notebook benchmark/graphs/Graph.ipynb```

The jupyter script will open in a new chrome tab. Click Run to run the scipt and display the graphs. 
At the bottom of the page, you will see a summary of the results. The graphs correspond to Figure 15 (Section 6.1) 

## STEP 3: Run examples, written with mpst-ocaml

###  Test an oAuth use case (Section 6.3)
* run the run_oAuth script 
```./examples/run_oauth.sh```

This will trigger a facebook authentication (a tab in chrome will open). 
You can either use your own facebook account to login, or use our test account. 
If you use your own account, a message dispalying that no access is allowd will be displayed. 

* the source code of the example is in examples/oAuth.ml

###  Test a dns server 

* run the run_dns script 
```./examples/run_oauth.sh```

Follow the instructions 

* the source code of the example is in examples/oAuth.ml






