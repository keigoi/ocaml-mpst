# Examples

* [protocols](protocols/): Protocols (Fig. 16 in ECOOP'20 paper)
* [oauth](oauth/): An OAuth implementation (Fig. 17 in ECOOP'20 paper) using `Mpst_plug` and `Mpst_http`
* [dns](dns/): A DNS implementation using `Mpst_plug`
* [mpst](mpst/): Various usecases and features

## Build

To build all examples at once, type the following at the project root:

```
dune build @examples/all
```

or 


```
dune build @examples/mpst/all
dune build @examples/protocols/all
dune build @examples/oauth/all
dune build @examples/dns/all
```
