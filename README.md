# ocaml-jit-example

This project shows how you can load and unload OCaml modules for a JIT

## How to try

```sh
esy start
```

## Limitations

Currently I can only guarantee that code itself will not have any reference, for data more work is needed

Most OCaml modules will have data, but you can workaround that by marshal and unmarshal the data as soon as it get's out of the called function.

## Overhead

Currently the loading overhead is quite high, as the OCaml backend emit's directly to a file and we call `gcc` + dlopen and dlsym, but after having it running the overhead is probably about 2 function calls, in reality anything outside of small examples like the interpreter at `Example.ml` that shouldn't matter
