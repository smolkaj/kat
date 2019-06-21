# Kleene Algebra with Test

Bare-bones implementation of Kleene Algebra with Tests (KAT).

## Dependencies
You will need OCaml (tested with 4.07.1) and OPAM (v2 or higher), OCaml's packet manager.

We also use the library "hashcons" and OCamls build tool "dune"; you can install both using OPAM as follows:
```
opam install dune hashcons
```


## Quickstart

You can quickly experiment with the library by install utop,
```
opam install utop
```
and then starting an interactive OCaml session with the library loaded:
```
dune utop .
```

Then execute the following in utop, for example:
```
open Kat;;

let e = Exp.(Seq (Test (Test T1), Action A1));;
let dfa = ExpACI.brzozowski_dfa e;;
let s0 = dfa.start;;
let s1 = dfa.trans s0 (fun _ -> true) A1;;
let s2 = dfa.trans s0 (fun _ -> false) A1;;
```
