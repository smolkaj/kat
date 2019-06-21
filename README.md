# Kleene Algebra with Test
Bare-bones implementation of Kleene Algebra with Tests (KAT) and Brzozowski derivatives.


## Dependencies
You will need OCaml (tested with 4.07.1) and OPAM (v2 or higher), OCaml's packet manager.

We also use a few libraries and the OCaml build tool "dune"; you can install everything using OPAM as follows:
```
opam install dune hashcons ppx_jane
```


## Quickstart
You can quickly experiment with the library by install utop,
```
opam install utop
```
and then starting an interactive OCaml session with the library loaded, by executing the following from the root of the project:
```
dune utop .
```

Execute the following in the utop session:
```
$ open Kat;;
$ #install_printer ExpACI.pp;;
```

Then, experiment as you please. For example:
```
$ let e = Exp.(Star (Union (Seq (Test (Test T1), Seq (Union (Action A1, Action A2), Action A3)), (Seq (Test (Not (Test T2)), Action A2)))));;

$ let dfa = ExpACI.brzozowski_dfa e;;
> val dfa : ExpACI.t dfa =
>   {start = (T1·(A1 + A2)·A3 + ¬T2·A2)*; obs = <fun>; trans = <fun>}

$ let s0 = dfa.start;;
> val s0 : ExpACI.t = (T1·(A1 + A2)·A3 + ¬T2·A2)*

$ let s1 = dfa.trans s0 (function T1 -> true | _ -> false) A2;;
> val s1 : ExpACI.t = (1 + A3)·(T1·(A1 + A2)·A3 + ¬T2·A2)*

$ let s2 = dfa.trans s0 (fun _ -> true) A2;;
> val s2 : ExpACI.t = A3·(T1·(A1 + A2)·A3 + ¬T2·A2)*
```
