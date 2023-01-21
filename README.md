# Description
Basically an implementation of the dependently typed lambda calculus I built to play around with.

# How to Run
1. Make sure you have [OCaml](https://ocaml.org/), [Opam](https://opam.ocaml.org/), and [Dune](https://dune.build/) installed
2. Run `dune build bin`
3. Run `./_build/default/bin/main.exe`
4. Checkout the examples in the `examples/` directory. These def files can be loaded into the REPL using `:load <PATH>`.

# TODO
- [ ] Add totality checker and guarentee types are total
- [ ] Unify defs, let expression, and command line `:let`
- [ ] Give normalization semantics to letrec in semantics doc
