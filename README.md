![Build and test](https://github.com/edwintorok/dune-compiledb/actions/workflows/workflow.yml/badge.svg)

`Dune-compiledb` — generate `compile_commands.json`
===================================================

The [`compile_commands.json`](https://clang.llvm.org/docs/JSONCompilationDatabase.html) is used by language servers like `clangd` to determine what flags to use when analysing C source code.
It is also used by static analysers such as [`goblint`](https://goblint.in.tum.de/overview) to determine how to preprocess C files.

Using the correct include flags is important, otherwise the `<caml/...>` headers won't be found by these tools.

# Installation

`opam install dune-compiledb`

Requires OCaml 4.08+ and Dune 2.7+.

# Usage

Run this to create a `compile-commands.json`:
`dune rules | dune-compiledb`

Now `clangd` integration with your editor should work.

# Alternatives

[bear](https://github.com/rizsotto/Bear) can be used if your project doesn't have header files generated at build time:
```
dune clean && bear -- dune build @check --cache=disabled
```

However, the generated `compile_commands.json` lacks the `-iquote` flag and `clangd` won't be able to find the generated header files (if any).