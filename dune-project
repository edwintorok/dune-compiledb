(lang dune 3.17)
(cram enable)
(implicit_transitive_deps false)

(generate_opam_files)
(name dune-compiledb)
(source (codeberg edwintorok/dune-compiledb))
(authors "Edwin Török <edwin.torok@cloud.com>")
(maintainers "Edwin Török <edwin@etorok.net>")
(license LGPL-2.1-or-later)

(package
 (name dune-compiledb)
 (synopsis "Generate compile_commands.json from dune rules")
 (description "Generates a compile_commands.json from dune rules that can be used by language server like 'clangd', or static analyzers like 'goblint'. Works with generated headers.")
 (depends
  (ocaml (>= 4.08))
  (ezjsonm (>= 1.0.0))
  sexplib
  sexplib0
  (fpath (>= 0.7.1))
 )
)
