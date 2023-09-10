Set up the example in a directory of its own:
  $ mkdir -p workspace/test/dir && cd workspace/test
  $ cat >dir/dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs
  >  (language c)
  >  (names foostubs)))
  > EOF
  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > EOF
  $ touch dune
  $ touch dir/foostubs.c

Generate a dune compilation database
  $ dune rules | dune-compiledb

Check just the directory and file entries (arguments is system dependent):
  $ tail -n4 compile_commands.json
      "directory": "$TESTCASE_ROOT/workspace/test/_build/default/dir",
      "file": "dir/foostubs.c"
    }
  ]
  $ rm compile_commands.json

Check that it works in a subdirectory
  $ dune rules | (cd dir && dune-compiledb)
  $ tail -n4 dir/compile_commands.json
      "directory": "$TESTCASE_ROOT/workspace/test/_build/default/dir",
      "file": "foostubs.c"
    }
  ]
  $ rm dir/compile_commands.json
