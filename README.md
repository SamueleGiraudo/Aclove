# Aclove
`/\<<|_`

A programming language based on combinatory logic and term rewrite systems, with an advanced
type system.

Copyright (C) 2020--2024 [Samuele Giraudo](https://igm.univ-mlv.fr/~giraudo/) -
`giraudo.samuele@uqam.ca` -


## Quick overview
TODO


## First examples
TODO


## Versions
Here is the [changelog](Versions.md) of the different versions.


### Dependencies
The following programs or libraries are needed:

+ `pkg-config`
+ `make`
+ `ocaml` (Version `>= 5.0.0`. An inferior but not too old version may be suitable.)
+ `opam`
+ `ocamlbuild` (Available by `opam install ocamlbuild`.)
+ `ocamlfind` (Available by `opam install ocamlfind`.)
+ `extlib` (Available by `opam install extlib`.)
+ `zarith` (Available by `opam install zarith`.)
+ `menhir` (Available by `opam install menhir`.)


### Building
Here are the required steps to build the interpreter `aclove`:

1. Clone the repository somewhere by running
   `git clone https://github.com/SamueleGiraudo/Aclove.git`.

2. Install all dependencies (see the section above).

3. Build the project by running `make noassert`.

This creates an executable `aclove`. The following sections explain how to use it.


## User guide
This [page](Help.md) contains the description of the Aclove language.

Aclove program files must have `.acl` as extension. The main command is

```
./aclove [--help] [--version] --file PATH [--verbose LVL] [--no-rules] [--no-shadows] [--short-names]
```

where

+ `--help` prints the short help.
+ `--version` prints the version and other information.
+ `--file PATH` sets `PATH` as the path to the Aclove program to consider.
+ `--verbose LVL` enables the verbose mode at level LVL from 0 (nothing) to 2 (full). By default, the level is 1.
+ `--no-rules` drops the rules of the result expression.
+ `--no-shadows` drops the shadows (that are, types) of the result expression.
+ `--short-names` keeps only the last part of the variable and constant names of the result expression.


### Standard library
The [standard library](Stdlib) contains some useful definitions.


### Documentation of the standard library
TODO


## Miscellaneous
To get the syntax highlighting in the text editor `vim` for the Aclove language, put the
file [acl.vim](Vim/syntax/acl.vim) at `~/.vim/syntax/acl.vim` and the file
[acl.vim](Vim/ftdetect/acl.vim) at `~/.vim/fdetect/acl.vim`.


## Theoretical aspects
TODO


### Bibliography
TODO

