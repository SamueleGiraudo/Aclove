# Aclove
`/\<<|_`

A programming language based on combinatory logic and term rewrite systems, with an advanced
type system.

Copyright (C) 2020--2023 [Samuele Giraudo](https://igm.univ-mlv.fr/~giraudo/) -
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

Aclove files have `.acl` as extension. Given such a file `Program.acl`, the command

TODO



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

