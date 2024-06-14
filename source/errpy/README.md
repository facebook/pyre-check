[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# Errpy: An Error Recovering Rust Python Parser

This is a new project to provide an error-recovering Python parser, implemented
in Rust based on tree-sitter. Our short-term goal is to use it in the [pyre
type checker](https://github.com/facebook/pyre-check), in order to provide
better IDE features.

## License

Errpy is licensed under the MIT license.

## Building
Errpy depends upon the following:
 * Ocaml
 * Dune
 * Rust
 * Cargo

### Platform
ERRPY is verified as being buildable on Linux on OCaml 4.14.0

### Installing Ocaml and Dune
Install `opam` and run `opam switch install "4.14.0"`

## Usage
It is recommended to use dune as your build system. To use errpy in your dune project, you can add errpy to the libraries stanza in your dune file. For example,

```
(library
  (name mylib)
  (libraries errpy))
```

### For local development
If you want to test your changes to errpy locally and use them in another OCaml project (.e.g. Pyre) you can try the following,

First clone the repo with: `$ git clone https://github.com/facebook/errpy.git`

Or use `$ git clone --branch release https://github.com/facebook/errpy.git` if you're operating within an `offline` rust enviroment.

```
... make the required changed ...
$ cd errpy
$ dune build @install          # Build the errpy library
$ dune test                    # Run tests
$ opam pin add errpy . -n      # pin opam to errpy
$ opam install errpy --verbose # install local build as errpy in opam
```

## Formatting
When making Ocaml changes you can run `ocamlformat` in order to ensure code is
formatted in a conistent manner. This is invoked as follows:
```
$ dune build @fmt
$ dune promote
```
