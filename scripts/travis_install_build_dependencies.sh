#!/usr/bin/env bash

set -x
set -e

ROOT="$(mktemp -d)"
pushd "${ROOT}"

# See https://opam.ocaml.org/doc/Install.html#Binary-distribution
wget https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh
# The installation script is interactive, press "enter" a few times to accept default choices.
printf "\n\n\n" | sh install.sh

popd
