#!/usr/bin/env bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -x
set -e

ROOT="$(mktemp -d)"
pushd "${ROOT}"

# See https://opam.ocaml.org/doc/Install.html#Binary-distribution
wget https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh
# The installation script is interactive, press "enter" (not "y") to accept default choices.
yes '' | sh install.sh

popd
