#!/bin/bash

# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

set -euxo pipefail

if ! command -v opam &> /dev/null
then
    echo "opam is not installed, which is a dependency for building targets in ocaml."
    exit
fi

set +u
if [ -z "$OPAM_SWITCH_PREFIX" ]; then
    echo "OPAM_SWITCH_PREFIX is undefined. First execute \`eval (\$opam env)\` and then try running $0 again."
    exit
fi
set -u

# Link 'third-party/ocaml/opam'.
if [ ! -L shim/third-party/ocaml/opam ]; then
  (cd shim/third-party/ocaml && ln -s "$OPAM_SWITCH_PREFIX" opam)
else
    echo "Link 'shim/third-party/ocaml/opam' exists. To overwrite it, first remove it and run $0 again"
fi
