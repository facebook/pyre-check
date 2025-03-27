#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This should be a singular script which does everything needed to start the pyre2 website locally. By Default
# The website will be accessible on localhost:3000.
# This script currently:
# 1) runs the wasm build script in the pyre2_wasm folder, we expect the wasm file to get copied over to the website folder
# 2) build and serve the website locally (done through yarn start)
#
# If there are more steps needed to building the Pyre2 website, they should be included here.
#
../pyre2_wasm/build.sh
echo "copying wasm files from pyre2_wasm/ to website/"
cp ../pyre2_wasm/target/pyre2_wasm.js src/try-pyre2/pyre2_wasm.js
cp ../pyre2_wasm/target/pyre2_wasm_bg.wasm.opt src/try-pyre2/pyre2_wasm_bg.wasm
echo "finished copying wasm files"
yarn build
