#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

cd "$(dirname "$0")"
export RUSTFLAGS='--cfg getrandom_backend="wasm_js"'
wasm-pack build --out-dir target -t web --no-typescript
cp target/pyre2_wasm.js ../website/src/try-pyre2/pyre2_wasm.js
cp target/pyre2_wasm_bg.wasm ../website/src/try-pyre2/pyre2_wasm_bg.wasm
