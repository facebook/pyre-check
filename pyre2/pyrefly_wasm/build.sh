#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

cd "$(dirname "$0")"
wasm-pack build --no-opt --out-dir target -t web --no-typescript

# wasm-pack can run wasm-opt, but it is buggy [1] and wasm-opt bundled by
# wasm-pack is outdated (slow, 30s vs 1s). So we disabled wasm-opt feature
# of wasm-pack in Cargo.toml. Assume wasm-opt is in PATH and is newer.
# [1]: https://github.com/rustwasm/wasm-pack/issues/1190
# [3]: https://github.com/WebAssembly/binaryen

wasm-opt target/pyrefly_wasm_bg.wasm -Os -o target/pyrefly_wasm_bg.wasm.opt
