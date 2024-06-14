// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

pub struct Foo(pub Bar, pub Bar);

#[rust_to_ocaml(and)]
pub struct Bar(pub Option<Foo>, pub Option<Foo>);
