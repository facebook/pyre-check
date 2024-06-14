// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

pub struct MyStruct {
    pub foo: isize,
    pub bar: isize,
}

#[rust_to_ocaml(prefix = "a_")]
pub struct StructA {
    pub foo: isize,
    pub bar: isize,
}

#[rust_to_ocaml(prefix = "b_")]
pub struct StructB {
    pub foo: isize,
    pub bar: isize,
}
