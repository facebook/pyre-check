// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

pub type A = pos::Pos;

pub type B = crate::relative_path::RelativePath;

pub type C = collections::s_set::SSet;

pub type TypeNameMatchesModuleName = D;

pub mod foo {
    pub type Foo = E;
    pub type Maybe = Option<Foo>;
}
