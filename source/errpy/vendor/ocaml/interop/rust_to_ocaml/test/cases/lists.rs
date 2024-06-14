// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

pub type MyVec = Vec<X>;
pub type BoxedSlice = Box<[X]>;
pub type Slice<'a> = &'a [X];

pub type StdVec = std::vec::Vec<X>;
pub type StdBoxedSlice = std::boxed::Box<[X]>;
