/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;

pub const AENTER: Name = Name::new_static("__aenter__");
pub const AEXIT: Name = Name::new_static("__aexit__");
pub const ENTER: Name = Name::new_static("__enter__");
pub const EXIT: Name = Name::new_static("__exit__");
pub const GETITEM: Name = Name::new_static("__getitem__");
pub const INIT: Name = Name::new_static("__init__");
pub const ITER: Name = Name::new_static("__iter__");
pub const NEXT: Name = Name::new_static("__next__");
pub const SETITEM: Name = Name::new_static("__setitem__");
