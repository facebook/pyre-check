/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_typed_dict,
    r#"
from typing import TypedDict
class Coord(TypedDict):
    x: int
    y: int
    "#,
);
