/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_enum,
    r#"
from typing import assert_type, Literal
from enum import Enum

class MyEnum(Enum):
    X = 1
    Y = 2

assert_type(MyEnum.X, Literal[MyEnum.X])
"#,
);
