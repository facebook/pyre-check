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

testcase!(
    test_enum_meta,
    r#"
from typing import assert_type, Literal
from enum import EnumMeta

class CustomEnumType(EnumMeta):
    pass

class CustomEnum(metaclass=CustomEnumType):
    pass

class Color(CustomEnum):
    RED = 1
    GREEN = 2
    BLUE = 3

assert_type(Color.RED, Literal[Color.RED])
"#,
);

testcase!(
    test_iterate,
    r#"
from typing import assert_type
from enum import Enum
class E(Enum):
    X = 1
    Y = 2
for e in E:
    assert_type(e, E)
    "#,
);
