/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use itertools::Itertools;

use crate::test::util::get_class;
use crate::test::util::mk_state;
use crate::testcase;

#[test]
fn test_fields() {
    let (module, state) = mk_state(
        r#"
import enum
class E(enum.Enum):
    X = 1
    Y = 2
        "#,
    );
    let cls = get_class("E", module, &state).unwrap();
    let fields = cls
        .fields()
        .iter()
        .map(|f| f.as_str())
        .sorted()
        .collect::<Vec<_>>();
    assert_eq!(fields, vec!["X", "Y"]);
}

testcase!(
    test_enum,
    r#"
from typing import assert_type, Literal
from enum import Enum

class MyEnum(Enum):
    X = 1
    Y = 2
    __PRIVATE = 3

assert_type(MyEnum.X, Literal[MyEnum.X])
assert_type(MyEnum.__PRIVATE, Literal[3])
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

testcase!(
    test_value_annotation,
    r#"
from enum import Enum

class MyEnum(Enum):
    _value_: int
    X = 1
    Y = "FOO"  # E: The value for enum member `Y` must match the annotation of the _value_ attribute.
"#,
);

testcase!(
    test_member_annotation,
    r#"
from enum import Enum

class MyEnum(Enum):
    X: int = 1  # E: Enum member `X` may not be annotated directly. Instead, annotate the _value_ attribute.
"#,
);
