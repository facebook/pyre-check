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
use crate::testcase_with_bug;

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
assert_type(MyEnum.X.name, Literal["X"])
assert_type(MyEnum.X._name_, Literal["X"])
assert_type(MyEnum.X.value, Literal[1])
assert_type(MyEnum.X._value_, Literal[1])
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
    test_enum_functional,
    r#"
from typing import assert_type, Literal
from enum import Enum

Color5 = Enum('Color5', 'RED, GREEN, BLUE')
Color6 = Enum('Color6', 'RED GREEN BLUE')

assert_type(Color5.RED, Literal[Color5.RED])
assert_type(Color6.RED, Literal[Color6.RED])
"#,
);

// TODO(stroxler) This test case involves a metaclass method that uses a `self` annotation
// to tie the output type to the class object being called on.
//
// At one point we had a special code path that could handle exactly this one case, but
// there were structural issues with that logic. The actual solution - which is mandatory
// to handle other cases like overloads that use `self` annotations - is to rework method
// types and method calls so that binding is handled at call time.
//
// This test should be fixed as part of making that work in a general way.
testcase_with_bug!(
    "Call resolution for metaclasses does not play well with the constraint solver, we need better method types to fix.",
    test_iterate,
    r#"
from typing import assert_type
from enum import Enum
class E(Enum):
    X = 1
    Y = 2
for e in E:  # E: EXPECTED EnumMeta <: type[@_]
    assert_type(e, E)  # E: assert_type(Any, E)
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

testcase!(
    test_generic_enum,
    r#"
from typing import assert_type, Literal
from enum import Enum
class E[T](Enum):  # E: Enums may not be generic
    X = 1
# Even though a generic enum is an error, we still want to handle it gracefully.
assert_type(E.X, Literal[E.X])
    "#,
);
