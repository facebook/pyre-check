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
    let (handle, state) = mk_state(
        r#"
import enum
class E(enum.Enum):
    X = 1
    Y = 2
        "#,
    );
    let cls = get_class("E", &handle, &state).unwrap();
    let fields = cls
        .fields()
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
assert_type(MyEnum.__PRIVATE, int)
assert_type(MyEnum.X.name, Literal["X"])
assert_type(MyEnum.X._name_, Literal["X"])
assert_type(MyEnum.X.value, int)
assert_type(MyEnum.X._value_, int)
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

Color2 = Enum('Color2', 'RED', 'GREEN', 'BLUE')
Color3 = Enum('Color3', ['RED', 'GREEN', 'BLUE'])
Color4 = Enum('Color4', ('RED', 'GREEN', 'BLUE'))
Color5 = Enum('Color5', 'RED, GREEN, BLUE')
Color6 = Enum('Color6', 'RED GREEN BLUE')
Color7 = Enum('Color7', [('RED', 1), ('GREEN', 2), ('BLUE', 3)])
Color8 = Enum('Color8', (('RED', 1), ('GREEN', 2), ('BLUE', 3)))
Color9 = Enum('Color9', {'RED': 1, 'GREEN': 2, 'BLUE': 3})

assert_type(Color2.RED, Literal[Color2.RED])
assert_type(Color3.RED, Literal[Color3.RED])
assert_type(Color4.RED, Literal[Color4.RED])
assert_type(Color5.RED, Literal[Color5.RED])
assert_type(Color6.RED, Literal[Color6.RED])
assert_type(Color7.RED, Literal[Color7.RED])
assert_type(Color8.RED, Literal[Color8.RED])
assert_type(Color9.RED, Literal[Color9.RED])
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
for e in E:  # E: `type[E]` is not iterable
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
    test_enum_member,
    r#"
from enum import Enum, nonmember, member
from typing import reveal_type

class MyEnum(Enum):
    A = 1
    B = nonmember(2)
    @member
    def C(self) -> None: pass
    def D(self) -> None: pass

reveal_type(MyEnum.A)  # E: revealed type: Literal[MyEnum.A]
reveal_type(MyEnum.B)  # E: revealed type: nonmember[int]
reveal_type(MyEnum.C)  # E: revealed type: Literal[MyEnum.C]
reveal_type(MyEnum.D)  # E: revealed type: (self: MyEnum) -> None
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
    test_flag,
    r#"
from enum import Flag
from typing import assert_type

class MyFlag(Flag):
    X = 1
    Y = 2

def foo(f: MyFlag) -> None:
    if f == MyFlag.X:
        pass
    else:
        assert_type(f, MyFlag)
"#,
);

testcase!(
    test_enum_instance_only_attr,
    r#"
from typing import reveal_type
from enum import Enum

class MyEnum(Enum):
    Y: int

MyEnum.Y  # E: Instance-only attribute `Y` of class `MyEnum` is not visible on the class
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

testcase!(
    test_enum_name,
    r#"
from typing import assert_type, Literal
from enum import Enum
class E(Enum):
    X = 1
# Even though a generic enum is an error, we still want to handle it gracefully.
assert_type(E.X._name_, Literal["X"])
    "#,
);
