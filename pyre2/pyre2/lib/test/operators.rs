/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_binop,
    r#"
from typing import assert_type
def f(a: int, b: int) -> None:
    c = a + b
    assert_type(c, int)
    d = a + 1
    assert_type(d, int)
"#,
);

testcase!(
    test_negative_literals,
    r#"
from typing import Literal
x: Literal[-1] = -1
"#,
);

testcase!(
    test_positive_literals,
    r#"
from typing import Literal
y: int = -1
x: Literal[-1] = +y
"#,
);
testcase!(
    test_positive_literals_error,
    r#"
from typing import Literal
y: int = 1
x: Literal[-1] = +y # E: `Literal[1]` is not assignable to `Literal[-1]`
"#,
);

testcase!(
    test_inversion_literals,
    r#"
from typing import Literal
y: int = -1
x: Literal[0] = ~y
"#,
);

testcase!(
    test_inversion_literals_error,
    r#"
from typing import Literal
y: int = -2
x: Literal[0] = ~y # E: `Literal[1]` is not assignable to `Literal[0]`
"#,
);

testcase!(
    test_union_unary_op,
    r#"
from typing import Literal, Union, assert_type
x: Union[Literal[-1], Literal[2]] = 2
y = -x
assert_type(y, Literal[-2])
"#,
);

testcase!(
    test_boolean_or_simple,
    r#"
from typing import assert_type
def f(x: int, y: str) -> None:
    z = x or y
    assert_type(z, int | str)
    "#,
);

testcase!(
    test_boolean_or_filter,
    r#"
from typing import assert_type, Literal

x = False or True
assert_type(x, Literal[True])

y = False or None
assert_type(y, None)

def f(a: None, b: int, c: str, cond: bool) -> None:
    if cond:
        b2 = a
    else:
        b2 = b
    d = b2 or c
    assert_type(d, int | str)
    "#,
);

testcase!(
    test_boolean_or_shortcircuit,
    r#"
from typing import assert_type, Literal
x = True or False
assert_type(x, Literal[True])
    "#,
);

testcase!(
    test_integer_or_shortcircuit,
    r#"
from typing import assert_type, Literal
x = 1 or 0
assert_type(x, Literal[1])
    "#,
);

testcase!(
    test_string_or_shortcircuit,
    r#"
from typing import assert_type, Literal
x = "a" or ""
assert_type(x, Literal["a"])
    "#,
);

testcase!(
    test_boolean_and_simple,
    r#"
from typing import assert_type, Literal

def f(x: int, y: str) -> None:
    z = x and y
    assert_type(z, Literal[0] | str)
    "#,
);

testcase!(
    test_boolean_and_filter,
    r#"
from typing import assert_type, Literal
x = True and False
assert_type(x, Literal[False])

y = True and True
assert_type(y, Literal[True])
    "#,
);

testcase!(
    test_boolean_and_shortcircuit,
    r#"
from typing import assert_type, Literal
x = False and True
assert_type(x, Literal[False])
    "#,
);

testcase!(
    test_integer_and_shortcircuit,
    r#"
from typing import assert_type, Literal
x = 0 and 1
assert_type(x, Literal[0])
    "#,
);

testcase!(
    test_string_and_shortcircuit,
    r#"
from typing import assert_type, Literal
x = "" and "a"
assert_type(x, Literal[""])
    "#,
);

testcase!(
    bug = "Should narrow",
    test_boolean_operator_narrow,
    r#"
from typing import assert_type, Literal

def f(x: bool, y: int):
    assert_type(x and y, Literal[False] | int)

def g(x: bool, y: Literal['a'], z: Literal['b']):
    assert_type((x and y) or z, Literal['a', 'b'])

def h(x: int, y: str, z: bool, v: float):
    assert_type(x or y or z or v, int | float | str | Literal[True])
    assert_type(x and y and z and v, Literal[0, "", False] | float)
"#,
);

testcase!(
    test_boolean_union,
    r#"
from typing import assert_type, Literal

def f(x: int, y: str | Literal[False]):
    assert_type(x or y, int | str | Literal[False])
"#,
);

testcase!(
    test_unary_not_unknown,
    r#"
from typing import assert_type
def f(x):
    y = not x
    assert_type(y, bool)
    "#,
);

testcase!(
    test_unary_not_literal,
    r#"
from typing import assert_type, Literal

x1 = True
y1 = not x1
assert_type(y1, Literal[False])

x2 = False
y2 = not x2
assert_type(y2, Literal[True])

x3 = 1
y3 = not x3
assert_type(y3, Literal[False])

x4 = 0
y4 = not x4
assert_type(y4, Literal[True])

x5 = "a"
y5 = not x5
assert_type(y5, Literal[False])

x6 = ""
y6 = not x6
assert_type(y6, Literal[True])
    "#,
);

testcase!(
    test_unary_dunders,
    r#"
from typing import Literal, assert_type
class C:
    def __pos__(self) -> Literal[5]:
        return 5
    def __neg__(self) -> Literal[-5]:
        return -5
    def __invert__(self) -> Literal[100]:
        return 100
c = C()
assert_type(+c, Literal[5])
assert_type(-c, Literal[-5])
assert_type(~c, Literal[100])
    "#,
);

testcase!(
    test_unary_error,
    r#"
+None  # E: Unary `+` is not supported on `None`
+"oh no"  # E: Unary `+` is not supported on `Literal['oh no']`
-"oops"  # E: Unary `-` is not supported on `Literal['oops']`
class A:
    def __invert__(self, extra_arg):
        pass
~A()  # E: Unary `~` is not supported on `A`\n  Missing argument `extra_arg` in function `A.__invert__`
    "#,
);

testcase!(
    test_unary_enum,
    r#"
from enum import Enum
class A(Enum):
    X = 1
class B(Enum):
    X = 1
    def __pos__(self):
        return 0
+A.X  # E: Unary `+` is not supported on `Literal[A.X]`
+B.X  # OK
    "#,
);

testcase!(
    test_operator_error,
    r#"
class C: pass

x = C() + 1  # E:  `+` is not supported between `C` and `Literal[1]`"#,
);

testcase!(
    test_float_int_add,
    r#"
from typing import assert_type
x = 3 + 3.0
assert_type(x, float)
"#,
);

testcase!(
    test_float_int_compare,
    r#"
0 < 1.0
1.0 < 2
1 < 2
1.0 < 3.0
"#,
);

testcase!(
    test_inplace_operator_rhs_union,
    r#"
class A:
    def __radd__(self, other: int) -> int:
        return other
def f(x: int, y: int | A) -> None:
    x += y
    "#,
);

testcase!(
    test_unop_on_any,
    r#"
from typing import Any, assert_type
def f(x: Any):
    assert_type(-x, Any)
    "#,
);

testcase!(
    test_comparison_return_type,
    r#"
from typing import Literal, assert_type
class A:
  def __lt__(self, other):
    return 1
assert_type(A() < A(), Literal[1])
    "#,
);
