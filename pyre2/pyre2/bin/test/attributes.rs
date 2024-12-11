/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::simple_test;

simple_test!(
    test_set_attribute,
    r#"
class A:
    x: int
def f(a: A):
    a.x = 1  # OK
    a.x = "oops"  # E: EXPECTED Literal['oops'] <: int
    "#,
);

simple_test!(
    test_self_attribute_unannotated,
    r#"
from typing import assert_type
class A:
    def __init__(self, x: int):
        self.x = x
def f(a: A):
    assert_type(a.x, int)
    "#,
);

simple_test!(
    test_self_attribute_assign_twice,
    r#"
from typing import assert_type
class A:
    def f(self, x: str):
        self.x = x  # E: EXPECTED str <: int
    def __init__(self, x: int):
        self.x = x
    "#,
);

simple_test!(
    test_annotating_non_self_attributes,
    r#"
class A:
    x: int

class B:
    def __init__(self, a: A):
        a.x: int = 1  # E: Type cannot be declared in assignment to non-self attribute `a.x`

a: A
a.x: int = 5  # E: Type cannot be declared in assignment to non-self attribute `a.x`
    "#,
);

simple_test!(
    test_self_attribute_annotated_in_class_body,
    r#"
from typing import assert_type
class A:
    x: str
    def __init__(self, x: int):
        self.x = x  # E: EXPECTED int <: str
    "#,
);

simple_test!(
    test_self_attribute_annotated_assignment,
    r#"
from typing import assert_type

class A:
    def __init__(self, x: str):
        self.x: int = x  # E: EXPECTED str <: int
def f(a: A):
    assert_type(a.x, int)
    "#,
);

simple_test!(
    test_self_attribute_annotated_twice,
    r#"
from typing import assert_type, Literal
class A:
    x: int
    def __init__(self):
        self.x: Literal[1] = 1  # E: Inconsistent type annotations for x: Literal[1], int
def f(a: A):
    assert_type(a.x, int)
    "#,
);

simple_test!(
    test_self_attribute_qualified,
    r#"
from typing import assert_type, Final, Literal
class A:
    def __init__(self):
        self.x: Final = 0
def f(a: A):
    assert_type(a.x, Literal[0])
    "#,
);

simple_test!(
    test_self_attribute_bare_annotation,
    r#"
from typing import assert_type
class A:
    def __init__(self, x: str):
        self.x: int
        self.x = x  # E: EXPECTED str <: int
def f(a: A):
    assert_type(a.x, int)
    "#,
);

simple_test!(
    test_attribute_inference,
    r#"
class C:
    x: list[int | str]
def f(c: C):
    c.x = [5]
    "#,
);

simple_test!(
    test_set_attribute_in_init_nested,
    r#"
from typing import assert_type, Literal
class C:
    def __init__(self):
        def f():
            self.x = 0
        f()
def f(c: C):
    assert_type(c.x, Literal[0])
    "#,
);

// TODO: Should `C.x` exist?
simple_test!(
    test_set_attribute_in_init_indirect,
    r#"
class C:
    def __init__(self):
        self.f()
    def f(self):
        self.x = 0  # E: Object of class `C` has no attribute `x`
def f(c: C):
    return c.x  # E: Object of class `C` has no attribute `x`
    "#,
);
