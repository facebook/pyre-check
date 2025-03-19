/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_method_cannot_see_class_scope,
    r#"
class C:
    x: int

    def m(self) -> None:
        x  # E: Could not find name `x`
"#,
);

testcase!(
    test_more_class_scope,
    r#"
x: int = 0
class C:
    x: str = x # E: `Literal[0]` is not assignable to `str`
    y: int = x # E: `str` is not assignable to `int`
    def m(self) -> str:
        # x refers to global x: int
        return x # E: Returned type `Literal[0]` is not assignable to declared return type `str`
"#,
);

testcase_with_bug!(
    "Need scoping changes to handle nonlocal",
    test_nonlocal,
    r#"
def f1() -> None:
  x: str = "John"
  def f2() -> None:
    del x  # E: `x` is not mutable from the current scope
  def f3() -> None:
    nonlocal x  # E: TODO: StmtNonlocal
    del x  # OK  # E: `x` is not mutable from the current scope
  def f4() -> None:
    nonlocal x  # E: TODO: StmtNonlocal
    x = 1  # Not OK
  def f5() -> None:
    x = 1  # OK, this is a new x
"#,
);

testcase!(
    test_unbound_name,
    r#"
x: int
x + 1  # E: `x` is uninitialized
x = 1
x + 1  # OK
del x
x + 1  # E: `x` is unbound

y = 1
y + 1  # OK
del y
y + 1  # E: `y` is unbound

# check that we don't fall back to Any when the variable is annotated
z: int
z = str(z)  # E: `z` is uninitialized  # E: `str` is not assignable to variable `z` with type `int`
"#,
);

testcase!(
    test_unbound_merge_flow,
    r#"
def test(cond: bool):
    if cond:
        a: int
    else:
        a = 1
    a  # E: `a` may be uninitialized
    if cond:
        b: int
    else:
        b = 1
        del b
    b  # E: `b` is uninitialized
    if cond:
        c: int
    else:
        c: int = 1
    c  # E: `c` may be uninitialized
    if cond:
        d = 1
    else:
        d = 1
        del d
    d  # E: `d` may be unbound
    if cond:
        e = 1
    else:
        e: int
    e  # E: `e` may be uninitialized
    if cond:
        f = 1
        del f
    else:
        f: int
    f  # E: `f` is uninitialized
    if cond:
        g = 1
        del g
    else:
        g = 1
    g  # E: `g` may be unbound
    if cond:
        h = 1
        del h
    else:
        h = 1
        del h
    h  # E: `h` is unbound
"#,
);
