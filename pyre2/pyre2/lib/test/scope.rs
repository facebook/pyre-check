/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

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

testcase!(
    test_global,
    r#"
def f1():
  global x
x: str = ""
global x  # E: `x` was assigned in the current scope before the global declaration
global a  # E: Could not find name `a`
def f2() -> None:
  del x  # E: `x` is not mutable from the current scope
def f3() -> None:
  global x
  del x  # OK
def f4() -> None:
  global x
  x = 1  # E: `Literal[1]` is not assignable to variable `x` with type `str`
def f5() -> None:
  x = 1  # OK, this is a new x
def f6() -> None:
  global x
  def f7() -> None:
    del x   # E: `x` is not mutable from the current scope
def f8() -> None:
  y: int = 1
  global y  # E: `y` was assigned in the current scope before the global declaration
  def f9():
    global y  # E: Found `y`, but it was not the global scope
"#,
);

testcase!(
    test_nonlocal,
    r#"
a: str = ""
nonlocal a  # E: `a` was assigned in the current scope before the nonlocal declaration
nonlocal b  # E: Could not find name `b`
def f1() -> None:
  nonlocal b  # E: Could not find name `b`
  def f9():
    nonlocal x
  x: str = "John"
  def f2() -> None:
    del x  # E: `x` is not mutable from the current scope
  def f3() -> None:
    nonlocal x
    del x  # OK
  def f4() -> None:
    nonlocal x
    x = 1  # E: `Literal[1]` is not assignable to variable `x` with type `str`
  def f5() -> None:
    x = 1  # OK, this is a new x
  def f6() -> None:
    nonlocal x
    def f7() -> None:
      del x   # E: `x` is not mutable from the current scope
  def f8() -> None:
    y: int = 1
    nonlocal y  # E: `y` was assigned in the current scope before the nonlocal declaration
  def f10(c: str) -> None:
    def f11() -> None:
      nonlocal c
      c = 1  # E: `Literal[1]` is not assignable to variable `c` with type `str`
  def f12(c: str) -> None:
    def f13() -> None:
      c = 1  # OK, this is a new c
  def f14() -> None:
    nonlocal x
    x: int = 1  # E: Inconsistent type annotations for x: int, str
  nonlocal a  # E: Found `a`, but it was not in a valid enclosing scope
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
