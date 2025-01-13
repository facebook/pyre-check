/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_if_simple,
    r#"
from typing import assert_type, Literal
def b() -> bool:
    return True
if b():
    x = 100
else:
    x = "test"
y = x
assert_type(y, Literal['test', 100])
"#,
);

testcase!(
    test_if_else,
    r#"
from typing import assert_type, Literal
def b() -> bool:
    return True
if b():
    x = 100
elif b():
    x = "test"
else:
    x = True
y = x
assert_type(y, Literal['test', 100, True])
"#,
);

testcase!(
    test_if_only,
    r#"
from typing import assert_type, Literal
def b() -> bool:
    return True
x = 7
if b():
    x = 100
y = x
assert_type(y, Literal[7, 100])
"#,
);

testcase!(
    test_while_simple,
    r#"
from typing import assert_type, Literal
def f(condition) -> None:
    x = None
    while condition():
        assert_type(x, Literal["hello world"] | None)
        x = "hello world"
        assert_type(x, Literal["hello world"])
    assert_type(x, Literal["hello world"] | None)
    "#,
);

testcase!(
    test_while_infinite,
    r#"
from typing import assert_type, Any, Literal
def f(condition) -> None:
    x = 1
    while condition():
        assert_type(x, Literal[1] | list[int | Any])
        x = [x]
        assert_type(x, list[int | list[int | Any]])
    assert_type(x, Literal[1] | list[int | Any])
    "#,
);

testcase!(
    test_while_noop,
    r#"
from typing import assert_type, Literal
def f(condition) -> None:
    x = 1
    while condition():
        pass
    assert_type(x, Literal[1])
    "#,
);

testcase!(
    test_while_fancy_noop,
    r#"
from typing import assert_type, Any, Literal
def f(condition) -> None:
    x = 1
    while condition():
        x = x
    assert_type(x, Literal[1])
    "#,
);

testcase!(
    test_while_if,
    r#"
from typing import assert_type, Any, Literal
def f(condition1, condition2) -> None:
    x = None
    while condition1():
        if condition2():
            x = "hello"
    assert_type(x, Literal['hello'] | None)
    "#,
);

testcase!(
    test_while_two_vars,
    r#"
from typing import assert_type, Any, Literal
def f(cond1, cond2, cond3) -> None:
    x = 1
    y = ""
    while cond1():
        if cond2():
            x = y
        if cond3():
            y = x
    assert_type(x, Literal["", 1])
    assert_type(y, Literal["", 1])
    "#,
);

testcase!(
    test_while_else,
    r#"
from typing import assert_type, Literal
def f(condition) -> None:
    x = None
    while condition():
        x = 1
    else:
        x = ""
    assert_type(x, Literal[""])
    "#,
);

testcase!(
    test_while_break_else,
    r#"
from typing import assert_type, Any, Literal
def f(cond1, cond2) -> None:
    x = None
    while cond1():
        if cond2():
            x = "value"
            break
        else:
            x = "overwritten"
    else:
        assert_type(x, Literal["overwritten"] | None)
        x = "default"
    assert_type(x, Literal["default", "value"])
    "#,
);

testcase!(
    test_while_else_while,
    r#"
while False:
    x = 0
else:
    while False:
        x = 1
    "#,
);

testcase!(
    test_while_reassignment_with_annotation,
    r#"
from typing import assert_type, Literal
def f(cond):
    x: int = 0
    while cond():
        x: int = 1
    assert_type(x, Literal[0, 1])
    "#,
);

testcase!(
    test_for_simple,
    r#"
from typing import assert_type
def f(x: list[int]) -> None:
    for i in x:
        assert_type(i, int)
    assert_type(i, int)
    "#,
);

testcase!(
    test_for_tuple,
    r#"
from typing import assert_type
def f(x: tuple[int, str]) -> None:
    for i in x:
        assert_type(i, int | str)
    "#,
);

testcase!(
    test_for_literal_string,
    r#"
from typing import assert_type
for i in "abcd":
    assert_type(i, str)
    "#,
);

testcase!(
    test_for_any,
    r#"
from typing import Any, assert_type
def f(x: Any):
    for i in x:
        assert_type(i, Any)
    "#,
);

testcase!(
    test_for_reassign,
    r#"
from typing import assert_type
def f(x: list[int]):
    y = None
    for i in x:
        y = i
    assert_type(y, int | None)
    "#,
);

testcase!(
    test_for_else_reassign,
    r#"
from typing import assert_type, Literal
def f(x: list[int]):
    y = None
    for i in x:
        y = i
    else:
        y = 'done'
    assert_type(y, Literal['done'])
    "#,
);

testcase!(
    test_for_multiple_targets,
    r#"
from typing import assert_type
def f(x: list[tuple[int, str]]) -> None:
    for (i, j) in x:
        assert_type(i, int)
        assert_type(j, str)
    "#,
);

testcase!(
    test_for_scope,
    r#"
from typing import assert_type
def f(x: list[int]) -> None:
    for i in x:
        pass
    assert_type(i, int)
    "#,
);

testcase!(
    test_for_target_annot_compatible,
    r#"
def f(x: list[int]) -> None:
    i: int = 0
    for i in x:
        pass
    "#,
);

testcase!(
    test_for_target_annot_incompatible,
    r#"
def f(x: list[int]) -> None:
    i: str = ""
    for i in x: # E: EXPECTED list[int] <: Iterable[str]
        pass
    "#,
);

testcase!(
    test_listcomp_simple,
    r#"
from typing import assert_type
y = [x for x in [1, 2, 3]]
assert_type(y, list[int])
    "#,
);

testcase!(
    test_listcomp_no_leak,
    r#"
def f():
    y = [x for x in [1, 2, 3]]
    return x  # E: Could not find name `x`
    "#,
);

testcase!(
    test_listcomp_no_overwrite,
    r#"
from typing import assert_type
x = None
y = [x for x in [1, 2, 3]]
assert_type(x, None)
    "#,
);

testcase!(
    test_listcomp_read_from_outer_scope,
    r#"
from typing import assert_type
x = None
y = [x for _ in [1, 2, 3]]
assert_type(y, list[None])
    "#,
);

testcase!(
    test_listcomp_iter_error,
    r#"
class C:
    pass
[None for x in C.error]  # E: Class `C` has no class attribute `error`
    "#,
);

testcase!(
    test_listcomp_if_error,
    r#"
class C:
    pass
def f(x):
    [None for y in x if C.error]  # E: Class `C` has no class attribute `error`
    "#,
);

testcase!(
    test_listcomp_target_error,
    r#"
def f(x: list[tuple[int]]):
    [None for (y, z) in x]  # E: Cannot unpack
    "#,
);

testcase!(
    test_listcomp_splat,
    r#"
from typing import assert_type
def f(x: list[tuple[int, str, bool]]):
    z = [y for (_, *y) in x]
    assert_type(z, list[list[bool | str]])
    "#,
);

testcase!(
    test_setcomp,
    r#"
from typing import assert_type
y = {x for x in [1, 2, 3]}
assert_type(y, set[int])
    "#,
);

testcase!(
    test_dictcomp,
    r#"
from typing import assert_type
def f(x: list[tuple[str, int]]):
    d = {y: z for (y, z) in x}
    assert_type(d, dict[str, int])
    "#,
);

testcase!(
    test_generator,
    r#"
from typing import assert_type, Generator
y = (x for x in [1, 2, 3])
assert_type(y, Generator[int, None, None])
    "#,
);

testcase!(
    test_bad_loop_command,
    r#"
break  # E: Cannot `break` outside loop
continue  # E: Cannot `continue` outside loop
    "#,
);

testcase!(
    test_break,
    r#"
from typing import assert_type, Literal
def f(cond):
    x = None
    for i in [1, 2, 3]:
        x = i
        if cond():
            break
        x = "hello world"
    assert_type(x, Literal["hello world"] | int | None)
    "#,
);

testcase!(
    test_continue,
    r#"
from typing import assert_type, Literal
def f(cond1, cond2):
    x = None
    while cond1():
        x = 1
        if cond2():
            x = 2
            continue
        assert_type(x, Literal[1])
        x = "hello world"
    assert_type(x, Literal["hello world", 2] | None)
    "#,
);

testcase!(
    test_early_return,
    r#"
from typing import assert_type, Literal
def f(x):
    if x:
        y = 1
        return
    else:
        y = "2"
    assert_type(y, Literal["2"])
    "#,
);

testcase!(
    test_return_in_for,
    r#"
def f(x: str):
    for c in x:
        return
    "#,
);

testcase!(
    test_flow_scope_type,
    r#"
from typing import assert_type

# C itself is in scope, which means it ends up bound to a Phi
# which can cause confusion as both a type and a value
class C: pass

c = C()

while True:
    if True:
        c = C()

assert_type(c, C)
    "#,
);

testcase!(
    test_flow_crash,
    r#"
def test():
    while False:
        if False:
            x: int
        else:
            x: int
            if False:
                continue
"#,
);

testcase!(
    test_flow_crash2,
    r#"
def magic_breakage(argument):
    for it in []:
        continue
        break
    else:
        raise
"#,
);

testcase!(
    test_try,
    r#"
from typing import assert_type, Literal

try:
    x = 1
except:
    x = 2

assert_type(x, Literal[1, 2])
"#,
);

testcase!(
    test_exception_handler,
    r#"
from typing import reveal_type

class Exception1(Exception): pass
class Exception2(Exception): pass

x1: tuple[type[Exception], ...] = (Exception1, Exception2)
x2 = (Exception1, Exception2)

try:
    pass
except int as e1:  # E: EXPECTED int <: BaseException
    reveal_type(e1)  # E: revealed type: int
except int:  # E: EXPECTED int <: BaseException
    pass
except Exception as e2:
    reveal_type(e2)  # E: revealed type: Exception
except ExceptionGroup as e3:
    reveal_type(e3)  # E: revealed type: ExceptionGroup[Error]
except (Exception1, Exception2) as e4:
    reveal_type(e4)  # E: revealed type: Exception1 | Exception2
except Exception1 as e5:
    reveal_type(e5)  # E: revealed type: Exception1
except x1 as e6:
    reveal_type(e6)  # E: revealed type: Exception
except x2 as e7:
    reveal_type(e6)  # E: revealed type: Exception1 | Exception2
"#,
);

testcase!(
    test_exception_group_handler,
    r#"
from typing import reveal_type

class Exception1(Exception): pass
class Exception2(Exception): pass

try:
    pass
except* int as e1:  # E: EXPECTED int <: BaseException
    reveal_type(e1)  # E: revealed type: ExceptionGroup[int]
except* Exception as e2:
    reveal_type(e2)  # E: revealed type: ExceptionGroup[Exception]
except* ExceptionGroup as e3:  # E: Exception handler annotation in `except*` clause may not extend `BaseExceptionGroup`
    reveal_type(e3)  # E: ExceptionGroup[ExceptionGroup[Error]]
except* (Exception1, Exception2) as e4:
    reveal_type(e4)  # E: ExceptionGroup[Exception1 | Exception2]
except* Exception1 as e5:
    reveal_type(e5)  # E: ExceptionGroup[Exception1]
"#,
);

testcase!(
    test_try_else,
    r#"
from typing import assert_type, Literal

try:
    x = 1
except:
    x = 2
else:
    x = 3

assert_type(x, Literal[2, 3])
"#,
);

testcase!(
    test_try_finally,
    r#"
from typing import assert_type, Literal

try:
    x = 1
except:
    x = 2
finally:
    x = 3

assert_type(x, Literal[3])
"#,
);

testcase!(
    test_match,
    r#"
from typing import assert_type

def point() -> int:
    return 3

match point():
    case 1:
        x = 8
    case q:
        x = q
assert_type(x, int)  # E: assert_type(Literal[8] | int, int) failed
"#,
);

testcase!(
    test_match_narrow,
    r#"
from typing import assert_type, Literal

def foo(x: int):
    match x:
        case 1:
            assert_type(x, Literal[1])
        case 2 as q:
            assert_type(x, int)
            assert_type(q, Literal[2])
        case q:
            assert_type(x, int)
            assert_type(q, int)
"#,
);

testcase!(
    test_match_mapping,
    r#"
from typing import assert_type

x: dict[str, int] = { "a": 1, "b": 2, "c": 3 }

match x:
    case { "a": 1, "b": y, **c }:
        assert_type(y, int)
        assert_type(c, dict[str, int])
"#,
);

testcase!(
    test_empty_loop,
    r#"
# These generate syntax that is illegal, but reachable with parser error recovery

for x in []:
pass  # E: Expected an indented block

while True:
pass  # E: Expected an indented block
"#,
);

testcase!(
    test_match_class_narrow,
    r#"
from typing import assert_type

class A:
    x: int
    y: str
    __match_args__ = ("x", "y")

class B:
    x: int
    y: str
    __match_args__ = ("x", "y")

class C:
    x: int
    y: str
    __match_args__ = ("x", "y")

def fun(x: A | B | C) -> None:
    match x:
        case A(1, "a"):
            assert_type(x, A)
    match x:
        case B(2, "b"):
            assert_type(x, B)
    match x:
        case B(3, "B") as y:
            assert_type(x, A | B | C)
            assert_type(y, B)
    match x:
        case A(1, "a") | B(2, "b"):
            assert_type(x, A | B)
"#,
);

testcase!(
    test_match_class,
    r#"
from typing import assert_type

class Foo:
    x: int
    y: str
    __match_args__ = ("x", "y")

class Bar:
    x: int
    y: str

class Baz:
    x: int
    y: str
    __match_args__ = (1, 2)

def fun(foo: Foo, bar: Bar, baz: Baz) -> None:
    match foo:
        case Foo(1, "a"):
            pass
        case Foo(a, b):
            assert_type(a, int)
            assert_type(b, str)
        case Foo(x = b, y = a):
            assert_type(a, str)
            assert_type(b, int)
        case Foo(a, b, c):  # E: Index 2 out of range for `__match_args__`
            pass
    match bar:
        case Bar(1):  # E: Object of class `Bar` has no attribute `__match_args__`
            pass
        case Bar(a):  # E: Object of class `Bar` has no attribute `__match_args__`
            pass
        case Bar(x = a):
            assert_type(a, int)
    match baz:
        case Baz(1):  # E: Expected literal string in `__match_args__`
            pass
"#,
);

testcase!(
    test_match_sequence_concrete,
    r#"
from typing import assert_type, Never

def foo(x: tuple[int, str, bool, int]) -> None:
    match x:
        case [a, b, c, d]:
            assert_type(a, int)
            assert_type(b, str)
            assert_type(c, bool)
            assert_type(d, int)
        case [a, *rest]:
            assert_type(a, int)
            assert_type(rest, list[str | bool | int])
        case [a, *middle, b]:
            assert_type(a, int)
            assert_type(b, int)
            assert_type(middle, list[str | bool])
        case [a, b, c, d, e]:  # E: Cannot unpack tuple[int, str, bool, int] (of size 4) into 5 values
            pass
        case [a, b, *middle, c, d]:
            assert_type(a, int)
            assert_type(b, str)
            assert_type(c, bool)
            assert_type(d, int)
            assert_type(middle, list[Never])
        case [*first, c, d]:
            assert_type(first, list[int | str])
            assert_type(c, bool)
            assert_type(d, int)
"#,
);

testcase!(
    test_match_sequence_unbounded,
    r#"
from typing import assert_type, Never

def foo(x: list[int]) -> None:
    match x:
        case []:
            pass
        case [a]:
            assert_type(a, int)
        case [a, b, c]:
            assert_type(a, int)
            assert_type(b, int)
            assert_type(c, int)
        case [a, *rest]:
            assert_type(a, int)
            assert_type(rest, list[int])
        case [a, *middle, b]:
            assert_type(a, int)
            assert_type(b, int)
            assert_type(middle, list[int])
        case [*first, a]:
            assert_type(first, list[int])
            assert_type(a, int)
        case [*all]:
            assert_type(all, list[int])
"#,
);

testcase!(
    test_match_or,
    r#"
from typing import assert_type

x: list[int] = [1, 2, 3]

match x:
    case [a] | a:
        assert_type(a, list[int] | int)
    case [b] | _:  # E: Could not find flow binding for `b`
        assert_type(b, list[int] | int)

match x:
    case _ | _:  # E: Only the last subpattern in MatchOr may be irrefutable
        pass
"#,
);

testcase!(
    test_crashing_match,
    r#"
match []:
    case [[1]]:
        pass
    case _:
        pass
"#,
);
