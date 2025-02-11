/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_missing_return,
    r#"

def f() -> int:  # E: EXPECTED None <: int
    pass
"#,
);

testcase!(
    test_missing_return_none,
    r#"
def f() -> None:
    pass
"#,
);

testcase!(
    test_missing_return_implicit,
    r#"
from typing import assert_type

def f():
    pass
assert_type(f(), None)
"#,
);

testcase!(
    test_return_unions,
    r#"
from typing import assert_type, Literal

def f(b: bool):
    if b:
        return 1
    else:
        return "test"
assert_type(f(True), Literal['test', 1])
"#,
);

testcase!(
    test_return_some_return,
    r#"
from typing import assert_type

def f(b: bool) -> int:  # E: EXPECTED None <: int
    if b:
        return 1
    else:
        pass
"#,
);

testcase!(
    test_return_catch,
    r#"
def f(b: bool) -> int:
    try:
        return 1
    except Exception:
        return 2
"#,
);

testcase!(
    test_return_never,
    r#"
from typing import NoReturn

def fail() -> NoReturn:
    raise Exception()

def f(b: bool) -> int:
    if b:
        return 1
    else:
        fail()
"#,
);

testcase!(
    test_return_never_should_not_fail,
    r#"
from typing import NoReturn

def fail() -> NoReturn:
    raise Exception()

def f() -> int:
   fail()
"#,
);

testcase!(
    test_return_none_should_fail,
    r#"

def does_not_fail() -> None:
    return None

def f(b: bool) -> int: 
    if b:
        return 1
    else:
        does_not_fail() # E: Expr has type None but should have type int
"#,
);

testcase!(
    test_return_should_fail,
    r#"

def fail():
    pass

def f() -> int:
   fail() # E: Expr has type None but should have type int

"#,
);

testcase!(
    test_return_if_no_else_real,
    r#"
def f(b: bool) -> int:  # E: EXPECTED None <: int
    if b:
        return 1
"#,
);

testcase!(
    test_return_if_no_else_none,
    r#"
def f(b: bool) -> None:
    if b:
        return None
"#,
);

testcase!(
    test_return_then_dead_code,
    r#"
def f(b: bool) -> int:  # E: EXPECTED None <: int
    return 1
    # This code is unreachable. A linter should spot this.
    # But for now, it's perfectly reasonble to say the `pass`
    # has the wrong type, and a `return` should be here.
    pass
"#,
);

testcase!(
    test_infer_never,
    r#"
from typing import assert_type, Never

def f():
    raise Exception()

assert_type(f(), Never)
"#,
);

testcase!(
    test_infer_never2,
    r#"
from typing import NoReturn, assert_type, Literal

def fail() -> NoReturn:
    raise Exception()

def f(b: bool):
    if b:
        return 1
    else:
        fail()

assert_type(f(True), Literal[1])
"#,
);

testcase!(
    test_infer_never3,
    r#"
from typing import assert_type

def f() -> int:
   raise Exception()
assert_type(f(), int)
"#,
);

testcase!(
    test_return_never_with_unreachable,
    r#"
from typing import NoReturn

def fail() -> NoReturn:
    raise Exception()

def f(b: bool) -> int:
    if b:
        return 1
    else:
        fail()
        return 4
"#,
);

testcase!(
    test_return_never_error_return,
    r#"
def f(x: int): pass

def g():
   return f("test") # E: EXPECTED Literal['test'] <: int
"#,
);

testcase!(
    test_return_no_error,
    r#"
def B() -> None:
    (3)
"#,
);

testcase!(
    test_return_never_with_wrong_type,
    r#"
from typing import NoReturn

def fail() -> NoReturn:
    raise Exception()

def f(b: bool) -> int:
    if b:
        return None # E: EXPECTED None <: int
    else:
        fail()
"#,
);
