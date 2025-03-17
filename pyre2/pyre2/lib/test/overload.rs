/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::test::util::TestEnv;
use crate::testcase;

testcase!(
    test_py,
    r#"
from typing import overload, assert_type

@overload
def f(x: int) -> int: ...

@overload
def f(x: str) -> str: ...

def f(x):
    return x

assert_type(f(1), int)

def anywhere():
    assert_type(f(1), int)
    "#,
);

testcase!(
    test_branches,
    r#"
from typing import assert_type, overload
def test(x: bool):
    if x:
        def f(x: str) -> bytes: ...
    else:
        @overload
        def f(x: int) -> int: ...
        @overload
        def f(x: str) -> str: ...
        def f(x: int | str) -> int | str:
            return x
    def g(x: str):
        assert_type(f(x), bytes | str)
    "#,
);

fn env_with_stub() -> TestEnv {
    let mut t = TestEnv::new();
    t.add_with_path(
        "foo",
        r#"
from typing import overload

@overload
def f(x: int) -> int: ...

@overload
def f(x: str) -> str: ...
    "#,
        "foo.pyi",
    );
    t
}

testcase!(
    test_pyi,
    env_with_stub(),
    r#"
from typing import assert_type
import foo
assert_type(foo.f(1), int)
    "#,
);

testcase!(
    test_protocol,
    r#"
from typing import Protocol, assert_type, overload

class P(Protocol):
    @overload
    def m(self, x: int) -> int: ...
    @overload
    def m(self, x: str) -> str: ...

def test(o: P):
    assert_type(o.m(1), int)
    "#,
);

testcase!(
    test_method,
    r#"
from typing import assert_type, overload

class C:
    @overload
    def m(self, x: int) -> int: ...
    @overload
    def m(self, x: str) -> str: ...
    def m(self, x: int | str) -> int | str:
        return x

def test(o: C):
    assert_type(o.m(1), int)
    "#,
);

testcase!(
    test_overload_arg_errors,
    r#"
from typing import overload, assert_type

@overload
def f(x: int) -> int: ...
@overload
def f(x: str) -> str: ...
def f(x: int | str) -> int | str: ...

def g(x: str) -> int: ...
def h(x: str) -> str: ...

assert_type(f(g(0)), int) # E: Argument `Literal[0]` is not assignable to parameter `x` with type `str`
assert_type(f(h(0)), str) # E: Argument `Literal[0]` is not assignable to parameter `x` with type `str`
"#,
);

testcase!(
    test_overload_missing_implementation,
    r#"
from typing import overload, assert_type

@overload
def f(x: int) -> int: ... # E: Overloaded function must have an implementation
@overload
def f(x: str) -> str: ...

# still behaves like an overload
assert_type(f(0), int)
assert_type(f(""), str)
"#,
);

testcase!(
    test_overload_static_config,
    r#"
from typing import overload, assert_type
import sys

@overload
def f(x: int) -> int: ... # E: Overloaded function must have an implementation

if sys.version_info >= (3, 11):
    @overload
    def f(x: str) -> str: ...
else:
    @overload
    def f(x: int, int) -> bool: ...

if sys.version_info >= (3, 12):
    @overload
    def f() -> None: ...

assert_type(f(0), int)
assert_type(f(""), str)
assert_type(f(), None)
f(0, 0) # E: No matching overload found  # E: Expected 1 positional argument, got 2
"#,
);

testcase!(
    test_only_one_overload,
    r#"
from typing import overload, Protocol

@overload
def f(x: int) -> int: ...  # E: Overloaded function needs at least two signatures
def f(x: int) -> int:
    return x

@overload
def g(x: int) -> int: ...  # E: Overloaded function must have an implementation  # E: Overloaded function needs at least two signatures

class P(Protocol):
    @overload
    def m(x: int) -> int: ...  # E: Overloaded function needs at least two signatures
"#,
);

testcase!(
    test_overload_ignore,
    r#"
from typing import Never, overload, assert_type

@overload
def f(x: int) -> int: ...
@overload
def f(x: str) -> str: ...
def f(x: int | str) -> int | str:
    return x

x = f("foo") # type: ignore
# intentionally blank: make sure we don't ignore the assert_type below
assert_type(x, str)
"#,
);

testcase!(
    test_typeguard,
    r#"
from typing import assert_type, overload, TypeGuard

class Animal: ...
class Mammal(Animal): ...
class Cat(Mammal): ...
class Bird(Animal): ...
class Robin(Bird): ...

@overload
def f(x: Mammal) -> TypeGuard[Cat]: ...
@overload
def f(x: Bird) -> TypeGuard[Robin]: ...
def f(x: Animal) -> bool: ...

class A:
    @overload
    def f(self, x: Mammal) -> TypeGuard[Cat]: ...
    @overload
    def f(self, x: Bird) -> TypeGuard[Robin]: ...
    def f(self, x: Animal) -> bool: ...

def g(meow: Mammal, chirp: Bird):
    if f(meow):
        assert_type(meow, Cat)
    if A().f(chirp):
        assert_type(chirp, Robin)
    "#,
);

testcase!(
    test_classmethod,
    r#"
from typing import assert_type, overload
class A:
    @overload
    @classmethod
    def f(cls, x: int) -> int: ...

    @overload
    @classmethod
    def f(cls, x: str) -> str: ...

    @classmethod
    def f(cls, x: int | str) -> int | str:
        return x

assert_type(A().f(1), int)
    "#,
);

testcase!(
    test_invalid_decoration,
    r#"
from typing import overload

def decorate(f) -> float:
    return 0

@overload
def f(x: str) -> str: ...

@decorate
@overload
def f(x: int) -> int: ...  # E: `f` has type `float` after decorator application, which is not callable

def f(x: str | int) -> str | int:
    return x
    "#,
);

testcase!(
    test_decoration,
    r#"
from typing import Callable, assert_type, overload

def decorate(f) -> Callable[[int], int]:
    return lambda x: x

@overload
@decorate
def f(x: bytes) -> bytes: ...

@overload
def f(x: str) -> str: ...

def f(x: object) -> object:
    return x

assert_type(f(0), int)
f(b"")  # E: No matching overload found for function `f`, reporting errors for closest overload: `(int) -> int`  # E: `Literal[b'']` is not assignable to parameter with type `int`
    "#,
);

testcase!(
    test_final_decoration,
    r#"
from typing import assert_type, final, overload

@overload
@final
def f(x: int) -> int: ...

@overload
def f(x: str) -> str: ...

def f(x: int | str) -> int | str:
    return x

assert_type(f(0), int)
    "#,
);
