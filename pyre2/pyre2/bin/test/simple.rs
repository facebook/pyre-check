/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::test::util::TestEnv;
use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_canonicalized_call,
    r#"
from typing import Literal
def foo(x: Literal[1]) -> int:
    return x.__add__(1)
"#,
);

testcase!(
    test_union_call,
    r#"
from typing import assert_type
class A:
    def foo(self) -> int: ...
class B:
    def foo(self) -> str: ...
def foo(x: A | B) -> None:
    assert_type(x.foo(), int | str)
"#,
);

testcase!(
    test_simple_call,
    r#"
from typing import assert_type
def f(x: str) -> int:
    return 1
y = f("test")
assert_type(y, int)
"#,
);

testcase!(
    test_mypy_demo,
    r#"
from typing import Any
def input(prompt: str) -> str:
    return ""

def print(msg: str, val: Any) -> None:
    pass

def plus(x: int, y: int) -> int:
    return x

number = input("What is your favourite number?")
print("It is", plus(number, 1))  # E: str <: int
"#,
);

testcase!(
    test_error_in_function,
    r#"
def f(x: str) -> int:
    return x  # E: str <: int
"#,
);

testcase!(
    test_create_class,
    r#"
from typing import assert_type
class C:
    pass
x = C()
assert_type(x, C)
"#,
);

testcase!(
    test_class_method,
    r#"
from typing import assert_type
class C:
    def method(self, arg: str) -> int:
        return 1
x = C()
y = x.method("test")
assert_type(y, int)
"#,
);

testcase!(
    test_generics,
    r#"
from typing import Literal
class C[T]: ...
def append[T](x: C[T], y: T):
    pass
v: C[int]
append(v, "test")  # E: Literal['test'] <: int
"#,
);

testcase!(
    test_list_class_basic,
    r#"
from typing import assert_type
x = [3]
x.append(4)
assert_type(x, list[int])
"#,
);

testcase!(
    test_list_class_inner_generic,
    r#"
x = [3]
x.append("test")  # E: Literal['test'] <: int
"#,
);

testcase!(
    test_empty_list_class,
    r#"
from typing import assert_type, Any
x = []
assert_type(x, list[Any])
"#,
);

testcase!(
    test_empty_list_is_generic,
    r#"
from typing import assert_type
def foo[T](x: list[T], y: list[T]) -> T: ...
r = foo([], [1])
assert_type(r, int)
"#,
);

testcase!(
    test_empty_list_append,
    r#"
from typing import assert_type
x = []
x.append(4)
assert_type(x, list[int])
"#,
);

testcase!(
    test_empty_list_check,
    r#"
from typing import Literal, assert_type
x = []
def f(x: list[Literal[4]]): ...
f(x)
assert_type(x, list[Literal[4]])
"#,
);

testcase!(
    test_shadow_var,
    r#"
from typing import assert_type, Literal
x = 1
def f(x: str) -> str:
    return x
y = x
assert_type(y, Literal[1])
"#,
);

testcase!(
    test_unordered_defs,
    r#"
def f() -> int:
    return g()  # E: str <: int
def g() -> str:
    return "test"
"#,
);

testcase!(
    test_function_uses_class,
    r#"
from typing import assert_type
class C: pass

def foo() -> C:
    x : C = C()
    return x

assert_type(foo(), C)
"#,
);

testcase!(
    test_union_alias,
    r#"
from typing import TypeAlias
StringOrInt: TypeAlias = str | int
x: StringOrInt = 1
"#,
);

testcase!(
    test_alias_import,
    TestEnv::one(
        "foo",
        "from typing import TypeAlias\nStringOrInt: TypeAlias = str | int"
    ),
    r#"
from foo import StringOrInt
x: StringOrInt = 1
"#,
);

testcase!(
    test_type_as_string,
    r#"
class Foo: ...
x: "list[Foo]" = []
def f(y: "None") -> list["Foo"]:
    return x
"#,
);

testcase!(
    test_line_file,
    r#"
from typing import assert_type
assert_type(__file__, str)
assert_type(__name__, str)
"#,
);

testcase!(
    test_union_none,
    r#"
from typing import TypeAlias
NoneOrInt: TypeAlias = None | int
IntOrNone: TypeAlias = int | None
NoneOrStr = None | str
StrOrNone = str | None

a: NoneOrInt = None
b: IntOrNone = 1
c: NoneOrStr = "test"
d: StrOrNone = None
e: NoneOrInt = "test"  # E: Literal['test'] <: int | None
"#,
);

testcase!(
    test_argument_shadows_type,
    r#"
class C: ...
def f(C: C, D: C) -> None:
    return None
"#,
);

testcase!(
    test_union_or_in_argument,
    r#"
def f(x: int | str) -> None:
    return None
f(1)
f("test")
f(None)  # E: None <: int | str
"#,
);

testcase!(
    test_types_with_flow,
    r#"
from typing import assert_type
T = str
def f() -> T: ...
T = int

assert_type(f(), str)
"#,
);

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
    test_class_rebind_attribute,
    r#"
from typing import assert_type
def f(x: str) -> int:
    return 42

attribute = "test"

class C:
    attribute = f(attribute)

assert_type(C().attribute, int)
"#,
);

testcase!(
    test_class_attribute_lookup,
    r#"
from typing import assert_type, Literal
class C:
    x = 1

assert_type(C.x, Literal[1])
"#,
);

testcase!(
    test_nested_class_attribute_with_inheritance,
    r#"
from typing import assert_type

class B:
    class Nested:
        x: int

class C(B):
    pass

N0: B.Nested = C.Nested()
N1: C.Nested = B.Nested()
assert_type(N1.x, int)
"#,
);

testcase!(
    test_class_generic_attribute_lookup,
    r#"
class C[T]:
    x = T

C.x  # E: Generic attribute `x` of class `C` is not visible on the class
"#,
);

testcase!(
    test_use_self,
    r#"
from typing import assert_type
from typing import Self
import typing
from typing import Self as Myself

class C:
    def m(self, x: Self, y: Myself) -> list[typing.Self]:
        return [self, x, y]

assert_type(C().m(C(), C()), list[C])
"#,
);

testcase!(
    test_str_type_triple_quote,
    r#"
value: """
    str |
    int |
    list[int]
"""

value = 1
value = "test"
value = None  # E: None <: int | list[int] | str
"#,
);

testcase!(
    test_nested_func,
    r#"
from typing import assert_type
def f(x: int) -> int:
    def g(y: str) -> int:
        return x
    return g("test")
assert_type(f(1), int)
"#,
);

testcase!(
    test_simple_inheritance,
    r#"
from typing import assert_type
class B:
    x: int

class HasBase(B):
    y: str

assert_type(HasBase().x, int)
"#,
);

testcase!(
    test_generic_multiple_inheritance,
    r#"
from typing import assert_type
class A[T]:
    x: T

class B[T]:
    y: T

class C[T](A[int], B[T]):
    z: bool

c: C[str]
assert_type(c.x, int)
assert_type(c.y, str)
assert_type(c.z, bool)
"#,
);

testcase!(
    test_generic_chained_inheritance,
    r#"
from typing import assert_type
class A[T]:
    x: T

class B[T](A[list[T]]):
    y: T

class C[T](B[T]):
    z: bool

c: C[str]
assert_type(c.x, list[str])
assert_type(c.y, str)
assert_type(c.z, bool)
"#,
);

testcase!(
    test_final_annotated,
    r#"
from typing import Final, assert_type, Literal
x: Final[int] = 1
y: Final = "test"
z: Final[str]
w: Final[int] = "bad"  # E: Literal['bad'] <: int

assert_type(x, Literal[1])
assert_type(y, Literal['test'])
assert_type(z, str)
"#,
);

testcase!(
    test_solver_variables,
    r#"
from typing import assert_type, Any

def foo[T](x: list[T]) -> T: ...

def bar():
    if False:
        return foo([])
    return foo([])

assert_type(bar(), Any)
"#,
);

testcase!(
    test_solver_variables_2,
    r#"
from typing import assert_type, Any
def foo[T](x: list[T]) -> T: ...
def bar(random: bool):
    if random:
        x = foo([])
    else:
        x = foo([1])
    assert_type(x, int)
    "#,
);

testcase!(
    test_reveal_type,
    r#"
from typing import reveal_type
reveal_type()  # E: reveal_type needs 1 argument, got 0
reveal_type(1)  # E: revealed type: Literal[1]
    "#,
);

testcase!(
    test_reveal_type_expand_var,
    r#"
from typing import reveal_type
def f[T](x: T) -> T:
    return x
reveal_type(f(0))  # E: revealed type: int
    "#,
);

testcase!(
    test_forward_refs_in_bases,
    r#"
from typing import assert_type, Any

class Base[T]:
    x: T

class A(Base["int"]):
    pass

class B("Base[str]"):  # E: Cannot use string annotation `Base[str]` as a base class
    pass

assert_type(A().x, int)
assert_type(B().x, str)
    "#,
);

testcase!(
    test_class_var,
    r#"
from typing import assert_type
class B:
    x: int = 1

assert_type(B.x, int)
"#,
);

testcase!(
    test_fstring,
    r#"
from typing import assert_type, Literal
x0 = f"abc"
assert_type(x0, Literal["abc"])

x1 = f"abc{x0}"
assert_type(x1, str)

x2 = f"abc" "def"
assert_type(x2, Literal["abcdef"])

x3 = f"abc" f"def"
assert_type(x3, Literal["abcdef"])

x4 = "abc" f"def"
assert_type(x4, Literal["abcdef"])

x5 = "abc" f"def{x0}g" "hij" f"klm"
assert_type(x5, str)
"#,
);

testcase!(
    test_ternary_expression,
    r#"
from typing import assert_type, Literal

def derp() -> bool:
    ...

assert_type(0 if True else 1, Literal[0])
assert_type(0 if False else 1, Literal[1])
assert_type(0 if derp() else 1, Literal[0] | Literal[1])
"#,
);

testcase!(
    test_type_alias_full_name,
    r#"
import typing
from typing import assert_type
X: typing.TypeAlias = int
def f(x: X | str):
    assert_type(x, int | str)
    "#,
);

testcase!(
    test_raise,
    r#"
def test_raise() -> None:
    raise
    raise None  # E: does not derive from BaseException
    raise BaseException
    raise BaseException()
    raise 42  # E: does not derive from BaseException
    raise BaseException from None
    raise BaseException() from None
    raise BaseException() from BaseException
    raise BaseException() from BaseException()
    raise BaseException() from 42   # E: does not derive from BaseException
"#,
);

testcase!(
    test_raise_filter_type,
    r#"
from typing import assert_type, Literal
def f(x):
    if x:
        y = "error"
        raise BaseException()
    else:
        y = "ok"
    assert_type(y, Literal["ok"])
"#,
);

testcase!(
    test_special_form_argument_counts,
    r#"
from typing import Callable, Optional, Type, TypeGuard, TypeIs

def test0() -> Type[int, int]: ...  # E: requires exactly one argument
def test1() -> TypeGuard[int, int]: ...  # E: requires exactly one argument
def test2() -> TypeIs[int, int]: ...  # E: requires exactly one argument
def test3() -> Optional[int, int]: ...  # E: requires exactly one argument
def test4() -> Callable[[], int, int]: ...  # E: requires exactly two arguments
"#,
);

testcase!(
    test_infinite_solver_1,
    r#"
from typing import Any, assert_type
x = [[], [], [[]]]
# Not too important it is precisely this type, but detect changes
assert_type(x, list[list[list[Any]]])
"#,
);

testcase!(
    test_infinite_solver_2,
    r#"
from typing import Any, assert_type
x = []
y = []
for z in []:
    x.append(z)
for z in x:
    y.append(z)
x = y
assert_type(x, list[Any])
"#,
);

testcase!(
    test_self_param_name,
    r#"
from typing import assert_type
class C:
    def f(this):
        assert_type(this, C)
    "#,
);

testcase!(
    test_getitem,
    r#"
from typing import assert_type
class C:
    pass
class D:
    __getitem__: int = 1

def f(x: list[int], y: dict[str, bool]) -> None:
    assert_type(x[0], int)
    assert_type(y["test"], bool)
    x["foo"]  # E: Literal['foo'] <: int
    c = C()
    c[0]  # E: `C` has no attribute `__getitem__`
    d = D()
    d[0]  # E: Expected `__getitem__` to be a callable, got int
"#,
);

testcase!(
    test_iterable,
    r#"
from typing import Iterable, assert_type
def f(x: Iterable[int]):
    for i in x:
        assert_type(i, int)
"#,
);

testcase!(
    test_ignore,
    r#"
x: int = "1"  # type: ignore

# type: ignore
y: int = "2"

z: int = "3"  # E: Literal['3'] <: int
"#,
);

// This test is a placeholder, we've commented out the check for missing
// type arguments because until we have configurable errors it causes
// too many problems.
testcase_with_bug!(
    test_untype_with_missing_targs,
    r#"
class C[T]: pass

x: C        # TODO: The generic class `C` is missing type arguments.
y: C | int  # TODO: The generic class `C` is missing type arguments.
    "#,
);

// TODO: implement reflective operators
testcase_with_bug!(
    test_complex,
    r#"
z: complex =  3 + 4j # E: EXPECTED Literal[0+4j] <: int # E: EXPECTED int <: complex

    "#,
);

testcase!(
    test_iterable_error,
    r#"
class A:
    pass
def f(x: A):
    for _ in x:  # E: Class `A` is not iterable
        pass
    "#,
);

testcase!(
    test_iterable_class_error,
    r#"
class A:
    pass
for _ in A:  # E: Class object `A` is not iterable
    pass
    "#,
);

testcase!(
    test_iterable_generic_class,
    r#"
from typing import Iterator, assert_type
class M(type):
    def __iter__(self) -> Iterator[int]: ...
class A[T](metaclass=M):
    pass
class B[T]:
    pass
for x in A[str]:
    assert_type(x, int)
for _ in B[str]:  # E: Class object `B[str]` is not iterable
    pass
    "#,
);

testcase!(
    test_iterable_bad_callable,
    r#"
from typing import Self
class A:
    __iter__: bool
class B:
    def __iter__(self) -> Self:
        return self
    __next__: str
def f(x: A, y: B):
    for _ in x:  # E: Expected `__iter__` to be a callable, got bool
        pass
    for _ in y:  # E: Expected `__next__` to be a callable, got str
        pass
    "#,
);

testcase!(
    test_iterable_bad_iterator,
    r#"
class A:
    def __iter__(self) -> None:
        return None
def f(x: A):
    for _ in x:  # E: `None` has no attribute `__next__`
        pass
    "#,
);

testcase!(
    test_getitem_iteration,
    r#"
from typing import assert_type
class A:
    def __getitem__(self, i: int) -> str:
        return ""
def f(x: A):
    for s in x:
        assert_type(s, str)
    "#,
);

testcase!(
    test_getitem_iteration_bad,
    r#"
class A:
    def __getitem__(self, s: str) -> str:
        return s
def f(x: A):
    for _ in x:  # E: EXPECTED int <: str
        pass
    "#,
);

testcase!(
    test_assert,
    r#"
def foo() -> str: ...
assert foo(42)  # E: Expected 0 positional arguments
assert False, foo(42)  # E: Expected 0 positional arguments
    "#,
);

testcase!(
    test_subscript_error,
    r#"
class A:
    def __getitem__(self, i: int) -> int:
        return i
def f(a: A):
    return a["oops"]  # E: EXPECTED Literal['oops'] <: int
    "#,
);

testcase!(
    test_function_in_type,
    r#"
x = 42
def foo(y): ...
z: foo(y=x)  # E: untype, got Never
"#,
);

// FIXME: Should give a better error message
testcase_with_bug!(
    test_invalid_literal,
    r#"
from typing import Literal
x = 1
y: Literal[x]  # E: TODO: Name(ExprName - Lit::from_expr
"#,
);

testcase!(
    test_invalid_type_arguments,
    r#"
from typing import assert_type
x: list[int, int] = []  # E: Expected 1 type argument for class `list`, got 2
assert_type(x, list[int])
    "#,
);

testcase_with_bug!(
    test_type_of_type,
    r#"
class C:
    pass
c: type[C] = C
# TODO(stroxler): Handle `type[Any]` correctly here.
c: type[C, C] = C  # E: Expected 1 type argument for class `type`, got 2
    "#,
);

testcase!(
    test_annotated,
    r#"
from typing import Annotated, assert_type
def f(x: Annotated[int, "test"], y: Annotated[int, "test", "test"]):
    assert_type(x, int)
    assert_type(y, int)
    "#,
);

testcase!(
    test_no_backtracking,
    r#"
from typing import assert_type
def foo(x: tuple[list[int], list[int]] | tuple[list[str], list[str]]) -> None: ...
def test(x: list[str]) -> None:
    y = ([], x)
    # Because we pin down the `[]` first, we end up with a type error.
    # If we had backtracking we wouldn't.
    foo(y)  # E: EXPECTED tuple[list[int], list[str]] <: tuple[list[int], list[int]] | tuple[list[str], list[str]]
"#,
);

testcase!(
    test_reassign_parameter,
    r#"
def foo(x: int):
    x = "test"  # E: Literal['test'] <: int
"#,
);

testcase!(
    test_await_simple,
    r#"
from typing import Any, Awaitable, assert_type
class Foo(Awaitable[int]):
    pass
async def bar() -> str: ...

async def test() -> None:
    assert_type(await Foo(), int)
    assert_type(await bar(), str)
"#,
);

testcase!(
    test_generic_create_literal,
    r#"
from typing import assert_type, Literal

class Foo[T]:
    def __init__(self, x: T) -> None: ...

x: Literal[42] = 42
assert_type(Foo(x), Foo[int])
"#,
);

testcase!(
    test_generic_get_literal,
    r#"
from typing import assert_type, Literal

class Foo[T]:
    def get(self) -> T: ...

def test(x: Foo[Literal[42]]) -> None:
    assert_type(x.get(), Literal[42])
"#,
);

testcase!(
    test_await_literal,
    r#"
from typing import Awaitable, Literal
class Foo(Awaitable[Literal[42]]):
    pass
async def test() -> Literal[42]:
    return await Foo()
"#,
);

testcase!(
    test_await_non_awaitable,
    r#"
async def test() -> None:
    await 42  # E: Expression is not awaitable
"#,
);

testcase!(
    test_await_wrong_await_return_type,
    r#"
class Foo:
    def __await__(self) -> int:
        ...

async def test() -> None:
    await Foo()  # E: Expression is not awaitable
"#,
);

testcase!(
    test_anywhere_binding,
    r#"
from typing import assert_type, Literal
x = 1
def foo():
    assert_type(x, Literal['test', 1])
foo()
x = "test"
"#,
);

testcase!(
    test_incorrect_inference,
    r#"
from typing import assert_type, Any
def id[T](x: T) -> T:
    return x

assert_type(id([0]), list[int])
"#,
);

testcase!(
    test_union_never,
    r#"
from typing import Never, assert_type
def fail() -> Never: ...
def f(x: int):
    y = x or fail()
    assert_type(y, int)
    "#,
);
