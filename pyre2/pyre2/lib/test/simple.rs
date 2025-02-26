/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
    test_generics_legacy_unqualified,
    r#"
from typing import TypeVar, Generic
T = TypeVar("T")
class C(Generic[T]): ...
def append(x: C[T], y: T):
    pass
v: C[int]
append(v, "test")  # E: Literal['test'] <: int
"#,
);

testcase_with_bug!(
    "special_base_class doesn't support qualified names",
    test_generics_legacy_qualified,
    r#"
import typing
T = typing.TypeVar("T")
class C(typing.Generic[T]): ...  # E: TODO: Answers::apply_special_form cannot handle `Generic[T]`
def append(x: C[T], y: T):
    pass
v: C[int]
append(v, "test")  # E: Literal['test'] <: int
"#,
);

testcase!(
    test_generic_default,
    r#"
from typing import assert_type
class C[T1, T2 = int]:
    pass
def f9(c1: C[int, str], c2: C[str]):
    assert_type(c1, C[int, str])
    assert_type(c2, C[str, int])
    "#,
);

testcase!(
    test_generic_type,
    r#"
from typing import reveal_type
class A: ...
class B: ...
class C[T]: ...
class D[T = A]: ...
def f[E](e: type[E]) -> E: ...
reveal_type(f(A)) # E: revealed type: A
reveal_type(f(B)) # E: revealed type: B
reveal_type(f(C)) # E: revealed type: C[Unknown]
reveal_type(f(D)) # E: revealed type: D[A]
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
    test_more_class_scope,
    r#"
x: int = 0
class C:
    x: str = x # E: EXPECTED Literal[0] <: str
    y: int = x # E: EXPECTED str <: int
    def m(self) -> str:
        # x refers to global x: int
        return x # E: EXPECTED Literal[0] <: str
"#,
);

testcase!(
    test_class_attribute_lookup,
    r#"
from typing import assert_type
class C:
    x = 1

assert_type(C.x, int)
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

testcase_with_bug!(
    "final annotations don't prevent writes on locals",
    test_final_annotated_local,
    r#"
from typing import Final

x: Final[int] = 0
x = 1 # TODO: x can not be assigned

y: Final = "foo"
y = "bar" # TODO: y can not be assigned
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
reveal_type()  # E: reveal_type needs 1 positional argument, got 0
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
    test_fstring_error,
    r#"
def f(x: str) -> str:
    return x

x = f"abc{f(1)}def"  # E: EXPECTED Literal[1] <: str
"#,
);

testcase!(
    test_fstring_literal,
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
    test_dict_unpack,
    r#"
from typing import assert_type
x1: dict[str, int] = {"foo": 1, **{"bar": 2}}
x2: dict[str, int] = {"foo": 1, **{"bar": "bar"}}  # E: EXPECTED dict[str, int | str] <: dict[str, int]
assert_type({"foo": 1, **{"bar": "bar"}}, dict[str, int | str])
{"foo": 1, **1}  # E: Expected a mapping, got Literal[1]
"#,
);

testcase!(
    test_dict_unpack_mapping,
    r#"
from typing import Mapping, assert_type
def test(m: Mapping[str, int]) -> None:
    x1: dict[str, int] = {**m}
    x2: dict[int, int] = {**m} # E: EXPECTED dict[str, int] <: dict[int, int]
    assert_type({"foo": 1, **m}, dict[str, int])
"#,
);

testcase!(
    test_dict_unpack_subclass,
    r#"
from typing import assert_type
class Counter[T](dict[T, int]): ...
def test(c: Counter[str]) -> None:
    x1: dict[str, int] = {**c}
    x2: dict[int, int] = {**c}  # E: EXPECTED dict[str, int] <: dict[int, int]
    assert_type({"foo": 1, **c}, dict[str, int])
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

testcase_with_bug!(
    "An ignore comment should attach to either the current line or next line, but not both",
    test_ignore_attachment,
    r#"
x: int = "1"  # type: ignore
y: int = "2"  # TODO: this error should not be suppressed
"#,
);

testcase_with_bug!(
    "This test is a placeholder, we've commented out the check for missing type arguments because until we have configurable errors it causes too many problems.",
    test_untype_with_missing_targs,
    r#"
class C[T]: pass

x: C        # TODO: The generic class `C` is missing type arguments.
y: C | int  # TODO: The generic class `C` is missing type arguments.
    "#,
);

testcase_with_bug!(
    "TODO: implement reflective operators",
    test_complex,
    r#"
z: complex =  3 + 4j # E: EXPECTED complex <: int
    "#,
);

testcase!(
    test_iterable_error,
    r#"
class A:
    pass
def f(x: A):
    for _ in x:  # E: Type `A` is not iterable
        pass
    "#,
);

testcase!(
    test_iterable_class_error,
    r#"
class A:
    pass
for _ in A:  # E: Type `type[A]` is not iterable
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
for _ in B[str]:  # E: Type `type[B[str]]` is not iterable
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
    for _ in x:  # E: Type `A` is not iterable
        pass
    for _ in y:  # E: Type `B` is not iterable
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
    for _ in x:  # E: Type `A` is not iterable
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
    test_not_iterable,
    r#"
for _ in None:  # E: `None` is not iterable
    pass
    "#,
);

testcase!(
    test_invalid_targets,
    r#"
[1 for x.y in []]  # E: Could not find name `x`
[1 for z[0] in []]  # E: Could not find name `z`
a.b += 1  # E: Could not find name `a`
(c.d := 1)  # E: Could not find name `c`  # E: Parse error: Assignment expression target must be an identifier
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
z: foo(y=x)  # E: Expected a type form, got instance of `Never`
"#,
);

testcase_with_bug!(
    "We think that Type::None is a type",
    test_function_in_type_none,
    r#"
x = 42
def foo(y):
    pass
z: foo(y=x)  # TODO: not legal
"#,
);

testcase!(
    test_invalid_literal,
    r#"
from typing import Literal
x = 1
y: Literal[x]  # E: Invalid literal expression
"#,
);

testcase!(
    test_large_int_literal,
    r#"
from typing import assert_type, Literal
x = 1
y = 0xFFFFFFFFFFFFFFFFFF
assert_type(x, Literal[1])
assert_type(y, int)
"#,
);

testcase!(
    test_large_int_type,
    r#"
from typing import Literal
x: Literal[0xFFFFFFFFFFFFFFFFFF]  # E: Int literal exceeds range
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
    "TODO",
    test_type_of_type,
    r#"
class C:
    pass
c1: type[C] = C
# TODO(stroxler): Handle `type[Any]` correctly here.
c2: type[C, C] = C  # E: Expected 1 type argument for class `type`, got 2
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
    test_anywhere_class,
    r#"
from typing import assert_type
class C:
    def p(self) -> int: ...
    def p(self) -> str: ...

assert_type(C().p(), str)
"#,
);

testcase!(
    test_identity_applied_to_list,
    r#"
from typing import assert_type
def id[T](x: T) -> T:
    return x

assert_type(id([0]), list[int])
"#,
);

testcase!(
    test_unpack_in_list_literal,
    r#"
from typing import assert_type
def test(x: list[int], y: list[str]):
    assert_type([*x, 1], list[int])
    assert_type([*x, "test"], list[int | str])
    assert_type([*x, *y], list[int | str])
    [*1]  # E: Expected an iterable
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

testcase!(
    test_type_of_class,
    r#"
from typing import assert_type
class A: pass
assert_type(A, type[A])
    "#,
);

testcase!(
    test_literal_string_after_if,
    r#"
from typing import Literal

if True:
    pass

x: Literal["little", "big"] = "big"
"#,
);

testcase!(
    test_compare_int_str_error,
    r#"
0 < "oops"  # E: EXPECTED Literal['oops'] <: int
    "#,
);

testcase!(
    test_contains_error,
    r#"
class C:
    def __contains__(self, x: int) -> bool:
        return True
def f(c: C, x: int, y: str):
    x in c  # OK
    y in c  # E: EXPECTED str <: int
    "#,
);

testcase!(
    test_mangled_syntax,
    r#"
# This parse error results in two identical Identifiers,
# which previously caused a panic.
# It should probably not produce identical identifiers - https://github.com/astral-sh/ruff/issues/16140
f"{None for y}" # E: Parse # E: Parse # E: Parse # E: Parse # E: Parse
"#,
);

testcase!(
    test_mangled_for,
    r#"
# This has identical Identifiers in the AST, which seems like the right AST.
for # E: Parse
"#,
);

testcase!(
    test_invalid_return,
    r#"
def f(x: str): ...
return f(0) # E: Invalid `return` outside of a function # E: EXPECTED Literal[0] <: str
"#,
);

testcase!(
    test_class_field_init_error,
    r#"
class C:
    x: int = oops  # E: Could not find name `oops`
    "#,
);

testcase!(
    test_pyrereadonly,
    r#"
from pyre_extensions import PyreReadOnly
def f(x: PyreReadOnly[str]):
    pass
f("test")
    "#,
);

// TODO(stroxler): We currently are using a raw name match to handle `Any`, which
// causes two problems: `typing.Any` won't work correctly, and a user can't define
// a new name `Any` in their own namespace.
//
// We encountered a surprising stub in typeshed that affects our options for
// solving this, so we are deferring the fix for now, this test records the problem.
testcase_with_bug!(
    "Any should be resolved properly rather than using a raw name match",
    test_resolving_any_correctly,
    r#"
import typing
x: typing.Any = 1  # E: Expected a type form, got instance of `object`
class Any: ...
a: Any = Any()  # E: Expected a callable, got type[Any]
"#,
);

testcase!(
    test_literal_none,
    r#"
from typing import Literal
Literal[None]
    "#,
);

testcase!(
    test_literal_alias,
    r#"
from typing import Literal as L
x: L["foo"] = "foo"
"#,
);

testcase!(
    test_assert_type_forward_ref,
    r#"
from typing import assert_type
x: "ForwardRef"
assert_type(x, "ForwardRef")
class ForwardRef:
    pass
    "#,
);

testcase!(
    test_assert_type_variations,
    r#"
import typing
# Calling by fully qualified name should work.
typing.assert_type(0, str)  # E: assert_type(Literal[0], str) failed
# Make sure that calling by bare name without importing performs the assertion, as this is very convenient for debugging.
# It's fine if a name error is also generated.
assert_type(0, str)  # E: assert_type(Literal[0], str) failed  # E: Could not find name `assert_type`
    "#,
);

testcase!(
    test_reveal_type_variations,
    r#"
import typing
# Calling by fully qualified name should work.
typing.reveal_type(0)  # E: revealed type: Literal[0]
# Make sure that calling by bare name without importing reveals the type, as this is very convenient for debugging.
# It's fine if a name error is also generated.
reveal_type(0)  # E: revealed type: Literal[0]  # E: Could not find name `reveal_type`
    "#,
);

testcase!(
    test_cast,
    r#"
from typing import assert_type, cast

x = cast(str, 1)
assert_type(x, str)

y = cast("str", 1)
assert_type(y, str)

z = cast(val=1, typ=str)
assert_type(z, str)

w = cast(val=1, typ="str")
assert_type(w, str)

cast()  # E: `typing.cast` missing required argument `typ`  # E: `typing.cast` missing required argument `val`
cast(1, 1)  # E: First argument to `typing.cast` must be a type
    "#,
);

testcase!(
    test_special_calls_unexpected_keyword,
    r#"
from typing import assert_type, reveal_type, Literal
assert_type(0, Literal[0], oops=1)  # E: `assert_type` got an unexpected keyword argument `oops`
reveal_type(0, oops=1)  # E: revealed type: Literal[0]  # E: `reveal_type` got an unexpected keyword argument `oops`
    "#,
);

testcase!(
    test_special_calls_alias,
    r#"
from typing import assert_type, reveal_type
at = assert_type
rt = reveal_type
at(0, str)  # E: assert_type(Literal[0], str) failed
rt(0)  # E: revealed type: Literal[0]
    "#,
);

testcase!(
    test_special_calls_name_clash,
    r#"
def assert_type():
    pass
def reveal_type(x, y, z):
    pass
assert_type()
reveal_type(1, 2, 3)
    "#,
);
