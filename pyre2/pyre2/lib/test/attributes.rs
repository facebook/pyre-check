/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::test::util::TestEnv;
use crate::testcase;

testcase!(
    test_set_attribute,
    r#"
class A:
    x: int
def f(a: A):
    a.x = 1  # OK
    a.x = "oops"  # E: `Literal['oops']` is not assignable to attribute `x` with type `int`
    "#,
);

testcase!(
    test_set_attribute_in_unpacked_assign,
    r#"
class A:
    x: int
    y: str
def f(a: A):
    a.x, a.y = "x", "y"  # E: `Literal['x']` is not assignable to attribute `x` with type `int`
    "#,
);

testcase!(
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

testcase!(
    test_unannotated_attribute_bad_assignment,
    r#"
class A:
    def __init__(self):
        self.x = 0
    def f(self):
        self.x = "oops"  # E: `Literal['oops']` is not assignable to attribute `x` with type `int`
    "#,
);

testcase!(
    test_self_attribute_assign_twice,
    r#"
from typing import assert_type
class A:
    def f(self, x: str):
        self.x = x  # E: `str` is not assignable to attribute `x` with type `int`
    def __init__(self, x: int):
        self.x = x
    "#,
);

testcase!(
    test_self_attribute_in_test_setup,
    r#"
class MyTestCase:
    def setUp(self):
        self.x = 5
    def run(self):
        assert self.x == 5
    "#,
);

testcase!(
    bug = "Example of how making methods read-write but not invariant is unsound",
    test_method_assign,
    r#"
from typing import Protocol
class X(Protocol):
    def foo(self) -> object:
        return 1
class Y:
    def foo(self) -> int:
        return 1
def func(x: X):
    x.foo = lambda: "hi"
y: Y = Y()
func(y)
y.foo()  # result is "hi"
    "#,
);

testcase!(
    test_attribute_union,
    r#"
class A:
    x: int
class B:
    x: str
def test(x: A | B):
    del x.x
    x.x = 1  # E: `Literal[1]` is not assignable to attribute `x` with type `str`
    "#,
);

testcase!(
    test_callable_boundmethod_subset,
    r#"
from typing import Callable

class C:
    def f(self, x: int, /) -> str:
        return ""
class C2:
    @classmethod
    def f(cls, x: int, /) -> str:
        return ""
class C3:
    @staticmethod
    def f(x: int, /) -> str:
        return ""
def foo(x: Callable[[int], str], c: C, c2: C2, c3: C3):
    C.f = x  # E: `(int) -> str` is not assignable to attribute `f` with type `(Self@C, int) -> str`
    c.f = x
    C2.f = x
    c2.f = x
    C3.f = x
    c3.f = x
    "#,
);

testcase!(
    test_use_of_class_body_scope_in_class_body_statement,
    r#"
class A:
    x: int = 5
    y: int = x
    "#,
);

testcase!(
    test_annotating_non_self_attributes,
    r#"
class A:
    x: int

class B:
    def __init__(self, a: A):
        a.x: int = 1  # E: Type cannot be declared in assignment to non-self attribute `a.x`

a: A = A()
a.x: int = 5  # E: Type cannot be declared in assignment to non-self attribute `a.x`
    "#,
);

testcase!(
    test_self_attribute_annotated_in_class_body,
    r#"
from typing import assert_type
class A:
    x: str
    def __init__(self, x: int):
        self.x = x  # E: `int` is not assignable to attribute `x` with type `str`
    "#,
);

testcase!(
    test_self_attribute_annotated_assignment,
    r#"
from typing import assert_type

class A:
    def __init__(self, x: str):
        self.x: int = x  # E: `str` is not assignable to attribute `x` with type `int`
def f(a: A):
    assert_type(a.x, int)
    "#,
);

testcase!(
    test_generic_classvar,
    r#"
from typing import ClassVar
class A[T]:
    x: ClassVar[T]  # E: `ClassVar` arguments may not contain any type variables
    y: ClassVar[list[T]]  # E: `ClassVar` arguments may not contain any type variables
    "#,
);

testcase!(
    test_self_attribute_annotated_twice,
    r#"
from typing import assert_type, Literal, Final
class A:
    x: int
    y: str
    def __init__(self):
        self.x: Literal[1] = 1  # E: Cannot annotate attribute `x`, which is already annotated in the class body
        self.y: Final = "y"  # E: Cannot annotate attribute `y`, which is already annotated in the class body
def f(a: A):
    assert_type(a.x, int)
    "#,
);

testcase!(
    bug = "TODO(stroxler): We are always promoting literals. It is sound to preserve literals for read-only attributes",
    test_final_attribute_assined_in_init,
    r#"
from typing import assert_type, Final, Literal
class A:
    def __init__(self):
        self.x: Final = 0
def f(a: A):
    assert_type(a.x, Literal[0])  # E: assert_type(int, Literal[0])
    "#,
);

testcase!(
    test_literal_attr_with_annotation,
    r#"
from typing import ClassVar, assert_type
class C:
    x0 = 0
    x1: ClassVar = 0
assert_type(C.x0, int)
assert_type(C.x1, int)
"#,
);

testcase!(
    test_final_annotated_override,
    r#"
from typing import Final
def f() -> int: ...
class Base:
    p: Final = f()
class Derived(Base):
    p = f()  # E: `p` is declared as final in parent class `Base`
"#,
);

testcase!(
    test_self_attribute_bare_annotation,
    r#"
from typing import assert_type
class A:
    def __init__(self, x: str):
        self.x: int
        self.x = x  # E: `str` is not assignable to attribute `x` with type `int`
def f(a: A):
    assert_type(a.x, int)
    "#,
);

testcase!(
    test_attribute_inference,
    r#"
class C:
    x: list[int | str]
def f(c: C):
    c.x = [5]
    "#,
);

testcase!(
    test_set_attribute_in_init_nested,
    r#"
from typing import assert_type
class C:
    def __init__(self):
        def f():
            self.x = 0
        f()
def f(c: C):
    assert_type(c.x, int)
    "#,
);

// TODO: Should `C.x` exist?
testcase!(
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

testcase!(
    test_missing_self_parameter,
    r#"
class C:
    def f():
        pass
C().f()  # E: Expected 0 positional arguments, got 1 (including implicit `self`)
    "#,
);

testcase!(
    test_generic_instance_method,
    r#"
class C:
    def f[T](self: T, x: T):
        pass
C().f(C())  # OK
C().f(0)    # E: Argument `Literal[0]` is not assignable to parameter `x` with type `C`
    "#,
);

// Make sure we treat `callable_attr` as plain instance data, not a bound method.
testcase!(
    test_callable_instance_only_attribute,
    r#"
from typing import Callable, assert_type, Literal, reveal_type
class C:
    callable_attr: Callable[[int], int]
    def __init__(self):
       self.callable_attr = lambda x: x
c = C()
x = c.callable_attr(42)
assert_type(x, int)
    "#,
);

testcase!(
    test_class_access_of_instance_only_attribute,
    r#"
from typing import assert_type, Any
class C:
    x: int
    def __init__(self, y: str):
        self.x = 0
        self.y = y
assert_type(C.x, Any)  # E: Instance-only attribute `x` of class `C` is not visible on the class
assert_type(C.y, Any)  # E: Instance-only attribute `y` of class `C` is not visible on the class
c = C("y")
assert_type(c.x, int)
assert_type(c.y, str)
"#,
);

testcase!(
    test_match_method_against_callable,
    r#"
from typing import Callable
class C:
    def f(self, x: int) -> None:
        pass
def f1(c: Callable[[int], None]):
    pass
def f2(c: Callable[[C, int], None]):
    pass
f1(C.f)  # E: Argument `(self: Self@C, x: int) -> None` is not assignable to parameter `c` with type `(int) -> None`
f1(C().f)
f2(C.f)
f2(C().f)  # E: Argument `BoundMethod[C, (self: Self@C, x: int) -> None]` is not assignable to parameter `c` with type `(C, int) -> None`
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

c: C[str] = C()
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

c: C[str] = C()
assert_type(c.x, list[str])
assert_type(c.y, str)
assert_type(c.z, bool)
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
    test_var_attribute,
    r#"
from typing import assert_type
def f[T](x: T) -> T:
    return x
class C:
    def __init__(self):
        self.x = 42
assert_type(f(C()).x, int)
    "#,
);

testcase!(
    test_never_attr,
    r#"
from typing import Never, NoReturn, assert_type
def f() -> NoReturn: ...
def g():
    x = f().x
    assert_type(x, Never)
    "#,
);

testcase!(
    test_callable_attr,
    r#"
from typing import assert_type
from types import CodeType
def f():
    pass
def g():
    assert_type(f.__code__, CodeType)
    "#,
);

testcase!(
    test_boundmethod_attr,
    r#"
from typing import assert_type
class A:
    def f(self):
        pass
def g(a: A):
    assert_type(a.f.__self__, object)
    "#,
);

testcase!(
    test_ellipsis_attr,
    r#"
x = ...
x.x  # E: Object of class `EllipsisType` has no attribute `x`
    "#,
);

testcase!(
    test_forall_attr,
    r#"
from typing import assert_type
from types import CodeType
def f[T](x: T) -> T:
    return x
assert_type(f.__code__, CodeType)
    "#,
);

testcase!(
    test_metaclass_attr,
    r#"
from typing import assert_type

class A: ...
class B[T]: ...
assert_type(A.mro(), list[type])
assert_type(B[int].mro(), list[type])

class Meta(type):
    x: int
class C(metaclass=Meta):
    pass
assert_type(C.x, int)
    "#,
);

fn env_with_stub() -> TestEnv {
    let mut t = TestEnv::new();
    t.add_with_path(
        "foo",
        r#"
class A:
    x: int = ...
    y: int
    "#,
        "foo.pyi",
    );
    t
}

testcase!(
    test_stub_initializes_attr_with_ellipses,
    env_with_stub(),
    r#"
from typing import assert_type
from foo import A

assert_type(A.x, int)
A.y  # E: Instance-only attribute `y` of class `A` is not visible on the class
    "#,
);

testcase!(
    test_object_getattr,
    r#"
from typing import assert_type

class Foo:
    def __getattr__(self, name: str) -> int: ...

def test(foo: Foo) -> None:
    assert_type(foo.x, int)
    assert_type(foo.y, int)
    foo.x = 1  # E: Object of class `Foo` has no attribute `x`
    del foo.y  # E: Object of class `Foo` has no attribute `y`
    "#,
);

testcase!(
    test_object_getattr_wrong_signature,
    r#"
from typing import assert_type

class Foo:
    def __getattr__(self, name: int) -> int: ...

def test(foo: Foo) -> None:
    assert_type(foo.x, int)  # E: Argument `Literal['x']` is not assignable to parameter `name`
    assert_type(foo.y, int)  # E: Argument `Literal['y']` is not assignable to parameter `name`
    foo.x = 1  # E: Object of class `Foo` has no attribute `x`
    del foo.y  # E: Object of class `Foo` has no attribute `y`
    "#,
);

testcase!(
    test_module_getattr,
    TestEnv::one("foo", "def __getattr__(name: str) -> int: ..."),
    r#"
from typing import assert_type
import foo
assert_type(foo.x, int)
assert_type(foo.y, int)
foo.x = 1  # E: No attribute `x` in module `foo`
del foo.y  # E: No attribute `y` in module `foo`
    "#,
);

testcase!(
    test_any_subclass,
    r#"
from typing import Any, assert_type

class A(Any):
    x: int

class B(A):
    y: str

def test0(a: A, b: B, ta: type[A]) -> None:
    assert_type(a.x, int)
    assert_type(b.x, int)
    assert_type(b.y, str)
    assert_type(ta.mro(), list[type])

    assert_type(a.z, Any)
    assert_type(b.z, Any)
    assert_type(ta.z, Any)

class Test(B):
    def m(self) -> None:
        assert_type(super().z, Any)
    @classmethod
    def m2(cls) -> None:
        assert_type(super().z, Any)
    "#,
);

testcase!(
    test_field_using_method_scope_type_variable,
    r#"
from typing import assert_type, Any

class C:
    def __init__[R](self, field: R):
        self.field = field  # E: Cannot initialize attribute `field` to a value that depends on method-scoped type variable `R`

c = C("test")
assert_type(c.field, Any)
"#,
);

testcase!(
    bug = "PyTorch TODO: This test should typecheck and the reveal_type should be Optional[Any]",
    test_attr_arg,
    r#"
from typing import Any, Optional, reveal_type

Arg = Optional[tuple["Arg", ...]]

class N:
    _args: tuple["Arg", ...]
    type: Optional[Any]

    @property
    def args(self) -> tuple[Arg, ...]:
        return self._args


def f(n: N):
    assert isinstance(n.args[0], N)
    t1 = n.args[0].type # E:  Object of class `NoneType` has no attribute `type` # E: Object of class `tuple` has no attribute `type`
    reveal_type(t1) # E: revealed type: Error

"#,
);

testcase!(
    bug = "PyTorch TODO: Should we admit this program and should reveal_type be Any",
    test_attr_unknown,
    r#"
class Op:
    default: str
class Namespace:
    def __getattr__(self, op_name):
        if op_name == "__file__":
            return "test"
        return Op()
x = Namespace().some_op.default # E:  Object of class `str` has no attribute `default

"#,
);

testcase!(
    test_with,
    r#"
class C:
    def __init__(self) -> None:
        self.prev = False
    def __enter__(self) -> None:
        self.prev = False
    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        self.prev = False
    def __new__(cls, orig_func=None):
        if orig_func is None:
            return super().__new__(cls) 
def f():
    with C():  # E: `NoneType` has no attribute `__enter__`  # E: `NoneType` has no attribute `__exit__`
        pass
    "#,
);

testcase!(
    bug = "TODO(stroxler): We need to define the semantics of generic class nesting and avoid leaked type variables",
    test_class_nested_inside_generic_class,
    r#"
from typing import Any, assert_type, reveal_type
class Outer[T]:
    class Inner:
        x: T | None = None
assert_type(Outer[int].Inner, type[Outer.Inner])
assert_type(Outer.Inner, type[Outer.Inner])
reveal_type(Outer[int].Inner.x)  # E: revealed type: ?_TypeVar | None
reveal_type(Outer.Inner.x)  # E: revealed type: ?_TypeVar | None
reveal_type(Outer[int].Inner().x)  # E: revealed type: ?_TypeVar | None
reveal_type(Outer.Inner().x)  # E: revealed type: ?_TypeVar | None
   "#,
);

testcase!(
    test_attr_base,
    r#"
def f(x, key, dict):
    for param in x:
        if key in dict:
            pass
    "#,
);

testcase!(
    test_classvar_no_value,
    r#"
from typing import ClassVar, assert_type
class C:
    x: ClassVar[int]
assert_type(C.x, int)
    "#,
);
