/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_protocol,
    r#"
from typing import Protocol
class P(Protocol):
    x: int
    y: str
class C1:
    x: int
    y: str
class C2:
    x: str
class C3(P, C1): ...
class C4(P):
    y: int # E: Class member `C4.y` overrides parent class `P` in an inconsistent manner
class C5:
    x: int
    y: int
def f(proto: P) -> None: ...
def g(p: P, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5) -> None:
    f(c1)
    f(c2)  # E: Argument `C2` is not assignable to parameter `proto` with type `P`
    f(c3)
    f(c4)
    f(c5)  # E: Argument `C5` is not assignable to parameter `proto` with type `P`
    c: C1 = p  # E: `P` is not assignable to `C1`
 "#,
);

testcase!(
    test_protocol_base,
    r#"
from typing import Protocol
class C1:
    x: int
    y: str
class P1(Protocol, C1):  # E: If `Protocol` is included as a base class, all other bases must be protocols
    x: int
class P2(Protocol):
    x: int
class P3(Protocol, P2):
    y: str
 "#,
);

testcase!(
    test_callable_protocol,
    r#"
from typing import Callable, Protocol
class P(Protocol):
    def __call__(self, x: int) -> str: ...
def f1(x: int) -> str: ...
def f2(x: str) -> str: ...

p1: P = f1
p2: P = f2  # E: `(x: str) -> str` is not assignable to `P`

def g(p: P) -> None:
    c1: Callable[[int], str] = p
    c2: Callable[[str], str] = p  # E: `P` is not assignable to `(str) -> str`
 "#,
);

testcase!(
    test_protocol_variance,
    r#"
from typing import Protocol
# read-write attributes
class P1(Protocol):
    x: int
class P2(Protocol):
    x: object
# read-only properties
class P3(Protocol):
    @property
    def x(self) -> int: ...
class P4(Protocol):
    @property
    def x(self) -> object: ...
def f(p1: P1, p2: P2, p3: P3, p4: P4):
    # read-write attributes are invariant
    x1: P1 = p2  # E: `P2` is not assignable to `P1`
    x2: P2 = p1  # E: `P1` is not assignable to `P2`
    # properties are covariant w/ the getter/setter types
    x3: P3 = p4  # E: `P4` is not assignable to `P3`
    x4: P4 = p3
    x5: P3 = p1
    x6: P3 = p2  # E: `P2` is not assignable to `P3`
    x7: P4 = p1
    x8: P4 = p2
 "#,
);

testcase!(
    test_protocol_attr_subtype,
    r#"
from typing import Protocol
class P1(Protocol):
    @property
    def x(self) -> int:
        return 1
    @x.setter
    def x(self, y: int) -> None:
        pass
class P2(Protocol):
    x: int
class P3(Protocol):
    @property
    def x(self) -> int:
        return 1
class P4(Protocol):
    @property
    def x(self) -> int:
        return 1
    @x.setter
    def x(self, y: object) -> None:
        pass
class P5(Protocol):
    @property
    def x(self) -> int:
        return 1
    @x.setter
    def x(self, y: str) -> None:
        pass
class ExtendsInt(int): ...
class P6(Protocol):
    @property
    def x(self) -> int:
        return 1
    @x.setter
    def x(self, y: ExtendsInt) -> None:
        pass
def f(p1: P1, p2: P2, p3: P3, p4: P4):
    x1: P1 = p2
    # read-only properties can't be used as read-write properties
    x2: P1 = p3  # E: `P3` is not assignable to `P1`
    # properties can't be used as regular attributes
    x3: P2 = p1  # E: `P1` is not assignable to `P2`
    x4: P2 = p3  # E: `P3` is not assignable to `P2`
    x5: P3 = p1
    x6: P3 = p2
    # setter type compatibility
    x7: P4 = p2
    x8: P5 = p2  # E: `P2` is not assignable to `P5`
    x9: P6 = p2  # E: `P2` is not assignable to `P6`
"#,
);

testcase!(
    test_generic_protocol,
    r#"
from typing import Protocol, TypeVar
T = TypeVar("T")
class P(Protocol[T]):
   x: T
class C1:
   x: int
   y: str
class C2:
   x: str
   y: str
def f(proto: P[str]) -> None: ...
def g(c1: C1, c2: C2) -> None:
    f(c1)  # E: Argument `C1` is not assignable to parameter `proto` with type `P[str]`
    f(c2)
"#,
);

testcase!(
    test_protocol_property,
    r#"
from typing import Protocol
class P1(Protocol):
    @property
    def foo(self) -> object:
        return 1
class C1:
    @property
    def foo(self) -> int:
        return 1 
a: P1 = C1()

class P2(Protocol):
    @property
    def foo(self) -> int:
        return 1
class C2:
    @property
    def foo(self) -> object:
        return 1 
b: P2 = C2()  # E: `C2` is not assignable to `P2`

class P3(Protocol):
    @property
    def foo(self) -> object:
        return 1
    @foo.setter
    def foo(self, val: object) -> None:
        pass
class C3:
    @property
    def foo(self) -> int:
        return 1 
    @foo.setter
    def foo(self, val: int) -> None:
        pass
c: P3 = C3()  # E: `C3` is not assignable to `P3`

class P4(Protocol):
    @property
    def foo(self) -> object:
        return 1
    @foo.setter
    def foo(self, val: int) -> None:
        pass
class C4:
    @property
    def foo(self) -> int:
        return 1 
    @foo.setter
    def foo(self, val: object) -> None:
        pass
d: P4 = C4()

class P5(Protocol):
    @property
    def foo(self) -> object:
        return 1
class C5:
    @property
    def foo(self) -> int:
        return 1 
    @foo.setter
    def foo(self, val: object) -> None:
        pass
e: P5 = C5()

class P6(Protocol):
    @property
    def foo(self) -> object:
        return 1
    @foo.setter
    def foo(self, val: object) -> None:
        pass
class C6:
    @property
    def foo(self) -> int:
        return 1
f: P6 = C6()  # E: `C6` is not assignable to `P6`
"#,
);

testcase!(
    test_protocol_overload,
    r#"
from typing import Protocol, overload

class P(Protocol):
    @overload
    def foo(self, x: int) -> int: ...
    @overload
    def foo(self, x: str) -> str: ...

class C1:
    @overload
    def foo(self, x: int) -> int: ...
    @overload
    def foo(self, x: str) -> str: ...
    def foo(self, x: int | str) -> int | str:
        return x

x1: P = C1() # OK
"#,
);

testcase!(
    test_hashable,
    r#"
from typing import ClassVar, Hashable
class A:
    pass
class B:
    __hash__: ClassVar[None]
def f(x: Hashable):
    pass
f(A())
f(B())  # E: Argument `B` is not assignable to parameter `x` with type `Hashable`
    "#,
);

testcase!(
    test_protocol_getattr,
    r#"
from typing import Protocol
class P(Protocol):
    x: int
def f(proto: P) -> None: ...

class C:
    def __getattr__(self, name: str) -> int: ...

f(C()) # E: Argument `C` is not assignable to parameter `proto` with type `P`
    "#,
);

testcase!(
    bug = "The conformance tests require that we accept this, and mypy and pyright do so, but it is unsound. Consider emitting an error.",
    test_self_param,
    r#"
from typing import Protocol, Self
class P(Protocol):
    def f(self, x: Self):
        pass
class C:
    def f(self, x: Self):
        pass
def f(x: P):
    pass
f(C())
    "#,
);
