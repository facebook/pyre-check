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
    y: int # E: Class member `y` overrides parent class `P` in an inconsistent manner
class C5:
    x: int
    y: int
def f(proto: P) -> None: ...
def g(p: P, c1: C1, c2: C2, c3: C3, c4: C4, c5: C5) -> None:
    f(c1)
    f(c2)  # E: EXPECTED C2 <: P
    f(c3)
    f(c4)
    f(c5)  # E: EXPECTED C5 <: P
    c: C1 = p  # E: EXPECTED P <: C1
 "#,
);

testcase!(
    test_protocol_base,
    r#"
from typing import Protocol
class C1:
    x: int
    y: str
class P1(Protocol, C1):  # E: If `Protocol` is included as a base class, all other bases must be protocols.
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
p2: P = f2  # E: EXPECTED (x: str) -> str <: P

def g(p: P) -> None:
    c1: Callable[[int], str] = p
    c2: Callable[[str], str] = p  # E: EXPECTED P <: (str) -> str
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
    x1: P1 = p2  # E: EXPECTED P2 <: P1
    x2: P2 = p1  # E: EXPECTED P1 <: P2
    # properties are covariant w/ the getter/setter types
    x3: P3 = p4  # E: EXPECTED P4 <: P3
    x4: P4 = p3
    x5: P3 = p1
    x6: P3 = p2  # E: EXPECTED P2 <: P3
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
def f(p1: P1, p2: P2, p3: P3):
    x1: P1 = p2
    # read-only properties can't be used as read-write properties
    x2: P1 = p3  # E: EXPECTED P3 <: P1
    # properties can't be used as regular attributes
    x3: P2 = p1  # E: EXPECTED P1 <: P2
    x4: P2 = p3  # E: EXPECTED P3 <: P2
    x5: P3 = p1
    x5: P3 = p2
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
    f(c1)  # E: EXPECTED C1 <: P[str]
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
b: P2 = C2()  # E: EXPECTED C2 <: P2

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
c: P3 = C3()  # E: EXPECTED C3 <: P3

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
f: P6 = C6()  # E: EXPECTED C6 <: P6
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
