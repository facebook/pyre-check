/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::simple_test;

simple_test!(
    test_primitive_subtyping,
    r#"
class A: pass
class B(A): pass
class C(B): pass
class D: pass

b: A = B()
c: A = C()
oops: A = D()  # E: EXPECTED D <: A
"#,
);

simple_test!(
    test_object_is_top,
    r#"
class A: pass

a: object = A()
s: object = ""
"#,
);

simple_test!(
    test_simple_generic_subtyping,
    r#"
class A[T]: pass
class B[T](A[T]): pass
class C(B[int]): pass
class D[T]: pass

b: A[int] = B[int]()
c: A[int] = C()
oops0: A[int] = D[int]()  # E: EXPECTED D[int] <: A[int]
oops1: A[int] = A[str]()  # E: EXPECTED A[str] <: A[int]
"#,
);

simple_test!(
    test_simple_class_object_subtyping,
    r#"
class A: pass
class B(A): pass

a: type[A] = B
b: type[B] = A  # E: EXPECTED type[A] <: type[B]
"#,
);

simple_test!(
    test_generic_class_object_subtyping,
    r#"
class A[T]: pass
class B[T](A[T]): pass

a0: type[A] = B
b0: type[B] = A  # E: EXPECTED type[A] <: type[B[Error]]

a1: type[A[int]] = B
b1: type[B[int]] = A  # E: EXPECTED type[A] <: type[B[int]]

a2: type[A] = B[int]
b2: type[B] = A[int]  # E: EXPECTED type[A[int]] <: type[B[Error]]
"#,
);

simple_test!(
    test_literal_string_subtyping,
    r#"
from typing import Literal, LiteralString

def l0() -> Literal["foo"]: ...
def l1() -> Literal["foo"] | Literal["bar"]: ...
def l2() -> LiteralString: ...
def l3() -> LiteralString | Literal["foo"]: ...
def l4() -> str: ...

test0: LiteralString = l0()
test1: LiteralString = l1()
test2: str = l0()
test3: str = l1()
test4: str = l2()
test5: Literal["foo"] = l2()  # E: LiteralString <: Literal['foo']
test6: str = l3()
test7: LiteralString = l4()  # E: str <: LiteralString

test10: object = l0()
test11: object = l3()
"#,
);
