/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_generic_bounds,
    r#"
class A: ...
class B(A): ...
class C(B): ...

def test[T: B](x: T) -> None:
    a: A = x  # OK
    b: B = x  # OK
    c: C = x  # E: `?T` is not assignable to `C`

test(A())  # E: Argument `A` is not assignable to parameter `x` with type `@_` in function `test`
test(B())
test(C())
 "#,
);

testcase!(
    test_generic_constraints,
    r#"
class A: ...
class B(A): ...
class C(A): ...
class D(C): ...

def test[T: (B, C)](x: T) -> None:
    a: A = x  # OK
    b: B = x  # E: `?T` is not assignable to `B`
    c: C = x  # E: `?T` is not assignable to `C`

test(A())  # E: Argument `A` is not assignable to parameter `x` with type `@_` in function `test`
test(B())
test(C())
test(D())
 "#,
);
