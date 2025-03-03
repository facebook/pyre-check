/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_class_super_no_args,
    r#"
from typing import assert_type

class A:
    def m(self) -> int:
        return 0

class B(A):
    def m(self) -> int:
        return super().m()

class C(A):
    def m(self) -> bool:
        return True
    def f(self):
        assert_type(super().m(), int)
"#,
);

testcase!(
    test_class_super_with_args,
    r#"
from typing import assert_type

class A:
    def f(self) -> int:
        return 0

class B:
    def f(self) -> bool:
        return True

class C(B, A):
    def g(self):
        assert_type(super(C, self).f(), bool)
        assert_type(super(B, self).f(), int)
    "#,
);

testcase_with_bug!(
    "Demonstration of a limitation of our super() implementation",
    test_inherit_method_with_super,
    r#"
from typing import assert_type

class A:
    def f(self) -> int:
        return 0
class B(A):
    def f(self):
        return super().f()
class C:
    def f(self) -> bool:
        return True
class D(B, C, A):
    pass

# At runtime, the super() call in B.f is evaluated with D as the starting class, so that C.f is called.
# We can't do this statically without re-analyzing the body of B.f (too expensive).
assert_type(D().f(), bool)  # E: assert_type(int, bool)
    "#,
);

testcase!(
    test_bad_args,
    r#"
super(1, 2, 3)  # E: `super` takes at most 2 arguments, got 3

class Unrelated:
    pass

class C:
    def f1(self):
        super(C, self, oops=42)  # E: `super` got an unexpected keyword argument `oops`
    def f2(self):
        super(42, self)  # E: Expected first argument to `super` to be a class object, got `Literal[42]`
    def f3(self):
        super(C, int | str)  # E: Expected second argument to `super` to be a class instance, got `type[int | str]`
    def f4(self):
        super(Unrelated, self)  # E: Illegal `super(type[Unrelated], C)` call: `C` is not an instance or subclass of `type[Unrelated]`
    "#,
);

testcase!(
    test_super_object,
    r#"
# Trying to call super() on `object` is a weird thing to do (although the Python runtime allows it).
# Either accepting this or producing a good error message would be ok.
class C:
    def f(self):
        super(object, self)
    "#,
);

testcase!(
    test_super_alias,
    r#"
_super = super
class C:
    def f(self):
        _super(C, self)
    "#,
);

testcase!(
    test_illegal_location,
    r#"
class A:
    pass

# This is unusual but legal
super(A, A())

# The following are runtime errors
super()  # E: `super` call with no arguments is valid only inside a method
class B:
    super()  # E: `super` call with no arguments is valid only inside a method
    "#,
);
