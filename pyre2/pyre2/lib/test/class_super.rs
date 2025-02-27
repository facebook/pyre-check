/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase_with_bug;

testcase_with_bug!(
    "We don't understand super()",
    test_class_super_no_args,
    r#"
class A:
    def m(self) -> int:
        return 0

class B(A):
    def m(self) -> int:
        return super().m() # E: Object of class `super` has no attribute `m`
"#,
);

testcase_with_bug!(
    "We don't understand super()",
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
        assert_type(super(C, self).f(), bool)  # E: assert_type  # E: no attribute `f`
        assert_type(super(B, self).f(), int)  # E: assert_type  # E: no attribute `f`
    "#,
);

testcase_with_bug!(
    "We don't understand super()",
    test_inherit_method_with_super,
    r#"
from typing import assert_type

class A:
    def f(self) -> int:
        return 0
class B(A):
    def f(self):
        return super().f()  # E: Object of class `super` has no attribute `f`
class C:
    def f(self) -> bool:
        return True
class D(B, C, A):
    pass

# The super() call in B.f should be evaluated with D as the starting class, so that C.f is called
assert_type(D().f(), bool)  # E: assert_type
    "#,
);
