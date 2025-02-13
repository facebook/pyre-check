/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase_with_bug;

testcase_with_bug!(
    "We don't understand super()",
    test_class_super,
    r#"
class A:
    def m(self) -> int:
        return 0

class B(A):
    def m(self) -> int:
        return super().m() # E: Object of class `super` has no attribute `m`
"#,
);
