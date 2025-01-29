/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase_with_bug;

testcase_with_bug!(
    "Support for classmethod is incomplete",
    test_classmethod,
    r#"
from typing import reveal_type
class C:
    @classmethod
    def foo(cls) -> int:
        return 42
def f(c: C):
    reveal_type(C.foo)  # E: revealed type: classmethod[(cls: C) -> int]
    reveal_type(c.foo)  # E: revealed type: classmethod[(cls: C) -> int]
    "#,
);
