/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase_with_bug;

testcase_with_bug!(
    "Simple function decorators should be evaluated",
    test_simple_function_decorator,
    r#"
from typing import assert_type, Callable, Any

def decorator(f: Callable[[int], int]) -> int: ...

@decorator
def decorated(x: int) -> int:
   return x

# Oops: should be int
assert_type(decorated, int)  # E: assert_type(Any, int)
    "#,
);
