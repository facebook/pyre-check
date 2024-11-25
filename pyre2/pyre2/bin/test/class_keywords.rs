/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::simple_test;

simple_test!(
    test_keywords,
    r#"
def f(x: bool) -> bool: ...

class A(foo=f(15)):  # E: EXPECTED Literal[15] <: bool
    pass
"#,
);
