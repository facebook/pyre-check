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

# TODO(stroxler): we should detect a type error here
class A(foo=f(15)): pass
"#,
);
