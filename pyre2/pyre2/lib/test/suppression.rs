/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_pyrefly_suppression,
    r#"
def foo() -> str:
  # pyrefly: ignore
  return 1
"#,
);
