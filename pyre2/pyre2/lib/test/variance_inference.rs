/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    bug = "T is only used in covariant positions so it should be inferred as covariant",
    test_covariance_inference,
    r#"
from typing import Sequence, reveal_type
def id[T](x: Sequence[T]) -> Sequence[T]:
    return x
def test(x: Sequence[int] | Sequence[str]):
    reveal_type(id(x))  # E: revealed type: Sequence[int] | int  # E: Argument `Sequence[int] | Sequence[str]` is not assignable to parameter `x` with type `Sequence[int]` in function `id`
"#,
);
