/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_type_var_tuple,
    r#"
from typing import TypeVarTuple, Generic
Ts = TypeVarTuple('Ts')
class Array(Generic[*Ts]): ...
def foo(*args: *Ts): ...
"#,
);

testcase_with_bug!(
    "We should disallow star-unpacking in invalid contexts",
    test_invalid_star,
    r#"
from typing import TypeVarTuple, Generic
Ts = TypeVarTuple('Ts')
*Ts
"#,
);
