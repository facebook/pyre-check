/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_def,
    r#"
from typing import assert_type
import dataclasses
@dataclasses.dataclass
class Data:
    x: int
    y: str
assert_type(Data, type[Data])
    "#,
);

testcase!(
    test_fields,
    r#"
from typing import assert_type
import dataclasses
@dataclasses.dataclass
class Data:
    x: int
    y: str
def f(d: Data):
    assert_type(d.x, int)
    assert_type(d.y, str)
    "#,
);

testcase!(
    test_generic,
    r#"
from typing import assert_type
import dataclasses
@dataclasses.dataclass
class Data[T]:
    x: T
def f(d: Data[int]):
    assert_type(d.x, int)
    "#,
);

testcase_with_bug!(
    "Need to support dataclass-generated __init__",
    test_construction,
    r#"
import dataclasses
@dataclasses.dataclass
class Data:
    x: int
    y: str
Data(0, "1")  # Should be ok  # E: Expected 0 positional arguments
Data(0, 1)  # Should be an arg type mismatch  # E: Expected 0 positional arguments
    "#,
);
