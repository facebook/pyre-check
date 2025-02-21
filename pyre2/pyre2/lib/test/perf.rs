/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

// At some point in the past, this test took many minutes and consumed 50Gb of RAM.
testcase!(
    test_quadratic,
    r#"
from typing import TypeVar
_T = TypeVar("_T")
def table() -> _T: ...
class Configs:
    def __init__(self):
        self.value = {
            "1": {
                "a": table(),
                "b": table(),
                "c": table(),
                "d": table(),
                "e": table(),
                "f": table(),
                "g": table(),
                "h": table(),
                "i": table(),
                "j": table(),
            },
            "2": {
                "a": table(),
                "b": table(),
                "c": table(),
                "d": table(),
                "e": table(),
                "f": table(),
                "g": table(),
                "h": table(),
                "i": table(),
            },
            "3": {},
        }
"#,
);
