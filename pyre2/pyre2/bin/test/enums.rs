#![warn(dead_code)]

use crate::simple_test;

simple_test!(
    test_enum,
    r#"
from typing import assert_type, Literal
from enum import Enum

class MyEnum(Enum):
    X = 1
    Y = 2

assert_type(MyEnum.X, Literal[MyEnum.X])
"#,
);
