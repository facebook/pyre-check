/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_new_type_simple,
    r#"
from typing import NewType, assert_type

UserId = NewType("UserId", int)
UserId("user")  # E: EXPECTED Literal['user'] <: int 
u1: UserId = 42 # E: EXPECTED Literal[42] <: UserId
u2: UserId = UserId(42) 

assert_type(UserId(5) + 1, int)
     "#,
);

testcase!(
    test_new_type_naming,
    r#"
from typing import NewType 

GoodName = NewType("BadName", int) # E: Expected string literal "GoodName"  

GoodNewType1 = NewType("GoodNewType1", list)  

GoodNewType2 = NewType("GoodNewType2", GoodNewType1) 

nt1: GoodNewType1[int] # E: Expected 0 type arguments for class `GoodNewType1`, got 1.

     "#,
);

testcase!(
    test_new_type_wrong_arity,
    r#"
from typing import NewType 
UserId = NewType("UserId", int, int) # E: Expected 2 positional arguments, got 3
UserId = NewType("UserId") # E: Missing argument `tp`
userId = NewType() # E: Missing argument `name` # E: Missing argument `tp`
     "#,
);
