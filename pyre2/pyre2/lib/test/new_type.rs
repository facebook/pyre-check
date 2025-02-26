/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase_with_bug;

testcase_with_bug!(
    "TODO: fix NewType initialization",
    test_new_type_simple,
    r#"
from typing import NewType, assert_type

UserId = NewType("UserId", int)
UserId("user") # E: Expected 0 positional arguments, got 1 
u1: UserId = 42 # E: EXPECTED Literal[42] <: UserId
u2: UserId = UserId(42) # E: Expected 0 positional arguments, got 1

assert_type(UserId(5) + 1, int) # E: Expected 0 positional arguments, got 1
     "#,
);

testcase_with_bug!(
    "TODO: fix NewType initialization",
    test_new_type_naming,
    r#"
from typing import NewType 

GoodName = NewType("BadName", int) # E: Expected string literal "GoodName"  

GoodNewType1 = NewType("GoodNewType1", list)  

GoodNewType2 = NewType("GoodNewType2", GoodNewType1) 

nt1: GoodNewType1[int] # E: Expected 0 type arguments for class `GoodNewType1`, got 1.

     "#,
);

testcase_with_bug!(
    "TODO: determine what binding to generate for the LHS of a NewType declaration name NewType has the wrong arguments",
    test_new_type_wrong_arity,
    r#"
from typing import NewType 
UserId = NewType("UserId", int, int) # E: NewType expects 2 args. Got `3`. # E: Could not find flow binding for `UserId`
UserId = NewType("UserId") # E: NewType expects 2 args. Got `1`.
userId = NewType() # E: NewType expects 2 args. Got `0`. # E: Could not find flow binding for `userId 
     "#,
);
