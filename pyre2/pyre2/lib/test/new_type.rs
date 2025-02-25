/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase_with_bug;

testcase_with_bug!(
    "TODO: support NewType",
    test_new_type_simple,
    r#"
from typing import NewType, assert_type # E: Could not import `NewType` from `typing`

UserId = NewType("UserId", int)

UserId("user") 
u1: UserId = 42  
u2: UserId = UserId(42)  

assert_type(UserId(5) + 1, int) # E: assert_type(Any, int) failed
     "#,
);

testcase_with_bug!(
    "TODO: support NewType. Type name must match variable name so first assignment should fail. 
    Also cannot be generic so last line should fail.",
    test_new_type_naming,
    r#"
from typing import NewType # E: Could not import `NewType` from `typing`

GoodName = NewType("BadName", int)  

GoodNewType1 = NewType("GoodNewType1", list)  

GoodNewType2 = NewType("GoodNewType2", GoodNewType1) 

nt1: GoodNewType1[int] 

     "#,
);
