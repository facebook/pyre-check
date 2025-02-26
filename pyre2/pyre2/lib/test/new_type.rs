/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase_with_bug!(
    "TODO: handle NewType",
    test_new_type_simple,
    r#"
from typing import NewType, assert_type

UserId = NewType("UserId", int)

UserId("user") 
u1: UserId = 42 # E: Expected a type form, got instance of `NewType`
u2: UserId = UserId(42) # E: Expected a type form, got instance of `NewType`

assert_type(UserId(5) + 1, int) 
     "#,
);

testcase_with_bug!(
    "TODO: support NewType. Type name must match variable name so first assignment should fail. 
    Also cannot be generic. We might want to improve the error message here.",
    test_new_type_naming,
    r#"
from typing import NewType 

GoodName = NewType("BadName", int)  

GoodNewType1 = NewType("GoodNewType1", list)  

GoodNewType2 = NewType("GoodNewType2", GoodNewType1) 

nt1: GoodNewType1[int] # E: `NewType` has no attribute `__getitem__`

     "#,
);

testcase!(
    test_new_type_wrong_arity,
    r#"
from typing import NewType 
UserId = NewType("UserId", int, int) # E: Expected 2 positional arguments, got 3
     "#,
);
