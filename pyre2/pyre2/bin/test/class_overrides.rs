/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase_with_bug!(
    "todo: parent can derive from Any. This program is well-typed. Add decorator to typeshed",
    test_override_any,
    r#"
from typing import override, Any
 
class ParentB(Any):
    pass


class ChildB(ParentB):
    @override
    def method1(self) -> None:  # OK
        pass        
 "#,
);

testcase!(
    test_override_basic_method,
    r#"
 
class A:
    def f(self, x:str, y:str) -> str:
        return x + y

class B(A):
    def f(self, x:int, y:int) -> int: # E: Class member `f` overrides parent class `A` in an inconsistent manner
        return x + y        
 "#,
);

testcase!(
    test_override_basic_field,
    r#"
class A:
    x: int
    y: bool
    z: bool

class B(A):
    pass

class C(B):
    x: int
    y: str # E: Class member `y` overrides parent class `B` in an inconsistent manner
 "#,
);

testcase!(
    test_overload_override,
    r#"
from typing import overload

class A:
    @overload
    def method(self, x: int) -> int:
        ...

    @overload
    def method(self, x: str) -> str:
        ...

    def method(self, x: int | str) -> int | str:
        return 0        
 "#,
);

testcase_with_bug!(
    "Todo: should raise an error because method does not exist in base class",
    test_no_base_override,
    r#"
from typing import override

class A:
    def method1(self) -> int:
        return 1


class B(A):
    @override
    def method2(self) -> int:  
        return 1
 "#,
);

testcase_with_bug!(
    "Todo: consistent override",
    test_default_value_consistent,
    r#"
class A:
    x: int

class B(A):
    def __init__(self) -> None:
        self.x = 0 # E:  Class member `x` overrides parent class `A` in an inconsistent manner
 "#,
);

testcase!(
    test_default_value_inconsistent,
    r#"
class A:
    x: str

class B(A):
    def __init__(self) -> None:
        self.x = 0 # E: Class member `x` overrides parent class `A` in an inconsistent manner
 "#,
);
