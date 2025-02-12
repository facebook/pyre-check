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

testcase_with_bug!(
    "Todo: Method f overrides class A in an incompatible manner",
    test_override_basic,
    r#"
 
class A:
    def f(self, x:str, y:str) -> str:
        return x + y

class B(A):
    def f(self, x:int, y:int) -> int:
        return x + y        
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
