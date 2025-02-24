/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase!(
    test_override_any,
    r#"
from typing import override, Any
 
class ParentB(Any):
    pass


class ChildB(ParentB):
    @override
    def method1(self) -> None:
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


class B(A):

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

testcase!(
    test_no_base_override,
    r#"
from typing import override

class A:
    def method1(self) -> int:
        return 1


class B(A):
    @override
    def method2(self) -> int: # E: Class member `method2` is marked as an override, but no parent class has a matching attribute
        return 1
 "#,
);

testcase!(
    test_default_value_consistent,
    r#"
class A:
    x: int

class B(A):
    def __init__(self) -> None:
        self.x = 0
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

testcase!(
    test_override_decorators,
    r#"
from typing import override

class ParentA:
    pass

class ChildA(ParentA):
    @staticmethod
    @override
    def static_method1() -> int: # E: Class member `static_method1` is marked as an override, but no parent class has a matching attribute
        return 1

    @classmethod
    @override
    def class_method1(cls) -> int: # E: Class member `class_method1` is marked as an override, but no parent class has a matching attribute
        return 1

    @property
    @override
    def property1(self) -> int: # E: Class member `property1` is marked as an override, but no parent class has a matching attribute
        return 1
    
 "#,
);

testcase!(
    test_override_decorators_switched,
    r#"
from typing import override

class ParentA:
    pass

class ChildA(ParentA):
    @override
    @staticmethod
    def static_method1() -> int: # E: Class member `static_method1` is marked as an override, but no parent class has a matching attribute
        return 1
    
 "#,
);

testcase_with_bug!(
    "TODO: Handle custom wrappers",
    test_override_custom_wrapper,
    r#"

from typing import Any, Callable, override

def wrapper(func: Callable[..., Any], /) -> Any:
    def wrapped(*args: Any, **kwargs: Any) -> Any:
        raise NotImplementedError # E: Could not find name `NotImplementedError`

    return wrapped


class ParentA:

    @staticmethod
    def static_method1() -> int:
        return 1

class ChildA(ParentA):

    @wrapper # E: EXPECTED override[staticmethod[() -> bool]] <: (...) -> Any
    @override
    @staticmethod
    def static_method1() -> bool: 
        return 1 # E: EXPECTED Literal[1] <: bool
    
 "#,
);

testcase_with_bug!(
    "TODO: is attr subset should be edited to potentially strip/ignore override decorators from the type.
    after this is accomplished, we can remove the duplicate code that strips the decorator when calculating 
    fields",
    test_override_duplicate_decorator,
    r#"

from typing import  override

class ParentA:

    @staticmethod
    def static_method1() -> int:
        return 1

class ChildA(ParentA):

    @staticmethod
    @override
    @staticmethod
    def static_method1() -> int: # E: Class member `static_method1` overrides parent class `ParentA` in an inconsistent manner
        return 1
    
 "#,
);

testcase_with_bug!(
    "TODO: method4 should be marked as an error since it doesn't exist in the parent class",
    test_overload_override_error,
    r#"

from typing import overload, override

class ParentA:
    ...

class ChildA(ParentA):
    @overload
    def method4(self, x: int) -> int:
        ...

    @overload
    def method4(self, x: str) -> str:
        ...

    @override
    def method4(self, x: int | str) -> int | str: 
        return 0
 "#,
);
