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
assert_type(Data(x=0), Data[int])
Data[int](x=0)  # OK
Data[int](x="")  # E: EXPECTED Literal[''] <: int
    "#,
);

testcase!(
    test_construction,
    r#"
import dataclasses
@dataclasses.dataclass
class Data:
    x: int
    y: str
Data(0, "1")  # OK
Data(0, 1)  # E: EXPECTED Literal[1] <: str
    "#,
);

testcase!(
    test_inheritance,
    r#"
import dataclasses

@dataclasses.dataclass
class A:
    w: int

class B(A):
    x: str
# B is not decorated as a dataclass, so w is the only dataclass field
B(w=0)

@dataclasses.dataclass
class C(B):
    y: bytes
# C is decorated as a dataclass again, so w and y are the dataclass fields
C(w=0, y=b"1")

@dataclasses.dataclass
class D(C):
    z: float
# Make sure we get the parameters in the right order when there are multiple @dataclass bases
D(0, b"1", 2.0)
    "#,
);

testcase!(
    test_duplicate_field,
    r#"
import dataclasses
@dataclasses.dataclass
class A:
    x: int
    y: float
@dataclasses.dataclass
class B(A):
    x: str
# Overwriting x doesn't change the param order but does change its type
B('0', 1.0)  # OK
B(0, 1.0)  # E: EXPECTED Literal[0] <: str
    "#,
);

testcase!(
    test_inherit_from_multiple_dataclasses,
    r#"
import dataclasses
@dataclasses.dataclass
class A:
    x: int
@dataclasses.dataclass
class B:
    y: str

class C(B, A):
    pass
C(y="0")  # First base (B) wins

@dataclasses.dataclass
class D(B, A):
    z: float
D(0, "1", 2.0)
    "#,
);

testcase!(
    test_inherit_from_generic_dataclass,
    r#"
import dataclasses
@dataclasses.dataclass
class A[T]:
    x: T
@dataclasses.dataclass
class B(A[int]):
    y: str
B(x=0, y="1")  # OK
B(x="0", y="1")  # E: EXPECTED Literal['0'] <: int
    "#,
);

testcase!(
    test_decorate_with_call_return,
    r#"
from dataclasses import dataclass
@dataclass()
class C:
    x: int
C(x=0)  # OK
C(x='0')  # E: EXPECTED Literal['0'] <: int
    "#,
);

testcase!(
    test_init_already_defined,
    r#"
from dataclasses import dataclass
@dataclass
class C:
    x: int
    def __init__(self):
        self.x = 42
C()  # OK
C(x=0)  # E: Unexpected keyword argument
    "#,
);

testcase!(
    test_init_false,
    r#"
from dataclasses import dataclass
@dataclass(init=False)
class C:
    x: int = 0
C()  # OK
C(x=0)  # E: Unexpected keyword argument
    "#,
);

testcase_with_bug!(
    "TODO: consider erroring on unannotated attributes",
    test_unannotated_attribute,
    r#"
import dataclasses
@dataclasses.dataclass
class C:
    # Not annotating a field with value dataclasses.field(...) is a runtime error, so we should
    # probably error on this.
    x = dataclasses.field()
    # This is confusing and likely indicative of a programming error; consider erroring on this, too.
    y = 3
    "#,
);

testcase!(
    test_frozen,
    r#"
from dataclasses import dataclass
@dataclass
class C:
    x: int

@dataclass(frozen=True)
class D:
    x: int

def f(c: C, d: D):
    c.x = 0
    d.x = 0  # E: Could not assign to read-only field `x`
    "#,
);

testcase!(
    test_match_args,
    r#"
from typing import assert_type, Literal
from dataclasses import dataclass
@dataclass
class C_has_match_args_default:
    x: int
@dataclass(match_args=True)
class C_has_match_args_explicit:
    x: int
@dataclass(match_args=False)
class C_no_match_args:
    x: int
assert_type(C_has_match_args_default.__match_args__, tuple[Literal['x']])
assert_type(C_has_match_args_explicit.__match_args__, tuple[Literal['x']])
C_no_match_args.__match_args__ # E: no class attribute `__match_args__`
    "#,
);

testcase!(
    test_match_args_no_overwrite,
    r#"
from typing import assert_type
from dataclasses import dataclass
@dataclass(match_args=True)
class C:
    __match_args__ = ()
    x: int
assert_type(C.__match_args__, tuple[()])
    "#,
);

testcase!(
    test_kw_only_arg,
    r#"
from typing import assert_type
from dataclasses import dataclass
@dataclass(kw_only=True)
class C:
    x: int
C(x=0)  # OK
C(0)  # E: Expected 0 positional arguments
assert_type(C.__match_args__, tuple[()])
    "#,
);

testcase!(
    test_kw_only_sentinel,
    r#"
from typing import assert_type, Literal
import dataclasses
@dataclasses.dataclass
class C:
    x: int
    _: dataclasses.KW_ONLY
    y: str
C(0, y="1")  # OK
C(x=0, y="1")  # OK
C(0, "1")  # E: Expected 1 positional argument
assert_type(C.__match_args__, tuple[Literal["x"]])
    "#,
);

testcase!(
    test_order,
    r#"
from dataclasses import dataclass
@dataclass
class D1:
    x: int
def f(d: D1, e: D1):
    if d < e: ...  # E: `<` not supported between `D1` and `D1`
    if d == e: ...  # OK: `==` and `!=` never error regardless

@dataclass(order=True)
class D2:
    x: int
@dataclass(order=True)
class D3:
    x: int
def f(d: D2, e: D2, f: D3):
    if d < e: ...  # OK
    if e < f: ...  # E: EXPECTED D3 <: D2
    if e != f: ...  # OK: `==` and `!=` never error regardless
    "#,
);

testcase!(
    test_bad_keyword,
    r#"
from dataclasses import dataclass
@dataclass(flibbertigibbet=True)  # E: Unexpected keyword argument
class C:
    pass
    "#,
);

testcase_with_bug!(
    "TODO",
    test_dataclasses_field,
    r#"
from dataclasses import dataclass, field
@dataclass
class C:
    x: int = field(init=False)
    y: str
C(y="")  # Should be OK  # E: Missing argument
C(x=0, y="")  # Should be an error: Unexpected keyword argument `x`
    "#,
);
