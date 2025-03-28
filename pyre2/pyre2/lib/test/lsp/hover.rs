/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pretty_assertions::assert_eq;
use ruff_text_size::TextSize;

use crate::state::handle::Handle;
use crate::state::state::State;
use crate::test::util::get_batched_lsp_operations_report;

fn get_test_report(state: &State, handle: &Handle, position: TextSize) -> String {
    if let Some(t) = state.transaction().hover(handle, position) {
        format!("Hover Result: `{}`", t)
    } else {
        "Hover Result: None".to_owned()
    }
}

#[test]
fn basic_test() {
    let code = r#"
from typing import Literal
#        ^
def f(x: list[int], y: str, z: Literal[42]):
#   ^               ^       ^
    return x
#          ^
yyy = f([1, 2, 3], "test", 42)
#     ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
2 | from typing import Literal
             ^
Hover Result: `Module[typing]`

4 | def f(x: list[int], y: str, z: Literal[42]):
        ^
Hover Result: `(x: list[int], y: str, z: Literal[42]) -> list[int]`

4 | def f(x: list[int], y: str, z: Literal[42]):
                        ^
Hover Result: `str`

4 | def f(x: list[int], y: str, z: Literal[42]):
                                ^
Hover Result: `Literal[42]`

6 |     return x
               ^
Hover Result: `list[int]`

8 | yyy = f([1, 2, 3], "test", 42)
          ^
Hover Result: `(x: list[int], y: str, z: Literal[42]) -> list[int]`
"#
        .trim(),
        report.trim(),
    );
}
#[test]
fn import_test() {
    let code = r#"
import typing
#        ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
2 | import typing
             ^
Hover Result: `Module[typing]`
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn duplicate_import_test() {
    let code = r#"
from typing import List
import typing
#        ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    // todo(kylei): The result should be `Module[typing]`
    assert_eq!(
        r#"
# main.py
3 | import typing
             ^
Hover Result: None
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn dead_code_tests() {
    let code = r#"
if 1 == 0:
  def f():
  #   ^
      pass

  x = 3
# ^
  x
# ^
  f
# ^
if False:
  def f():
  #   ^
      pass

  x = 3
# ^
  x
# ^
  f
# ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
3 |   def f():
          ^
Hover Result: None

7 |   x = 3
      ^
Hover Result: None

9 |   x
      ^
Hover Result: None

11 |   f
       ^
Hover Result: None

14 |   def f():
           ^
Hover Result: None

18 |   x = 3
       ^
Hover Result: None

20 |   x
       ^
Hover Result: None

22 |   f
       ^
Hover Result: None
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn attribute_tests() {
    let code = r#"
class MyClass:
  x = 5
c1 = MyClass()
c1.x
#  ^

class MyClassWithImplicitField:
  def __init__(self, name: str):
    self.name = name

c2 = MyClassWithImplicitField("")
c2.name
#  ^

class ExtendsMyClass(MyClass):
  y = 6
c3 = ExtendsMyClass()
c3.x
#  ^
c3.y
#  ^

class Union1:
  x = 5

class Union2:
  x = 6

c4: Union1 | Union2 = Union1()
c4.x
#  ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
5 | c1.x
       ^
Hover Result: `int`

13 | c2.name
        ^
Hover Result: `str`

19 | c3.x
        ^
Hover Result: `int`

21 | c3.y
        ^
Hover Result: `int`

31 | c4.x
        ^
Hover Result: `int`
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn var_expansion_test() {
    let code = r#"
x = 5
while True:
  x = x + 1
y = x
#   ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
5 | y = x
        ^
Hover Result: `Literal[5] | Unknown`
"#
        .trim(),
        report.trim(),
    );
}
