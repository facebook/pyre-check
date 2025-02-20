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
    format!("Hover Result: `{}`", state.hover(handle, position).unwrap())
}

#[test]
fn basic_test() {
    let code = r#"from typing import Literal
 
def f(x: list[int], y: str, z: Literal[42]):
    return x
#          ^
yyy = f([1, 2, 3], "test", 42)
#     ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
4 |     return x
               ^
Hover Result: `list[int]`

6 | yyy = f([1, 2, 3], "test", 42)
          ^
Hover Result: `(x: list[int], y: str, z: Literal[42]) -> list[int]`
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
