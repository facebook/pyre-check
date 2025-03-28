/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use lsp_types::CompletionItem;
use pretty_assertions::assert_eq;
use ruff_text_size::TextSize;

use crate::state::handle::Handle;
use crate::state::state::State;
use crate::test::util::get_batched_lsp_operations_report_allow_error;

fn get_test_report(state: &State, handle: &Handle, position: TextSize) -> String {
    let mut report = "Completion Results:".to_owned();
    for CompletionItem {
        label,
        detail,
        kind,
        ..
    } in state.transaction().completion(handle, position)
    {
        report.push_str("\n- (");
        report.push_str(&format!("{:?}", kind.unwrap()));
        report.push_str(") ");
        report.push_str(&label);
        if let Some(detail) = detail {
            report.push_str(": ");
            report.push_str(&detail);
        }
    }
    report
}

#[test]
fn dot_complete_basic_test() {
    let code = r#"
class Foo:
    x: int
foo = Foo()
foo.
#   ^
class Bar(Foo):
    y: int
bar = Bar()
bar.
#   ^
"#;
    let report = get_batched_lsp_operations_report_allow_error(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
5 | foo.
        ^
Completion Results:
- (Field) x: int

10 | bar.
         ^
Completion Results:
- (Field) y: int
- (Field) x: int
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn dot_complete_rankded_test() {
    let code = r#"
class Foo:
    _private: bool
    __special__: str
    y: int
    x: int

foo = Foo()
foo.
#   ^
"#;
    let report = get_batched_lsp_operations_report_allow_error(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
9 | foo.
        ^
Completion Results:
- (Field) y: int
- (Field) x: int
- (Field) _private: bool
- (Field) __special__: str
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn variable_complete_basic_test() {
    let code = r#"
def foo():
  xxxx = 3
  x
# ^
  def bar():
    yyyy = 4;
    y
#   ^
"#;
    let report = get_batched_lsp_operations_report_allow_error(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
4 |   x
      ^
Completion Results:
- (Variable) xxxx: Literal[3]
- (Variable) bar: () -> None
- (Variable) foo: () -> None

8 |     y
        ^
Completion Results:
- (Variable) yyyy: Literal[4]
- (Variable) xxxx: Literal[3]
- (Variable) bar: () -> None
- (Variable) foo: () -> None
"#
        .trim(),
        report.trim(),
    );
}
