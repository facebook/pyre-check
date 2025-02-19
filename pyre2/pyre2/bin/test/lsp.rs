/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pretty_assertions::assert_eq;

use crate::test::util::code_frame_of_source_at_position;
use crate::test::util::code_frame_of_source_at_range;
use crate::test::util::get_batched_lsp_operations_report;
use crate::test::util::mk_multi_file_state_assert_no_errors;

#[test]
fn test_hover() {
    let code = r#"from typing import Literal

def f(x: list[int], y: str, z: Literal[42]):
    return x
#          ^
yyy = f([1, 2, 3], "test", 42)
#     ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], |state, handle, position| {
        format!("Hover Result: `{}`", state.hover(handle, position).unwrap())
    });
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
fn test_inlay_hint() {
    let code = r#"from typing import Literal

def f(x: list[int], y: str, z: Literal[42]):
    return x

yyy = f([1, 2, 3], "test", 42) 
"#;
    let files = [("main", code)];
    let (handles, state) = mk_multi_file_state_assert_no_errors(&files);
    let mut report = String::new();
    for (name, code) in &files {
        report.push_str("# ");
        report.push_str(name);
        report.push_str(".py\n");
        let handle = handles.get(name).unwrap();
        for (pos, hint) in state.inlay_hints(handle).unwrap() {
            report.push_str(&code_frame_of_source_at_position(code, pos));
            report.push_str(" inlay-hint: `");
            report.push_str(&hint);
            report.push_str("`\n\n");
        }
        report.push('\n');
    }
    assert_eq!(
        r#"
# main.py
3 | def f(x: list[int], y: str, z: Literal[42]):
                                               ^ inlay-hint: ` -> list[int]`

6 | yyy = f([1, 2, 3], "test", 42) 
       ^ inlay-hint: `: list[int]`
"#
        .trim(),
        report.trim()
    );
}

#[test]
fn test_goto_definition() {
    let code = r#"
from typing import Literal

def f(x: list[int], y: str, z: Literal[42]):
#   ^                           ^
    return x

yyy = f([1, 2, 3], "test", 42)
#     ^

class A: pass
class B(A): pass
#       ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], |state, handle, position| {
        if let Some((handle, range)) = state.goto_definition(handle, position) {
            let info = state.get_module_info(&handle).unwrap();
            format!(
                "Definition Result:\n{}",
                code_frame_of_source_at_range(info.contents(), range)
            )
        } else {
            "Definition Result: None".to_owned()
        }
    });
    assert_eq!(
        r#"
# main.py
4 | def f(x: list[int], y: str, z: Literal[42]):
        ^
Definition Result: None

4 | def f(x: list[int], y: str, z: Literal[42]):
                                    ^
Definition Result:
2 | from typing import Literal
                       ^^^^^^^

8 | yyy = f([1, 2, 3], "test", 42)
          ^
Definition Result:
4 | def f(x: list[int], y: str, z: Literal[42]):
        ^

12 | class B(A): pass
             ^
Definition Result:
11 | class A: pass
           ^
"#
        .trim(),
        report.trim(),
    );
}
