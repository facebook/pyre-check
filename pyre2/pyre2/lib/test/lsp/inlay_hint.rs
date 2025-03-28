/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use pretty_assertions::assert_eq;

use crate::test::util::code_frame_of_source_at_position;
use crate::test::util::mk_multi_file_state_assert_no_errors;

#[test]
fn basic_test() {
    let code = r#"from typing import Literal

def f(x: list[int], y: str, z: Literal[42]):
    return x

yyy = f([1, 2, 3], "test", 42)

def g() -> int:
    return 42
"#;
    let files = [("main", code)];
    let (handles, state) = mk_multi_file_state_assert_no_errors(&files);
    let mut report = String::new();
    for (name, code) in &files {
        report.push_str("# ");
        report.push_str(name);
        report.push_str(".py\n");
        let handle = handles.get(name).unwrap();
        for (pos, hint) in state.transaction().inlay_hints(handle).unwrap() {
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
