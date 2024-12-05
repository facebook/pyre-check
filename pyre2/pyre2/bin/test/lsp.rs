/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use itertools::Itertools;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use crate::module::module_name::ModuleName;
use crate::state::state::State;
use crate::test::util::simple_test_driver;
use crate::test::util::TestEnv;

static CODE: &str = r#"
1: from typing import Literal
2:
3: def f(x: list[int], y: str, z: Literal[42]):
4:     return x
5:
6: yyy = f([1, 2, 3], "test", 42)
7: zzz: int = 1
8:
9: class A: pass
10: class B(A): pass
"#;

fn mk_state() -> (ModuleName, State<'static>) {
    let code = CODE
        .lines()
        .enumerate()
        .skip(1)
        .map(|(i, x)| {
            let (line_no, line) = x.split_once(':').unwrap();
            assert_eq!(line_no, i.to_string());
            if line.is_empty() { line } else { &line[1..] }
        })
        .join("\n");
    let state = simple_test_driver(TestEnv::one("main", &code));
    assert_eq!(state.count_errors(), 0);
    (ModuleName::from_str("main"), state)
}

/// Find the TextRange of the given needle on the line, but must occur
fn at_after(line_no: usize, before: &str, needle: &str) -> TextRange {
    let (module, state) = mk_state();
    let info = state.get_module_info(module).unwrap();
    let line = info.contents().lines().nth(line_no - 1).unwrap();
    let (pre1, post1) = line.split_once(before).unwrap();
    let (pre2, _) = post1.split_once(needle).unwrap();
    let start_col = pre1.len() + before.len() + pre2.len();
    let start = info.to_text_size((line_no - 1) as u32, start_col as u32);
    TextRange::new(start, start + TextSize::new(needle.len() as u32))
}

fn at(line_no: usize, needle: &str) -> TextRange {
    at_after(line_no, "", needle)
}

#[test]
fn test_hover() {
    let (module, state) = mk_state();
    assert_eq!(
        state.hover(module, at(4, "x").start()).unwrap().to_string(),
        "list[int]"
    );
    assert_eq!(
        state.hover(module, at(4, "x").end()).unwrap().to_string(),
        "list[int]"
    );
    assert_eq!(
        state.hover(module, at(6, "f").end()).unwrap().to_string(),
        "Callable[[list[int], str, Literal[42]], list[int]]"
    );
}

#[test]
fn test_inlay_hint() {
    let (module, state) = mk_state();
    assert_eq!(
        state.inlay_hints(module).unwrap(),
        vec![
            (at(3, ")").end(), " -> list[int]".to_owned()),
            (at(6, "yyy").end(), ": list[int]".to_owned()),
        ]
    );
}

#[test]
fn test_goto_definition() {
    let (module, state) = mk_state();
    assert_eq!(
        state.goto_definition(module, at(6, "f").start()),
        Some((module, at_after(3, "def", "f")))
    );
    assert_eq!(
        state.goto_definition(module, at(3, "Liter").end()),
        Some((module, at(1, "Literal")))
    );
    assert_eq!(
        state.goto_definition(module, at(10, "A").start()),
        Some((module, at_after(9, "class", "A")))
    );
}
