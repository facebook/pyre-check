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
use crate::test::util::code_frame_of_source_at_range;
use crate::test::util::get_batched_lsp_operations_report;
use crate::test::util::get_batched_lsp_operations_report_allow_error;

fn get_test_report(state: &State, handle: &Handle, position: TextSize) -> String {
    if let Some((handle, range)) = state.goto_definition(handle, position) {
        let info = state.get_module_info(&handle).unwrap();
        format!(
            "Definition Result:\n{}",
            code_frame_of_source_at_range(info.contents(), range)
        )
    } else {
        "Definition Result: None".to_owned()
    }
}

#[test]
fn ignored_test() {
    let code = r#"
x = 1 # go-to-definition is unsupported for literals
#   ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
2 | x = 1 # go-to-definition is unsupported for literals
        ^
Definition Result: None
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn basic_test() {
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
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
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

#[test]
fn named_import_tests() {
    let code_import_provider: &str = r#"
from typing import Literal

def f(x: list[int], y: str, z: Literal[42]):
    return x
"#;
    let code_test: &str = r#"
from typing import Literal
from .import_provider import f
#                            ^

foo: Literal[1] = 1 # todo: ideally should jump through the import
#        ^  
bar = f([1], "", 42) # todo: ideally should jump through the import
#     ^
"#;

    let report = get_batched_lsp_operations_report(
        &[
            ("main", code_test),
            ("import_provider", code_import_provider),
        ],
        get_test_report,
    );
    assert_eq!(
        r#"
# main.py
3 | from .import_provider import f
                                 ^
Definition Result: None

6 | foo: Literal[1] = 1 # todo: ideally should jump through the import
             ^
Definition Result:
2 | from typing import Literal
                       ^^^^^^^

8 | bar = f([1], "", 42) # todo: ideally should jump through the import
          ^
Definition Result:
3 | from .import_provider import f
                                 ^


# import_provider.py    
"#
        .trim(),
        report.trim()
    );
}

#[test]
fn star_import_tests() {
    let code_import_provider: &str = r#"
def f():
    pass
"#;
    let code_test: &str = r#"
from .import_provider import *

bar = f() # should jump to definition in import_provider
#     ^
"#;

    let report = get_batched_lsp_operations_report(
        &[
            ("main", code_test),
            ("import_provider", code_import_provider),
        ],
        get_test_report,
    );
    assert_eq!(
        r#"
# main.py
4 | bar = f() # should jump to definition in import_provider
          ^
Definition Result:
2 | def f():
        ^


# import_provider.py    
"#
        .trim(),
        report.trim()
    );
}

#[test]
fn unresolved_named_import_test() {
    let code: &str = r#"
from .unresolved_import import f

bar = f()
#     ^
"#;

    let report = get_batched_lsp_operations_report_allow_error(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
4 | bar = f()
          ^
Definition Result:
2 | from .unresolved_import import f
                                   ^
"#
        .trim(),
        report.trim()
    );
}

#[test]
fn unresolved_star_import_test() {
    let code: &str = r#"
from .unresolved_import import *

bar = f()
#     ^
"#;

    let report = get_batched_lsp_operations_report_allow_error(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
4 | bar = f()
          ^
Definition Result: None

"#
        .trim(),
        report.trim()
    );
}

#[test]
fn multi_definition_test() {
    let code = r#"
if True:
    xxxx = 1
else:
    xxxx = 2
xxxx # it's reasonable to only return the first def, but also reasonable to return both defs
# ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
6 | xxxx # it's reasonable to only return the first def, but also reasonable to return both defs
      ^
Definition Result:
3 |     xxxx = 1
        ^^^^
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn untyped_param_test() {
    let code = r#"
def f(untyped):
  untyped
#    ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
3 |   untyped
         ^
Definition Result:
2 | def f(untyped):
          ^^^^^^^
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn global_reference_test() {
    let code = r#"
x = 3
def f(untyped):
  global x
  x
# ^
"#;
    let report = get_batched_lsp_operations_report_allow_error(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
5 |   x
      ^
Definition Result:
2 | x = 3
    ^
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn reassigned_test() {
    let code = r#"
xxxx = 1
xxxx = 2
xxxx # should jump to the most recent definition
# ^
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
4 | xxxx # should jump to the most recent definition
      ^
Definition Result:
3 | xxxx = 2
    ^^^^
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn generics_test() {
    let code = r#"
def f[T](input: T):
#     ^         ^
  pass
"#;
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
2 | def f[T](input: T):
          ^
Definition Result: None

2 | def f[T](input: T):
                    ^
Definition Result:
2 | def f[T](input: T):
          ^
"#
        .trim(),
        report.trim(),
    );
}

#[test]
fn property_test() {
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

dict = {"foo": '', "bar": 3}
dict["foo"]
#      ^
dict["bar"]
#      ^
"#;
    // TODO: property go-to-definition is unsupported
    let report = get_batched_lsp_operations_report(&[("main", code)], get_test_report);
    assert_eq!(
        r#"
# main.py
5 | c1.x
       ^
Definition Result: None

13 | c2.name
        ^
Definition Result: None

19 | c3.x
        ^
Definition Result: None

21 | c3.y
        ^
Definition Result: None

31 | c4.x
        ^
Definition Result: None

35 | dict["foo"]
            ^
Definition Result: None

37 | dict["bar"]
            ^
Definition Result: None
"#
        .trim(),
        report.trim(),
    );
}
