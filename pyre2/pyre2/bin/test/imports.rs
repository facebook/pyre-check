/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::simple_test;
use crate::test::util::TestEnv;

fn env_class_x() -> TestEnv {
    TestEnv::one(
        "foo",
        r#"
class X: ...
x: X = X()
"#,
    )
}

fn env_class_x_deeper() -> TestEnv {
    TestEnv::one(
        "foo.bar",
        r#"
class X: ...
x: X = X()
"#,
    )
}

simple_test!(
    test_imports_works,
    env_class_x(),
    r#"
from typing import assert_type
from foo import x, X
assert_type(x, X)
"#,
);

simple_test!(
    test_imports_broken,
    env_class_x(),
    r#"
from foo import x, X
class Y: ...
b: Y = x  # E: X <: Y
"#,
);

simple_test!(
    test_imports_star,
    env_class_x(),
    r#"
from typing import assert_type
from foo import *
y: X = x
assert_type(y, X)
"#,
);

simple_test!(
    test_imports_module_single,
    env_class_x(),
    r#"
from typing import assert_type
import foo
y: foo.X = foo.x
assert_type(y, foo.X)
"#,
);

simple_test!(
    test_imports_module_as,
    env_class_x(),
    r#"
from typing import assert_type
import foo as bar
y: bar.X = bar.x
assert_type(y, bar.X)
"#,
);

simple_test!(
    test_imports_module_nested,
    env_class_x_deeper(),
    r#"
from typing import assert_type
import foo.bar
y: foo.bar.X = foo.bar.x
assert_type(y, foo.bar.X)
"#,
);

simple_test!(
    test_import_overwrite,
    env_class_x(),
    r#"
from foo import X, x
class X: ...
y: X = x  # E: foo.X <: main.X
"#,
);

fn env_imports_dot() -> TestEnv {
    let mut t = TestEnv::new();
    t.add("foo.bar.baz", "from .qux import x");
    t.add("foo.bar.qux", "x: int = 1");
    t
}

simple_test!(
    test_imports_dot,
    env_imports_dot(),
    r#"
from typing import assert_type
from foo.bar.baz import x
assert_type(x, int)
"#,
);

fn env_star_reexport() -> TestEnv {
    let mut t = TestEnv::new();
    t.add("base", "class Foo: ...");
    t.add("second", "from base import *");
    t
}

simple_test!(
    test_imports_star_transitive,
    env_star_reexport(),
    r#"
from typing import assert_type
from second import *
assert_type(Foo(), Foo)
"#,
);

fn env_redefine_class() -> TestEnv {
    TestEnv::one("foo", "class Foo: ...")
}

simple_test!(
    test_redefine_class,
    env_redefine_class(),
    r#"
from typing import assert_type
from foo import *
class Foo: ...
def f(x: Foo) -> Foo:
    return Foo()
assert_type(f(Foo()), Foo)
"#,
);

simple_test!(
    test_dont_export_underscore,
    TestEnv::one("foo", "x: int = 1\n_y: int = 2"),
    r#"
from typing import assert_type, Any
from foo import *
assert_type(x, int)
assert_type(_y, Any)  # E: Could not find name `_y`
"#,
);

fn env_import_different_submodules() -> TestEnv {
    let mut t = TestEnv::new();
    t.add("foo.bar", "x: int = 1");
    t.add("foo.baz", "x: str = 'a'");
    t
}

simple_test!(
    test_import_different_submodules,
    env_import_different_submodules(),
    r#"
from typing import assert_type
import foo.bar
import foo.baz

assert_type(foo.bar.x, int)
assert_type(foo.baz.x, str)
"#,
);

simple_test!(
    test_import_flow,
    env_import_different_submodules(),
    r#"
from typing import assert_type
import foo.bar

def test():
    assert_type(foo.bar.x, int)
    assert_type(foo.baz.x, str)

import foo.baz
"#,
);

simple_test!(
    test_bad_import,
    r#"
from typing import assert_type, Any
from builtins import not_a_real_value  # E: Could not import `not_a_real_value` from `builtins`
assert_type(not_a_real_value, Any)
"#,
);

simple_test!(
    test_bad_relative_import,
    r#"
from ... import does_not_exist  # E: Could not resolve relative import `...`
"#,
);

simple_test!(
    test_import_all,
    TestEnv::one(
        "foo",
        r#"
__all__ = ["x"]
x: int = 1
y: int = 3
    "#
    ),
    r#"
from foo import *
z = y  # E: Could not find name `y`
"#,
);

fn env_broken_export() -> TestEnv {
    let mut t = TestEnv::new();
    t.add("foo", "from foo.bar import *");
    t.add(
        "foo.bar",
        r#"
from foo import baz  # E: Could not import `baz` from `foo`
__all__ = []
"#,
    );
    t
}

simple_test!(
    test_broken_export,
    env_broken_export(),
    r#"
import foo
"#,
);

fn env_relative_import_star() -> TestEnv {
    let mut t = TestEnv::new();
    t.add_with_path("foo", "from .bar import *", "foo/__init__.pyi");
    t.add_with_path("foo.bar", "x: int = 5", "foo/bar.pyi");
    t
}

simple_test!(
    test_relative_import_star,
    env_relative_import_star(),
    r#"
from typing import assert_type
import foo

assert_type(foo.x, int)
"#,
);

fn env_export_all_wrongly() -> TestEnv {
    let mut t = TestEnv::new();
    t.add(
        "foo",
        r#"
__all__ = ['bad_definition']
__all__.extend(bad_module.__all__)  # E: Could not find name `bad_module`
"#,
    );
    t
}

simple_test!(
    test_export_all_wrongly,
    env_export_all_wrongly(),
    r#"
from foo import bad_definition  # E: Could not import `bad_definition` from `foo`
"#,
);

simple_test!(
    test_export_all_wrongly_star,
    env_export_all_wrongly(),
    r#"
from foo import *  # E: Could not import `bad_definition` from `foo`
"#,
);

fn env_blank() -> TestEnv {
    let mut t = TestEnv::new();
    t.add("foo", "");
    t
}

simple_test!(
    test_import_blank,
    env_blank(),
    r#"
import foo

x = foo.bar  # E: No attribute `bar` in module `foo`
"#,
);

simple_test!(
    test_missing_import_named,
    r#"
from foo import bar  # E: Could not find import of `foo`  # E: Could not import `bar`
"#,
);

simple_test!(
    test_missing_import_star,
    r#"
from foo import *  # E: Could not find import of `foo`
"#,
);

simple_test!(
    test_direct_import_toplevel,
    r#"
import typing

typing.assert_type(None, None)
"#,
);

simple_test!(
    test_direct_import_function,
    r#"
import typing

def foo():
    typing.assert_type(None, None)
"#,
);

fn env_import_fail_to_load() -> TestEnv {
    let mut env = TestEnv::new();
    env.add_error("foo", "Disk go urk");
    env
}

simple_test!(
    test_import_fail_to_load,
    env_import_fail_to_load(),
    r#"
import foo
"#,
    |errs| {
        assert_eq!(errs.len(), 1);
        assert!(
            errs[0]
                .to_string()
                .contains("foo.py:1:1: Failed to load foo from foo.py, got Disk go urk")
        );
        Ok(())
    },
);
