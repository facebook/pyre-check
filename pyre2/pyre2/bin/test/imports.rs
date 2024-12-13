/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::test::util::TestEnv;
use crate::testcase;
use crate::testcase_with_bug;

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

testcase!(
    test_imports_works,
    env_class_x(),
    r#"
from typing import assert_type
from foo import x, X
assert_type(x, X)
"#,
);

testcase!(
    test_imports_broken,
    env_class_x(),
    r#"
from foo import x, X
class Y: ...
b: Y = x  # E: X <: Y
"#,
);

testcase!(
    test_imports_star,
    env_class_x(),
    r#"
from typing import assert_type
from foo import *
y: X = x
assert_type(y, X)
"#,
);

testcase!(
    test_imports_module_single,
    env_class_x(),
    r#"
from typing import assert_type
import foo
y: foo.X = foo.x
assert_type(y, foo.X)
"#,
);

testcase!(
    test_imports_module_as,
    env_class_x(),
    r#"
from typing import assert_type
import foo as bar
y: bar.X = bar.x
assert_type(y, bar.X)
"#,
);

testcase!(
    test_imports_module_nested,
    env_class_x_deeper(),
    r#"
from typing import assert_type
import foo.bar
y: foo.bar.X = foo.bar.x
assert_type(y, foo.bar.X)
"#,
);

testcase!(
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

testcase!(
    test_imports_dot,
    env_imports_dot(),
    r#"
from typing import assert_type
from foo.bar.baz import x
assert_type(x, int)
"#,
);

testcase_with_bug!(
    test_access_nonexistent_module,
    env_imports_dot(),
    r#"
import foo.bar.baz
foo.qux.wibble.wobble # TODO: error
"#,
);

fn env_star_reexport() -> TestEnv {
    let mut t = TestEnv::new();
    t.add("base", "class Foo: ...");
    t.add("second", "from base import *");
    t
}

testcase!(
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

testcase!(
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

testcase!(
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

testcase!(
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

testcase!(
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

testcase!(
    test_bad_import,
    r#"
from typing import assert_type, Any
from builtins import not_a_real_value  # E: Could not import `not_a_real_value` from `builtins`
assert_type(not_a_real_value, Any)
"#,
);

testcase!(
    test_bad_relative_import,
    r#"
from ... import does_not_exist  # E: Could not resolve relative import `...`
"#,
);

testcase!(
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

testcase!(
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

testcase!(
    test_relative_import_star,
    env_relative_import_star(),
    r#"
from typing import assert_type
import foo

assert_type(foo.x, int)
"#,
);

fn env_dunder_init_with_submodule() -> TestEnv {
    let mut t = TestEnv::new();
    t.add_with_path("foo", "x: str = ''", "foo/__init__.py");
    t.add_with_path("foo.bar", "x: int = 0", "foo/bar.py");
    t
}

// TODO: foo.bar.x should exist and should be an int
testcase_with_bug!(
    test_import_dunder_init_and_submodule,
    env_dunder_init_with_submodule(),
    r#"
from typing import assert_type
import foo
import foo.bar
assert_type(foo.x, str)
foo.bar.x # TODO # E: No attribute `bar` in module `foo`
"#,
);

testcase!(
    test_import_dunder_init_without_submodule,
    env_dunder_init_with_submodule(),
    r#"
from typing import assert_type
import foo
assert_type(foo.x, str)
foo.bar.x # E: No attribute `bar` in module `foo`
"#,
);

// TODO: `foo.x` should be an error
// The assert_type(foo.x) call should fail, but the error message is not great
testcase_with_bug!(
    test_import_dunder_init_submodule_only,
    env_dunder_init_with_submodule(),
    r#"
from typing import assert_type
import foo.bar
foo.x
assert_type(foo.x, str) # E: assert_type(Module[foo.x], str) failed
assert_type(foo.bar.x, int)
"#,
);

fn env_dunder_init_overlap_submodule() -> TestEnv {
    let mut t = TestEnv::new();
    t.add_with_path("foo", "bar: str = ''", "foo/__init__.py");
    t.add_with_path("foo.bar", "x: int = 0", "foo/bar.py");
    t
}

// TODO: foo.bar should not be a str (it should be the module object)
// TODO: foo.bar.x should exist and should be an int
testcase_with_bug!(
    test_import_dunder_init_overlap_submodule_last,
    env_dunder_init_overlap_submodule(),
    r#"
from typing import assert_type
import foo
import foo.bar
assert_type(foo.bar, str) # TODO: error
foo.bar.x # TODO # E: Object of class `str` has no attribute `x`
"#,
);

// TODO: Surprisingly (to Sam), importing __init__ after the submodule does not
// overwrite foo.bar with the global from __init__.py.
testcase_with_bug!(
    test_import_dunder_init_overlap_submodule_first,
    env_dunder_init_overlap_submodule(),
    r#"
from typing import assert_type
import foo.bar
import foo
assert_type(foo.bar, str) # TODO: error
foo.bar.x # TODO # E: Object of class `str` has no attribute `x`
"#,
);

testcase!(
    test_import_dunder_init_overlap_without_submodule,
    env_dunder_init_overlap_submodule(),
    r#"
from typing import assert_type
import foo
assert_type(foo.bar, str)
foo.bar.x # E: Object of class `str` has no attribute `x`
"#,
);

testcase!(
    test_import_dunder_init_overlap_submodule_only,
    env_dunder_init_overlap_submodule(),
    r#"
from typing import assert_type
import foo.bar
assert_type(foo.bar, str) # E: assert_type(Module[foo.bar], str) failed
assert_type(foo.bar.x, int)
"#,
);

fn env_export_all_wrongly() -> TestEnv {
    TestEnv::one(
        "foo",
        r#"
__all__ = ['bad_definition']
__all__.extend(bad_module.__all__)  # E: Could not find name `bad_module`
"#,
    )
}

testcase!(
    test_export_all_wrongly,
    env_export_all_wrongly(),
    r#"
from foo import bad_definition  # E: Could not import `bad_definition` from `foo`
"#,
);

testcase!(
    test_export_all_wrongly_star,
    env_export_all_wrongly(),
    r#"
from foo import *  # E: Could not import `bad_definition` from `foo`
"#,
);

testcase_with_bug!(
    test_export_all_not_module,
    r#"
class not_module:
    __all__ = []

__all__ = []
__all__.extend(not_module.__all__)  # Should get an error about not_module not being imported
    # But Pyright doesn't give an error, so maybe we shouldn't either??
"#,
);

fn env_blank() -> TestEnv {
    TestEnv::one("foo", "")
}

testcase!(
    test_import_blank,
    env_blank(),
    r#"
import foo

x = foo.bar  # E: No attribute `bar` in module `foo`
"#,
);

testcase!(
    test_missing_import_named,
    r#"
from foo import bar  # E: Could not find import of `foo`
"#,
);

testcase!(
    test_missing_import_star,
    r#"
from foo import *  # E: Could not find import of `foo`
"#,
);

testcase!(
    test_missing_import_module,
    r#"
import foo, bar.baz  # E: Could not find import of `foo`  # E: Could not find import of `bar.baz`
"#,
);

testcase!(
    test_direct_import_toplevel,
    r#"
import typing

typing.assert_type(None, None)
"#,
);

testcase!(
    test_direct_import_function,
    r#"
import typing

def foo():
    typing.assert_type(None, None)
"#,
);

#[test]
fn test_import_fail_to_load() {
    let mut env = TestEnv::new();
    env.add_error("foo", "Disk go urk");
    env.add("main", "import foo");
    let errs = env.to_state().collect_errors();
    assert_eq!(errs.len(), 1);
    assert!(
        errs[0]
            .to_string()
            .contains("foo.py:1:1: Failed to load foo from foo.py, got Disk go urk")
    );
}
