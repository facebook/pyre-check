/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;

use crate::module::module_name::ModuleName;
use crate::simple_test;
use crate::state::driver::Driver;
use crate::test::stdlib::Stdlib;
use crate::test::util::simple_test_driver;
use crate::test::util::TestEnv;
use crate::types::class::ClassType;
use crate::types::class_metadata::ClassMetadata;
use crate::types::literal::Lit;
use crate::types::types::Type;

fn mk_driver(code: &str) -> (ModuleName, Driver) {
    let driver = simple_test_driver(Stdlib::new(), TestEnv::one("main", code));
    (ModuleName::from_str("main"), driver)
}

fn get_class_metadata<'b, 'a>(
    name: &'b str,
    module_name: ModuleName,
    driver: &'a Driver,
) -> &'a ClassMetadata {
    driver
        .class_metadata_of_export(module_name, name)
        .unwrap_or_else(|| panic!("No MRO for {name}"))
}

fn get_class_keyword(
    class_name: &str,
    keyword_name: &str,
    module_name: ModuleName,
    driver: &Driver,
) -> Option<Type> {
    get_class_metadata(class_name, module_name, driver)
        .keywords()
        .get(&Name::new(keyword_name))
        .cloned()
}

fn get_metaclass(class_name: &str, module_name: ModuleName, driver: &Driver) -> Option<ClassType> {
    get_class_metadata(class_name, module_name, driver)
        .metaclass()
        .cloned()
}

#[test]
fn test_look_up_class_keywords() {
    let (module_name, driver) = mk_driver(
        r#"
class A(foo=True): pass
"#,
    );
    assert_eq!(
        get_class_keyword("A", "foo", module_name, &driver),
        Some(Type::Literal(Lit::Bool(true))),
    );
    assert_eq!(get_class_keyword("A", "bar", module_name, &driver), None,);
}

#[test]
fn test_direct_metaclass() {
    let (module_name, driver) = mk_driver(
        r#"
class M0(type): pass
class M1(M0): pass
class B(metaclass=M0): pass
class C(B, metaclass=M1): pass
"#,
    );
    assert_eq!(
        get_metaclass("C", module_name, &driver).unwrap().name().id,
        "M1"
    );
}

#[test]
fn test_inherited_metaclass() {
    let (module_name, driver) = mk_driver(
        r#"
class M0(type): pass
class M1(M0): pass
class B0(metaclass=M0): pass
class B1(metaclass=M1): pass
class C(B0, B1): pass
"#,
    );
    assert_eq!(
        get_metaclass("C", module_name, &driver).unwrap().name().id,
        "M1"
    );
}

simple_test!(
    test_that_keywords_type_check,
    r#"
def f(x: bool) -> bool: ...

class A(foo=f(15)):  # E: EXPECTED Literal[15] <: bool
    pass
"#,
);

simple_test!(
    test_metaclass_must_subclass_type,
    r#"
class BadMeta: pass
class A(metaclass=BadMeta):  # E: Metaclass of `A` has type `BadMeta` which is not a subclass of `type`
    pass
"#,
);

simple_test!(
    test_direct_metaclass_collides_with_base,
    r#"
class M0(type): pass
class M1(type): pass
class B(metaclass=M0): pass
class A(B, metaclass=M1):  # E:  Class `A` has metaclass `M1` which is not a subclass of metaclass `M0` from base class `B`
    pass
"#,
);

simple_test!(
    test_inherited_metaclass_collides_with_base,
    r#"
class M0(type): pass
class M1(type): pass
class B0(metaclass=M0): pass
class B1(metaclass=M1): pass
class A(B0, B1):  # E:  Class `A` has metaclass `M0` which is not a subclass of metaclass `M1` from base class `B1`
    pass
"#,
);

// TODO(stroxler): Ideally we would not only report a duplicate keyword here,
// but also catch all type errors even in duplicate keywords. Here we miss a type
// error in the first value for `foo` because it gets overwritten.
simple_test!(
    test_duplicate_class_keyword,
    r#"
class A(foo="x" + 5, foo=True):  # E: Parse error: Duplicate keyword argument "foo"
    pass
"#,
);
