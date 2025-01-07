/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;

use crate::module::module_name::ModuleName;
use crate::state::state::State;
use crate::test::mro::get_class_metadata;
use crate::test::util::mk_state;
use crate::testcase;
use crate::testcase_with_bug;
use crate::types::class::ClassType;
use crate::types::literal::Lit;
use crate::types::types::Type;

fn get_class_keyword(
    class_name: &str,
    keyword_name: &str,
    module_name: ModuleName,
    state: &State,
) -> Option<Type> {
    get_class_metadata(class_name, module_name, state)
        .keywords()
        .get(&Name::new(keyword_name))
        .cloned()
}

fn get_metaclass(class_name: &str, module_name: ModuleName, state: &State) -> Option<ClassType> {
    get_class_metadata(class_name, module_name, state)
        .metaclass()
        .cloned()
}

#[test]
fn test_look_up_class_keywords() {
    let (module_name, state) = mk_state(
        r#"
class A(foo=True): pass
"#,
    );
    assert_eq!(
        get_class_keyword("A", "foo", module_name, &state),
        Some(Type::Literal(Lit::Bool(true))),
    );
    assert_eq!(get_class_keyword("A", "bar", module_name, &state), None);
}

#[test]
fn test_direct_metaclass() {
    let (module_name, state) = mk_state(
        r#"
class M0(type): pass
class M1(M0): pass
class B(metaclass=M0): pass
class C(B, metaclass=M1): pass
"#,
    );
    assert_eq!(
        get_metaclass("C", module_name, &state).unwrap().name().id,
        "M1"
    );
}

#[test]
fn test_inherited_metaclass() {
    let (module_name, state) = mk_state(
        r#"
class M0(type): pass
class M1(M0): pass
class B0(metaclass=M0): pass
class B1(metaclass=M1): pass
class C(B0, B1): pass
"#,
    );
    assert_eq!(
        get_metaclass("C", module_name, &state).unwrap().name().id,
        "M1"
    );
}

testcase!(
    test_that_keywords_type_check,
    r#"
def f(x: bool) -> bool: ...

class A(foo=f(15)):  # E: EXPECTED Literal[15] <: bool
    pass
"#,
);

testcase!(
    test_metaclass_must_subclass_type,
    r#"
class BadMeta: pass
class A(metaclass=BadMeta):  # E: Metaclass of `A` has type `BadMeta` which is not a subclass of `type`
    pass
"#,
);

testcase!(
    test_direct_metaclass_collides_with_base,
    r#"
class M0(type): pass
class M1(type): pass
class B(metaclass=M0): pass
class A(B, metaclass=M1):  # E:  Class `A` has metaclass `M1` which is not a subclass of metaclass `M0` from base class `B`
    pass
"#,
);

testcase!(
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

testcase_with_bug!(
    r#"
TODO(stroxler): Ideally we would not only report a duplicate keyword here,
but also catch all type errors even in duplicate keywords. Here we miss a type
error in the first value for `foo` because it gets overwritten.
    "#,
    test_duplicate_class_keyword,
    r#"
class A(foo="x" + 5, foo=True):  # E: Parse error: Duplicate keyword argument "foo"
    pass
"#,
);

testcase!(
    test_metaclass_instance,
    r#"
class Meta(type):
    pass
class C1(metaclass=Meta):
    pass
class C2[T](metaclass=Meta):
    pass
def f(m: Meta):
    pass
f(C1)
f(C2[int])
    "#,
);
