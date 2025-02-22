/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;
use std::sync::LazyLock;

use regex::Regex;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprDict;
use ruff_python_ast::ExprList;
use ruff_python_ast::ExprName;
use ruff_python_ast::ExprTuple;
use ruff_python_ast::Identifier;
use ruff_python_ast::StmtClassDef;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::binding::binding::Binding;
use crate::binding::binding::BindingClass;
use crate::binding::binding::BindingClassField;
use crate::binding::binding::BindingClassMetadata;
use crate::binding::binding::BindingClassSynthesizedFields;
use crate::binding::binding::ClassBinding;
use crate::binding::binding::ClassFieldInitialValue;
use crate::binding::binding::Key;
use crate::binding::binding::KeyClass;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyClassSynthesizedFields;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::bindings::LegacyTParamBuilder;
use crate::binding::scope::InstanceAttribute;
use crate::binding::scope::Scope;
use crate::binding::scope::ScopeKind;
use crate::dunder;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::class::ClassFieldProperties;
use crate::types::types::AnyStyle;
use crate::util::prelude::SliceExt;

impl<'a> BindingsBuilder<'a> {
    pub fn class_def(&mut self, mut x: StmtClassDef) {
        let body = mem::take(&mut x.body);
        let decorators = self.ensure_and_bind_decorators(mem::take(&mut x.decorator_list));

        self.scopes.push(Scope::annotation());

        let class_name = ShortIdentifier::new(&x.name);
        let class_key = KeyClass(class_name.clone());
        let definition_key = self.table.classes.0.insert(class_key);

        x.type_params.iter_mut().for_each(|x| {
            self.type_params(x);
        });

        let mut legacy = Some(LegacyTParamBuilder::new(x.type_params.is_some()));
        let bases = x.bases().map(|base| {
            let mut base = base.clone();
            // Forward refs are fine *inside* of a base expression in the type arguments,
            // but outermost class cannot be a forward ref.
            match &base {
                Expr::StringLiteral(v) => {
                    self.error(
                        base.range(),
                        format!(
                            "Cannot use string annotation `{}` as a base class",
                            v.value.to_str()
                        ),
                    );
                }
                _ => {}
            }
            self.ensure_type(&mut base, &mut legacy);
            base
        });

        let mut keywords = Vec::new();
        x.keywords().iter().for_each(|keyword| {
            if let Some(name) = &keyword.arg {
                self.ensure_expr(&keyword.value);
                keywords.push((name.id.clone(), keyword.value.clone()));
            } else {
                self.error(
                    keyword.range(),
                    format!(
                        "The use of unpacking in class header of `{}` is not supported",
                        x.name
                    ),
                )
            }
        });

        self.table.insert(
            KeyClassMetadata(class_name.clone()),
            BindingClassMetadata {
                def: definition_key,
                bases: bases.clone().into_boxed_slice(),
                keywords: keywords.into_boxed_slice(),
                decorators: decorators.clone().into_boxed_slice(),
            },
        );
        self.table.insert(
            KeyClassSynthesizedFields(class_name.clone()),
            BindingClassSynthesizedFields(definition_key),
        );

        let legacy_tparam_builder = legacy.unwrap();
        legacy_tparam_builder.add_name_definitions(self);

        self.scopes.push(Scope::class_body(x.name.clone()));
        self.init_static_scope(&body, false);
        self.stmts(body);

        let last_scope = self.scopes.pop();
        self.scopes.pop(); // annotation scope
        let mut fields = SmallMap::new();
        for (name, info) in last_scope.flow.info.iter() {
            // A name with flow info but not static info is a reference to something that's not a class field.
            if let Some(stat_info) = last_scope.stat.0.get(name) {
                let binding = BindingClassField {
                    class: definition_key,
                    name: name.clone(),
                    value: Binding::Forward(info.key),
                    annotation: stat_info.annot,
                    range: stat_info.loc,
                    initial_value: info.as_initial_value(),
                };
                fields.insert(
                    name.clone(),
                    ClassFieldProperties {
                        is_annotated: stat_info.annot.is_some(),
                        range: Some(stat_info.loc),
                    },
                );
                self.table
                    .insert(KeyClassField(class_name.clone(), name.clone()), binding);
            }
        }
        if let ScopeKind::ClassBody(body) = last_scope.kind {
            for (method_name, instance_attributes) in body.instance_attributes_by_method {
                if method_name == dunder::INIT {
                    for (name, InstanceAttribute(value, annotation, range)) in instance_attributes {
                        if !fields.contains_key(&name) {
                            fields.insert(
                                name.clone(),
                                ClassFieldProperties {
                                    is_annotated: annotation.is_some(),
                                    range: None,
                                },
                            );
                            self.table.insert(
                                KeyClassField(class_name.clone(), name.clone()),
                                BindingClassField {
                                    class: definition_key,
                                    name,
                                    value,
                                    annotation,
                                    range,
                                    initial_value: ClassFieldInitialValue::Instance,
                                },
                            );
                        } else if annotation.is_some() {
                            self.error(range, format!("Attribute `{name}` is declared in the class body, so the assignment here should not have an annotation."));
                        }
                    }
                }
            }
        } else {
            unreachable!("Expected class body scope, got {:?}", last_scope.kind);
        }

        let self_binding = Binding::SelfType(definition_key);
        self.table.insert(Key::SelfType(class_name), self_binding);

        let legacy_tparams = legacy_tparam_builder.lookup_keys(self);

        self.bind_definition(
            &x.name,
            Binding::ClassDef(definition_key, decorators.into_boxed_slice()),
            None,
        );
        self.table.insert_idx(
            definition_key,
            BindingClass::ClassDef(ClassBinding {
                def: x,
                fields,
                bases: bases.into_boxed_slice(),
                legacy_tparams: legacy_tparams.into_boxed_slice(),
            }),
        );
    }

    fn extract_string_literals(
        &mut self,
        items: &[Expr],
    ) -> Vec<(String, TextRange, Option<Expr>)> {
        items
            .iter()
            .filter_map(|item| match item {
                Expr::StringLiteral(x) => Some((x.value.to_string(), x.range(), None)),
                _ => {
                    self.error(item.range(), "Expected a string literal".to_owned());
                    None
                }
            })
            .collect()
    }

    fn decompose_key_value_pairs(
        &mut self,
        items: &[Expr],
    ) -> Vec<(String, TextRange, Option<Expr>)> {
        items
            .iter()
            .filter_map(|item| match item {
                Expr::Tuple(ExprTuple { elts, .. }) => match elts.as_slice() {
                    [Expr::StringLiteral(k), v] => {
                        Some((k.value.to_string(), k.range(), Some(v.clone())))
                    }
                    [k, _] => {
                        self.error(
                            k.range(),
                            "Expected first item to be a string literal".to_owned(),
                        );
                        None
                    }
                    _ => {
                        self.error(item.range(), "Expected a pair".to_owned());
                        None
                    }
                },
                _ => {
                    self.error(item.range(), "Expected a tuple".to_owned());
                    None
                }
            })
            .collect()
    }

    pub fn synthesize_enum_def(
        &mut self,
        class_name: Identifier,
        base_name: ExprName,
        members: &[Expr],
    ) {
        let short_class_name = ShortIdentifier::new(&class_name);
        let class_key = KeyClass(short_class_name.clone());
        let definition_key = self.table.classes.0.insert(class_key.clone());
        self.table.insert(
            KeyClassMetadata(short_class_name.clone()),
            BindingClassMetadata {
                def: definition_key,
                bases: Box::new([Expr::Name(base_name)]),
                keywords: Box::new([]),
                decorators: Box::new([]),
            },
        );
        self.table.insert(
            KeyClassSynthesizedFields(short_class_name.clone()),
            BindingClassSynthesizedFields(definition_key),
        );
        let mut fields = SmallMap::new();
        let member_definitions: Vec<(String, TextRange, Option<Expr>)> = match members {
            // Enum('Color', 'RED, GREEN, BLUE')
            // Enum('Color', 'RED GREEN BLUE')
            [Expr::StringLiteral(x)] => {
                let s = x.value.to_str();
                if s.contains(',') {
                    s.split(',')
                        .map(str::trim)
                        .map(|s| (s.to_owned(), x.range(), None))
                        .collect()
                } else {
                    s.split_whitespace()
                        .map(|s| (s.to_owned(), x.range(), None))
                        .collect()
                }
            }
            // Enum('Color', 'RED', 'GREEN', 'BLUE')
            [Expr::StringLiteral(_), ..] => self.extract_string_literals(members),
            // Enum('Color', ['RED', 'GREEN', 'BLUE'])
            [Expr::List(ExprList { elts, .. })]
                if matches!(elts.as_slice(), [Expr::StringLiteral(_), ..]) =>
            {
                self.extract_string_literals(elts)
            }
            // Enum('Color', ('RED', 'GREEN', 'BLUE'))
            [Expr::Tuple(ExprTuple { elts, .. })]
                if matches!(elts.as_slice(), [Expr::StringLiteral(_), ..]) =>
            {
                self.extract_string_literals(elts)
            }
            // Enum('Color', [('RED', 1), ('GREEN', 2), ('BLUE', 3)])
            [Expr::List(ExprList { elts, .. })]
                if matches!(elts.as_slice(), [Expr::Tuple(_), ..]) =>
            {
                self.decompose_key_value_pairs(elts)
            }
            // Enum('Color', (('RED', 1), ('GREEN', 2), ('BLUE', 3)))
            [Expr::Tuple(ExprTuple { elts, .. })]
                if matches!(elts.as_slice(), [Expr::Tuple(_), ..]) =>
            {
                self.decompose_key_value_pairs(elts)
            }
            // Enum('Color', {'RED': 1, 'GREEN': 2, 'BLUE': 3})
            [Expr::Dict(ExprDict { items, .. })] => items
                .iter()
                .filter_map(|item| match (&item.key, &item.value) {
                    (Some(Expr::StringLiteral(k)), v) => {
                        Some((k.value.to_string(), k.range(), Some(v.clone())))
                    }
                    (Some(k), _) => {
                        self.error(
                            k.range(),
                            "Expected first item to be a string literal".to_owned(),
                        );
                        None
                    }
                    _ => {
                        self.error(item.range(), "Expected a key-value pair".to_owned());
                        None
                    }
                })
                .collect(),
            _ => {
                self.error(
                    class_name.range,
                    "Expected valid functional enum definition".to_owned(),
                );
                Vec::new()
            }
        };
        for (member_name, range, member_value) in member_definitions {
            if is_valid_identifier(member_name.as_str()) {
                let member_name = Name::new(member_name);
                fields.insert(
                    member_name.clone(),
                    ClassFieldProperties {
                        is_annotated: false,
                        range: None,
                    },
                );
                let value_binding = match member_value {
                    Some(value) => Binding::Expr(None, value),
                    None => Binding::AnyType(AnyStyle::Implicit),
                };
                self.table.insert(
                    KeyClassField(short_class_name.clone(), member_name.clone()),
                    BindingClassField {
                        class: definition_key,
                        name: member_name,
                        value: value_binding,
                        annotation: None,
                        range,
                        initial_value: ClassFieldInitialValue::Class(None),
                    },
                );
            } else {
                self.error(range, format!("{member_name} is not a valid identifier"))
            }
        }
        let self_binding = Binding::SelfType(definition_key);
        self.table
            .insert(Key::SelfType(short_class_name), self_binding);
        self.bind_definition(
            &class_name,
            Binding::ClassDef(definition_key, Box::new([])),
            None,
        );
        self.table.insert(
            class_key,
            BindingClass::FunctionalClassDef(class_name.clone(), fields),
        );
    }
}

pub fn is_valid_identifier(name: &str) -> bool {
    static IDENTIFIER_REGEX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new("^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap());
    IDENTIFIER_REGEX.is_match(name)
}
