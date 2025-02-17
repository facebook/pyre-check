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
use ruff_python_ast::ExprName;
use ruff_python_ast::Identifier;
use ruff_python_ast::StmtClassDef;
use ruff_text_size::Ranged;
use starlark_map::small_map::SmallMap;

use crate::binding::binding::Binding;
use crate::binding::binding::BindingClass;
use crate::binding::binding::BindingClassField;
use crate::binding::binding::BindingClassMetadata;
use crate::binding::binding::BindingClassSynthesizedFields;
use crate::binding::binding::ClassBinding;
use crate::binding::binding::ClassFieldInitialization;
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

        let class_key = KeyClass(ShortIdentifier::new(&x.name));
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
            KeyClassMetadata(ShortIdentifier::new(&x.name)),
            BindingClassMetadata {
                def: definition_key,
                bases: bases.clone().into_boxed_slice(),
                keywords: keywords.into_boxed_slice(),
                decorators: decorators.clone().into_boxed_slice(),
            },
        );
        self.table.insert(
            KeyClassSynthesizedFields(ShortIdentifier::new(&x.name)),
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
                let initialization = if info.is_initialized() {
                    ClassFieldInitialization::Class
                } else {
                    ClassFieldInitialization::Instance
                };
                let binding = BindingClassField {
                    class: definition_key,
                    name: name.clone(),
                    value: Binding::Forward(info.key),
                    annotation: stat_info.annot,
                    range: stat_info.loc,
                    initialization,
                };
                fields.insert(
                    name.clone(),
                    ClassFieldProperties {
                        is_annotated: stat_info.annot.is_some(),
                        range: Some(stat_info.loc),
                    },
                );
                self.table.insert(
                    KeyClassField(ShortIdentifier::new(&x.name), name.clone()),
                    binding,
                );
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
                                KeyClassField(ShortIdentifier::new(&x.name), name.clone()),
                                BindingClassField {
                                    class: definition_key,
                                    name,
                                    value,
                                    annotation,
                                    range,
                                    initialization: ClassFieldInitialization::Instance,
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
        self.table
            .insert(Key::SelfType(ShortIdentifier::new(&x.name)), self_binding);

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

    pub fn synthesize_enum_def(
        &mut self,
        class_name: Identifier,
        base_name: ExprName,
        members: &[Expr],
    ) {
        let class_key = KeyClass(ShortIdentifier::new(&class_name));
        let definition_key = self.table.classes.0.insert(class_key.clone());
        self.table.insert(
            KeyClassMetadata(ShortIdentifier::new(&class_name)),
            BindingClassMetadata {
                def: definition_key,
                bases: Box::new([Expr::Name(base_name)]),
                keywords: Box::new([]),
                decorators: Box::new([]),
            },
        );
        self.table.insert(
            KeyClassSynthesizedFields(ShortIdentifier::new(&class_name)),
            BindingClassSynthesizedFields(definition_key),
        );
        let mut fields = SmallMap::new();
        match members {
            // Enum('Color5', 'RED, GREEN, BLUE')
            // Enum('Color6', 'RED GREEN BLUE')
            [Expr::StringLiteral(x)] => {
                let s = x.value.to_str();
                let parts: Vec<&str> = if s.contains(',') {
                    s.split(',').map(str::trim).collect()
                } else {
                    s.split_whitespace().collect()
                };
                for member in parts {
                    if is_valid_identifier(member) {
                        let member_name = Name::new(member);
                        fields.insert(
                            member_name.clone(),
                            ClassFieldProperties {
                                is_annotated: false,
                                range: None,
                            },
                        );
                        self.table.insert(
                            KeyClassField(ShortIdentifier::new(&class_name), member_name.clone()),
                            BindingClassField {
                                class: definition_key,
                                name: member_name,
                                value: Binding::AnyType(AnyStyle::Implicit),
                                annotation: None,
                                range: x.range(),
                                initialization: ClassFieldInitialization::Class,
                            },
                        );
                    } else {
                        self.error(x.range, format!("{member} is not a valid identifier"))
                    }
                }
            }
            _ => self.error(class_name.range, "TODO enum functional syntax".to_owned()),
        }
        let self_binding = Binding::SelfType(definition_key);
        self.table.insert(
            Key::SelfType(ShortIdentifier::new(&class_name)),
            self_binding,
        );
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
