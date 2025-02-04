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
use starlark_map::small_set::SmallSet;

use crate::binding::binding::Binding;
use crate::binding::binding::BindingClassField;
use crate::binding::binding::BindingClassMetadata;
use crate::binding::binding::ClassFieldInitialization;
use crate::binding::binding::Key;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
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
        let decorators = mem::take(&mut x.decorator_list);

        for x in decorators.iter() {
            self.ensure_expr(&x.expression);
        }

        self.scopes.push(Scope::class_body(x.name.clone()));

        let definition_key = self
            .table
            .types
            .0
            .insert(Key::Definition(ShortIdentifier::new(&x.name)));

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
            BindingClassMetadata(definition_key, bases.clone(), keywords, decorators.clone()),
        );

        let legacy_tparam_builder = legacy.unwrap();
        legacy_tparam_builder.add_name_definitions(self);

        let non_field_names = self
            .scopes
            .current()
            .flow
            .info
            .keys()
            .cloned()
            .collect::<SmallSet<_>>();

        self.scopes.current_mut().stat.stmts(
            &body,
            &self.module_info,
            false,
            self.lookup,
            self.config,
        );
        self.stmts(body);

        let last_scope = self.scopes.pop();
        let mut fields = SmallMap::new();
        for (name, info) in last_scope.flow.info.iter() {
            if non_field_names.contains(name) {
                // TODO: this incorrectly filters out fields that reuse already-in-scope names.
                continue;
            }
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
                    annotation: info.ann(),
                    range: stat_info.loc,
                    initialization,
                };
                fields.insert(
                    name.clone(),
                    ClassFieldProperties {
                        is_annotated: info.ann().is_some(),
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

        let name = x.name.clone();
        self.bind_definition(
            &name,
            Binding::ClassDef(
                Box::new((x, fields)),
                bases.into_boxed_slice(),
                decorators.into_boxed_slice(),
                legacy_tparams.into_boxed_slice(),
            ),
            None,
        );
    }

    pub fn synthesize_enum_def(
        &mut self,
        class_name: Identifier,
        base_name: ExprName,
        members: &[Expr],
    ) {
        let definition_key = self
            .table
            .types
            .0
            .insert(Key::Definition(ShortIdentifier::new(&class_name)));
        self.table.insert(
            KeyClassMetadata(ShortIdentifier::new(&class_name)),
            BindingClassMetadata(definition_key, vec![Expr::Name(base_name)], vec![], vec![]),
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
            Binding::FunctionalClassDef(class_name.clone(), fields),
            None,
        );
    }
}

pub fn is_valid_identifier(name: &str) -> bool {
    static IDENTIFIER_REGEX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new("^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap());
    IDENTIFIER_REGEX.is_match(name)
}
