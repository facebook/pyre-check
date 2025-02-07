/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;

use itertools::Either;
use ruff_python_ast::ExceptHandler;
use ruff_python_ast::Expr;
use ruff_python_ast::Parameters;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtExpr;
use ruff_python_ast::StmtFunctionDef;
use ruff_python_ast::StmtReturn;
use ruff_text_size::Ranged;
use starlark_map::small_set::SmallSet;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::FunctionBinding;
use crate::binding::binding::FunctionKind;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::bindings::FuncInfo;
use crate::binding::bindings::LegacyTParamBuilder;
use crate::binding::scope::FlowStyle;
use crate::binding::scope::Scope;
use crate::binding::scope::ScopeKind;
use crate::config::Config;
use crate::dunder;
use crate::graph::index::Idx;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::types::AnyStyle;
use crate::types::types::Type;

impl<'a> BindingsBuilder<'a> {
    fn parameters(&mut self, x: &mut Parameters, self_type: Option<Idx<Key>>) {
        let mut self_name = None;
        for x in x.iter() {
            let name = x.name();
            if self_type.is_some() && self_name.is_none() {
                self_name = Some(name.clone());
            }
            let ann_val = match x.annotation() {
                Some(a) => BindingAnnotation::AnnotateExpr(a.clone(), self_type),
                None => {
                    if let Some(self_name) = &self_name
                        && name.id == *self_name.id
                    {
                        BindingAnnotation::Forward(self_type.unwrap())
                    } else {
                        BindingAnnotation::Type(Type::any_implicit())
                    }
                }
            };
            let ann_key = self.table.insert(
                KeyAnnotation::Annotation(ShortIdentifier::new(name)),
                ann_val,
            );
            let bind_key = self.table.insert(
                Key::Definition(ShortIdentifier::new(name)),
                Binding::AnnotatedType(ann_key, Box::new(Binding::AnyType(AnyStyle::Implicit))),
            );
            self.scopes.current_mut().stat.add(
                name.id.clone(),
                name.range,
                Some(ShortIdentifier::new(name)),
            );
            self.bind_key(
                &name.id,
                bind_key,
                Some(FlowStyle::Annotated {
                    ann: ann_key,
                    is_initialized: true,
                }),
            );
        }
        if let Scope {
            kind: ScopeKind::Method(method),
            ..
        } = self.scopes.current_mut()
        {
            method.self_name = self_name;
        }
    }

    pub fn function_def(&mut self, mut x: StmtFunctionDef) {
        let body = mem::take(&mut x.body);
        let decorators = mem::take(&mut x.decorator_list);
        let kind = if is_ellipse(&body) {
            FunctionKind::Stub
        } else {
            FunctionKind::Impl
        };
        let mut return_annotation = mem::take(&mut x.returns);
        self.functions.push(FuncInfo::default());

        let func_name = x.name.clone();
        let self_type = match &self.scopes.current().kind {
            ScopeKind::ClassBody(body) => Some(self.table.types.0.insert(body.as_self_type_key())),
            _ => None,
        };

        for x in decorators.iter() {
            self.ensure_expr(&x.expression);
        }

        self.scopes.push(Scope::annotation());

        let tparams = x
            .type_params
            .as_mut()
            .map(|tparams| self.type_params(tparams));

        let mut legacy = Some(LegacyTParamBuilder::new(tparams.is_some()));

        // We need to bind all the parameters expressions _after_ the type params, but before the parameter names,
        // which might shadow some types.
        for (param, default) in Ast::parameters_iter_mut(&mut x.parameters) {
            self.ensure_type_opt(param.annotation.as_deref_mut(), &mut legacy);
            if let Some(default) = default {
                self.ensure_expr_opt(default.as_deref());
            }
        }
        self.ensure_type_opt(return_annotation.as_deref_mut(), &mut legacy);

        let return_ann = return_annotation.clone().map(|x| {
            self.table.insert(
                KeyAnnotation::ReturnAnnotation(ShortIdentifier::new(&func_name)),
                BindingAnnotation::AnnotateExpr(*x, self_type),
            )
        });

        let never = function_last_expressions(&body, self.config);
        if never != Some(Vec::new()) && kind == FunctionKind::Impl {
            // If we can reach the end, and the code is real (not just ellipse),
            // check None is an OK return type.
            // Note that we special case ellipse even in non-interface, as that is what Pyright does.
            self.functions.last_mut().returns.push(StmtReturn {
                range: match never.as_deref() {
                    Some([x]) => x.range(), // Try and narrow the range
                    _ => x.range,
                },
                value: None,
            });
        }

        let legacy_tparam_builder = legacy.unwrap();
        legacy_tparam_builder.add_name_definitions(self);

        if self_type.is_none() {
            self.scopes.push(Scope::function());
        } else {
            self.scopes.push(Scope::method(func_name.clone()));
        }

        let legacy_tparams = legacy_tparam_builder.lookup_keys(self);

        self.parameters(
            &mut x.parameters,
            if func_name.id == dunder::NEW {
                // __new__ is a staticmethod that is special-cased at runtime to not need @staticmethod decoration.
                None
            } else {
                self_type
            },
        );

        self.scopes.current_mut().stat.stmts(
            &body,
            &self.module_info,
            false,
            self.lookup,
            self.config,
        );
        self.stmts(body);
        let func_scope = self.scopes.pop();
        self.scopes.pop();

        if let ScopeKind::Method(method) = func_scope.kind
            && let ScopeKind::ClassBody(body) = &mut self.scopes.current_mut().kind
        {
            body.instance_attributes_by_method
                .insert(method.name.id, method.instance_attributes);
        }
        let is_async = x.is_async;

        let accumulate = self.functions.pop().unwrap();

        let mut return_expr_keys = SmallSet::with_capacity(accumulate.returns.len());
        for x in accumulate.returns.clone() {
            let key = self.table.insert(
                Key::ReturnExpression(ShortIdentifier::new(&func_name), x.range),
                Binding::ReturnExpr(
                    return_ann,
                    Box::new(Ast::return_or_none_owned(x)),
                    !accumulate.yields.is_empty(),
                ),
            );
            return_expr_keys.insert(key);
        }

        let mut return_type = Binding::phi(return_expr_keys);
        if !accumulate.yields.is_empty() {
            let mut yield_expr_keys = SmallSet::with_capacity(accumulate.yields.len());
            for x in accumulate.yields.clone() {
                // create the appropriate bindings depending on whether we see a yield or a yieldFrom
                // not all bindings are needed for all exprs
                match x {
                    Either::Left(x) => {
                        let key = self.table.insert(
                            Key::YieldTypeOfYield(ShortIdentifier::new(&func_name), x.range),
                            // collect the value of the yield expression.
                            Binding::YieldTypeOfYield(x.clone()),
                        );
                        yield_expr_keys.insert(key);

                        self.table.insert(
                            Key::SendTypeOfYieldAnnotation(x.range),
                            // collect the value of the yield expression.
                            Binding::SendTypeOfYieldAnnotation(return_ann, x.range),
                        );
                        self.table.insert(
                            Key::YieldTypeOfYieldAnnotation(x.range),
                            // collect the yield value of the yield expression.
                            Binding::YieldTypeOfYieldAnnotation(return_ann, x.range, is_async),
                        );
                    }

                    Either::Right(x) => {
                        let key = self.table.insert(
                            Key::YieldTypeOfYield(ShortIdentifier::new(&func_name), x.range),
                            // collect the value of the yield expression.
                            Binding::YieldTypeOfYieldFrom(x.clone()),
                        );
                        yield_expr_keys.insert(key);

                        self.table.insert(
                            Key::ReturnTypeOfYieldAnnotation(x.range),
                            // collect the value of the yield expression.
                            Binding::ReturnTypeOfYieldAnnotation(return_ann, x.range),
                        );
                    }
                }
            }
            let yield_type = Binding::phi(yield_expr_keys);
            self.table.insert(
                Key::YieldTypeOfGenerator(ShortIdentifier::new(&func_name)),
                yield_type.clone(),
            );
            if is_async {
                // combine the original (syntactic) return type and the yield type to analyze later and obtain the final return type.
                return_type = Binding::AsyncGenerator(Box::new(yield_type));

                // if our function is async, then record the overall return type and bind it to each return type
                // this way, we can later check if the return expr gives a value and raise an error if so
                for x in accumulate.returns {
                    self.table.insert(
                        Key::AsyncReturnType(x.range),
                        Binding::AsyncReturnType(Box::new((
                            Ast::return_or_none_owned(x),
                            return_type.clone(),
                        ))),
                    );
                }
            } else {
                // combine the original (syntactic) return type and the yield type to analyze later and obtain the final return type.
                return_type = Binding::Generator(Box::new(yield_type), Box::new(return_type));
            }
        }
        if let Some(ann) = return_ann {
            return_type = Binding::AnnotatedType(ann, Box::new(return_type));
        }
        self.table.insert(
            Key::ReturnType(ShortIdentifier::new(&func_name)),
            return_type.clone(),
        );

        for x in accumulate.yields {
            self.table.insert(
                Key::TypeOfYieldAnnotation(x.either(|x| x.range, |x| x.range)),
                return_type.clone(),
            );
        }

        self.bind_definition(
            &func_name,
            Binding::Function(Box::new(FunctionBinding {
                def: x,
                kind,
                decorators: decorators.into_boxed_slice(),
                legacy_tparams: legacy_tparams.into_boxed_slice(),
            })),
            None,
        );
    }
}

/// Given the body of a function, what are the potential expressions that
/// could be the last ones to be executed, where the function then falls off the end.
///
/// * Return None to say there are branches that fall off the end always.
/// * Return Some([]) to say that we can never reach the end (e.g. always return, raise)
/// * Return Some(xs) to say this set might be the last expression.
fn function_last_expressions<'a>(x: &'a [Stmt], config: &Config) -> Option<Vec<&'a Expr>> {
    fn f<'a>(config: &Config, x: &'a [Stmt], res: &mut Vec<&'a Expr>) -> Option<()> {
        match x.last()? {
            Stmt::Expr(x) => res.push(&x.value),
            Stmt::Return(_) | Stmt::Raise(_) => {}
            Stmt::If(x) => {
                let mut last_test = None;
                for (test, body) in config.pruned_if_branches(x) {
                    last_test = test;
                    f(config, body, res)?;
                }
                if last_test.is_some() {
                    // The final `if` can fall through, so the `if` itself might be the last statement.
                    return None;
                }
            }
            Stmt::Try(x) => {
                if !x.finalbody.is_empty() {
                    f(config, &x.finalbody, res)?;
                } else {
                    if x.orelse.is_empty() {
                        f(config, &x.body, res)?;
                    } else {
                        f(config, &x.orelse, res)?;
                    }
                    for handler in &x.handlers {
                        match handler {
                            ExceptHandler::ExceptHandler(x) => f(config, &x.body, res)?,
                        }
                    }
                    // If we don't have a matching handler, we raise an exception, which is fine.
                }
            }
            _ => return None,
        }
        Some(())
    }

    let mut res = Vec::new();
    f(config, x, &mut res)?;
    Some(res)
}

fn is_ellipse(x: &[Stmt]) -> bool {
    match x {
        [
            Stmt::Expr(StmtExpr {
                value: box Expr::EllipsisLiteral(_),
                ..
            }),
        ] => true,
        _ => false,
    }
}
