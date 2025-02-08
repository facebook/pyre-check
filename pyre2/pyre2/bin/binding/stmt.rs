/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;
use std::sync::Arc;

use ruff_python_ast::Expr;
use ruff_python_ast::ExprCall;
use ruff_python_ast::Identifier;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtImportFrom;
use ruff_text_size::Ranged;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::BindingExpect;
use crate::binding::binding::ContextManagerKind;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyExpect;
use crate::binding::binding::RaisedException;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::narrow::NarrowOps;
use crate::binding::scope::FlowStyle;
use crate::binding::scope::LoopExit;
use crate::export::special::SpecialExport;
use crate::graph::index::Idx;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::special_form::SpecialForm;
use crate::types::types::AnyStyle;
use crate::util::display::DisplayWith;

impl<'a> BindingsBuilder<'a> {
    fn bind_unimportable_names(&mut self, x: &StmtImportFrom) {
        for x in &x.names {
            if &x.name != "*" {
                let asname = x.asname.as_ref().unwrap_or(&x.name);
                // We pass None as imported_from, since we are really faking up a local error definition
                self.bind_definition(asname, Binding::AnyType(AnyStyle::Error), None);
            }
        }
    }

    /// Evaluate the statements and update the bindings.
    /// Every statement should end up in the bindings, perhaps with a location that is never used.
    pub fn stmt(&mut self, x: Stmt) {
        match x {
            Stmt::FunctionDef(x) => {
                self.function_def(x);
            }
            Stmt::ClassDef(x) => self.class_def(x),
            Stmt::Return(x) => {
                self.ensure_expr_opt(x.value.as_deref());
                self.functions.last_mut().returns.push(x);
                self.scopes.current_mut().flow.no_next = true;
            }
            Stmt::Delete(x) => self.todo("Bindings::stmt", &x),
            Stmt::Assign(x) => {
                let name = if x.targets.len() == 1
                    && let Expr::Name(name) = &x.targets[0]
                {
                    Some(name.id.clone())
                } else {
                    None
                };
                let mut value = *x.value;
                let mut is_synthesized_class = false;
                match &mut value {
                    // Handle forward references in a TypeVar call.
                    Expr::Call(ExprCall {
                        range: _,
                        func,
                        arguments,
                    }) if self.as_special_export(func) == Some(SpecialExport::TypeVar)
                        && !arguments.is_empty() =>
                    {
                        self.ensure_expr(func);
                        // The constraints (i.e., any positional arguments after the first)
                        // and some keyword arguments are types.
                        for arg in arguments.args.iter_mut().skip(1) {
                            self.ensure_type(arg, &mut None);
                        }
                        for kw in arguments.keywords.iter_mut() {
                            if let Some(id) = &kw.arg
                                && (id.id == "bound" || id.id == "default")
                            {
                                self.ensure_type(&mut kw.value, &mut None);
                            } else {
                                self.ensure_expr(&kw.value);
                            }
                        }
                    }
                    Expr::Call(ExprCall {
                        range: _,
                        func: box ref func @ Expr::Name(ref base_name),
                        arguments,
                    }) if matches!(
                        self.as_special_export(func),
                        Some(SpecialExport::Enum | SpecialExport::IntEnum | SpecialExport::StrEnum)
                    ) && arguments.keywords.is_empty()
                        && let Some(name) = &name =>
                    {
                        self.ensure_expr(func);
                        for arg in arguments.args.iter_mut() {
                            self.ensure_expr(arg);
                        }
                        // Use the variable name, not the string literal argument
                        self.synthesize_enum_def(
                            Identifier::new(name.clone(), x.targets[0].range()),
                            base_name.clone(),
                            &arguments.args[1..],
                        );
                        is_synthesized_class = true;
                    }
                    _ => self.ensure_expr(&value),
                }
                if !is_synthesized_class {
                    for target in x.targets {
                        let make_binding = |k: Option<Idx<KeyAnnotation>>| {
                            if let Some(name) = &name {
                                Binding::NameAssign(name.clone(), k, Box::new(value.clone()))
                            } else {
                                Binding::Expr(k, value.clone())
                            }
                        };
                        self.bind_target(&target, &make_binding, Some(&value));
                        self.ensure_expr(&target);
                    }
                }
            }
            Stmt::AugAssign(x) => {
                self.ensure_expr(&x.target);
                self.ensure_expr(&x.value);
                let make_binding = |_: Option<Idx<KeyAnnotation>>| Binding::AugAssign(x.clone());
                self.bind_target(&x.target, &make_binding, None);
            }
            Stmt::AnnAssign(mut x) => match *x.target {
                Expr::Name(name) => {
                    let name = Ast::expr_name_identifier(name);
                    let ann_key = KeyAnnotation::Annotation(ShortIdentifier::new(&name));
                    self.ensure_type(&mut x.annotation, &mut None);
                    let ann_val = if let Some(special) = SpecialForm::new(&name.id, &x.annotation) {
                        BindingAnnotation::Type(special.to_type())
                    } else {
                        BindingAnnotation::AnnotateExpr(*x.annotation.clone(), None)
                    };
                    let ann_key = self.table.insert(ann_key, ann_val);

                    let (value, is_initialized) = if let Some(value) = x.value {
                        // Treat a name as initialized, but skip actually checking the value, if we are assigning `...` in a stub.
                        if self.module_info.path().is_interface()
                            && matches!(&*value, Expr::EllipsisLiteral(_))
                        {
                            (None, true)
                        } else {
                            (Some(value), true)
                        }
                    } else {
                        (None, false)
                    };

                    let binding = if let Some(mut value) = value {
                        // Handle forward references in explicit type aliases.
                        if self.as_special_export(&x.annotation) == Some(SpecialExport::TypeAlias) {
                            self.ensure_type(&mut value, &mut None);
                        } else {
                            self.ensure_expr(&value);
                        }
                        Binding::NameAssign(name.id.clone(), Some(ann_key), value)
                    } else {
                        Binding::AnnotatedType(
                            ann_key,
                            Box::new(Binding::AnyType(AnyStyle::Implicit)),
                        )
                    };
                    if let Some(ann) = self.bind_definition(
                        &name,
                        binding,
                        Some(FlowStyle::Annotated { is_initialized }),
                    ) && ann != ann_key
                    {
                        self.table.insert(
                            KeyExpect(name.range),
                            BindingExpect::Eq(ann_key, ann, name.id.clone()),
                        );
                    }
                }
                Expr::Attribute(attr) => {
                    self.ensure_expr(&attr.value);
                    self.ensure_type(&mut x.annotation, &mut None);
                    let ann_key = self.table.insert(
                        KeyAnnotation::AttrAnnotation(x.annotation.range()),
                        BindingAnnotation::AnnotateExpr(*x.annotation, None),
                    );
                    let value_binding = match &x.value {
                        Some(v) => Binding::Expr(None, *v.clone()),
                        None => Binding::AnyType(AnyStyle::Implicit),
                    };
                    if !self.bind_attr_if_self(&attr, value_binding, Some(ann_key)) {
                        self.error(
                            x.range,
                            format!(
                                "Type cannot be declared in assignment to non-self attribute `{}.{}`",
                                attr.value.display_with(&self.module_info),
                                attr.attr.id,
                            ),
                        );
                    }
                    if let Some(box v) = x.value {
                        self.ensure_expr(&v);
                        self.table.insert(
                            KeyExpect(v.range()),
                            BindingExpect::CheckAssignExprToAttribute(Box::new((attr, v))),
                        );
                    }
                }
                _ => self.todo("Bindings::stmt AnnAssign", &x),
            },
            Stmt::TypeAlias(mut x) => {
                if let Expr::Name(name) = *x.name {
                    if let Some(params) = &mut x.type_params {
                        self.type_params(params);
                    }
                    self.ensure_type(&mut x.value, &mut None);
                    let binding = Binding::ScopedTypeAlias(name.id.clone(), x.type_params, x.value);
                    self.bind_definition(&Ast::expr_name_identifier(name), binding, None);
                } else {
                    self.todo("Bindings::stmt TypeAlias", &x);
                }
            }
            Stmt::For(x) => {
                self.setup_loop(x.range, &NarrowOps::new());
                self.ensure_expr(&x.iter);
                let make_binding = |k| Binding::IterableValue(k, *x.iter.clone());
                self.bind_target(&x.target, &make_binding, None);
                self.ensure_expr(&x.target);
                self.stmts(x.body);
                self.teardown_loop(x.range, &NarrowOps::new(), x.orelse);
            }
            Stmt::While(x) => {
                let narrow_ops = NarrowOps::from_expr(Some(&x.test));
                self.setup_loop(x.range, &narrow_ops);
                self.ensure_expr(&x.test);
                self.table
                    .insert(Key::Anon(x.test.range()), Binding::Expr(None, *x.test));
                self.stmts(x.body);
                self.teardown_loop(x.range, &narrow_ops, x.orelse);
            }
            Stmt::If(x) => {
                let range = x.range;
                let mut exhaustive = false;
                let mut branches = Vec::new();
                // Type narrowing operations that are carried over from one branch to the next. For example, in:
                //   if x is None:
                //     pass
                //   else:
                //     pass
                // x is bound to Narrow(x, Is(None)) in the if branch, and the negation, Narrow(x, IsNot(None)),
                // is carried over to the else branch.
                let mut negated_prev_ops = NarrowOps::new();
                for (test, body) in Ast::if_branches_owned(x) {
                    let b = self.config.evaluate_bool_opt(test.as_ref());
                    if b == Some(false) {
                        continue; // We won't pick this branch
                    }
                    let mut base = self.scopes.current().flow.clone();
                    let new_narrow_ops = NarrowOps::from_expr(test.as_ref());
                    if let Some(e) = test {
                        self.ensure_expr(&e);
                        self.table
                            .insert(Key::Anon(e.range()), Binding::Expr(None, e));
                    }
                    if let Some(stmt) = body.first() {
                        let use_range = stmt.range();
                        self.bind_narrow_ops(&negated_prev_ops, use_range);
                        self.bind_narrow_ops(&new_narrow_ops, use_range);
                    }
                    negated_prev_ops.and_all(new_narrow_ops.negate());
                    self.stmts(body);
                    mem::swap(&mut self.scopes.current_mut().flow, &mut base);
                    branches.push(base);
                    if b == Some(true) {
                        exhaustive = true;
                        break; // We picked this branch, none others stand a chance
                    }
                }
                if !exhaustive {
                    branches.push(mem::take(&mut self.scopes.current_mut().flow));
                }
                self.scopes.current_mut().flow = self.merge_flow(branches, range);
            }
            Stmt::With(x) => {
                let kind = if x.is_async {
                    ContextManagerKind::Async
                } else {
                    ContextManagerKind::Sync
                };
                for item in x.items {
                    self.ensure_expr(&item.context_expr);
                    if let Some(opts) = item.optional_vars {
                        let make_binding = |k: Option<Idx<KeyAnnotation>>| {
                            Binding::ContextValue(k, item.context_expr.clone(), kind)
                        };
                        self.bind_target(&opts, &make_binding, None);
                        self.ensure_expr(&opts);
                    } else {
                        self.table.insert(
                            Key::Anon(item.range()),
                            Binding::ContextValue(None, item.context_expr, kind),
                        );
                    }
                }
                self.stmts(x.body);
            }
            Stmt::Match(x) => {
                self.stmt_match(x);
            }
            Stmt::Raise(x) => {
                if let Some(exc) = x.exc {
                    self.ensure_expr(&exc);
                    let raised = if let Some(cause) = x.cause {
                        self.ensure_expr(&cause);
                        RaisedException::WithCause(Box::new((*exc, *cause)))
                    } else {
                        RaisedException::WithoutCause(*exc)
                    };
                    self.table.insert(
                        KeyExpect(x.range),
                        BindingExpect::CheckRaisedException(raised),
                    );
                } else {
                    // If there's no exception raised, don't bother checking the cause.
                }
                self.scopes.current_mut().flow.no_next = true;
            }
            Stmt::Try(x) => {
                let range = x.range;
                let mut branches = Vec::new();
                let mut base = self.scopes.current().flow.clone();

                // We branch before the body, conservatively assuming that any statement can fail
                // entry -> try -> else -> finally
                //   |                     ^
                //   ----> handler --------|

                self.stmts(x.body);
                self.stmts(x.orelse);
                mem::swap(&mut self.scopes.current_mut().flow, &mut base);
                branches.push(base);

                for h in x.handlers {
                    base = self.scopes.current().flow.clone();
                    let range = h.range();
                    let h = h.except_handler().unwrap(); // Only one variant for now
                    if let Some(name) = h.name
                        && let Some(type_) = h.type_
                    {
                        self.ensure_expr(&type_);
                        self.bind_definition(
                            &name,
                            Binding::ExceptionHandler(type_, x.is_star),
                            None,
                        );
                    } else if let Some(type_) = h.type_ {
                        self.ensure_expr(&type_);
                        self.table.insert(
                            Key::Anon(range),
                            Binding::ExceptionHandler(type_, x.is_star),
                        );
                    }
                    self.stmts(h.body);
                    mem::swap(&mut self.scopes.current_mut().flow, &mut base);
                    branches.push(base);
                }

                self.scopes.current_mut().flow = self.merge_flow(branches, range);
                self.stmts(x.finalbody);
            }
            Stmt::Assert(x) => {
                self.ensure_expr(&x.test);
                self.bind_narrow_ops(&NarrowOps::from_expr(Some(&x.test)), x.range);
                self.table
                    .insert(Key::Anon(x.test.range()), Binding::Expr(None, *x.test));
                if let Some(msg_expr) = x.msg {
                    self.ensure_expr(&msg_expr);
                    self.table
                        .insert(Key::Anon(msg_expr.range()), Binding::Expr(None, *msg_expr));
                };
            }
            Stmt::Import(x) => {
                for x in x.names {
                    let m = ModuleName::from_name(&x.name.id);
                    if let Err(err) = self.lookup.get(m) {
                        self.error(x.range, Arc::unwrap_or_clone(err));
                    }
                    match x.asname {
                        Some(asname) => {
                            self.bind_definition(
                                &asname,
                                Binding::Module(m, m.components(), None),
                                Some(FlowStyle::ImportAs(m)),
                            );
                        }
                        None => {
                            let first = m.first_component();
                            let flow_info = self.scopes.current().flow.info.get(&first);
                            let module_key = match flow_info {
                                Some(flow_info)
                                    if matches!(
                                        flow_info.style,
                                        Some(FlowStyle::MergeableImport(_))
                                    ) =>
                                {
                                    Some(flow_info.key)
                                }
                                _ => None,
                            };
                            let key = self.table.insert(
                                Key::Import(first.clone(), x.name.range),
                                Binding::Module(m, vec![first.clone()], module_key),
                            );
                            self.bind_key(&first, key, Some(FlowStyle::MergeableImport(m)));
                        }
                    }
                }
            }
            Stmt::ImportFrom(x) => {
                if let Some(m) = self.module_info.name().new_maybe_relative(
                    self.module_info.path().is_init(),
                    x.level,
                    x.module.as_ref().map(|x| &x.id),
                ) {
                    match self.lookup.get(m) {
                        Ok(module_exports) => {
                            for x in x.names {
                                if &x.name == "*" {
                                    for name in module_exports.wildcard(self.lookup).iter() {
                                        let key = Key::Import(name.clone(), x.range);
                                        let val = if module_exports.contains(name, self.lookup) {
                                            Binding::Import(m, name.clone())
                                        } else {
                                            self.error(
                                                x.range,
                                                format!("Could not import `{name}` from `{m}`"),
                                            );
                                            Binding::AnyType(AnyStyle::Error)
                                        };
                                        let key = self.table.insert(key, val);
                                        self.bind_key(name, key, Some(FlowStyle::Import(m)));
                                    }
                                } else {
                                    let asname = x.asname.unwrap_or_else(|| x.name.clone());
                                    let val = if module_exports.contains(&x.name.id, self.lookup) {
                                        Binding::Import(m, x.name.id)
                                    } else {
                                        let x_as_module_name = m.append(&x.name.id);
                                        if self.lookup.get(x_as_module_name).is_ok() {
                                            Binding::Module(
                                                x_as_module_name,
                                                x_as_module_name.components(),
                                                None,
                                            )
                                        } else {
                                            self.error(
                                                x.range,
                                                format!(
                                                    "Could not import `{}` from `{m}`",
                                                    x.name.id
                                                ),
                                            );
                                            Binding::AnyType(AnyStyle::Error)
                                        }
                                    };
                                    self.bind_definition(&asname, val, Some(FlowStyle::Import(m)));
                                }
                            }
                        }
                        Err(err) => {
                            self.error(x.range, Arc::unwrap_or_clone(err));
                            self.bind_unimportable_names(&x);
                        }
                    }
                } else {
                    self.error(
                        x.range,
                        format!(
                            "Could not resolve relative import `{}`",
                            ".".repeat(x.level as usize)
                        ),
                    );
                    self.bind_unimportable_names(&x);
                }
            }
            Stmt::Global(x) => self.todo("Bindings::stmt", &x),
            Stmt::Nonlocal(x) => self.todo("Bindings::stmt", &x),
            Stmt::Expr(x) => {
                self.ensure_expr(&x.value);
                self.table.insert(
                    Key::StmtExpr(x.value.range()),
                    Binding::Expr(None, *x.value),
                );
            }
            Stmt::Pass(_) => { /* no-op */ }
            Stmt::Break(x) => {
                self.add_loop_exitpoint(LoopExit::Break, x.range);
            }
            Stmt::Continue(x) => {
                self.add_loop_exitpoint(LoopExit::Continue, x.range);
            }
            Stmt::IpyEscapeCommand(x) => {
                self.error(x.range, "IPython escapes are not supported".to_owned())
            }
        }
    }
}
