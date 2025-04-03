/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;

use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprName;
use ruff_python_ast::Identifier;
use ruff_python_ast::Keyword;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtImportFrom;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::binding::binding::AnnotationStyle;
use crate::binding::binding::AnnotationTarget;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::BindingExpect;
use crate::binding::binding::Initialized;
use crate::binding::binding::IsAsync;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyExpect;
use crate::binding::binding::RaisedException;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::bindings::LookupKind;
use crate::binding::narrow::NarrowOps;
use crate::binding::scope::FlowStyle;
use crate::binding::scope::LoopExit;
use crate::binding::scope::ScopeKind;
use crate::error::kind::ErrorKind;
use crate::export::special::SpecialExport;
use crate::graph::index::Idx;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;
use crate::ruff::ast::Ast;
use crate::state::loader::FindError;
use crate::types::alias::resolve_typeshed_alias;
use crate::types::special_form::SpecialForm;
use crate::types::types::Type;

impl<'a> BindingsBuilder<'a> {
    fn bind_unimportable_names(&mut self, x: &StmtImportFrom) {
        for x in &x.names {
            if &x.name != "*" {
                let asname = x.asname.as_ref().unwrap_or(&x.name);
                // We pass None as imported_from, since we are really faking up a local error definition
                self.bind_definition(asname, Binding::Type(Type::any_error()), None);
            }
        }
    }

    // Check that the variable name in a functional definition matches the first argument string
    fn check_functional_definition_name(&mut self, name: &Name, arg: &Expr) {
        if let Expr::StringLiteral(x) = arg {
            if x.value.to_str() != name.as_str() {
                self.error(
                    arg.range(),
                    format!("Expected string literal \"{}\"", name),
                    ErrorKind::InvalidArgument,
                );
            }
        } else {
            self.error(
                arg.range(),
                format!("Expected string literal \"{}\"", name),
                ErrorKind::InvalidArgument,
            );
        }
    }

    fn assign_type_var(&mut self, name: &ExprName, call: &mut ExprCall) {
        self.ensure_expr(&mut call.func);
        let mut iargs = call.arguments.args.iter_mut();
        if let Some(expr) = iargs.next() {
            self.ensure_expr(expr);
        }
        // The constraints (i.e., any positional arguments after the first)
        // and some keyword arguments are types.
        for arg in iargs {
            self.ensure_type(arg, &mut None);
        }
        for kw in call.arguments.keywords.iter_mut() {
            if let Some(id) = &kw.arg
                && (id.id == "bound" || id.id == "default")
            {
                self.ensure_type(&mut kw.value, &mut None);
            } else {
                self.ensure_expr(&mut kw.value);
            }
        }
        self.bind_assign(name, |ann| {
            Binding::TypeVar(
                ann,
                Identifier::new(name.id.clone(), name.range()),
                Box::new(call.clone()),
            )
        })
    }

    fn ensure_type_var_tuple_and_param_spec_args(&mut self, call: &mut ExprCall) {
        self.ensure_expr(&mut call.func);
        for arg in call.arguments.args.iter_mut() {
            self.ensure_expr(arg);
        }
        for kw in call.arguments.keywords.iter_mut() {
            if let Some(id) = &kw.arg
                && id.id == "default"
            {
                self.ensure_type(&mut kw.value, &mut None);
            } else {
                self.ensure_expr(&mut kw.value);
            }
        }
    }

    fn assign_param_spec(&mut self, name: &ExprName, call: &mut ExprCall) {
        self.ensure_type_var_tuple_and_param_spec_args(call);
        self.bind_assign(name, |ann| {
            Binding::ParamSpec(
                ann,
                Identifier::new(name.id.clone(), name.range()),
                Box::new(call.clone()),
            )
        })
    }

    fn assign_type_var_tuple(&mut self, name: &ExprName, call: &mut ExprCall) {
        self.ensure_type_var_tuple_and_param_spec_args(call);
        self.bind_assign(name, |ann| {
            Binding::TypeVarTuple(
                ann,
                Identifier::new(name.id.clone(), name.range()),
                Box::new(call.clone()),
            )
        })
    }

    fn assign_enum(
        &mut self,
        name: &ExprName,
        func: &mut Expr,
        arg_name: &mut Expr,
        members: &mut [Expr],
    ) {
        self.ensure_expr(func);
        self.ensure_expr(arg_name);
        for arg in &mut *members {
            self.ensure_expr(arg);
        }
        self.check_functional_definition_name(&name.id, arg_name);
        self.synthesize_enum_def(
            Identifier::new(name.id.clone(), name.range()),
            func.clone(),
            members,
        );
    }

    fn assign_typed_dict(
        &mut self,
        name: &ExprName,
        func: &mut Expr,
        arg_name: &Expr,
        args: &mut [Expr],
        keywords: &mut [Keyword],
    ) {
        self.ensure_expr(func);
        self.check_functional_definition_name(&name.id, arg_name);
        self.synthesize_typed_dict_def(
            Identifier::new(name.id.clone(), name.range),
            func.clone(),
            args,
            keywords,
        );
    }

    fn assign_typing_named_tuple(
        &mut self,
        name: &ExprName,
        func: &mut Expr,
        arg_name: &Expr,
        members: &[Expr],
    ) {
        self.ensure_expr(func);
        self.check_functional_definition_name(&name.id, arg_name);
        self.synthesize_typing_named_tuple_def(
            Identifier::new(name.id.clone(), name.range()),
            func.clone(),
            members,
        );
    }

    fn assign_collections_named_tuple(
        &mut self,
        name: &ExprName,
        func: &mut Expr,
        arg_name: &Expr,
        members: &mut [Expr],
        keywords: &mut [Keyword],
    ) {
        self.ensure_expr(func);
        self.check_functional_definition_name(&name.id, arg_name);
        self.synthesize_collections_named_tuple_def(
            Identifier::new(name.id.clone(), name.range()),
            members,
            keywords,
        );
    }

    fn assign_new_type(&mut self, name: &ExprName, new_type_name: &mut Expr, base: &mut Expr) {
        self.ensure_expr(new_type_name);
        self.check_functional_definition_name(&name.id, new_type_name);
        self.ensure_type(base, &mut None);
        self.synthesize_typing_new_type(
            Identifier::new(name.id.clone(), name.range()),
            base.clone(),
        );
    }

    fn ensure_mutable_name(&mut self, x: &ExprName) {
        let name = Ast::expr_name_identifier(x.clone());
        let binding = self
            .lookup_name(&name.id, LookupKind::Mutable)
            .map(Binding::Forward);
        self.ensure_name(&name, binding, LookupKind::Mutable);
    }

    fn ensure_nonlocal_name(&mut self, name: &Identifier) {
        let value = self
            .lookup_name(&name.id, LookupKind::Nonlocal)
            .map(Binding::Forward);
        let key = Key::Definition(ShortIdentifier::new(name));
        match value {
            Ok(value) => {
                self.table.insert(key, value);
            }
            Err(error) => {
                // Record a type error and fall back to `Any`.
                self.error(name.range, error.message(name), ErrorKind::UnknownName);
                self.table.insert(key, Binding::Type(Type::any_error()));
            }
        }
    }

    fn ensure_global_name(&mut self, name: &Identifier) {
        let value = self
            .lookup_name(&name.id, LookupKind::Global)
            .map(Binding::Forward);
        let key = Key::Definition(ShortIdentifier::new(name));
        match value {
            Ok(value) => {
                self.table.insert(key, value);
            }
            Err(error) => {
                // Record a type error and fall back to `Any`.
                self.error(name.range, error.message(name), ErrorKind::UnknownName);
                self.table.insert(key, Binding::Type(Type::any_error()));
            }
        }
    }

    /// If someone does `x = C["test"]`, that might be a type alias, it might not.
    /// Use this heuristic to detect things that are definitely type aliases.
    fn is_definitely_type_alias_rhs(&mut self, x: &Expr) -> bool {
        match x {
            Expr::Subscript(x) => matches!(
                self.as_special_export(&x.value),
                Some(SpecialExport::Union | SpecialExport::Optional)
            ),
            _ => false,
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
            Stmt::Return(mut x) => {
                self.ensure_expr_opt(x.value.as_deref_mut());
                self.functions.last_mut().returns.push(x);
                self.scopes.current_mut().flow.no_next = true;
            }
            Stmt::Delete(mut x) => {
                for target in &mut x.targets {
                    self.table.insert(
                        KeyExpect(target.range()),
                        BindingExpect::Delete(Box::new(target.clone())),
                    );
                    if let Expr::Name(name) = target {
                        self.ensure_mutable_name(name);
                    } else {
                        self.ensure_expr(target);
                    }
                    // If the target is a name, mark it as unbound
                    if let Expr::Name(name) = target {
                        let key = Key::Usage(ShortIdentifier::expr_name(name));
                        let idx = self.table.types.0.insert(key);
                        self.scopes
                            .update_flow_info(&name.id, idx, Some(FlowStyle::Unbound));
                    }
                }
            }
            Stmt::Assign(ref x)
                if let [Expr::Name(name)] = x.targets.as_slice()
                    && let Some((module, forward)) =
                        resolve_typeshed_alias(self.module_info.name(), &name.id, &x.value) =>
            {
                self.bind_assign(name, |_| Binding::Import(module, forward))
            }
            Stmt::Assign(mut x) => {
                if let [Expr::Name(name)] = x.targets.as_slice() {
                    if let Expr::Call(call) = &mut *x.value
                        && let Some(special) = self.as_special_export(&call.func)
                    {
                        match special {
                            SpecialExport::TypeVar => {
                                self.assign_type_var(name, call);
                                return;
                            }
                            SpecialExport::ParamSpec => {
                                self.assign_param_spec(name, call);
                                return;
                            }
                            SpecialExport::TypeVarTuple => {
                                self.assign_type_var_tuple(name, call);
                                return;
                            }
                            SpecialExport::Enum
                            | SpecialExport::IntEnum
                            | SpecialExport::StrEnum => {
                                if let Some((arg_name, members)) =
                                    call.arguments.args.split_first_mut()
                                {
                                    self.assign_enum(name, &mut call.func, arg_name, members);
                                    return;
                                }
                            }
                            SpecialExport::TypedDict => {
                                if let Some((arg_name, members)) =
                                    call.arguments.args.split_first_mut()
                                {
                                    self.assign_typed_dict(
                                        name,
                                        &mut call.func,
                                        arg_name,
                                        members,
                                        &mut call.arguments.keywords,
                                    );
                                    return;
                                }
                            }
                            SpecialExport::TypingNamedTuple => {
                                if let Some((arg_name, members)) =
                                    call.arguments.args.split_first_mut()
                                {
                                    self.assign_typing_named_tuple(
                                        name,
                                        &mut call.func,
                                        arg_name,
                                        members,
                                    );
                                    return;
                                }
                            }
                            SpecialExport::CollectionsNamedTuple => {
                                if let Some((arg_name, members)) =
                                    call.arguments.args.split_first_mut()
                                {
                                    self.assign_collections_named_tuple(
                                        name,
                                        &mut call.func,
                                        arg_name,
                                        members,
                                        &mut call.arguments.keywords,
                                    );
                                    return;
                                }
                            }
                            SpecialExport::NewType => {
                                if let [new_type_name, base] = &mut *call.arguments.args {
                                    self.assign_new_type(name, new_type_name, base);
                                    return;
                                }
                            }
                            _ => {}
                        }
                    }
                    if self.is_definitely_type_alias_rhs(&x.value) {
                        self.ensure_type(&mut x.value, &mut None);
                    } else {
                        self.ensure_expr(&mut x.value);
                    }
                    self.bind_assign(name, |k: Option<Idx<KeyAnnotation>>| {
                        Binding::NameAssign(
                            name.id.clone(),
                            k.map(|k| (AnnotationStyle::Forwarded, k)),
                            x.value,
                        )
                    });
                } else {
                    let mut value = *x.value;
                    self.ensure_expr(&mut value);
                    for target in &mut x.targets {
                        let make_binding =
                            |k: Option<Idx<KeyAnnotation>>| Binding::Expr(k, value.clone());
                        self.bind_target(target, &make_binding, Some(&value));
                        self.ensure_expr(target);
                    }
                }
            }
            Stmt::AugAssign(mut x) => {
                if let Expr::Name(name) = &*x.target {
                    self.ensure_mutable_name(name);
                } else {
                    self.ensure_expr(&mut x.target);
                }
                self.ensure_expr(&mut x.value);
                let make_binding = |k: Option<Idx<KeyAnnotation>>| Binding::AugAssign(k, x.clone());
                self.bind_target(&x.target, &make_binding, None);
            }
            Stmt::AnnAssign(mut x) => match *x.target {
                Expr::Name(name) => {
                    let name = Ast::expr_name_identifier(name);
                    let ann_key = KeyAnnotation::Annotation(ShortIdentifier::new(&name));
                    let in_class_body =
                        matches!(self.scopes.current().kind, ScopeKind::ClassBody(_));
                    self.ensure_type(&mut x.annotation, &mut None);
                    let ann_val = if let Some(special) = SpecialForm::new(&name.id, &x.annotation) {
                        BindingAnnotation::Type(
                            AnnotationTarget::Assign(name.id.clone(), Initialized::Yes),
                            special.to_type(),
                        )
                    } else {
                        BindingAnnotation::AnnotateExpr(
                            if in_class_body {
                                AnnotationTarget::ClassMember(name.id.clone())
                            } else {
                                AnnotationTarget::Assign(
                                    name.id.clone(),
                                    if x.value.is_some() {
                                        Initialized::Yes
                                    } else {
                                        Initialized::No
                                    },
                                )
                            },
                            *x.annotation.clone(),
                            None,
                        )
                    };
                    let ann_key = self.table.insert(ann_key, ann_val);
                    let flow_style = if in_class_body {
                        let initial_value = x.value.as_deref().cloned();
                        Some(FlowStyle::AnnotatedClassField { initial_value })
                    } else if x.value.is_some() {
                        None
                    } else {
                        Some(FlowStyle::Uninitialized)
                    };
                    let binding_value = if let Some(value) = x.value {
                        // Treat a name as initialized, but skip actually checking the value, if we are assigning `...` in a stub.
                        if self.module_info.path().is_interface()
                            && matches!(&*value, Expr::EllipsisLiteral(_))
                        {
                            None
                        } else {
                            Some(value)
                        }
                    } else {
                        None
                    };
                    let binding = if let Some(mut value) = binding_value {
                        // Handle forward references in explicit type aliases.
                        if self.as_special_export(&x.annotation) == Some(SpecialExport::TypeAlias) {
                            self.ensure_type(&mut value, &mut None);
                        } else {
                            self.ensure_expr(&mut value);
                        }
                        Binding::NameAssign(
                            name.id.clone(),
                            Some((AnnotationStyle::Direct, ann_key)),
                            value,
                        )
                    } else {
                        Binding::AnnotatedType(
                            ann_key,
                            Box::new(Binding::Type(Type::any_implicit())),
                        )
                    };
                    if let Some(ann) = self.bind_definition(&name, binding, flow_style)
                        && ann != ann_key
                    {
                        self.table.insert(
                            KeyExpect(name.range),
                            BindingExpect::Eq(ann_key, ann, name.id.clone()),
                        );
                    }
                }
                Expr::Attribute(mut attr) => {
                    self.ensure_expr(&mut attr.value);
                    self.ensure_type(&mut x.annotation, &mut None);
                    let ann_key = self.table.insert(
                        KeyAnnotation::AttrAnnotation(x.annotation.range()),
                        BindingAnnotation::AnnotateExpr(
                            AnnotationTarget::ClassMember(attr.attr.id.clone()),
                            *x.annotation,
                            None,
                        ),
                    );
                    let value_binding = match &x.value {
                        Some(v) => Binding::Expr(None, *v.clone()),
                        None => Binding::Type(Type::any_implicit()),
                    };
                    if !self.bind_attr_if_self(&attr, value_binding, Some(ann_key)) {
                        self.error(
                             x.range,
                             format!(
                                 "Type cannot be declared in assignment to non-self attribute `{}.{}`",
                                 self.module_info.display(&attr.value),
                                 attr.attr.id,
                             ),
                             ErrorKind::BadAssignment,
                         );
                    }
                    if let Some(box mut v) = x.value {
                        self.ensure_expr(&mut v);
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
                    let binding = Binding::ScopedTypeAlias(
                        name.id.clone(),
                        x.type_params.map(|x| *x),
                        x.value,
                    );
                    self.bind_definition(&Ast::expr_name_identifier(name), binding, None);
                } else {
                    self.todo("Bindings::stmt TypeAlias", &x);
                }
            }
            Stmt::For(mut x) => {
                self.setup_loop(x.range, &NarrowOps::new());
                self.ensure_expr(&mut x.iter);
                let make_binding =
                    |k| Binding::IterableValue(k, *x.iter.clone(), IsAsync::new(x.is_async));
                self.bind_target(&x.target, &make_binding, None);
                self.ensure_expr(&mut x.target);
                self.stmts(x.body);
                self.teardown_loop(x.range, &NarrowOps::new(), x.orelse);
            }
            Stmt::While(mut x) => {
                let narrow_ops = NarrowOps::from_expr(Some(&x.test));
                self.setup_loop(x.range, &narrow_ops);
                self.ensure_expr(&mut x.test);
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
                let mut implicit_else = true;
                for (range, test, body) in Ast::if_branches_owned(x) {
                    let b = self.config.evaluate_bool_opt(test.as_ref());
                    if b == Some(false) {
                        continue; // We won't pick this branch
                    }
                    self.bind_narrow_ops(&negated_prev_ops, range);
                    let mut base = self.scopes.current().flow.clone();
                    let new_narrow_ops = NarrowOps::from_expr(test.as_ref());
                    if let Some(mut e) = test {
                        self.ensure_expr(&mut e);
                        self.table
                            .insert(Key::Anon(e.range()), Binding::Expr(None, e));
                    } else {
                        implicit_else = false;
                    }
                    self.bind_narrow_ops(&new_narrow_ops, range);
                    negated_prev_ops.and_all(new_narrow_ops.negate());
                    self.stmts(body);
                    mem::swap(&mut self.scopes.current_mut().flow, &mut base);
                    branches.push(base);
                    if b == Some(true) {
                        exhaustive = true;
                        break; // We picked this branch, none others stand a chance
                    }
                }
                if implicit_else {
                    // If there is no explicit else branch, we still want to merge the negated ops
                    // from the previous branches into the flow env.
                    // Note, using a default use_range is OK. The range is only needed to make the
                    // key distinct from other keys.
                    self.bind_narrow_ops(&negated_prev_ops, TextRange::default());
                }
                if !exhaustive {
                    branches.push(mem::take(&mut self.scopes.current_mut().flow));
                }
                self.scopes.current_mut().flow = self.merge_flow(branches, range);
            }
            Stmt::With(x) => {
                let kind = IsAsync::new(x.is_async);
                for mut item in x.items {
                    self.ensure_expr(&mut item.context_expr);
                    let item_range = item.range();
                    let expr_range = item.context_expr.range();
                    let context_idx = self.table.insert(
                        Key::ContextExpr(expr_range),
                        Binding::Expr(None, item.context_expr),
                    );
                    if let Some(mut opts) = item.optional_vars {
                        let make_binding = |k: Option<Idx<KeyAnnotation>>| {
                            Binding::ContextValue(k, context_idx, expr_range, kind)
                        };
                        self.bind_target(&opts, &make_binding, None);
                        self.ensure_expr(&mut opts);
                    } else {
                        self.table.insert(
                            Key::Anon(item_range),
                            Binding::ContextValue(None, context_idx, expr_range, kind),
                        );
                    }
                }
                self.stmts(x.body);
            }
            Stmt::Match(x) => {
                self.stmt_match(x);
            }
            Stmt::Raise(x) => {
                if let Some(mut exc) = x.exc {
                    self.ensure_expr(&mut exc);
                    let raised = if let Some(mut cause) = x.cause {
                        self.ensure_expr(&mut cause);
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
                        && let Some(mut type_) = h.type_
                    {
                        self.ensure_expr(&mut type_);
                        self.bind_definition(
                            &name,
                            Binding::ExceptionHandler(type_, x.is_star),
                            None,
                        );
                    } else if let Some(mut type_) = h.type_ {
                        self.ensure_expr(&mut type_);
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
            Stmt::Assert(mut x) => {
                self.ensure_expr(&mut x.test);
                self.bind_narrow_ops(&NarrowOps::from_expr(Some(&x.test)), x.range);
                self.table
                    .insert(Key::Anon(x.test.range()), Binding::Expr(None, *x.test));
                if let Some(mut msg_expr) = x.msg {
                    self.ensure_expr(&mut msg_expr);
                    self.table.insert(
                        KeyExpect(msg_expr.range()),
                        BindingExpect::TypeCheckExpr(Box::new(*msg_expr)),
                    );
                };
            }
            Stmt::Import(x) => {
                for x in x.names {
                    let m = ModuleName::from_name(&x.name.id);
                    if let Err(FindError::NotFound(err)) = self.lookup.get(m) {
                        self.error(
                            x.range,
                            FindError::display(err, m),
                            ErrorKind::MissingModuleAttribute,
                        );
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
                            let exported = module_exports.exports(self.lookup);
                            for x in x.names {
                                if &x.name == "*" {
                                    for name in module_exports.wildcard(self.lookup).iter_hashed() {
                                        let key = Key::Import(name.into_key().clone(), x.range);
                                        let val = if exported.contains_key_hashed(name) {
                                            Binding::Import(m, name.into_key().clone())
                                        } else {
                                            self.error(
                                                x.range,
                                                format!("Could not import `{name}` from `{m}`"),
                                                ErrorKind::MissingModuleAttribute,
                                            );
                                            Binding::Type(Type::any_error())
                                        };
                                        let key = self.table.insert(key, val);
                                        self.bind_key(
                                            name.key(),
                                            key,
                                            Some(FlowStyle::Import(m, name.into_key().clone())),
                                        );
                                    }
                                } else {
                                    let asname = x.asname.unwrap_or_else(|| x.name.clone());
                                    // A `from x import y` statement is ambiguous; if `x` is a package with
                                    // an `__init__.py` file, then it might import the name `y` from the
                                    // module `x` defined by the `__init__.py` file, or it might import a
                                    // submodule `x.y` of the package `x`.
                                    //
                                    // If both are present, generally we prefer the name defined in `x`,
                                    // but there is an exception: if we are already looking at the
                                    // `__init__` module of `x`, we always prefer the submodule.
                                    let val = if (self.module_info.name() != m)
                                        && exported.contains_key(&x.name.id)
                                    {
                                        Binding::Import(m, x.name.id.clone())
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
                                                ErrorKind::MissingModuleAttribute,
                                            );
                                            Binding::Type(Type::any_error())
                                        }
                                    };
                                    self.bind_definition(
                                        &asname,
                                        val,
                                        Some(FlowStyle::Import(m, x.name.id)),
                                    );
                                }
                            }
                        }
                        Err(FindError::NotFound(err)) => {
                            self.error(
                                x.range,
                                FindError::display(err, m),
                                ErrorKind::MissingModuleAttribute,
                            );
                            self.bind_unimportable_names(&x);
                        }
                        Err(FindError::Ignored) => self.bind_unimportable_names(&x),
                    }
                } else {
                    self.error(
                        x.range,
                        format!(
                            "Could not resolve relative import `{}`",
                            ".".repeat(x.level as usize)
                        ),
                        ErrorKind::ImportError,
                    );
                    self.bind_unimportable_names(&x);
                }
            }
            Stmt::Global(x) => {
                for name in x.names {
                    self.ensure_global_name(&name);
                }
            }
            Stmt::Nonlocal(x) => {
                for name in x.names {
                    self.ensure_nonlocal_name(&name);
                }
            }
            Stmt::Expr(mut x) => {
                self.ensure_expr(&mut x.value);
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
            Stmt::IpyEscapeCommand(x) => self.error(
                x.range,
                "IPython escapes are not supported".to_owned(),
                ErrorKind::Unsupported,
            ),
        }
    }
}
