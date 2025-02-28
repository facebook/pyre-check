/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;

use itertools::Either;
use ruff_python_ast::Arguments;
use ruff_python_ast::BoolOp;
use ruff_python_ast::Comprehension;
use ruff_python_ast::Decorator;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprBoolOp;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprLambda;
use ruff_python_ast::ExprSubscript;
use ruff_python_ast::Identifier;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::Key;
use crate::binding::binding::SuperStyle;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::bindings::LegacyTParamBuilder;
use crate::binding::narrow::NarrowOps;
use crate::binding::scope::Flow;
use crate::binding::scope::Scope;
use crate::binding::scope::ScopeKind;
use crate::dunder;
use crate::export::special::SpecialExport;
use crate::graph::index::Idx;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::callable::unexpected_keyword;
use crate::types::types::AnyStyle;
use crate::visitors::Visitors;

impl<'a> BindingsBuilder<'a> {
    /// Given a name appearing in an expression, create a `Usage` key for that
    /// name at the current location. The binding will indicate how to compute
    /// the type if we found that name in scope; if we do not find the name we
    /// record an error and fall back to `Any`.
    ///
    /// This function is the the core scope lookup logic for binding creation.
    fn ensure_name(&mut self, name: &Identifier, value: Option<Binding>) {
        if name.is_empty() {
            // We only get empty identifiers if Ruff has done error correction,
            // so there must be a parse error.
            // Occasionally Ruff might give out the same Identifier twice in an error.
            return;
        }

        let key = Key::Usage(ShortIdentifier::new(name));
        match value {
            Some(value) => {
                self.table.insert(key, value);
            }
            None if name.id == dunder::FILE || name.id == dunder::NAME => {
                self.table.insert(key, Binding::StrType);
            }
            None => {
                // Name wasn't found. Record a type error and fall back to `Any`.
                self.error(name.range, format!("Could not find name `{name}`"));
                self.table.insert(key, Binding::AnyType(AnyStyle::Error));
            }
        }
    }

    fn bind_comprehensions(&mut self, range: TextRange, comprehensions: &mut [Comprehension]) {
        self.scopes.push(Scope::comprehension(range));
        for comp in comprehensions.iter_mut() {
            self.scopes.current_mut().stat.expr_lvalue(&comp.target);
            let make_binding = |k| Binding::IterableValue(k, comp.iter.clone());
            self.bind_target(&comp.target, &make_binding, None);
            self.ensure_expr(&mut comp.target);
            for x in comp.ifs.iter() {
                let narrow_ops = NarrowOps::from_expr(Some(x));
                self.bind_narrow_ops(&narrow_ops, comp.range);
            }
        }
    }

    pub fn bind_lambda(&mut self, lambda: &ExprLambda) {
        self.scopes.push(Scope::function(lambda.range));
        if let Some(parameters) = &lambda.parameters {
            for x in parameters.iter() {
                self.bind_lambda_param(x.name());
            }
        }
    }

    /// Helper to clean up an expression that does type narrowing. We merge flows for the narrowing
    /// operation and its negation, so that narrowing is limited to the body of the expression but
    /// newly defined names persist.
    fn negate_and_merge_flow(
        &mut self,
        base: Flow,
        ops: &NarrowOps,
        orelse: Option<&mut Expr>,
        range: TextRange,
    ) {
        let if_branch = mem::take(&mut self.scopes.current_mut().flow);
        self.scopes.current_mut().flow = base;
        self.bind_narrow_ops(&ops.negate(), range);
        self.ensure_expr_opt(orelse);
        let else_branch = mem::take(&mut self.scopes.current_mut().flow);
        self.scopes.current_mut().flow = self.merge_flow(vec![if_branch, else_branch], range);
    }

    /// Execute through the expr, ensuring every name has a binding.
    pub fn ensure_expr(&mut self, x: &mut Expr) {
        let new_scope = match x {
            Expr::If(x) => {
                // Ternary operation. We treat it like an if/else statement.
                let base = self.scopes.current().flow.clone();
                self.ensure_expr(&mut x.test);
                let narrow_ops = NarrowOps::from_expr(Some(&x.test));
                self.bind_narrow_ops(&narrow_ops, x.body.range());
                self.ensure_expr(&mut x.body);
                let range = x.range();
                self.negate_and_merge_flow(base, &narrow_ops, Some(&mut x.orelse), range);
                return;
            }
            Expr::BoolOp(ExprBoolOp { range, op, values }) => {
                let base = self.scopes.current().flow.clone();
                let mut narrow_ops = NarrowOps::new();
                for value in values {
                    self.bind_narrow_ops(&narrow_ops, value.range());
                    self.ensure_expr(value);
                    let new_narrow_ops = NarrowOps::from_expr(Some(value));
                    match op {
                        BoolOp::And => {
                            // Every subsequent value is evaluated only if all previous values were truthy.
                            narrow_ops.and_all(new_narrow_ops);
                        }
                        BoolOp::Or => {
                            // Every subsequent value is evaluated only if all previous values were falsy.
                            narrow_ops.and_all(new_narrow_ops.negate());
                        }
                    }
                }
                self.negate_and_merge_flow(base, &narrow_ops, None, *range);
                return;
            }
            Expr::Call(ExprCall {
                range: _,
                func,
                arguments,
            }) if self.as_special_export(func) == Some(SpecialExport::AssertType)
                && arguments.args.len() > 1 =>
            {
                // Handle forward references in the second argument to an assert_type call
                self.ensure_expr(func);
                for (i, arg) in arguments.args.iter_mut().enumerate() {
                    if i == 1 {
                        self.ensure_type(arg, &mut None);
                    } else {
                        self.ensure_expr(arg);
                    }
                }
                for kw in arguments.keywords.iter_mut() {
                    self.ensure_expr(&mut kw.value);
                }
                return;
            }
            Expr::Call(ExprCall {
                range: _,
                func,
                arguments,
            }) if self.as_special_export(func) == Some(SpecialExport::Cast)
                && !arguments.is_empty() =>
            {
                // Handle forward references in the first argument to a cast call
                self.ensure_expr(func);
                if let Some(arg) = arguments.args.first_mut() {
                    self.ensure_type(arg, &mut None)
                }
                for arg in arguments.args.iter_mut().skip(1) {
                    self.ensure_expr(arg);
                }
                for kw in arguments.keywords.iter_mut() {
                    if let Some(id) = &kw.arg
                        && id.as_str() == "typ"
                    {
                        self.ensure_type(&mut kw.value, &mut None);
                    } else {
                        self.ensure_expr(&mut kw.value);
                    }
                }
                return;
            }
            Expr::Call(ExprCall {
                range,
                func,
                arguments:
                    Arguments {
                        range: _,
                        args: posargs,
                        keywords,
                    },
            }) if self.as_special_export(func) == Some(SpecialExport::Super) => {
                self.ensure_expr(func);
                for kw in keywords {
                    self.ensure_expr(&mut kw.value);
                    unexpected_keyword(&|msg| self.error(*range, msg), "super", kw);
                }
                let nargs = posargs.len();
                let style = if nargs == 0 {
                    let mut self_type = None;
                    for scope in self.scopes.iter_rev() {
                        if let ScopeKind::ClassBody(class_body) = &scope.kind {
                            self_type = Some(class_body.as_self_type_key());
                            break;
                        }
                    }
                    match self_type {
                        Some(key) => SuperStyle::ImplicitArgs(self.table.types.0.insert(key)),
                        None => {
                            self.error(
                                *range,
                                "`super` call with no arguments is valid only inside a method"
                                    .to_owned(),
                            );
                            SuperStyle::Any
                        }
                    }
                } else if nargs == 2 {
                    let mut bind = |expr: &mut Expr| {
                        self.ensure_expr(expr);
                        self.table
                            .insert(Key::Anon(expr.range()), Binding::Expr(None, expr.clone()))
                    };
                    let cls_key = bind(&mut posargs[0]);
                    let obj_key = bind(&mut posargs[1]);
                    SuperStyle::ExplicitArgs(cls_key, obj_key)
                } else {
                    if nargs != 1 {
                        // Calling super() with one argument is technically legal: https://stackoverflow.com/a/30190341.
                        // This is a very niche use case, and we don't support it aside from not erroring.
                        self.error(
                            *range,
                            format!("`super` takes at most 2 arguments, got {}", nargs),
                        );
                    }
                    for arg in posargs {
                        self.ensure_expr(arg);
                    }
                    SuperStyle::Any
                };
                self.table.insert(
                    Key::SuperInstance(*range),
                    Binding::SuperInstance(style, *range),
                );
                return;
            }
            Expr::Name(x) => {
                let name = Ast::expr_name_identifier(x.clone());
                let binding = self.forward_lookup(&name);
                self.ensure_name(&name, binding);
                false
            }
            Expr::Named(x) => {
                self.scopes.current_mut().stat.expr_lvalue(&x.target);
                let make_binding = |k| Binding::Expr(k, (*x.value).clone());
                self.bind_target(&x.target, &make_binding, None);
                false
            }
            Expr::ListComp(x) => {
                self.bind_comprehensions(x.range, &mut x.generators);
                true
            }
            Expr::SetComp(x) => {
                self.bind_comprehensions(x.range, &mut x.generators);
                true
            }
            Expr::DictComp(x) => {
                self.bind_comprehensions(x.range, &mut x.generators);
                true
            }
            Expr::Generator(x) => {
                self.bind_comprehensions(x.range, &mut x.generators);
                true
            }
            Expr::Lambda(x) => {
                self.bind_lambda(x);
                true
            }
            Expr::Yield(x) => {
                self.functions
                    .last_mut()
                    .yields
                    .push(Either::Left(x.clone()));
                false
            }
            Expr::YieldFrom(x) => {
                self.functions
                    .last_mut()
                    .yields
                    .push(Either::Right(x.clone()));
                false
            }
            _ => false,
        };
        Visitors::visit_expr_mut(x, |x| self.ensure_expr(x));
        if new_scope {
            self.scopes.pop();
        }
    }

    /// Execute through the expr, ensuring every name has a binding.
    pub fn ensure_expr_opt(&mut self, x: Option<&mut Expr>) {
        if let Some(x) = x {
            self.ensure_expr(x);
        }
    }

    /// Execute through the expr, ensuring every name has a binding.
    pub fn ensure_type(&mut self, x: &mut Expr, tparams_builder: &mut Option<LegacyTParamBuilder>) {
        match x {
            Expr::Name(x) => {
                let name = Ast::expr_name_identifier(x.clone());
                let binding = match tparams_builder {
                    Some(legacy) => legacy.forward_lookup(self, &name),
                    None => self.forward_lookup(&name),
                };
                self.ensure_name(&name, binding);
            }
            Expr::Subscript(ExprSubscript { value, .. })
                if self.as_special_export(value) == Some(SpecialExport::Literal) =>
            {
                // Don't go inside a literal, since you might find strings which are really strings, not string-types
                self.ensure_expr(x);
            }
            Expr::Subscript(ExprSubscript {
                value,
                slice: box Expr::Tuple(tup),
                ..
            }) if self.as_special_export(value) == Some(SpecialExport::Annotated)
                && !tup.is_empty() =>
            {
                // Only go inside the first argument to Annotated, the rest are non-type metadata.
                self.ensure_type(&mut *value, tparams_builder);
                self.ensure_type(&mut tup.elts[0], tparams_builder);
                for e in tup.elts[1..].iter_mut() {
                    self.ensure_expr(e);
                }
            }
            Expr::StringLiteral(literal) => {
                match Ast::parse_type_literal(literal) {
                    Ok(expr) => {
                        *x = expr;
                        // You are not allowed to nest type strings in type strings,
                        self.ensure_expr(x);
                    }
                    Err(e) => {
                        self.error(
                            literal.range,
                            format!(
                                "Could not parse type string: {}, got {e}",
                                literal.value.to_str()
                            ),
                        );
                    }
                }
            }
            // Bind the lambda so we don't crash on undefined parameter names.
            Expr::Lambda(_) => self.ensure_expr(x),
            _ => Visitors::visit_expr_mut(x, |x| self.ensure_type(x, tparams_builder)),
        }
    }

    /// Execute through the expr, ensuring every name has a binding.
    pub fn ensure_type_opt(
        &mut self,
        x: Option<&mut Expr>,
        tparams_builder: &mut Option<LegacyTParamBuilder>,
    ) {
        if let Some(x) = x {
            self.ensure_type(x, tparams_builder);
        }
    }

    pub fn ensure_and_bind_decorators(&mut self, decorators: Vec<Decorator>) -> Vec<Idx<Key>> {
        let mut decorator_keys = Vec::with_capacity(decorators.len());
        for mut x in decorators {
            self.ensure_expr(&mut x.expression);
            let k = self
                .table
                .insert(Key::Anon(x.range), Binding::Decorator(x.expression));
            decorator_keys.push(k);
        }
        decorator_keys
    }
}
