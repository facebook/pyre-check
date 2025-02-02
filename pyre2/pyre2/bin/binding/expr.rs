/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;

use itertools::Either;
use ruff_python_ast::BoolOp;
use ruff_python_ast::Comprehension;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprBoolOp;
use ruff_python_ast::ExprLambda;
use ruff_python_ast::ExprSubscript;
use ruff_python_ast::Identifier;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::Key;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::bindings::LegacyTParamBuilder;
use crate::binding::narrow::NarrowOps;
use crate::binding::scope::Flow;
use crate::binding::scope::Scope;
use crate::dunder;
use crate::export::special::SpecialExport;
use crate::module::short_identifier::ShortIdentifier;
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

    fn bind_comprehensions(&mut self, comprehensions: &[Comprehension]) {
        self.scopes.push(Scope::comprehension());
        for comp in comprehensions.iter() {
            self.scopes.current_mut().stat.expr_lvalue(&comp.target);
            let make_binding = |k| Binding::IterableValue(k, comp.iter.clone());
            self.bind_target(&comp.target, &make_binding, None);
        }
    }

    fn bind_lambda(&mut self, lambda: &ExprLambda) {
        self.scopes.push(Scope::function());
        if let Some(parameters) = &lambda.parameters {
            for x in parameters.iter() {
                let name = x.name();
                let bind_key = self.table.insert(
                    Key::Definition(ShortIdentifier::new(name)),
                    Binding::AnyType(AnyStyle::Implicit),
                );
                self.scopes
                    .current_mut()
                    .stat
                    .add(name.id.clone(), name.range);
                self.bind_key(&name.id, bind_key, None);
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
        orelse: Option<&Expr>,
        range: TextRange,
    ) {
        let if_branch = mem::take(&mut self.scopes.current_mut().flow);
        self.scopes.current_mut().flow = base;
        self.bind_narrow_ops(&ops.negate(), range);
        self.ensure_expr_opt(orelse);
        let else_branch = mem::take(&mut self.scopes.current_mut().flow);
        self.scopes.current_mut().flow =
            self.merge_flow(vec![if_branch, else_branch], range, false);
    }

    /// Execute through the expr, ensuring every name has a binding.
    pub fn ensure_expr(&mut self, x: &Expr) {
        let new_scope = match x {
            Expr::If(x) => {
                // Ternary operation. We treat it like an if/else statement.
                let base = self.scopes.current().flow.clone();
                self.ensure_expr(&x.test);
                let narrow_ops = NarrowOps::from_expr(Some(&x.test));
                self.bind_narrow_ops(&narrow_ops, x.body.range());
                self.ensure_expr(&x.body);
                self.negate_and_merge_flow(base, &narrow_ops, Some(&x.orelse), x.range());
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
                self.bind_comprehensions(&x.generators);
                true
            }
            Expr::SetComp(x) => {
                self.bind_comprehensions(&x.generators);
                true
            }
            Expr::DictComp(x) => {
                self.bind_comprehensions(&x.generators);
                true
            }
            Expr::Generator(x) => {
                self.bind_comprehensions(&x.generators);
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
        Visitors::visit_expr(x, |x| self.ensure_expr(x));
        if new_scope {
            self.scopes.pop();
        }
    }

    /// Execute through the expr, ensuring every name has a binding.
    pub fn ensure_expr_opt(&mut self, x: Option<&Expr>) {
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
}
