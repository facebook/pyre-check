/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::slice;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use ruff_python_ast::DictItem;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprBooleanLiteral;
use ruff_python_ast::ExprName;
use ruff_python_ast::ExprNoneLiteral;
use ruff_python_ast::ExprStringLiteral;
use ruff_python_ast::Identifier;
use ruff_python_ast::ModModule;
use ruff_python_ast::Parameter;
use ruff_python_ast::ParameterWithDefault;
use ruff_python_ast::Parameters;
use ruff_python_ast::Pattern;
use ruff_python_ast::PatternMatchSingleton;
use ruff_python_ast::PySourceType;
use ruff_python_ast::Singleton;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtIf;
use ruff_python_ast::StringFlags;
use ruff_python_parser::parse_expression_range;
use ruff_python_parser::parse_unchecked_source;
use ruff_python_parser::ParseError;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;
use starlark_map::small_map::SmallMap;

use crate::module::module_name::ModuleName;
use crate::visitors::Visitors;

/// Just used for convenient namespacing - not a real type
pub struct Ast;

impl Ast {
    pub fn parse(contents: &str) -> (ModModule, Vec<ParseError>) {
        // PySourceType of Python vs Stub doesn't actually change the parsing
        let res = parse_unchecked_source(contents, PySourceType::Python);
        let errors = res.errors().to_owned();
        (res.into_syntax(), errors)
    }

    pub fn parse_expr(contents: &str, pos: TextSize) -> anyhow::Result<Expr> {
        // I really want to use Parser::new_starts_at, but it's private.
        // Discussion in https://github.com/astral-sh/ruff/pull/13542.
        // Until then, fake it with a lot of spaces.
        let s = format!("{}{contents}", " ".repeat(pos.to_usize()));
        let end = pos
            .checked_add(TextSize::new(contents.len() as u32))
            .unwrap();
        Ok(*parse_expression_range(&s, TextRange::new(pos, end))?
            .into_syntax()
            .body)
    }

    pub fn parse_type_literal(x: &ExprStringLiteral) -> anyhow::Result<Expr> {
        let mut s = x.value.to_str().to_owned();
        if x.value.iter().any(|x| x.flags.is_triple_quoted()) {
            // Implicitly bracketed, so add them explicitly
            s = format!("({s})");
        }
        // These positions are adequate (they are at least inside the literal) but might not be precise
        // given string gaps.
        Ast::parse_expr(&s, x.range.start())
    }

    pub fn unpack_slice(x: &Expr) -> &[Expr] {
        match x {
            Expr::Tuple(x) => &x.elts,
            _ => slice::from_ref(x),
        }
    }

    pub fn is_literal(x: &Expr) -> bool {
        matches!(
            x,
            Expr::BooleanLiteral(_)
                | Expr::NumberLiteral(_)
                | Expr::StringLiteral(_)
                | Expr::BytesLiteral(_)
                | Expr::NoneLiteral(_)
                | Expr::EllipsisLiteral(_)
        )
    }

    /// All the imports in this module, pointing to the first place that defines them.
    #[expect(dead_code)] // Useful primitive to have
    fn imports(
        module: &ModModule,
        module_name: ModuleName,
        is_init: bool,
    ) -> SmallMap<ModuleName, TextRange> {
        fn stmts(
            xs: &[Stmt],
            module_name: ModuleName,
            is_init: bool,
            imports: &mut SmallMap<ModuleName, TextRange>,
        ) {
            for x in xs {
                stmt(x, module_name, is_init, imports);
            }
        }
        fn stmt(
            x: &Stmt,
            module_name: ModuleName,
            is_init: bool,
            imports: &mut SmallMap<ModuleName, TextRange>,
        ) {
            match x {
                Stmt::Import(x) => {
                    for x in &x.names {
                        imports
                            .entry(ModuleName::from_name(&x.name.id))
                            .or_insert(x.name.range);
                    }
                }
                Stmt::ImportFrom(x) => {
                    if let Some(module_name) = module_name.new_maybe_relative(
                        is_init,
                        x.level,
                        x.module.as_ref().map(|x| &x.id),
                    ) {
                        imports.entry(module_name).or_insert(x.range);
                    }
                }
                _ => {}
            }
            Visitors::visit_stmt(x, |xs| stmts(xs, module_name, is_init, imports));
        }
        let mut imports = SmallMap::new();
        stmts(&module.body, module_name, is_init, &mut imports);
        imports
    }

    /// Iterates over the branches of an if statement, returning the test and body.
    /// A test on `None` is an `else` branch that is always taken.
    pub fn if_branches(x: &StmtIf) -> impl Iterator<Item = (Option<&Expr>, &[Stmt])> {
        let first = [(Some(&*x.test), x.body.as_slice())].into_iter();
        let elses = x
            .elif_else_clauses
            .iter()
            .map(|x| (x.test.as_ref(), x.body.as_slice()));
        first.chain(elses)
    }

    /// Like `if_branches`, but returns owned values.
    pub fn if_branches_owned(
        x: StmtIf,
    ) -> impl Iterator<Item = (TextRange, Option<Expr>, Vec<Stmt>)> {
        let first = std::iter::once((x.range, Some(*x.test), x.body));
        let elses = x
            .elif_else_clauses
            .into_iter()
            .map(|x| (x.range, x.test, x.body));
        first.chain(elses)
    }

    /// Iterates over parameters, returning the parameters and defaults
    pub fn parameters_iter_mut(
        x: &mut Parameters,
    ) -> impl Iterator<Item = (&mut Parameter, Option<&mut Option<Box<Expr>>>)> {
        fn param_default(
            x: &mut ParameterWithDefault,
        ) -> (&mut Parameter, Option<&mut Option<Box<Expr>>>) {
            (&mut x.parameter, Some(&mut x.default))
        }
        fn param(x: &mut Box<Parameter>) -> (&mut Parameter, Option<&mut Option<Box<Expr>>>) {
            (&mut *x, None)
        }

        x.posonlyargs
            .iter_mut()
            .map(param_default)
            .chain(x.args.iter_mut().map(param_default))
            .chain(x.vararg.iter_mut().map(param))
            .chain(x.kwonlyargs.iter_mut().map(param_default))
            .chain(x.kwarg.iter_mut().map(param))
    }

    /// We really want to avoid "making up" identifiers out of nowhere.
    /// But there, there isn't an identifier, but morally should be, so create the implicit one.
    pub fn expr_name_identifier(x: ExprName) -> Identifier {
        Identifier::new(x.id, x.range)
    }

    /// Calls a function on all of the names bound by this lvalue expression.
    pub fn expr_lvalue<'a>(x: &'a Expr, f: &mut impl FnMut(&'a ExprName)) {
        match x {
            Expr::Name(x) => {
                f(x);
            }
            Expr::Tuple(x) => {
                for x in &x.elts {
                    Ast::expr_lvalue(x, f);
                }
            }

            Expr::List(x) => {
                for x in &x.elts {
                    Ast::expr_lvalue(x, f);
                }
            }
            Expr::Starred(x) => {
                Ast::expr_lvalue(&x.value, f);
            }
            Expr::Subscript(_) => { /* no-op */ }
            Expr::Attribute(_) => { /* no-op */ }
            _ => {
                // Should not occur in well-formed Python code, doesn't introduce bindings.
                // Will raise an error later.
            }
        }
    }

    /// The [`Pattern`] type contains lvalues as identifiers. Although some patterns like
    /// MatchValue contain [`Expr`], those do not contain lvalues and thus are ignored.
    pub fn pattern_lvalue<'a>(x: &'a Pattern, f: &mut impl FnMut(&'a Identifier)) {
        match x {
            Pattern::MatchStar(x) => {
                if let Some(x) = &x.name {
                    f(x);
                }
            }
            Pattern::MatchAs(x) => {
                if let Some(x) = &x.name {
                    f(x);
                }
            }
            Pattern::MatchMapping(x) => {
                if let Some(x) = &x.rest {
                    f(x);
                }
            }
            _ => {}
        }
        Visitors::visit_pattern(x, |x| Ast::pattern_lvalue(x, f));
    }

    /// Pull all dictionary items up to the top level, so `{a: 1, **{b: 2}}`
    /// has the same items as `{a: 1, b: 2}`.
    pub fn flatten_dict_items<'b>(x: &'b [DictItem]) -> Vec<&'b DictItem> {
        fn f<'b>(xs: &'b [DictItem], res: &mut Vec<&'b DictItem>) {
            for x in xs {
                if x.key.is_none()
                    && let Expr::Dict(dict) = &x.value
                {
                    f(&dict.items, res);
                } else {
                    res.push(x);
                }
            }
        }
        let mut res = Vec::new();
        f(x, &mut res);
        res
    }

    pub fn pattern_match_singleton_to_expr(x: &PatternMatchSingleton) -> Expr {
        match x.value {
            Singleton::None => Expr::NoneLiteral(ExprNoneLiteral { range: x.range }),
            Singleton::True | Singleton::False => Expr::BooleanLiteral(ExprBooleanLiteral {
                range: x.range,
                value: x.value == Singleton::True,
            }),
        }
    }
}

pub struct AtomicTextRange(AtomicU64);

impl AtomicTextRange {
    pub fn new(range: TextRange) -> Self {
        Self(AtomicU64::new(Self::to_u64(range)))
    }

    pub fn get(&self) -> TextRange {
        Self::from_u64(self.0.load(Ordering::Relaxed))
    }

    pub fn set(&self, range: TextRange) {
        self.0.store(Self::to_u64(range), Ordering::Relaxed);
    }

    fn to_u64(range: TextRange) -> u64 {
        let low = range.start().to_u32();
        let high = range.end().to_u32();
        ((high as u64) << 32) | (low as u64)
    }

    fn from_u64(packed: u64) -> TextRange {
        let high = (packed >> 32) as u32;
        let low = packed as u32;
        TextRange::new(TextSize::new(low), TextSize::new(high))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_atomic_text_range() {
        let range1 = TextRange::new(TextSize::new(0), TextSize::new(1));
        let range2 = TextRange::new(TextSize::new(100), TextSize::new(800));
        let atomic = AtomicTextRange::new(range1);
        assert_eq!(range1, atomic.get());
        atomic.set(range2);
        assert_eq!(range2, atomic.get());
    }
}
