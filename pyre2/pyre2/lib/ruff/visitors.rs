/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::visitor::source_order::walk_stmt;
use ruff_python_ast::visitor::source_order::SourceOrderVisitor;
use ruff_python_ast::ExceptHandler;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprFString;
use ruff_python_ast::FStringElement;
use ruff_python_ast::FStringPart;
use ruff_python_ast::ModModule;
use ruff_python_ast::Pattern;
use ruff_python_ast::Stmt;

use crate::util::visit::Visit;
use crate::util::visit::VisitMut;

impl Visit<Expr> for ModModule {
    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a Expr)) {
        for x in &self.body {
            x.recurse(f);
        }
    }
}

impl VisitMut for Expr {
    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut Self)) {
        match self {
            Expr::BoolOp(x) => x.values.recurse_mut(f),
            Expr::Named(x) => {
                f(&mut x.target);
                f(&mut x.value);
            }
            Expr::BinOp(x) => {
                f(&mut x.left);
                f(&mut x.right);
            }
            Expr::UnaryOp(x) => f(&mut x.operand),
            Expr::Lambda(x) => f(&mut x.body),
            Expr::If(x) => {
                f(&mut x.test);
                f(&mut x.body);
                f(&mut x.orelse);
            }
            Expr::Dict(x) => {
                x.items.iter_mut().for_each(|x| {
                    x.key.recurse_mut(f);
                    f(&mut x.value);
                });
            }
            Expr::Set(x) => x.elts.recurse_mut(f),
            Expr::ListComp(x) => {
                f(&mut x.elt);
                for x in &mut x.generators {
                    f(&mut x.iter);
                    x.ifs.recurse_mut(f);
                }
            }
            Expr::SetComp(x) => {
                f(&mut x.elt);
                for x in &mut x.generators {
                    f(&mut x.iter);
                    x.ifs.recurse_mut(f);
                }
            }
            Expr::DictComp(x) => {
                f(&mut x.key);
                f(&mut x.value);
                for x in &mut x.generators {
                    f(&mut x.iter);
                    x.ifs.recurse_mut(f);
                }
            }
            Expr::Generator(x) => {
                f(&mut x.elt);
                for x in &mut x.generators {
                    f(&mut x.iter);
                    x.ifs.recurse_mut(f);
                }
            }
            Expr::Await(x) => f(&mut x.value),
            Expr::Yield(x) => x.value.recurse_mut(f),
            Expr::YieldFrom(x) => f(&mut x.value),
            Expr::Compare(x) => {
                f(&mut x.left);
                x.comparators.recurse_mut(f);
            }
            Expr::Call(x) => {
                f(&mut x.func);
                x.arguments.args.recurse_mut(f);
                x.arguments
                    .keywords
                    .iter_mut()
                    .for_each(|x| f(&mut x.value));
            }
            Expr::FString(x) => {
                for x in x.value.iter_mut() {
                    match x {
                        FStringPart::Literal(_) => {}
                        FStringPart::FString(x) => {
                            for x in x.elements.iter_mut() {
                                match x {
                                    FStringElement::Literal(_) => {}
                                    FStringElement::Expression(x) => f(&mut x.expression),
                                }
                            }
                        }
                    }
                }
            }
            Expr::StringLiteral(_)
            | Expr::BytesLiteral(_)
            | Expr::NumberLiteral(_)
            | Expr::BooleanLiteral(_)
            | Expr::NoneLiteral(_)
            | Expr::EllipsisLiteral(_) => {}
            Expr::Attribute(x) => f(&mut x.value),
            Expr::Subscript(x) => {
                f(&mut x.value);
                f(&mut x.slice);
            }
            Expr::Starred(x) => f(&mut x.value),
            Expr::Name(_) => {}
            Expr::List(x) => x.elts.recurse_mut(f),
            Expr::Tuple(x) => x.elts.recurse_mut(f),
            Expr::Slice(x) => {
                x.lower.recurse_mut(f);
                x.upper.recurse_mut(f);
                x.step.recurse_mut(f);
            }
            Expr::IpyEscapeCommand(_) => {}
        }
    }
}

impl Visit for Stmt {
    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a Self)) {
        match self {
            Stmt::FunctionDef(x) => x.body.recurse(f),
            Stmt::ClassDef(x) => x.body.recurse(f),
            Stmt::For(x) => {
                x.body.recurse(f);
                x.orelse.recurse(f);
            }
            Stmt::While(x) => {
                x.body.recurse(f);
                x.orelse.recurse(f);
            }
            Stmt::If(x) => {
                x.body.recurse(f);
                for x in x.elif_else_clauses.iter() {
                    x.body.recurse(f);
                }
            }
            Stmt::With(x) => x.body.recurse(f),
            Stmt::Match(x) => {
                for x in x.cases.iter() {
                    x.body.recurse(f);
                }
            }
            Stmt::Try(x) => {
                x.body.recurse(f);
                x.handlers.iter().for_each(|x| match x {
                    ExceptHandler::ExceptHandler(x) => x.body.recurse(f),
                });
                x.orelse.recurse(f);
                x.finalbody.recurse(f);
            }
            _ => {}
        }
    }
}

impl Visit<Expr> for Stmt {
    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a Expr)) {
        struct X<T>(T);
        impl<'a, T: FnMut(&'a Expr)> SourceOrderVisitor<'a> for X<T> {
            fn visit_expr(&mut self, x: &'a Expr) {
                self.0(x);
            }
        }
        walk_stmt(&mut X(f), self);
    }
}

impl Visit<Expr> for ExprFString {
    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a Expr)) {
        self.value.iter().for_each(|x| match x {
            FStringPart::FString(x) => x.elements.iter().for_each(|x| match x {
                FStringElement::Literal(_) => {}
                FStringElement::Expression(x) => f(&x.expression),
            }),
            _ => {}
        });
    }
}

impl Visit for Expr {
    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a Self)) {
        match self {
            Expr::BoolOp(x) => x.values.recurse(f),
            Expr::Named(x) => {
                f(&x.target);
                f(&x.value);
            }
            Expr::BinOp(x) => {
                f(&x.left);
                f(&x.right);
            }
            Expr::UnaryOp(x) => f(&x.operand),
            Expr::Lambda(x) => f(&x.body),
            Expr::If(x) => {
                f(&x.test);
                f(&x.body);
                f(&x.orelse);
            }
            Expr::Dict(x) => {
                x.items.iter().for_each(|x| {
                    x.key.recurse(f);
                    f(&x.value);
                });
            }
            Expr::Set(x) => x.elts.recurse(f),
            Expr::ListComp(x) => {
                f(&x.elt);
                for x in &x.generators {
                    f(&x.iter);
                    x.ifs.recurse(f);
                }
            }
            Expr::SetComp(x) => {
                f(&x.elt);
                for x in &x.generators {
                    f(&x.iter);
                    x.ifs.recurse(f);
                }
            }
            Expr::DictComp(x) => {
                f(&x.key);
                f(&x.value);
                for x in &x.generators {
                    f(&x.iter);
                    x.ifs.recurse(f);
                }
            }
            Expr::Generator(x) => {
                f(&x.elt);
                for x in &x.generators {
                    f(&x.iter);
                    x.ifs.recurse(f);
                }
            }
            Expr::Await(x) => f(&x.value),
            Expr::Yield(x) => x.value.recurse(f),
            Expr::YieldFrom(x) => f(&x.value),
            Expr::Compare(x) => {
                f(&x.left);
                x.comparators.recurse(f);
            }
            Expr::Call(x) => {
                f(&x.func);
                x.arguments.args.recurse(f);
                x.arguments.keywords.iter().for_each(|x| f(&x.value));
            }
            Expr::FString(x) => {
                x.recurse(f);
            }
            Expr::StringLiteral(_)
            | Expr::BytesLiteral(_)
            | Expr::NumberLiteral(_)
            | Expr::BooleanLiteral(_)
            | Expr::NoneLiteral(_)
            | Expr::EllipsisLiteral(_) => {}
            Expr::Attribute(x) => f(&x.value),
            Expr::Subscript(x) => {
                f(&x.value);
                f(&x.slice);
            }
            Expr::Starred(x) => f(&x.value),
            Expr::Name(_) => {}
            Expr::List(x) => x.elts.recurse(f),
            Expr::Tuple(x) => x.elts.recurse(f),
            Expr::Slice(x) => {
                x.lower.recurse(f);
                x.upper.recurse(f);
                x.step.recurse(f);
            }
            Expr::IpyEscapeCommand(_) => {}
        }
    }
}

impl Visit for Pattern {
    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a Self)) {
        match self {
            Pattern::MatchValue(_) => {}
            Pattern::MatchSingleton(_) => {}
            Pattern::MatchSequence(x) => x.patterns.iter().for_each(f),
            Pattern::MatchMapping(x) => x.patterns.iter().for_each(f),
            Pattern::MatchClass(x) => x
                .arguments
                .patterns
                .iter()
                .chain(x.arguments.keywords.iter().map(|x| &x.pattern))
                .for_each(f),
            Pattern::MatchStar(_) => {}
            Pattern::MatchAs(x) => {
                if let Some(x) = &x.pattern {
                    f(x);
                }
            }
            Pattern::MatchOr(x) => {
                x.patterns.iter().for_each(f);
            }
        }
    }
}
