/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::mem;

use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::Pattern;
use ruff_python_ast::PatternKeyword;
use ruff_python_ast::StmtMatch;
use ruff_text_size::Ranged;
use starlark_map::smallmap;

use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingExpect;
use crate::binding::binding::Key;
use crate::binding::binding::KeyExpect;
use crate::binding::binding::SizeExpectation;
use crate::binding::binding::UnpackedPosition;
use crate::binding::bindings::BindingsBuilder;
use crate::binding::narrow::NarrowOp;
use crate::binding::narrow::NarrowOps;
use crate::binding::narrow::NarrowVal;
use crate::graph::index::Idx;

impl<'a> BindingsBuilder<'a> {
    // Traverse a pattern and bind all the names; key is the reference for the value that's being matched on
    fn bind_pattern(
        &mut self,
        subject_name: Option<&Name>,
        pattern: Pattern,
        key: Idx<Key>,
    ) -> NarrowOps {
        match pattern {
            Pattern::MatchValue(p) => {
                self.ensure_expr(&p.value);
                if let Some(subject_name) = subject_name {
                    NarrowOps(
                        smallmap! { subject_name.clone() => (NarrowOp::Eq(NarrowVal::Expr(p.value.clone())), p.range()) },
                    )
                } else {
                    NarrowOps::new()
                }
            }
            Pattern::MatchSingleton(p) => {
                let value = Ast::pattern_match_singleton_to_expr(&p);
                if let Some(subject_name) = subject_name {
                    NarrowOps(
                        smallmap! { subject_name.clone() => (NarrowOp::Is(NarrowVal::Expr(Box::new(value))), p.range()) },
                    )
                } else {
                    NarrowOps::new()
                }
            }
            Pattern::MatchAs(p) => {
                // If there's no name for this pattern, refine the variable being matched
                // If there is a new name, refine that instead
                let new_subject_name = if let Some(name) = &p.name {
                    self.bind_definition(name, Binding::Forward(key), None);
                    Some(&name.id)
                } else {
                    subject_name
                };
                if let Some(box pattern) = p.pattern {
                    self.bind_pattern(new_subject_name, pattern, key)
                } else {
                    NarrowOps::new()
                }
            }
            Pattern::MatchSequence(x) => {
                let mut narrow_ops = NarrowOps::new();
                let num_patterns = x.patterns.len();
                let mut unbounded = false;
                for (idx, x) in x.patterns.into_iter().enumerate() {
                    match x {
                        Pattern::MatchStar(p) => {
                            if let Some(name) = &p.name {
                                let position = UnpackedPosition::Slice(idx, num_patterns - idx - 1);
                                self.bind_definition(
                                    name,
                                    Binding::UnpackedValue(
                                        Box::new(Binding::Forward(key)),
                                        p.range,
                                        position,
                                    ),
                                    None,
                                );
                            }
                            unbounded = true;
                        }
                        _ => {
                            let position = if unbounded {
                                UnpackedPosition::ReverseIndex(num_patterns - idx)
                            } else {
                                UnpackedPosition::Index(idx)
                            };
                            let key = self.table.insert(
                                Key::Anon(x.range()),
                                Binding::UnpackedValue(
                                    Box::new(Binding::Forward(key)),
                                    x.range(),
                                    position,
                                ),
                            );
                            narrow_ops.and_all(self.bind_pattern(None, x, key));
                        }
                    }
                }
                let expect = if unbounded {
                    SizeExpectation::Ge(num_patterns - 1)
                } else {
                    SizeExpectation::Eq(num_patterns)
                };
                self.table.insert(
                    KeyExpect(x.range),
                    BindingExpect::UnpackedLength(Box::new(Binding::Forward(key)), x.range, expect),
                );
                narrow_ops
            }
            Pattern::MatchMapping(x) => {
                let mut narrow_ops = NarrowOps::new();
                x.keys
                    .into_iter()
                    .zip(x.patterns)
                    .for_each(|(key_expr, pattern)| {
                        let mapping_key = self.table.insert(
                            Key::Anon(key_expr.range()),
                            Binding::PatternMatchMapping(key_expr, key),
                        );
                        narrow_ops.and_all(self.bind_pattern(None, pattern, mapping_key))
                    });
                if let Some(rest) = x.rest {
                    self.bind_definition(&rest, Binding::Forward(key), None);
                }
                narrow_ops
            }
            Pattern::MatchClass(x) => {
                self.ensure_expr(&x.cls);
                let mut narrow_ops = if let Some(subject_name) = subject_name {
                    NarrowOps(
                        smallmap! { subject_name.clone() => (NarrowOp::IsInstance(NarrowVal::Expr(x.cls.clone())), x.cls.range()) },
                    )
                } else {
                    NarrowOps::new()
                };
                x.arguments
                    .patterns
                    .into_iter()
                    .enumerate()
                    .for_each(|(idx, pattern)| {
                        let attr_key = self.table.insert(
                            Key::Anon(pattern.range()),
                            Binding::PatternMatchClassPositional(
                                x.cls.clone(),
                                idx,
                                key,
                                pattern.range(),
                            ),
                        );
                        narrow_ops.and_all(self.bind_pattern(None, pattern.clone(), attr_key))
                    });
                x.arguments.keywords.into_iter().for_each(
                    |PatternKeyword {
                         range: _,
                         attr,
                         pattern,
                     }| {
                        let attr_key = self.table.insert(
                            Key::Anon(attr.range()),
                            Binding::PatternMatchClassKeyword(x.cls.clone(), attr, key),
                        );
                        narrow_ops.and_all(self.bind_pattern(None, pattern, attr_key))
                    },
                );
                narrow_ops
            }
            Pattern::MatchOr(x) => {
                let mut narrow_ops: Option<NarrowOps> = None;
                let range = x.range;
                let mut branches = Vec::new();
                let n_subpatterns = x.patterns.len();
                for (idx, pattern) in x.patterns.into_iter().enumerate() {
                    if pattern.is_irrefutable() && idx != n_subpatterns - 1 {
                        self.error(
                            pattern.range(),
                            "Only the last subpattern in MatchOr may be irrefutable".to_owned(),
                        )
                    }
                    let mut base = self.scopes.current().flow.clone();
                    let new_narrow_ops = self.bind_pattern(subject_name, pattern, key);
                    if let Some(ref mut ops) = narrow_ops {
                        ops.or_all(new_narrow_ops)
                    } else {
                        narrow_ops = Some(new_narrow_ops);
                    }
                    mem::swap(&mut self.scopes.current_mut().flow, &mut base);
                    branches.push(base);
                }
                self.scopes.current_mut().flow = self.merge_flow(branches, range, false);
                narrow_ops.unwrap_or_default()
            }
            Pattern::MatchStar(_) => NarrowOps::new(),
        }
    }

    pub fn stmt_match(&mut self, x: StmtMatch) {
        self.ensure_expr(&x.subject);
        let subject_name = if let Expr::Name(name) = &*x.subject {
            Some(name.id.clone())
        } else {
            None
        };
        let key = self.table.insert(
            Key::Anon(x.subject.range()),
            Binding::Expr(None, *x.subject),
        );
        let mut exhaustive = false;
        let range = x.range;
        let mut branches = Vec::new();
        // Type narrowing operations that are carried over from one case to the next. For example, in:
        //   match x:
        //     case None:
        //       pass
        //     case _:
        //       pass
        // x is bound to Narrow(x, Eq(None)) in the first case, and the negation, Narrow(x, NotEq(None)),
        // is carried over to the fallback case.
        let mut negated_prev_ops = NarrowOps::new();
        for case in x.cases {
            let mut base = self.scopes.current().flow.clone();
            if case.pattern.is_wildcard() || case.pattern.is_irrefutable() {
                exhaustive = true;
            }
            let new_narrow_ops = self.bind_pattern(subject_name.as_ref(), case.pattern, key);
            self.bind_narrow_ops(&negated_prev_ops, case.range);
            self.bind_narrow_ops(&new_narrow_ops, case.range);
            negated_prev_ops.and_all(new_narrow_ops.negate());
            if let Some(guard) = case.guard {
                self.ensure_expr(&guard);
                self.table
                    .insert(Key::Anon(guard.range()), Binding::Expr(None, *guard));
            }
            self.stmts(case.body);
            mem::swap(&mut self.scopes.current_mut().flow, &mut base);
            branches.push(base);
            if exhaustive {
                break;
            }
        }
        if !exhaustive {
            branches.push(mem::take(&mut self.scopes.current_mut().flow));
        }
        self.scopes.current_mut().flow = self.merge_flow(branches, range, false);
    }
}
