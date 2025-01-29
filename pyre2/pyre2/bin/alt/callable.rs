/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::Keyword;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::Iterable;
use crate::alt::answers::LookupAnswer;
use crate::alt::unwrap::UnwrappedDict;
use crate::types::callable::Callable;
use crate::types::callable::Param;
use crate::types::callable::Params;
use crate::types::callable::Required;
use crate::types::types::Type;
use crate::util::display::count;
use crate::util::prelude::SliceExt;

#[derive(Clone)]
pub enum CallArg<'a> {
    /// Bundles a `Type` with a `TextRange`, allowing us to typecheck function calls
    /// when we only know the types of the arguments but not the original expressions.
    Type(&'a Type, TextRange),
    Expr(&'a Expr),
    Star(&'a Expr, TextRange),
}

impl Ranged for CallArg<'_> {
    fn range(&self) -> TextRange {
        match self {
            Self::Type(_, r) => *r,
            Self::Expr(e) => e.range(),
            Self::Star(_, r) => *r,
        }
    }
}

impl CallArg<'_> {
    // Splat arguments might be fixed-length tuples, which are handled precisely, or have unknown
    // length. This function evaluates splat args to determine how many params should be consumed,
    // but does not evaulate other expressions, which might be contextually typed.
    fn pre_eval<Ans: LookupAnswer>(&self, solver: &AnswersSolver<Ans>) -> CallArgPreEval {
        match self {
            Self::Type(ty, _) => CallArgPreEval::Type(ty, false),
            Self::Expr(e) => CallArgPreEval::Expr(e, false),
            Self::Star(e, range) => {
                let ty = solver.expr_infer(e);
                let iterables = solver.iterate(&ty, *range);
                // If we have a union of iterables, use a fixed length only if every iterable is
                // fixed and has the same length. Otherwise, use star.
                let mut fixed_lens = Vec::new();
                for x in iterables.iter() {
                    match x {
                        Iterable::FixedLen(xs) => fixed_lens.push(xs.len()),
                        Iterable::OfType(_) => {}
                    }
                }
                if !fixed_lens.is_empty()
                    && fixed_lens.len() == iterables.len()
                    && fixed_lens.iter().all(|len| *len == fixed_lens[0])
                {
                    let mut fixed_tys = vec![Vec::new(); fixed_lens[0]];
                    for x in iterables {
                        if let Iterable::FixedLen(xs) = x {
                            for (i, ty) in xs.into_iter().enumerate() {
                                fixed_tys[i].push(ty);
                            }
                        }
                    }
                    let tys = fixed_tys.map(|tys| solver.unions(tys));
                    CallArgPreEval::Fixed(tys, 0)
                } else {
                    let mut star_tys = Vec::new();
                    for x in iterables {
                        match x {
                            Iterable::OfType(ty) => star_tys.push(ty.clone()),
                            Iterable::FixedLen(tys) => star_tys.extend(tys),
                        }
                    }
                    let ty = solver.unions(&star_tys);
                    CallArgPreEval::Star(ty, false)
                }
            }
        }
    }
}

// Pre-evaluated args are iterable. Type/Expr/Star variants iterate once (tracked via bool field),
// Fixed variant iterates over the the vec (tracked via usize field).
enum CallArgPreEval<'a> {
    Type(&'a Type, bool),
    Expr(&'a Expr, bool),
    Star(Type, bool),
    Fixed(Vec<Type>, usize),
}

impl CallArgPreEval<'_> {
    fn step(&self) -> bool {
        match self {
            Self::Type(_, done) | Self::Expr(_, done) | Self::Star(_, done) => !*done,
            Self::Fixed(tys, i) => *i < tys.len(),
        }
    }

    fn is_star(&self) -> bool {
        matches!(self, Self::Star(..))
    }

    fn post_check<Ans: LookupAnswer>(
        &mut self,
        solver: &AnswersSolver<Ans>,
        hint: &Type,
        vararg: bool,
        range: TextRange,
    ) {
        match self {
            Self::Type(ty, done) => {
                *done = true;
                solver.check_type(hint, ty, range);
            }
            Self::Expr(x, done) => {
                *done = true;
                solver.expr(x, Some(hint));
            }
            Self::Star(ty, done) => {
                *done = vararg;
                solver.check_type(hint, ty, range);
            }
            Self::Fixed(tys, i) => {
                solver.check_type(hint, &tys[*i], range);
                *i += 1;
            }
        }
    }

    fn post_infer<Ans: LookupAnswer>(&mut self, solver: &AnswersSolver<Ans>) {
        match self {
            Self::Expr(x, _) => {
                solver.expr_infer(x);
            }
            _ => {}
        }
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn callable_infer(
        &self,
        callable: Callable,
        self_arg: Option<CallArg>,
        args: &[CallArg],
        keywords: &[Keyword],
        range: TextRange,
    ) -> Type {
        let iargs = self_arg.iter().chain(args.iter());
        let ret = match callable.params {
            Params::List(params) => {
                let mut iparams = params.items().iter().enumerate().peekable();
                let mut num_positional_params = 0;
                let mut num_positional_args = 0;
                let mut seen_names: SmallMap<Name, usize> = SmallMap::new();
                let mut extra_arg_pos = None;
                for arg in iargs {
                    let mut arg_pre = arg.pre_eval(self);
                    while arg_pre.step() {
                        num_positional_args += 1;
                        let param = if let Some((p_idx, p)) = iparams.peek() {
                            match p {
                                Param::PosOnly(ty, _required) => {
                                    num_positional_params += 1;
                                    iparams.next();
                                    Some((ty, false))
                                }
                                Param::Pos(name, ty, _required) => {
                                    num_positional_params += 1;
                                    seen_names.insert(name.clone(), *p_idx);
                                    iparams.next();
                                    Some((ty, false))
                                }
                                Param::VarArg(ty) => Some((ty, true)),
                                Param::KwOnly(..) | Param::Kwargs(..) => None,
                            }
                        } else {
                            None
                        };
                        match param {
                            Some((hint, vararg)) => {
                                arg_pre.post_check(self, hint, vararg, arg.range())
                            }
                            None => {
                                arg_pre.post_infer(self);
                                if arg_pre.is_star() {
                                    num_positional_args -= 1;
                                }
                                if extra_arg_pos.is_none() && !arg_pre.is_star() {
                                    extra_arg_pos = Some(arg.range());
                                }
                                break;
                            }
                        }
                    }
                }
                if let Some(arg_range) = extra_arg_pos {
                    let (expected, actual) = if self_arg.is_none() {
                        (
                            count(num_positional_params as usize, "positional argument"),
                            num_positional_args.to_string(),
                        )
                    } else if num_positional_params < 1 {
                        (
                            "0 positional arguments".to_owned(),
                            format!("{} (including implicit `self`)", num_positional_args),
                        )
                    } else {
                        (
                            count(num_positional_params as usize - 1, "positional argument"),
                            (num_positional_args - 1).to_string(),
                        )
                    };
                    self.error(arg_range, format!("Expected {expected}, got {actual}"));
                }
                let mut need_positional = 0;
                let mut kwparams = SmallMap::new();
                let mut kwargs = None;
                for (p_idx, p) in iparams {
                    match p {
                        Param::PosOnly(_, required) => {
                            if *required == Required::Required {
                                need_positional += 1;
                            }
                        }
                        Param::VarArg(..) => {}
                        Param::Pos(name, _, _) | Param::KwOnly(name, _, _) => {
                            kwparams.insert(name.clone(), p_idx);
                        }
                        Param::Kwargs(ty) => {
                            kwargs = Some(ty);
                        }
                    }
                }
                if need_positional > 0 {
                    let range = keywords.first().map_or(range, |kw| kw.range);
                    self.error(
                        range,
                        format!(
                            "Expected {}",
                            count(need_positional, "more positional argument")
                        ),
                    );
                }
                let mut splat_kwargs = Vec::new();
                for kw in keywords {
                    match &kw.arg {
                        None => {
                            let ty = self.expr_infer(&kw.value);
                            // TODO: TypedDict, ParamSpec kwargs
                            match self.decompose_dict(&ty) {
                                Some(UnwrappedDict { key, value }) => {
                                    if self.solver().is_subset_eq(
                                        &key,
                                        &self.stdlib.str().to_type(),
                                        self.type_order(),
                                    ) {
                                        kwargs.iter().for_each(|want| {
                                            self.check_type(want, &value, kw.range);
                                        });
                                        splat_kwargs.push((value, kw.range));
                                    } else {
                                        self.error(
                                            kw.value.range(),
                                            format!("Expected argument after ** to have `str` keys, got: {}", key.deterministic_printing())
                                        );
                                    }
                                }
                                None => {
                                    self.error(
                                        kw.value.range(),
                                        format!(
                                            "Expected argument after ** to be a mapping, got: {}",
                                            ty.deterministic_printing()
                                        ),
                                    );
                                }
                            }
                        }
                        Some(id) => {
                            let mut hint = kwargs;
                            if let Some(&p_idx) = seen_names.get(&id.id) {
                                self.error(
                                    kw.range,
                                    format!("Multiple values for argument '{}'", id.id),
                                );
                                params.items()[p_idx].visit(|ty| hint = Some(ty));
                            } else if let Some(&p_idx) = kwparams.get(&id.id) {
                                seen_names.insert(id.id.clone(), p_idx);
                                params.items()[p_idx].visit(|ty| hint = Some(ty));
                            } else if kwargs.is_none() {
                                self.error(
                                    kw.range,
                                    format!("Unexpected keyword argument '{}'", id.id),
                                );
                            }
                            self.expr(&kw.value, hint);
                        }
                    }
                }
                for (name, &p_idx) in kwparams.iter() {
                    if !seen_names.contains_key(name) {
                        let param = &params.items()[p_idx];
                        if splat_kwargs.is_empty() && param.is_required() {
                            self.error(range, format!("Missing argument '{}'", name));
                        }
                        for (ty, range) in &splat_kwargs {
                            param.visit(|want| {
                                self.check_type(want, ty, *range);
                            });
                        }
                    }
                }
                callable.ret
            }
            Params::Ellipsis => {
                // Deal with Callable[..., R]
                for arg in iargs {
                    arg.pre_eval(self).post_infer(self)
                }
                callable.ret
            }
            _ => self.error(
                range,
                "Answers::expr_infer wrong number of arguments to call".to_owned(),
            ),
        };
        self.solver().expand(ret)
    }
}
