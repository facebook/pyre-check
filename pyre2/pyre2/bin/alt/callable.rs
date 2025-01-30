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
use crate::types::callable::ParamList;
use crate::types::callable::Params;
use crate::types::callable::Required;
use crate::types::types::Quantified;
use crate::types::types::Type;
use crate::util::display::count;
use crate::util::prelude::SliceExt;

#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
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
    fn is_param_spec_args(&self, x: &CallArg, q: Quantified) -> bool {
        match x {
            CallArg::Star(x, _) => self.expr_infer(x) == Type::Args(q),
            _ => false,
        }
    }

    fn is_param_spec_kwargs(&self, x: &Keyword, q: Quantified) -> bool {
        self.expr_infer(&x.value) == Type::Kwargs(q)
    }

    fn callable_infer_params(
        &self,
        params: &ParamList,
        self_arg: Option<CallArg>,
        args: &[CallArg],
        keywords: &[Keyword],
        range: TextRange,
    ) {
        let iargs = self_arg.iter().chain(args.iter());

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
                    Some((hint, vararg)) => arg_pre.post_check(self, hint, vararg, arg.range()),
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
                Param::Pos(name, _, required) | Param::KwOnly(name, _, required) => {
                    kwparams.insert(name.clone(), (p_idx, *required == Required::Required));
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
                    if let Type::TypedDict(typed_dict) = ty {
                        typed_dict.fields().iter().for_each(|(name, field)| {
                            let mut hint = kwargs;
                            splat_kwargs.push((field.ty.clone(), kw.range));
                            if let Some(&p_idx) = seen_names.get(name) {
                                self.error(
                                    kw.range,
                                    format!("Multiple values for argument '{}'", name),
                                );
                                params.items()[p_idx].visit(|ty| hint = Some(ty));
                            } else if let Some(&(p_idx, required)) = kwparams.get(name) {
                                if field.required {
                                    seen_names.insert(name.clone(), p_idx);
                                }
                                if field.required != required {
                                    self.error(
                                        kw.range,
                                        format!(
                                            "Expected key '{}' to be {}",
                                            name,
                                            if required { "required" } else { "optional" }
                                        ),
                                    );
                                }
                                params.items()[p_idx].visit(|ty| hint = Some(ty));
                            } else if kwargs.is_none() {
                                self.error(
                                    kw.range,
                                    format!("Unexpected keyword argument '{}'", name),
                                );
                            }
                            hint.iter().for_each(|want| {
                                self.check_type(want, &field.ty, kw.range);
                            });
                        })
                    } else {
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
                                    format!(
                                        "Expected argument after ** to have `str` keys, got: {}",
                                        key.deterministic_printing()
                                    ),
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
                }
                Some(id) => {
                    let mut hint = kwargs;
                    if let Some(&p_idx) = seen_names.get(&id.id) {
                        self.error(
                            kw.range,
                            format!("Multiple values for argument '{}'", id.id),
                        );
                        params.items()[p_idx].visit(|ty| hint = Some(ty));
                    } else if let Some(&(p_idx, _)) = kwparams.get(&id.id) {
                        seen_names.insert(id.id.clone(), p_idx);
                        params.items()[p_idx].visit(|ty| hint = Some(ty));
                    } else if kwargs.is_none() {
                        self.error(kw.range, format!("Unexpected keyword argument '{}'", id.id));
                    }
                    self.expr(&kw.value, hint);
                }
            }
        }
        for (name, &(p_idx, _)) in kwparams.iter() {
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
    }

    pub fn callable_infer(
        &self,
        callable: Callable,
        self_arg: Option<CallArg>,
        args: &[CallArg],
        keywords: &[Keyword],
        range: TextRange,
    ) -> Type {
        match callable.params {
            Params::List(params) => {
                self.callable_infer_params(&params, self_arg, args, keywords, range);
            }
            Params::Ellipsis => {
                // Deal with Callable[..., R]
                for arg in self_arg.iter().chain(args.iter()) {
                    arg.pre_eval(self).post_infer(self)
                }
            }
            Params::ParamSpec(concatenate, p) => {
                let mut p = self.solver().expand(p);
                if let Type::Var(v) = p {
                    // We are making a call, so if it hasn't been forced yet, do so now.
                    p = self.solver().force_var(v);
                }
                match p {
                    Type::ParamSpecValue(params) => self.callable_infer_params(
                        &params.prepend_types(&concatenate),
                        self_arg,
                        args,
                        keywords,
                        range,
                    ),
                    Type::Quantified(q) => {
                        if !args.last().is_some_and(|x| self.is_param_spec_args(x, q))
                            || !keywords
                                .last()
                                .is_some_and(|x| self.is_param_spec_kwargs(x, q))
                        {
                            self.error(
                                range,
                                "Expected a `*args` and `**kwargs` for `ParamSpec` (TODO: improve error message)".to_owned(),
                            );
                        } else {
                            self.callable_infer_params(
                                &ParamList::new_types(&concatenate),
                                self_arg,
                                &args[0..args.len() - 1],
                                &keywords[0..keywords.len() - 1],
                                range,
                            );
                        }
                    }
                    _ => {
                        // This could well be our error, but not really sure
                        self.error(range, "Unexpected ParamSpec type".to_owned());
                    }
                }
            }
        };
        self.solver().expand(callable.ret)
    }
}
