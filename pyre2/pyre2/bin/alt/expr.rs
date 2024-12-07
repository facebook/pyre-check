/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::slice;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::Arguments;
use ruff_python_ast::BoolOp;
use ruff_python_ast::Comprehension;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprBinOp;
use ruff_python_ast::ExprSlice;
use ruff_python_ast::Identifier;
use ruff_python_ast::Keyword;
use ruff_python_ast::Operator;
use ruff_python_ast::UnaryOp;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use super::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::binding::Key;
use crate::alt::unwrap::UnwrappedDict;
use crate::ast::Ast;
use crate::dunder;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::callable::Arg;
use crate::types::callable::Args;
use crate::types::callable::Callable;
use crate::types::callable::Required;
use crate::types::literal::Lit;
use crate::types::module::Module;
use crate::types::param_spec::ParamSpec;
use crate::types::simplify::as_attribute_base;
use crate::types::simplify::AttributeBase;
use crate::types::special_form::SpecialForm;
use crate::types::tuple::Tuple;
use crate::types::type_var::Restriction;
use crate::types::type_var::TypeVar;
use crate::types::type_var::TypeVarArgs;
use crate::types::type_var::Variance;
use crate::types::type_var_tuple::TypeVarTuple;
use crate::types::types::Type;
use crate::util::prelude::SliceExt;

enum CallStyle<'a> {
    Method(&'a Name),
    BinaryOp(Operator),
    FreeForm,
}

/// This struct bundles a `Type` with a `TextRange`, allowing us to typecheck function calls
/// when we only know the types of the arguments but not the original expressions.
pub struct TypeCallArg {
    ty: Type,
    range: TextRange,
}

impl Ranged for TypeCallArg {
    fn range(&self) -> TextRange {
        self.range
    }
}

impl TypeCallArg {
    pub fn new(ty: Type, range: TextRange) -> Self {
        Self { ty, range }
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    // Helper method for inferring the type of a boolean operation over a sequence of values.
    fn boolop(&self, values: &[Expr], op: BoolOp) -> Type {
        let target = match op {
            BoolOp::And => false,
            BoolOp::Or => true,
        };
        let should_shortcircuit = |t: &Type| t.as_bool() == Some(target);
        let should_discard = |t: &Type| t.as_bool() == Some(!target);

        let mut types = Vec::new();
        let last_index = values.len() - 1;
        for (i, value) in values.iter().enumerate() {
            let t = self.expr_infer(value);
            if should_shortcircuit(&t) {
                types.push(t);
                break;
            }
            // If we reach the last value, we should always keep it.
            if i != last_index && should_discard(&t) {
                continue;
            }
            match t {
                Type::Union(options) => {
                    for option in options {
                        if !should_discard(&option) {
                            types.push(option);
                        }
                    }
                }
                _ => types.push(t),
            }
        }
        self.unions(&types)
    }

    fn as_callable(&self, ty: Type) -> Option<Callable> {
        match ty {
            Type::Callable(c) => Some(*c),
            Type::ClassDef(cls) => {
                let constructor = self.get_constructor_for_class_object(&cls);
                self.as_callable(constructor)
            }
            Type::Type(box Type::ClassType(cls)) => {
                let constructor = self.get_constructor_for_class_type(&cls);
                self.as_callable(constructor)
            }
            Type::Forall(params, t) => {
                let t: Type = self.solver().fresh_quantified(
                    params.quantified().copied().collect::<Vec<_>>().as_slice(),
                    *t,
                    self.uniques,
                );
                self.as_callable(t)
            }
            Type::Var(v) if let Some(_guard) = self.recurser.recurse(v) => {
                self.as_callable(self.solver().force_var(v))
            }
            Type::Union(xs) => {
                let res = xs
                    .into_iter()
                    .map(|x| self.as_callable(x))
                    .collect::<Option<SmallSet<Callable>>>()?;
                if res.len() == 1 {
                    Some(res.into_iter().next().unwrap())
                } else {
                    None
                }
            }
            Type::Any(style) => Some(style.propagate_callable()),
            Type::TypeAlias(ta) => self.as_callable(ta.as_value(self.stdlib)),
            _ => None,
        }
    }

    fn as_callable_or_error(&self, ty: Type, call_style: CallStyle, range: TextRange) -> Callable {
        match self.as_callable(ty.clone()) {
            Some(callable) => callable,
            None => {
                let expect_message = match call_style {
                    CallStyle::Method(method) => {
                        format!("Expected `{}` to be a callable", method)
                    }
                    CallStyle::BinaryOp(op) => {
                        format!("Expected `{}` to be a callable", op.dunder())
                    }
                    CallStyle::FreeForm => "Expected a callable".to_owned(),
                };
                self.error_callable(
                    range,
                    format!("{}, got {}", expect_message, ty.deterministic_printing()),
                )
            }
        }
    }

    fn call_method_generic<T: Ranged>(
        &self,
        ty: &Type,
        method_name: &Name,
        range: TextRange,
        args: &[T],
        keywords: &[Keyword],
        // Function to check an argument against the type hint (if any) on the corresponding parameter
        check_arg: &dyn Fn(&T, Option<&Type>),
    ) -> Type {
        self.distribute_over_union(ty, |ty| {
            let callable = match as_attribute_base(ty.clone(), self.stdlib) {
                Some(AttributeBase::ClassInstance(class)) => {
                    let method_type =
                        self.get_instance_attribute_or_error(&class, method_name, range);
                    self.as_callable_or_error(method_type, CallStyle::Method(method_name), range)
                }
                Some(AttributeBase::ClassObject(class)) => {
                    let method_type = self.get_class_attribute_or_error(&class, method_name, range);
                    self.as_callable_or_error(method_type, CallStyle::Method(method_name), range)
                }
                Some(AttributeBase::Module(module)) => {
                    let method_type = self.lookup_module_attr(&module, method_name, range);
                    self.as_callable_or_error(method_type, CallStyle::Method(method_name), range)
                }
                Some(AttributeBase::Quantified(q)) => {
                    let method_type = q.as_value(self.stdlib).to_type();
                    self.as_callable_or_error(method_type, CallStyle::Method(method_name), range)
                }
                Some(AttributeBase::TypeAny(style)) => {
                    match self.get_instance_attribute(&self.stdlib.builtins_type(), method_name) {
                        None => style.propagate_callable(),
                        Some(method_type) => self.as_callable_or_error(
                            method_type,
                            CallStyle::Method(method_name),
                            range,
                        ),
                    }
                }
                Some(AttributeBase::Any(style)) => style.propagate_callable(),
                None => self.error_callable(
                    range,
                    format!(
                        "Expected class, got {}",
                        ty.clone().deterministic_printing()
                    ),
                ),
            };
            self.call_infer(callable, args, keywords, check_arg, range)
        })
    }

    pub fn call_method(
        &self,
        ty: &Type,
        method_name: &Name,
        range: TextRange,
        args: &[Expr],
        keywords: &[Keyword],
    ) -> Type {
        let check_arg = &|arg: &Expr, hint: Option<&Type>| {
            self.expr(arg, hint);
        };
        self.call_method_generic(ty, method_name, range, args, keywords, check_arg)
    }

    pub fn call_method_with_types(
        &self,
        ty: &Type,
        method_name: &Name,
        range: TextRange,
        args: &[TypeCallArg],
    ) -> Type {
        let check_arg = &|arg: &TypeCallArg, hint: Option<&Type>| {
            if let Some(hint) = hint {
                self.check_type(hint, &arg.ty, arg.range);
            }
        };
        self.call_method_generic(ty, method_name, range, args, &[], check_arg)
    }

    pub fn expr(&self, x: &Expr, check: Option<&Type>) -> Type {
        match check {
            Some(want) if !want.is_any() => {
                let got = self.expr_infer_with_hint(x, Some(want));
                self.check_type(want, &got, x.range())
            }
            _ => self.expr_infer(x),
        }
    }

    /// Infers types for `if` clauses in the given comprehensions.
    /// This is for error detection only; the types are not used.
    fn ifs_infer(&self, comps: &[Comprehension]) {
        for comp in comps.iter() {
            for if_clause in comp.ifs.iter() {
                self.expr_infer(if_clause);
            }
        }
    }

    fn call_infer<T: Ranged>(
        &self,
        callable: Callable,
        args: &[T],
        keywords: &[Keyword],
        check_arg: &dyn Fn(&T, Option<&Type>),
        range: TextRange,
    ) -> Type {
        match callable.args {
            Args::List(params) => {
                let mut iparams = params.iter().enumerate().peekable();
                let mut num_positional = 0;
                let mut seen_names: SmallMap<Name, usize> = SmallMap::new();
                for arg in args {
                    let mut hint = None;
                    if let Some((p_idx, p)) = iparams.peek() {
                        match p {
                            Arg::PosOnly(ty, _required) => {
                                num_positional += 1;
                                iparams.next();
                                hint = Some(ty)
                            }
                            Arg::Pos(name, ty, _required) => {
                                num_positional += 1;
                                seen_names.insert(name.clone(), *p_idx);
                                iparams.next();
                                hint = Some(ty)
                            }
                            Arg::VarArg(ty) => hint = Some(ty),
                            Arg::KwOnly(..) | Arg::Kwargs(..) => {
                                if num_positional >= 0 {
                                    self.error(
                                        arg.range(),
                                        format!(
                                            "Expected {} positional argument(s)",
                                            num_positional
                                        ),
                                    );
                                    num_positional = -1;
                                }
                            }
                        };
                    } else if num_positional >= 0 {
                        self.error(
                            arg.range(),
                            format!("Expected {} positional argument(s)", num_positional),
                        );
                        num_positional = -1;
                    }
                    check_arg(arg, hint);
                }
                let mut need_positional = 0;
                let mut kwparams = SmallMap::new();
                let mut kwargs = None;
                for (p_idx, p) in iparams {
                    match p {
                        Arg::PosOnly(_, required) => {
                            if *required == Required::Required {
                                need_positional += 1;
                            }
                        }
                        Arg::VarArg(..) => {}
                        Arg::Pos(name, _, _) | Arg::KwOnly(name, _, _) => {
                            kwparams.insert(name.clone(), p_idx);
                        }
                        Arg::Kwargs(ty) => {
                            kwargs = Some(ty);
                        }
                    }
                }
                for kw in keywords {
                    let mut hint = None;
                    if need_positional > 0 {
                        self.error(
                            kw.range,
                            format!("Expected {} more positional argument(s)", need_positional),
                        );
                        need_positional = 0;
                    } else {
                        match &kw.arg {
                            None => {
                                self.error_todo(
                                    "call_infer: unsupported **kwargs argument to call",
                                    kw.range,
                                );
                            }
                            Some(id) => {
                                hint = kwargs;
                                if let Some(&p_idx) = seen_names.get(&id.id) {
                                    self.error(
                                        kw.range,
                                        format!("Multiple values for argument '{}'", id.id),
                                    );
                                    params[p_idx].visit(|ty| hint = Some(ty));
                                } else if let Some(&p_idx) = kwparams.get(&id.id) {
                                    seen_names.insert(id.id.clone(), p_idx);
                                    params[p_idx].visit(|ty| hint = Some(ty));
                                } else if kwargs.is_none() {
                                    self.error(
                                        kw.range,
                                        format!("Unexpected keyword argument '{}'", id.id),
                                    );
                                }
                            }
                        }
                    }
                    self.expr(&kw.value, hint);
                }
                if need_positional > 0 {
                    self.error(
                        range,
                        format!("Expected {} more positional argument(s)", need_positional),
                    );
                } else if num_positional >= 0 {
                    for (name, &p_idx) in kwparams.iter() {
                        let required = params[p_idx].is_required();
                        if required && !seen_names.contains_key(name) {
                            self.error(range, format!("Missing argument '{}'", name));
                        }
                    }
                }
                callable.ret
            }
            Args::Ellipsis => {
                // Deal with Callable[..., R]
                for t in args {
                    check_arg(t, None);
                }
                callable.ret
            }
            _ => self.error(
                range,
                "Answers::expr_infer wrong number of arguments to call".to_owned(),
            ),
        }
    }

    fn lookup_module_attr(&self, module: &Module, attr_name: &Name, range: TextRange) -> Type {
        match module.as_single_module() {
            Some(module_name) => self.get_import(attr_name, module_name, range),
            None => module.push_path(attr_name.clone()).to_type(),
        }
    }

    pub fn attr_infer(&self, obj: &Type, attr_name: &Name, range: TextRange) -> Type {
        self.distribute_over_union(obj, |obj| {
            match as_attribute_base(obj.clone(), self.stdlib) {
                Some(AttributeBase::ClassInstance(class)) => {
                    self.get_instance_attribute_or_error(&class, attr_name, range)
                }
                Some(AttributeBase::ClassObject(class)) => {
                    self.get_class_attribute_or_error(&class, attr_name, range)
                }
                Some(AttributeBase::Module(module)) => {
                    self.lookup_module_attr(&module, attr_name, range)
                }
                Some(AttributeBase::Quantified(q)) => {
                    if q.is_param_spec() && attr_name == "args" {
                        Type::type_form(Type::Args(q.id()))
                    } else if q.is_param_spec() && attr_name == "kwargs" {
                        Type::type_form(Type::Kwargs(q.id()))
                    } else {
                        self.get_instance_attribute_or_error(
                            &q.as_value(self.stdlib),
                            attr_name,
                            range,
                        )
                    }
                }
                Some(AttributeBase::TypeAny(style)) => self
                    .get_instance_attribute(&self.stdlib.builtins_type(), attr_name)
                    .unwrap_or_else(|| style.propagate()),
                Some(AttributeBase::Any(style)) => style.propagate(),
                None => self.error(
                    range,
                    format!(
                        "TODO: Answers::expr_infer attribute: `{}`.{}",
                        obj.clone().deterministic_printing(),
                        attr_name,
                    ),
                ),
            }
        })
    }

    fn binop_infer(&self, x: &ExprBinOp) -> Type {
        let binop_call = |op: Operator, lhs: &Type, rhs: Type, range: TextRange| -> Type {
            // TODO(yangdanny): handle reflected dunder methods
            let method_type = self.attr_infer(lhs, &Name::new(op.dunder()), range);
            let callable = self.as_callable_or_error(method_type, CallStyle::BinaryOp(op), range);
            self.call_infer(
                callable,
                &[TypeCallArg::new(rhs, range)],
                &[],
                &|arg, hint| {
                    if let Some(hint) = hint {
                        self.check_type(hint, &arg.ty, arg.range);
                    }
                },
                range,
            )
        };
        let lhs = self.expr_infer(&x.left);
        let rhs = self.expr_infer(&x.right);
        if let Type::Any(style) = &lhs {
            return style.propagate();
        } else if x.op == Operator::BitOr
            && let Some(l) = self.untype_opt(lhs.clone(), x.left.range())
            && let Some(r) = self.untype_opt(rhs.clone(), x.right.range())
        {
            return Type::type_form(self.union(&l, &r));
        }
        self.distribute_over_union(&lhs, |lhs| binop_call(x.op, lhs, rhs.clone(), x.range))
    }

    /// When interpreted as static types (as opposed to when accounting for runtime
    /// behavior when used as values), `Type::ClassDef(cls)` is equivalent to
    /// `Type::Type(box Type::ClassType(cls, default_targs(cls)))` where `default_targs(cls)`
    /// is the result of looking up the class `tparams` and synthesizing default `targs` that
    /// are gradual if needed (e.g. `list` is treated as `list[Any]` when used as an annotation).
    ///
    /// This function canonicalizes to `Type::ClassType`.
    pub fn canonicalize_all_class_types(&self, ty: Type, range: TextRange) -> Type {
        ty.transform(|ty| match ty {
            Type::ClassDef(cls) => {
                *ty = Type::type_form(Type::ClassType(self.promote_to_class_type(cls, range)));
            }
            _ => {}
        })
    }

    fn literal_bool_infer(&self, x: &Expr) -> bool {
        let ty = self.expr_infer(x);
        match ty {
            Type::Literal(Lit::Bool(b)) => b,
            _ => {
                self.error(
                    x.range(),
                    format!("Expected literal True or False, got {ty}"),
                );
                false
            }
        }
    }

    fn tyvar_from_arguments(&self, arguments: &Arguments) -> TypeVar {
        let args = TypeVarArgs::from_arguments(arguments);

        let name = match args.name {
            Some(Expr::StringLiteral(x)) => Identifier::new(Name::new(x.value.to_str()), x.range),
            _ => {
                let msg = if args.name.is_none() {
                    "Missing `name` argument to TypeVar"
                } else {
                    "Expected first argument of TypeVar to be a string literal"
                };
                self.error(arguments.range, msg.to_owned());
                // FIXME: This isn't ideal - we are creating a fake Identifier, which is not good.
                Identifier::new(Name::new("unknown"), arguments.range)
            }
        };
        let constraints = args.constraints.map(|x| self.expr_untype(x));
        let bound = args.bound.map(|x| self.expr_untype(x));
        let default = args.default.map(|x| self.expr_untype(x));
        let covariant = args.covariant.map_or(false, |x| self.literal_bool_infer(x));
        let contravariant = args
            .contravariant
            .map_or(false, |x| self.literal_bool_infer(x));
        let infer_variance = args
            .infer_variance
            .map_or(false, |x| self.literal_bool_infer(x));

        for kw in args.unknown {
            self.error(
                kw.range,
                match &kw.arg {
                    Some(id) => format!("Unexpected keyword argument `{}` to TypeVar", id.id),
                    None => "Unexpected anonymous keyword to TypeVar".to_owned(),
                },
            );
        }
        let restriction = if let Some(bound) = bound {
            if !constraints.is_empty() {
                self.error(
                    arguments.range,
                    "TypeVar cannot have both constraints and bound".to_owned(),
                );
            }
            Restriction::Bound(bound)
        } else if !constraints.is_empty() {
            Restriction::Constraints(constraints)
        } else {
            Restriction::Unrestricted
        };
        if [covariant, contravariant, infer_variance]
            .iter()
            .filter(|x| **x)
            .count()
            > 1
        {
            self.error(
                arguments.range,
                "Contradictory variance specifications".to_owned(),
            );
        }
        let variance = if covariant {
            Some(Variance::Covariant)
        } else if contravariant {
            Some(Variance::Contravariant)
        } else if infer_variance {
            None
        } else {
            Some(Variance::Invariant)
        };
        TypeVar::new(
            name,
            self.module_info().dupe(),
            restriction,
            default,
            variance,
        )
    }

    fn expr_infer(&self, x: &Expr) -> Type {
        self.expr_infer_with_hint(x, None)
    }

    fn expr_infer_with_hint(&self, x: &Expr, hint: Option<&Type>) -> Type {
        match x {
            Expr::BoolOp(x) => self.boolop(&x.values, x.op),
            Expr::Named(_) => self.error_todo("Answers::expr_infer", x),
            Expr::BinOp(x) => self.binop_infer(x),
            Expr::UnaryOp(x) => {
                let t = self.expr_infer(&x.operand);
                match x.op {
                    UnaryOp::USub => match t {
                        Type::Literal(lit) => {
                            Type::Literal(lit.negate(self.module_info(), x.range, self.errors()))
                        }
                        _ => self.error_todo(&format!("Answers::expr_infer on {}", x.op), x),
                    },
                    UnaryOp::Not => match t.as_bool() {
                        None => self.stdlib.bool().to_type(),
                        Some(b) => Type::Literal(Lit::Bool(!b)),
                    },
                    _ => self.error_todo(&format!("Answers::expr_infer on {}", x.op), x),
                }
            }
            Expr::Lambda(_) => self.error_todo("Answers::expr_infer", x),
            Expr::If(x) => {
                let condition_type = self.expr_infer(&x.test);
                // TODO: Support type refinement
                let body_type = self.expr_infer(&x.body);
                let orelse_type = self.expr_infer(&x.orelse);
                match condition_type.as_bool() {
                    Some(true) => body_type,
                    Some(false) => orelse_type,
                    None => self.union(&body_type, &orelse_type),
                }
            }
            Expr::Dict(x) => {
                let hint = hint.and_then(|ty| self.unwrap_dict(ty));
                if x.is_empty() {
                    match hint {
                        Some(x) => x.to_type(self.stdlib),
                        None => self
                            .stdlib
                            .dict(
                                self.solver().fresh_contained(self.uniques).to_type(),
                                self.solver().fresh_contained(self.uniques).to_type(),
                            )
                            .to_type(),
                    }
                } else {
                    let (key_hint, value_hint) = match hint {
                        Some(UnwrappedDict { key, value }) => (Some(key), Some(value)),
                        None => (None, None),
                    };
                    let key_tys: Vec<Type> = x
                        .items
                        .iter()
                        .filter_map(|x| match &x.key {
                            Some(key) => {
                                let key_t = self.expr(key, key_hint.as_ref());
                                Some(self.promote(key_t, key_hint.as_ref()))
                            }
                            _ => {
                                self.error_todo("Answers::expr_infer expansion in dict literal", x);
                                None
                            }
                        })
                        .collect();
                    let value_tys: Vec<Type> = x
                        .items
                        .iter()
                        .filter_map(|x| match x.key {
                            Some(_) => {
                                let value_t = self.expr(&x.value, value_hint.as_ref());
                                Some(self.promote(value_t, value_hint.as_ref()))
                            }
                            _ => {
                                self.error_todo("Answers::expr_infer expansion in dict literal", x);
                                None
                            }
                        })
                        .collect();
                    self.stdlib
                        .dict(self.unions(&key_tys), self.unions(&value_tys))
                        .to_type()
                }
            }
            Expr::Set(x) => {
                let hint = hint.and_then(|ty| self.unwrap_set(ty));
                if x.is_empty() {
                    let elem_ty = match hint {
                        None => self.solver().fresh_contained(self.uniques).to_type(),
                        Some(elem_ty) => elem_ty,
                    };
                    self.stdlib.set(elem_ty).to_type()
                } else {
                    let tys = x.elts.map(|x| {
                        let t = self.expr(x, hint.as_ref());
                        self.promote(t, hint.as_ref())
                    });
                    self.stdlib.set(self.unions(&tys)).to_type()
                }
            }
            Expr::ListComp(x) => {
                self.ifs_infer(&x.generators);
                let elem_ty = self.expr_infer(&x.elt);
                self.stdlib.list(elem_ty).to_type()
            }
            Expr::SetComp(x) => {
                self.ifs_infer(&x.generators);
                let elem_ty = self.expr_infer(&x.elt);
                self.stdlib.set(elem_ty).to_type()
            }
            Expr::DictComp(x) => {
                self.ifs_infer(&x.generators);
                let k_ty = self.expr_infer(&x.key);
                let v_ty = self.expr_infer(&x.value);
                self.stdlib.dict(k_ty, v_ty).to_type()
            }
            Expr::Generator(x) => {
                self.ifs_infer(&x.generators);
                let yield_ty = self.expr_infer(&x.elt);
                self.stdlib
                    .generator(yield_ty, Type::None, Type::None)
                    .to_type()
            }
            Expr::Await(x) => {
                let awaiting_ty = self.expr_infer(&x.value);
                match self.unwrap_awaitable(&awaiting_ty) {
                    Some(ty) => ty,
                    None => self.error(x.range, "Expression is not awaitable".to_owned()),
                }
            }
            Expr::Yield(x) => self.error_todo("Answers::expr_infer", x),
            Expr::YieldFrom(_) => self.error_todo("Answers::expr_infer", x),
            Expr::Compare(x) => {
                let _ty = self.expr_infer(&x.left);
                let _tys = x.comparators.map(|x| self.expr_infer(x));
                // We don't actually check that comparing these types is sensible, which matches Pyright
                self.stdlib.bool().to_type()
            }
            Expr::Call(x) if is_special_name(&x.func, "assert_type") => {
                if x.arguments.args.len() == 2 {
                    let expr_a = &x.arguments.args[0];
                    let expr_b = &x.arguments.args[1];
                    let a = self.expr_infer(expr_a);
                    let b = self.expr_untype(expr_b);
                    let a = self.solver().deep_force(a).explicit_any();
                    let b = self.canonicalize_all_class_types(
                        self.solver().deep_force(b).explicit_any(),
                        expr_b.range(),
                    );
                    if a != b {
                        self.error(
                            x.range,
                            format!(
                                "assert_type({}, {}) failed",
                                a.deterministic_printing(),
                                b.deterministic_printing()
                            ),
                        );
                    }
                } else {
                    self.error(
                        x.range,
                        format!(
                            "assert_type needs 2 arguments, got {:#?}",
                            x.arguments.args.len()
                        ),
                    );
                }
                Type::None
            }
            Expr::Call(x) if is_special_name(&x.func, "reveal_type") => {
                if x.arguments.args.len() == 1 {
                    let t = self.expr_infer(&x.arguments.args[0]);
                    self.error(
                        x.range,
                        format!("revealed type: {}", t.deterministic_printing()),
                    );
                } else {
                    self.error(
                        x.range,
                        format!(
                            "reveal_type needs 1 argument, got {}",
                            x.arguments.args.len()
                        ),
                    );
                }
                Type::None
            }
            Expr::Call(x) => {
                let ty_fun = self.expr_infer(&x.func);
                if TypeVar::is_ctor(&ty_fun) {
                    Type::type_form(self.tyvar_from_arguments(&x.arguments).to_type())
                } else if TypeVarTuple::is_ctor(&ty_fun)
                    && let Some(name) = arguments_one_string(&x.arguments)
                {
                    Type::type_form(TypeVarTuple::new(name, self.module_info().dupe()).to_type())
                } else if ParamSpec::is_ctor(&ty_fun)
                    && let Some(name) = arguments_one_string(&x.arguments)
                {
                    Type::type_form(ParamSpec::new(name, self.module_info().dupe()).to_type())
                } else {
                    let func_range = x.func.range();
                    self.distribute_over_union(&ty_fun, |ty| {
                        let callable =
                            self.as_callable_or_error(ty.clone(), CallStyle::FreeForm, func_range);
                        self.call_infer(
                            callable,
                            &x.arguments.args,
                            &x.arguments.keywords,
                            &|arg, hint| {
                                self.expr(arg, hint);
                            },
                            func_range,
                        )
                    })
                }
            }
            Expr::FString(x) => {
                if let Some(lit) = Lit::from_fstring(x) {
                    lit.to_type()
                } else {
                    self.stdlib.str().to_type()
                }
            }
            Expr::StringLiteral(x) => Lit::from_string_literal(x).to_type(),
            Expr::BytesLiteral(x) => Lit::from_bytes_literal(x).to_type(),
            Expr::NumberLiteral(x) => {
                Lit::from_number_literal(x, self.module_info(), self.errors()).to_type()
            }
            Expr::BooleanLiteral(x) => Lit::from_boolean_literal(x).to_type(),
            Expr::NoneLiteral(_) => Type::None,
            Expr::EllipsisLiteral(_) => Type::Ellipsis,
            Expr::Attribute(x) => {
                let obj = self.expr_infer(&x.value);
                self.attr_infer(&obj, &x.attr.id, x.range)
            }
            Expr::Subscript(x) => {
                let xs = Ast::unpack_slice(&x.slice);
                // FIXME: We don't deal properly with hint here, we should.
                let mut fun = self.expr_infer(&x.value);
                if let Type::Var(v) = fun {
                    fun = self.solver().force_var(v);
                }
                if matches!(&fun, Type::ClassDef(t) if t.name() == "tuple") {
                    fun = Type::type_form(Type::SpecialForm(SpecialForm::Tuple));
                }
                match fun {
                    Type::Forall(params, ty) => {
                        let param_map = params
                            .quantified()
                            .copied()
                            .zip(xs.map(|x| self.expr_untype(x)))
                            .collect::<SmallMap<_, _>>();
                        ty.subst(&param_map)
                    }
                    Type::ClassDef(cls)
                        if cls.qname().module.name().as_str() == "builtins"
                            && cls.qname().name.id == "type" =>
                    {
                        let targ = match xs.len() {
                            // This causes us to treat `type[list]` as equivalent to `type[list[Any]]`,
                            // which may or may not be what we want.
                            1 => self.expr_untype(&xs[0]),
                            _ => {
                                self.error(
                                    x.range,
                                    format!(
                                        "Expected 1 type argument for class `type`, got {}",
                                        xs.len()
                                    ),
                                );
                                Type::any_error()
                            }
                        };
                        // TODO: Validate that `targ` refers to a "valid in-scope class or TypeVar"
                        // (https://typing.readthedocs.io/en/latest/spec/annotations.html#type-and-annotation-expressions)
                        Type::type_form(Type::type_form(targ))
                    }
                    Type::ClassDef(cls) => {
                        Type::type_form(Type::ClassType(self.specialize_as_class_type(
                            &cls,
                            xs.map(|x| self.expr_untype(x)),
                            x.range,
                        )))
                    }
                    Type::Type(box Type::SpecialForm(special)) => {
                        self.apply_special_form(special, xs, x.range)
                    }
                    Type::Tuple(Tuple::Concrete(elts)) if xs.len() == 1 => match &xs[0] {
                        Expr::Slice(ExprSlice {
                            lower: lower_expr,
                            upper: upper_expr,
                            step: None,
                            ..
                        }) => {
                            let lower_literal = match lower_expr {
                                Some(box expr) => {
                                    let lower_type = self.expr_infer(expr);
                                    match &lower_type {
                                        Type::Literal(Lit::Int(idx)) => Some(*idx),
                                        _ => None,
                                    }
                                }
                                None => Some(0),
                            };
                            let upper_literal = match upper_expr {
                                Some(box expr) => {
                                    let upper_type = self.expr_infer(expr);
                                    match &upper_type {
                                        Type::Literal(Lit::Int(idx)) => Some(*idx),
                                        _ => None,
                                    }
                                }
                                None => Some(elts.len() as i64),
                            };
                            match (lower_literal, upper_literal) {
                                (Some(lower), Some(upper))
                                    if lower <= upper
                                        && lower >= 0
                                        && upper >= 0
                                        && upper <= elts.len() as i64 =>
                                {
                                    Type::Tuple(Tuple::concrete(
                                        elts[lower as usize..upper as usize].to_vec(),
                                    ))
                                }
                                _ => self.error_todo("tuple slice", x),
                            }
                        }
                        _ => {
                            let idx_type = self.expr_infer(&xs[0]);
                            match &idx_type {
                                Type::Literal(Lit::Int(idx)) => {
                                    let elt_idx = if *idx >= 0 {
                                        *idx
                                    } else {
                                        elts.len() as i64 + *idx
                                    } as usize;
                                    if let Some(elt) = elts.get(elt_idx) {
                                        elt.clone()
                                    } else {
                                        self.error(
                                        x.range,
                                        format!(
                                            "Index {idx} out of range for tuple with {} elements",
                                            elts.len()
                                        ),
                                    )
                                    }
                                }
                                _ => self.call_method(
                                    &Type::Tuple(Tuple::Concrete(elts)),
                                    &dunder::GETITEM,
                                    x.range,
                                    slice::from_ref(&x.slice),
                                    &[],
                                ),
                            }
                        }
                    },
                    Type::Tuple(Tuple::Unbounded(elt)) if xs.len() == 1 => self.call_method(
                        &Type::Tuple(Tuple::Unbounded(elt)),
                        &dunder::GETITEM,
                        x.range,
                        slice::from_ref(&x.slice),
                        &[],
                    ),
                    Type::Any(style) => style.propagate(),
                    Type::ClassType(_) => self.call_method(
                        &fun,
                        &dunder::GETITEM,
                        x.range,
                        slice::from_ref(&x.slice),
                        &[],
                    ),
                    t => self.error(
                        x.range,
                        format!(
                            "Can't apply arguments to non-class, got {}",
                            t.deterministic_printing()
                        ),
                    ),
                }
            }
            Expr::Starred(_) => self.error_todo("Answers::expr_infer", x),
            Expr::Name(x) => match x.id.as_str() {
                "Any" => Type::type_form(Type::any_explicit()),
                _ => self
                    .get(&Key::Usage(ShortIdentifier::expr_name(x)))
                    .arc_clone(),
            },
            Expr::List(x) => {
                let hint = hint.and_then(|ty| self.unwrap_list(ty));
                if x.is_empty() {
                    let elem_ty = match hint {
                        None => self.solver().fresh_contained(self.uniques).to_type(),
                        Some(elem_ty) => elem_ty,
                    };
                    self.stdlib.list(elem_ty).to_type()
                } else {
                    let tys = x.elts.map(|x| {
                        let t = self.expr(x, hint.as_ref());
                        self.promote(t, hint.as_ref())
                    });
                    self.stdlib.list(self.unions(&tys)).to_type()
                }
            }
            Expr::Tuple(x) => {
                let ts = match hint {
                    Some(Type::Tuple(Tuple::Concrete(elts))) if elts.len() == x.elts.len() => elts,
                    Some(ty) => match self.unwrap_tuple(ty) {
                        Some(elem_ty) => &vec![elem_ty; x.elts.len()],
                        None => &Vec::new(),
                    },
                    None => &Vec::new(),
                };
                Type::tuple(
                    x.elts
                        .iter()
                        .enumerate()
                        .map(|(i, x)| self.expr(x, ts.get(i)))
                        .collect(),
                )
            }
            Expr::Slice(_) => {
                // TODO(stroxler, yangdanny): slices are generic, we should not hard code to int.
                let int = self.stdlib.int().to_type();
                self.stdlib.slice(int.clone(), int.clone(), int).to_type()
            }
            Expr::IpyEscapeCommand(_) => self.error_todo("Answers::expr_infer", x),
        }
    }
}

fn is_special_name(x: &Expr, name: &str) -> bool {
    match x {
        Expr::Name(x) => x.id.as_str() == name,
        _ => false,
    }
}

fn arguments_one_string(x: &Arguments) -> Option<Identifier> {
    if x.args.len() == 1 && x.keywords.is_empty() {
        match &x.args[0] {
            Expr::StringLiteral(x) => Some(Identifier {
                id: Name::new(x.value.to_str()),
                range: x.range,
            }),
            _ => None,
        }
    } else {
        None
    }
}
