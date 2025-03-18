/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use itertools::Either;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_python_ast::StmtFunctionDef;
use ruff_text_size::TextRange;
use vec1::Vec1;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::types::decorated_function::DecoratedFunction;
use crate::binding::binding::Binding;
use crate::binding::binding::FunctionSource;
use crate::binding::binding::Key;
use crate::binding::binding::KeyClass;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyFunction;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::dunder;
use crate::error::collector::ErrorCollector;
use crate::error::context::TypeCheckContext;
use crate::error::context::TypeCheckKind;
use crate::error::kind::ErrorKind;
use crate::graph::index::Idx;
use crate::module::module_path::ModuleStyle;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::annotation::Qualifier;
use crate::types::callable::Callable;
use crate::types::callable::FuncFlags;
use crate::types::callable::FuncMetadata;
use crate::types::callable::Function;
use crate::types::callable::FunctionKind;
use crate::types::callable::Param;
use crate::types::callable::ParamList;
use crate::types::callable::Required;
use crate::types::class::ClassKind;
use crate::types::types::CalleeKind;
use crate::types::types::Forall;
use crate::types::types::Forallable;
use crate::types::types::Overload;
use crate::types::types::OverloadType;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn solve_function_binding(
        &self,
        idx: Idx<KeyFunction>,
        predecessor: &mut Option<Idx<Key>>,
        class_metadata: Option<&Idx<KeyClassMetadata>>,
        errors: &ErrorCollector,
    ) -> Type {
        // Overloads in .pyi should not have an implementation.
        let skip_implementation = self.module_info().path().style() == ModuleStyle::Interface
            || class_metadata.is_some_and(|idx| self.get_idx(*idx).is_protocol());
        let def = self.get_idx(idx);
        if def.metadata.flags.is_overload {
            // This function is decorated with @overload. We should warn if this function is actually called anywhere.
            let successor = self.bindings().get(idx).successor;
            let ty = def.ty.clone();
            if successor.is_none() {
                // This is the last definition in the chain. We should produce an overload type.
                let mut acc = Vec1::new((def.id_range, ty));
                let mut first = def;
                while let Some(def) = self.step_overload_pred(predecessor) {
                    acc.push((def.id_range, def.ty.clone()));
                    first = def;
                }
                if !skip_implementation {
                    self.error(
                        errors,
                        first.id_range,
                        ErrorKind::InvalidOverload,
                        None,
                        "Overloaded function must have an implementation".to_owned(),
                    );
                }
                if acc.len() == 1 {
                    self.error(
                        errors,
                        first.id_range,
                        ErrorKind::InvalidOverload,
                        None,
                        "Overloaded function needs at least two signatures".to_owned(),
                    );
                    acc.split_off_first().0.1
                } else {
                    acc.reverse();
                    Type::Overload(Overload {
                        signatures: self.extract_signatures(
                            first.metadata.kind.as_func_id().func,
                            acc,
                            errors,
                        ),
                        metadata: Box::new(first.metadata.clone()),
                    })
                }
            } else {
                ty
            }
        } else {
            let mut acc = Vec::new();
            let mut first = def;
            while let Some(def) = self.step_overload_pred(predecessor) {
                acc.push((def.id_range, def.ty.clone()));
                first = def;
            }
            acc.reverse();
            if let Ok(defs) = Vec1::try_from_vec(acc) {
                if defs.len() == 1 {
                    self.error(
                        errors,
                        first.id_range,
                        ErrorKind::InvalidOverload,
                        None,
                        "Overloaded function needs at least two signatures".to_owned(),
                    );
                    defs.split_off_first().0.1
                } else {
                    Type::Overload(Overload {
                        signatures: self.extract_signatures(
                            first.metadata.kind.as_func_id().func,
                            defs,
                            errors,
                        ),
                        metadata: Box::new(first.metadata.clone()),
                    })
                }
            } else {
                first.ty.clone()
            }
        }
    }

    pub fn function_definition(
        &self,
        def: &StmtFunctionDef,
        source: FunctionSource,
        self_type: Option<&Idx<KeyClass>>,
        decorators: &[Idx<Key>],
        legacy_tparams: &[Idx<KeyLegacyTypeParam>],
        errors: &ErrorCollector,
    ) -> Arc<DecoratedFunction> {
        let check_default = |name: &Identifier, default: &Option<Box<Expr>>, ty: &Type| {
            let mut required = Required::Required;
            if let Some(default) = default {
                required = Required::Optional;
                if source != FunctionSource::Stub
                    || !matches!(default.as_ref(), Expr::EllipsisLiteral(_))
                {
                    self.expr(
                        default,
                        Some((ty, &|| {
                            TypeCheckContext::of_kind(TypeCheckKind::FunctionParameterDefault(
                                name.id.clone(),
                            ))
                        })),
                        errors,
                    );
                }
            }
            required
        };

        let defining_cls = self_type.and_then(|k| self.get_idx(*k).0.dupe());
        let mut self_type = if def.name.id == dunder::NEW {
            // __new__ is a staticmethod, and does not take a self parameter.
            None
        } else {
            defining_cls.as_ref().map(|cls| cls.self_type())
        };

        let mut is_overload = false;
        let mut is_staticmethod = false;
        let mut is_classmethod = false;
        let mut is_property_getter = false;
        let mut is_property_setter_with_getter = None;
        let mut has_enum_member_decoration = false;
        let mut is_override = false;
        let mut has_final_decoration = false;
        let decorators = decorators
            .iter()
            .filter(|k| {
                let decorator_ty = self.get_idx(**k);
                match decorator_ty.callee_kind() {
                    Some(CalleeKind::Function(FunctionKind::Overload)) => {
                        is_overload = true;
                        false
                    }
                    Some(CalleeKind::Class(ClassKind::StaticMethod)) => {
                        is_staticmethod = true;
                        false
                    }
                    Some(CalleeKind::Class(ClassKind::ClassMethod)) => {
                        is_classmethod = true;
                        false
                    }
                    Some(CalleeKind::Class(ClassKind::Property)) => {
                        is_property_getter = true;
                        false
                    }
                    Some(CalleeKind::Function(FunctionKind::PropertySetter(_))) => {
                        // When the `setter` attribute is accessed on a property, we return the
                        // getter with its kind set to FunctionKind::PropertySetter. See
                        // AnswersSolver::lookup_attr_from_attribute_base for details.
                        is_property_setter_with_getter = Some(decorator_ty.arc_clone());
                        false
                    }
                    Some(CalleeKind::Class(ClassKind::EnumMember)) => {
                        has_enum_member_decoration = true;
                        false
                    }
                    Some(CalleeKind::Function(FunctionKind::Override)) => {
                        is_override = true;
                        false
                    }
                    Some(CalleeKind::Function(FunctionKind::Final)) => {
                        has_final_decoration = true;
                        false
                    }
                    _ => true,
                }
            })
            .collect::<Vec<_>>();

        // Look for a @classmethod or @staticmethod decorator and change the "self" type
        // accordingly. This is not totally correct, since it doesn't account for chaining
        // decorators, or weird cases like both decorators existing at the same time.
        if is_staticmethod {
            self_type = None;
        } else if is_classmethod {
            self_type = self_type.map(Type::type_form);
        }

        let mut get_param_ty = |name: &Identifier| {
            let ty = match self.bindings().get_function_param(name) {
                Either::Left(idx) => self.get_idx(idx).annotation.get_type().clone(),
                Either::Right(var) => {
                    // If this is the first parameter and there is a self type, solve to `Self`.
                    // We only try to solve the first param for now. Other unannotated params
                    // are also Var, but will always be forced to Any. In the future, we might
                    // consider contextual information to infer parameter types, like decorator
                    // applications.
                    if let Some(ty) = &self_type {
                        self.solver()
                            .is_subset_eq(&var.to_type(), ty, self.type_order());
                    }
                    self.solver().force_var(var)
                }
            };
            self_type = None; // Stop using `self` type solve Var params after the first param.
            ty
        };
        let mut paramspec_args = None;
        let mut paramspec_kwargs = None;
        let mut params = Vec::with_capacity(def.parameters.len());
        params.extend(def.parameters.posonlyargs.iter().map(|x| {
            let ty = get_param_ty(&x.parameter.name);
            let required = check_default(&x.parameter.name, &x.default, &ty);
            Param::PosOnly(ty, required)
        }));
        params.extend(def.parameters.args.iter().map(|x| {
            let ty = get_param_ty(&x.parameter.name);
            let required = check_default(&x.parameter.name, &x.default, &ty);
            Param::Pos(x.parameter.name.id.clone(), ty, required)
        }));
        params.extend(def.parameters.vararg.iter().map(|x| {
            let ty = get_param_ty(&x.name);
            if let Type::Args(q) = ty {
                paramspec_args = Some(q);
            }
            Param::VarArg(ty)
        }));
        if paramspec_args.is_some()
            && let Some(param) = def.parameters.kwonlyargs.first()
        {
            self.error(
                errors,
                param.range,
                ErrorKind::BadFunctionDefinition,
                None,
                format!(
                    "Keyword-only parameter `{}` may not appear after ParamSpec args parameter",
                    param.parameter.name
                ),
            );
        }
        params.extend(def.parameters.kwonlyargs.iter().map(|x| {
            let ty = get_param_ty(&x.parameter.name);
            let required = check_default(&x.parameter.name, &x.default, &ty);
            Param::KwOnly(x.parameter.name.id.clone(), ty, required)
        }));
        params.extend(def.parameters.kwarg.iter().map(|x| {
            let ty = match self.bindings().get_function_param(&x.name) {
                Either::Left(idx) => {
                    let annot = self.get_idx(idx);
                    let ty = annot.annotation.get_type().clone();
                    if annot.annotation.qualifiers.contains(&Qualifier::Unpack) {
                        Type::Unpack(Box::new(ty))
                    } else {
                        ty
                    }
                }
                Either::Right(var) => self.solver().force_var(var),
            };
            if let Type::Kwargs(q) = ty {
                paramspec_kwargs = Some(q);
            }
            Param::Kwargs(ty)
        }));
        let ret = self
            .get(&Key::ReturnType(ShortIdentifier::new(&def.name)))
            .arc_clone();

        let ret = if def.is_async && !self.is_async_generator(&ret) {
            self.stdlib
                .coroutine(Type::any_implicit(), Type::any_implicit(), ret)
                .to_type()
        } else {
            ret
        };
        let mut tparams = self.scoped_type_params(def.type_params.as_deref(), errors);
        let legacy_tparams = legacy_tparams
            .iter()
            .filter_map(|key| self.get_idx(*key).deref().parameter().cloned());
        tparams.extend(legacy_tparams);
        if paramspec_args != paramspec_kwargs {
            if paramspec_args.is_some() != paramspec_kwargs.is_some() {
                self.error(
                    errors,
                    def.range,
                    ErrorKind::InvalidParamSpec,
                    None,
                    "ParamSpec *args and **kwargs must be used together".to_owned(),
                );
            } else {
                self.error(
                    errors,
                    def.range,
                    ErrorKind::InvalidParamSpec,
                    None,
                    "*args and **kwargs must come from the same ParamSpec".to_owned(),
                );
            }
            // If ParamSpec args and kwargs are invalid, fall back to Any
            params = params
                .into_iter()
                .map(|p| match p {
                    Param::Kwargs(Type::Kwargs(_)) => Param::Kwargs(Type::any_error()),
                    Param::VarArg(Type::Args(_)) => Param::VarArg(Type::any_error()),
                    _ => p,
                })
                .collect();
        } else {
            params = params
                .into_iter()
                .filter_map(|p| match p {
                    Param::Kwargs(Type::Kwargs(_)) | Param::VarArg(Type::Args(_)) => None,
                    _ => Some(p),
                })
                .collect();
        }
        let callable = if let Some(q) = paramspec_args
            && paramspec_args == paramspec_kwargs
        {
            Callable::concatenate(
                params
                    .into_iter()
                    .filter_map(|p| match p {
                        Param::PosOnly(ty, _) => Some(ty),
                        Param::Pos(_, ty, _) => Some(ty),
                        _ => None,
                    })
                    .collect(),
                Type::Quantified(q),
                ret,
            )
        } else {
            Callable::list(ParamList::new(params), ret)
        };
        let kind = FunctionKind::from_name(
            self.module_info().name(),
            defining_cls.as_ref().map(|cls| cls.name()),
            &def.name.id,
        );
        let metadata = FuncMetadata {
            kind,
            flags: FuncFlags {
                is_overload,
                is_staticmethod,
                is_classmethod,
                is_property_getter,
                is_property_setter_with_getter,
                has_enum_member_decoration,
                is_override,
                has_final_decoration,
            },
        };
        let mut ty = Forallable::Function(Function {
            signature: callable,
            metadata: metadata.clone(),
        })
        .forall(self.type_params(def.range, tparams, errors));
        for x in decorators.into_iter().rev() {
            ty = match self.apply_decorator(*x, ty, errors) {
                // Preserve function metadata, so things like method binding still work.
                Type::Callable(box c) => Type::Function(Box::new(Function {
                    signature: c,
                    metadata: metadata.clone(),
                })),
                t => t,
            }
        }
        Arc::new(DecoratedFunction {
            id_range: def.name.range,
            ty,
            metadata,
        })
    }

    // Given the index to a function binding, return the previous function binding, if any.
    fn step_overload_pred(&self, pred: &mut Option<Idx<Key>>) -> Option<Arc<DecoratedFunction>> {
        let pred_idx = (*pred)?;
        let mut b = self.bindings().get(pred_idx);
        while let Binding::Forward(k) = b {
            b = self.bindings().get(*k);
        }
        if let Binding::Function(idx, pred_, _) = b {
            let def = self.get_idx(*idx);
            if def.metadata.flags.is_overload {
                *pred = *pred_;
                Some(def)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn extract_signatures(
        &self,
        func: Name,
        ts: Vec1<(TextRange, Type)>,
        errors: &ErrorCollector,
    ) -> Vec1<OverloadType> {
        ts.mapped(|(range, t)| match t {
            Type::Callable(box callable) => OverloadType::Callable(callable),
            Type::Function(function) => OverloadType::Callable(function.signature),
            Type::Forall(box Forall {
                tparams,
                body: Forallable::Function(func),
            }) => OverloadType::Forall(Forall {
                tparams,
                body: func,
            }),
            Type::Any(any_style) => {
                OverloadType::Callable(Callable::ellipsis(any_style.propagate()))
            }
            _ => {
                self.error(
                    errors,
                    range,
                    ErrorKind::InvalidOverload,
                    None,
                    format!(
                        "`{}` has type `{}` after decorator application, which is not callable",
                        func,
                        self.for_display(t)
                    ),
                );
                OverloadType::Callable(Callable::ellipsis(Type::any_error()))
            }
        })
    }
}
