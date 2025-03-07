/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::Keyword;
use ruff_python_ast::Operator;
use ruff_text_size::TextRange;
use starlark_map::small_set::SmallSet;
use vec1::Vec1;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::attr::DescriptorBase;
use crate::alt::callable::CallArg;
use crate::dunder;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::error::style::ErrorStyle;
use crate::module::module_name::ModuleName;
use crate::types::callable::BoolKeywords;
use crate::types::callable::Callable;
use crate::types::callable::CallableKind;
use crate::types::callable::FuncId;
use crate::types::class::ClassType;
use crate::types::typed_dict::TypedDict;
use crate::types::types::AnyStyle;
use crate::types::types::BoundMethod;
use crate::types::types::Type;
use crate::types::types::Var;
pub enum CallStyle<'a> {
    Method(&'a Name),
    BinaryOp(Operator),
    FreeForm,
}

/// A thing that can be called (see as_call_target and call_infer).
/// Note that a single "call" may invoke multiple functions under the hood,
/// e.g., `__new__` followed by `__init__` for Class.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CallTarget {
    /// A thing whose type is a Callable, usually a function.
    Callable(Callable, CallableKind),
    /// The dataclasses.dataclass function.
    Dataclass(Callable),
    /// Method of a class. The `Type` is the self/cls argument.
    BoundMethod(Type, Callable, CallableKind),
    /// A class object.
    Class(ClassType),
    /// A TypedDict.
    TypedDict(TypedDict),
    /// An overload.
    Overload(Vec1<Option<(Vec<Var>, CallTarget)>>),
    Any(AnyStyle),
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    fn error_call_target(
        &self,
        errors: &ErrorCollector,
        range: TextRange,
        msg: String,
        error_kind: ErrorKind,
    ) -> (Vec<Var>, CallTarget) {
        errors.add(range, msg, error_kind, None);
        (Vec::new(), CallTarget::Any(AnyStyle::Error))
    }

    /// Return a pair of the quantified variables I had to instantiate, and the resulting call target.
    pub fn as_call_target(&self, ty: Type) -> Option<(Vec<Var>, CallTarget)> {
        match ty {
            Type::Callable(c, CallableKind::Dataclass(_)) => {
                Some((Vec::new(), CallTarget::Dataclass(*c)))
            }
            Type::Callable(c, kind) => Some((Vec::new(), CallTarget::Callable(*c, kind))),
            Type::Overload(overloads) => Some((
                Vec::new(),
                CallTarget::Overload(overloads.mapped(|ty| self.as_call_target(ty))),
            )),
            Type::BoundMethod(box BoundMethod { obj, func }) => match self.as_call_target(func) {
                Some((gs, CallTarget::Callable(c, kind))) => {
                    Some((gs, CallTarget::BoundMethod(obj, c, kind)))
                }
                Some((gs, CallTarget::Overload(overloads))) => {
                    let overloads = overloads.mapped(|x| match x {
                        Some((gs2, CallTarget::Callable(c, kind))) => {
                            Some((gs2, CallTarget::BoundMethod(obj.clone(), c, kind)))
                        }
                        _ => None,
                    });
                    Some((gs, CallTarget::Overload(overloads)))
                }
                _ => None,
            },
            Type::ClassDef(cls) => self.as_call_target(self.instantiate_fresh(&cls)),
            Type::Type(box Type::ClassType(cls)) => Some((Vec::new(), CallTarget::Class(cls))),
            Type::Forall(box (_, params, t)) => {
                let (mut qs, t) = self.solver().fresh_quantified(&params, t, self.uniques);
                self.as_call_target(t).map(|(qs2, x)| {
                    qs.extend(qs2);
                    (qs, x)
                })
            }
            Type::Var(v) if let Some(_guard) = self.recurser.recurse(v) => {
                self.as_call_target(self.solver().force_var(v))
            }
            Type::Union(xs) => {
                let res = xs
                    .into_iter()
                    .map(|x| self.as_call_target(x))
                    .collect::<Option<SmallSet<_>>>()?;
                if res.len() == 1 {
                    Some(res.into_iter().next().unwrap())
                } else {
                    None
                }
            }
            Type::Any(style) => Some((Vec::new(), CallTarget::Any(style))),
            Type::TypeAlias(ta) => self.as_call_target(ta.as_value(self.stdlib)),
            Type::ClassType(cls) => self
                .get_instance_attribute(&cls, &dunder::CALL)
                .and_then(|attr| self.resolve_as_instance_method(attr))
                .and_then(|ty| self.as_call_target(ty)),
            Type::Type(box Type::TypedDict(typed_dict)) => {
                Some((Vec::new(), CallTarget::TypedDict(*typed_dict)))
            }
            _ => None,
        }
    }

    pub fn as_call_target_or_error(
        &self,
        ty: Type,
        call_style: CallStyle,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> (Vec<Var>, CallTarget) {
        match self.as_call_target(ty.clone()) {
            Some(target) => target,
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
                self.error_call_target(
                    errors,
                    range,
                    format!("{}, got {}", expect_message, ty.deterministic_printing()),
                    ErrorKind::NotCallable,
                )
            }
        }
    }

    /// Calls a method. If no attribute exists with the given method name, returns None without attempting the call.
    pub fn call_method(
        &self,
        ty: &Type,
        method_name: &Name,
        range: TextRange,
        args: &[CallArg],
        keywords: &[Keyword],
        errors: &ErrorCollector,
    ) -> Option<Type> {
        let callee_ty = self.type_of_attr_get_if_found(
            ty.clone(),
            method_name,
            range,
            errors,
            "Expr::call_method",
        )?;
        let call_target =
            self.as_call_target_or_error(callee_ty, CallStyle::Method(method_name), range, errors);
        Some(self.call_infer(call_target, args, keywords, range, errors))
    }

    /// Calls a method. If no attribute exists with the given method name, logs an error and calls the method with
    /// an assumed type of Callable[..., Any].
    pub fn call_method_or_error(
        &self,
        ty: &Type,
        method_name: &Name,
        range: TextRange,
        args: &[CallArg],
        keywords: &[Keyword],
        errors: &ErrorCollector,
    ) -> Type {
        if let Some(ret) = self.call_method(ty, method_name, range, args, keywords, errors) {
            ret
        } else {
            self.call_infer(
                self.error_call_target(
                    errors,
                    range,
                    format!("`{ty}` has no attribute `{method_name}`"),
                    ErrorKind::MissingAttribute,
                ),
                args,
                keywords,
                range,
                errors,
            )
        }
    }

    /// If the metaclass defines a custom `__call__`, call it. If the `__call__` comes from `type`, ignore
    /// it because `type.__call__` behavior is baked into our constructor logic.
    pub fn call_metaclass(
        &self,
        cls: &ClassType,
        range: TextRange,
        args: &[CallArg],
        keywords: &[Keyword],
        errors: &ErrorCollector,
    ) -> Option<Type> {
        let dunder_call = match self.get_metaclass_dunder_call(cls)? {
            Type::BoundMethod(box BoundMethod { func, .. }) => {
                // This method was bound to a general instance of the metaclass, but we have more
                // information about the particular instance that it should be bound to.
                Type::BoundMethod(Box::new(BoundMethod {
                    obj: Type::type_form(Type::ClassType(cls.clone())),
                    func,
                }))
            }
            dunder_call => dunder_call,
        };
        Some(self.call_infer(
            self.as_call_target_or_error(
                dunder_call,
                CallStyle::Method(&dunder::CALL),
                range,
                errors,
            ),
            args,
            keywords,
            range,
            errors,
        ))
    }

    fn construct_class(
        &self,
        cls: ClassType,
        args: &[CallArg],
        keywords: &[Keyword],
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        // Based on https://typing.readthedocs.io/en/latest/spec/constructors.html.
        let instance_ty = Type::ClassType(cls.clone());
        if let Some(ret) = self.call_metaclass(&cls, range, args, keywords, errors)
            && !self
                .solver()
                .is_subset_eq(&ret, &instance_ty, self.type_order())
        {
            // Got something other than an instance of the class under construction.
            return ret;
        }
        let (overrides_new, dunder_new_has_errors) =
            if let Some(new_method) = self.get_dunder_new(&cls) {
                let cls_ty = Type::type_form(instance_ty.clone());
                let mut full_args = vec![CallArg::Type(&cls_ty, range)];
                full_args.extend_from_slice(args);
                let dunder_new_errors =
                    ErrorCollector::new(self.module_info().dupe(), ErrorStyle::Delayed);
                let ret = self.call_infer(
                    self.as_call_target_or_error(
                        new_method,
                        CallStyle::Method(&dunder::NEW),
                        range,
                        errors,
                    ),
                    &full_args,
                    keywords,
                    range,
                    &dunder_new_errors,
                );
                let has_errors = !dunder_new_errors.is_empty();
                errors.extend(dunder_new_errors);
                if !self
                    .solver()
                    .is_subset_eq(&ret, &instance_ty, self.type_order())
                {
                    // Got something other than an instance of the class under construction.
                    return ret;
                }
                (true, has_errors)
            } else {
                (false, false)
            };
        if let Some(init_method) = self.get_dunder_init(&cls, overrides_new) {
            let dunder_init_errors =
                ErrorCollector::new(self.module_info().dupe(), ErrorStyle::Delayed);
            self.call_infer(
                self.as_call_target_or_error(
                    init_method,
                    CallStyle::Method(&dunder::INIT),
                    range,
                    errors,
                ),
                args,
                keywords,
                range,
                &dunder_init_errors,
            );
            // Report `__init__` errors only when there are no `__new__` errors, to avoid redundant errors.
            if !dunder_new_has_errors {
                errors.extend(dunder_init_errors);
            }
        }
        cls.self_type()
    }

    fn construct_typed_dict(
        &self,
        typed_dict: TypedDict,
        args: &[CallArg],
        keywords: &[Keyword],
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        // We know `__init__` exists because we synthesize it.
        let init_method = self
            .get_dunder_init(&typed_dict.as_class_type(), false)
            .unwrap();
        self.call_infer(
            self.as_call_target_or_error(
                init_method,
                CallStyle::Method(&dunder::INIT),
                range,
                errors,
            ),
            args,
            keywords,
            range,
            errors,
        );
        Type::TypedDict(Box::new(typed_dict))
    }

    pub fn call_infer(
        &self,
        call_target: (Vec<Var>, CallTarget),
        args: &[CallArg],
        keywords: &[Keyword],
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        self.call_infer_inner(call_target, args, keywords, range, errors, errors)
    }

    // TODO: This function depends on an invariant that overloads only contain functions and bound
    // methods, never a constructor or another overload. See assertions marked with "Hack" below.
    // This is all quite hacky ("very very very grim," says Neil).
    fn call_infer_inner(
        &self,
        call_target: (Vec<Var>, CallTarget),
        args: &[CallArg],
        keywords: &[Keyword],
        range: TextRange,
        arg_errors: &ErrorCollector,
        call_errors: &ErrorCollector,
    ) -> Type {
        let is_dataclass = matches!(call_target.1, CallTarget::Dataclass(_));
        let res = match call_target.1 {
            CallTarget::Class(cls) => {
                // Hack
                assert!(
                    std::ptr::eq(arg_errors, call_errors),
                    "unexpected constructor inside overload"
                );
                self.construct_class(cls, args, keywords, range, arg_errors)
            }
            CallTarget::TypedDict(td) => {
                // Hack
                assert!(
                    std::ptr::eq(arg_errors, call_errors),
                    "unexpected TypedDict constructor inside overload"
                );
                self.construct_typed_dict(td, args, keywords, range, arg_errors)
            }
            CallTarget::BoundMethod(obj, c, kind) => {
                let first_arg = CallArg::Type(&obj, range);
                self.callable_infer(
                    c,
                    kind.as_func_id(),
                    Some(first_arg),
                    args,
                    keywords,
                    range,
                    arg_errors,
                    call_errors,
                )
            }
            CallTarget::Callable(callable, kind) => self.callable_infer(
                callable,
                kind.as_func_id(),
                None,
                args,
                keywords,
                range,
                arg_errors,
                call_errors,
            ),
            CallTarget::Dataclass(callable) => self.callable_infer(
                callable,
                Some(FuncId {
                    module: ModuleName::dataclasses(),
                    cls: None,
                    func: Name::new("dataclass"),
                }),
                None,
                args,
                keywords,
                range,
                arg_errors,
                call_errors,
            ),
            CallTarget::Overload(overloads) => {
                // Hack
                assert!(
                    std::ptr::eq(arg_errors, call_errors),
                    "unexpected nested overload"
                );
                let errors = arg_errors;
                for call_target in overloads.into_iter() {
                    if let Some(call_target) = call_target {
                        let arg_errors =
                            ErrorCollector::new(self.module_info().dupe(), ErrorStyle::Delayed);
                        let call_errors =
                            ErrorCollector::new(self.module_info().dupe(), ErrorStyle::Delayed);
                        let res = self.call_infer_inner(
                            call_target,
                            args,
                            keywords,
                            range,
                            &arg_errors,
                            &call_errors,
                        );
                        if call_errors.is_empty() {
                            errors.extend(arg_errors);
                            return res;
                        }
                    }
                }
                return self.error(
                    errors,
                    range,
                    ErrorKind::NoMatchingOverload,
                    None,
                    "No matching overload found".to_owned(),
                );
            }
            CallTarget::Any(style) => {
                // Make sure we still catch errors in the arguments.
                for arg in args {
                    match arg {
                        CallArg::Expr(e) | CallArg::Star(e, _) => {
                            self.expr_infer(e, arg_errors);
                        }
                        CallArg::Type(..) => {}
                    }
                }
                for kw in keywords {
                    self.expr_infer(&kw.value, arg_errors);
                }
                style.propagate()
            }
        };
        self.solver().finish_quantified(&call_target.0);
        if is_dataclass && let Type::Callable(c, _) = res {
            let mut kws = BoolKeywords::new();
            for kw in keywords {
                kws.set_keyword(kw.arg.as_ref(), self.expr_infer(&kw.value, arg_errors));
            }
            Type::Callable(c, CallableKind::Dataclass(Box::new(kws)))
        } else {
            res
        }
    }

    /// Helper function hide details of call synthesis from the attribute resolution code.
    pub fn call_property_getter(
        &self,
        getter_method: Type,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        let call_target =
            self.as_call_target_or_error(getter_method, CallStyle::FreeForm, range, errors);
        self.call_infer(call_target, &[], &[], range, errors)
    }

    /// Helper function hide details of call synthesis from the attribute resolution code.
    pub fn call_property_setter(
        &self,
        setter_method: Type,
        got: CallArg,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        let call_target =
            self.as_call_target_or_error(setter_method, CallStyle::FreeForm, range, errors);
        self.call_infer(call_target, &[got], &[], range, errors)
    }

    /// Helper function hide details of call synthesis from the attribute resolution code.
    pub fn call_descriptor_getter(
        &self,
        getter_method: Type,
        base: DescriptorBase,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        // When a descriptor is accessed on an instance, it gets the instance and the class object as
        // the `obj` and `objtype` arguments. When it is accessed on a class, it gets `None` as `obj`
        // and the class object as `objtype`.
        let (objtype, obj) = match base {
            DescriptorBase::Instance(classtype) => (
                Type::ClassDef(classtype.class_object().clone()),
                Type::ClassType(classtype),
            ),
            DescriptorBase::ClassDef(class) => (Type::ClassDef(class), Type::None),
        };
        let args = [CallArg::Type(&obj, range), CallArg::Type(&objtype, range)];
        let call_target =
            self.as_call_target_or_error(getter_method, CallStyle::FreeForm, range, errors);
        self.call_infer(call_target, &args, &[], range, errors)
    }

    /// Helper function hide details of call synthesis from the attribute resolution code.
    pub fn call_descriptor_setter(
        &self,
        setter_method: Type,
        class_type: ClassType,
        got: CallArg,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        // When a descriptor is set on an instance, it gets the instance `class_type` and the value `got` as arguments.
        // Descriptor setters cannot be called on a class (an attempt to assign will overwrite the
        // descriptor itself rather than call the setter).
        let instance = Type::ClassType(class_type);
        let args = [CallArg::Type(&instance, range), got];
        let call_target =
            self.as_call_target_or_error(setter_method, CallStyle::FreeForm, range, errors);
        self.call_infer(call_target, &args, &[], range, errors)
    }
}
