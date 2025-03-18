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
use crate::error::context::ErrorContext;
use crate::error::kind::ErrorKind;
use crate::error::style::ErrorStyle;
use crate::types::callable::BoolKeywords;
use crate::types::callable::Callable;
use crate::types::callable::FuncFlags;
use crate::types::callable::FuncMetadata;
use crate::types::callable::Function;
use crate::types::callable::FunctionKind;
use crate::types::class::ClassType;
use crate::types::literal::Lit;
use crate::types::typed_dict::TypedDict;
use crate::types::types::AnyStyle;
use crate::types::types::BoundMethod;
use crate::types::types::OverloadType;
use crate::types::types::TParams;
use crate::types::types::Type;
use crate::types::types::Var;
use crate::util::visit::VisitMut;
pub enum CallStyle<'a> {
    Method(&'a Name),
    BinaryOp(Operator),
    FreeForm,
}

/// A pair of a call target (see Target) and the Quantifieds it uses.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallTarget {
    qs: Vec<Var>,
    target: Target,
}

impl CallTarget {
    fn new(target: Target) -> Self {
        Self {
            qs: Vec::new(),
            target,
        }
    }

    fn forall(qs: Vec<Var>, target: Target) -> Self {
        Self { qs, target }
    }
}

/// A thing that can be called (see as_call_target and call_infer).
/// Note that a single "call" may invoke multiple functions under the hood,
/// e.g., `__new__` followed by `__init__` for Class.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Target {
    /// A typing.Callable.
    Callable(Callable),
    /// A function.
    Function(Function),
    /// Method of a class. The `Type` is the self/cls argument.
    BoundMethod(Type, Function),
    /// A class object.
    Class(ClassType),
    /// A TypedDict.
    TypedDict(TypedDict),
    /// An overloaded function.
    FunctionOverload(Vec1<Callable>, FuncMetadata),
    /// An overloaded method.
    BoundMethodOverload(Type, Vec1<Callable>, FuncMetadata),
    Any(AnyStyle),
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    fn error_call_target(
        &self,
        errors: &ErrorCollector,
        range: TextRange,
        msg: String,
        error_kind: ErrorKind,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> CallTarget {
        errors.add(range, msg, error_kind, context);
        CallTarget::new(Target::Any(AnyStyle::Error))
    }

    fn fresh_quantified_function(&self, tparams: &TParams, func: Function) -> (Vec<Var>, Function) {
        let (qs, t) =
            self.solver()
                .fresh_quantified(tparams, Type::Function(Box::new(func)), self.uniques);
        match t {
            Type::Function(box func) => (qs, func),
            // We passed a Function to fresh_quantified(), so we know we get a Function back out.
            _ => unreachable!(),
        }
    }

    /// Return a pair of the quantified variables I had to instantiate, and the resulting call target.
    pub fn as_call_target(&self, ty: Type) -> Option<CallTarget> {
        match ty {
            Type::Callable(c) => Some(CallTarget::new(Target::Callable(*c))),
            Type::Function(func) => Some(CallTarget::new(Target::Function(*func))),
            Type::Overload(overload) => {
                let mut qs = Vec::new();
                let sigs = overload.signatures.mapped(|ty| match ty {
                    OverloadType::Callable(signature) => signature,
                    OverloadType::Forall(forall) => {
                        let (qs2, func) =
                            self.fresh_quantified_function(&forall.tparams, forall.body);
                        qs.extend(qs2);
                        func.signature
                    }
                });
                Some(CallTarget::forall(
                    qs,
                    Target::FunctionOverload(sigs, *overload.metadata),
                ))
            }
            Type::BoundMethod(box BoundMethod { obj, func }) => {
                match self.as_call_target(func.as_type()) {
                    Some(CallTarget {
                        qs,
                        target: Target::Function(func),
                    }) => Some(CallTarget::forall(qs, Target::BoundMethod(obj, func))),
                    Some(CallTarget {
                        qs,
                        target: Target::FunctionOverload(overloads, meta),
                    }) => Some(CallTarget::forall(
                        qs,
                        Target::BoundMethodOverload(obj, overloads, meta),
                    )),
                    _ => None,
                }
            }
            Type::ClassDef(cls) => self.as_call_target(self.instantiate_fresh(&cls)),
            Type::Type(box Type::ClassType(cls)) => Some(CallTarget::new(Target::Class(cls))),
            Type::Forall(forall) => {
                let (qs, t) = self.solver().fresh_quantified(
                    &forall.tparams,
                    forall.body.as_type(),
                    self.uniques,
                );
                self.as_call_target(t)
                    .map(|x| CallTarget::forall(qs.into_iter().chain(x.qs).collect(), x.target))
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
            Type::Any(style) => Some(CallTarget::new(Target::Any(style))),
            Type::TypeAlias(ta) => self.as_call_target(ta.as_value(self.stdlib)),
            Type::ClassType(cls) => self
                .get_instance_attribute(&cls, &dunder::CALL)
                .and_then(|attr| self.resolve_as_instance_method(attr))
                .and_then(|ty| self.as_call_target(ty)),
            Type::Type(box Type::TypedDict(typed_dict)) => {
                Some(CallTarget::new(Target::TypedDict(*typed_dict)))
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
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> CallTarget {
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
                    format!("{}, got {}", expect_message, self.for_display(ty)),
                    ErrorKind::NotCallable,
                    context,
                )
            }
        }
    }

    fn make_call_target_and_call(
        &self,
        callee_ty: Type,
        method_name: &Name,
        range: TextRange,
        args: &[CallArg],
        keywords: &[Keyword],
        errors: &ErrorCollector,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        let call_target = self.as_call_target_or_error(
            callee_ty,
            CallStyle::Method(method_name),
            range,
            errors,
            context,
        );
        self.call_infer(call_target, args, keywords, range, errors, context)
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
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Option<Type> {
        let callee_ty = self.type_of_attr_get_if_found(
            ty,
            method_name,
            range,
            errors,
            context,
            "Expr::call_method",
        )?;
        Some(self.make_call_target_and_call(
            callee_ty,
            method_name,
            range,
            args,
            keywords,
            errors,
            context,
        ))
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
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        let callee_ty =
            self.type_of_attr_get(ty, method_name, range, errors, context, "Expr::call_method");
        self.make_call_target_and_call(
            callee_ty,
            method_name,
            range,
            args,
            keywords,
            errors,
            context,
        )
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
        context: Option<&dyn Fn() -> ErrorContext>,
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
                context,
            ),
            args,
            keywords,
            range,
            errors,
            context,
        ))
    }

    fn construct_class(
        &self,
        cls: ClassType,
        args: &[CallArg],
        keywords: &[Keyword],
        range: TextRange,
        errors: &ErrorCollector,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        // Based on https://typing.readthedocs.io/en/latest/spec/constructors.html.
        let instance_ty = Type::ClassType(cls.clone());
        if let Some(ret) = self.call_metaclass(&cls, range, args, keywords, errors, context)
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
                        context,
                    ),
                    &full_args,
                    keywords,
                    range,
                    &dunder_new_errors,
                    context,
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
                    context,
                ),
                args,
                keywords,
                range,
                &dunder_init_errors,
                context,
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
        context: Option<&dyn Fn() -> ErrorContext>,
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
                context,
            ),
            args,
            keywords,
            range,
            errors,
            context,
        );
        Type::TypedDict(Box::new(typed_dict))
    }

    pub fn call_infer(
        &self,
        call_target: CallTarget,
        args: &[CallArg],
        keywords: &[Keyword],
        range: TextRange,
        errors: &ErrorCollector,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        let is_dataclass = matches!(&call_target.target, Target::FunctionOverload(_, meta) if matches!(meta.kind, FunctionKind::Dataclass(_)));
        let res = match call_target.target {
            Target::Class(cls) => self.construct_class(cls, args, keywords, range, errors, context),
            Target::TypedDict(td) => {
                self.construct_typed_dict(td, args, keywords, range, errors, context)
            }
            Target::BoundMethod(obj, func) => {
                let first_arg = CallArg::Type(&obj, range);
                self.callable_infer(
                    func.signature,
                    Some(func.metadata.kind.as_func_id()),
                    Some(first_arg),
                    args,
                    keywords,
                    range,
                    errors,
                    errors,
                    context,
                )
            }
            Target::Callable(callable) => self.callable_infer(
                callable, None, None, args, keywords, range, errors, errors, context,
            ),
            Target::Function(Function {
                signature: callable,
                metadata,
            }) => self.callable_infer(
                callable,
                Some(metadata.kind.as_func_id()),
                None,
                args,
                keywords,
                range,
                errors,
                errors,
                context,
            ),
            Target::FunctionOverload(overloads, meta) => self.call_overloads(
                overloads, meta, None, args, keywords, range, errors, context,
            ),
            Target::BoundMethodOverload(obj, overloads, meta) => self.call_overloads(
                overloads,
                meta,
                Some(CallArg::Type(&obj, range)),
                args,
                keywords,
                range,
                errors,
                context,
            ),
            Target::Any(style) => {
                // Make sure we still catch errors in the arguments.
                for arg in args {
                    match arg {
                        CallArg::Expr(e) | CallArg::Star(e, _) => {
                            self.expr_infer(e, errors);
                        }
                        CallArg::Type(..) => {}
                    }
                }
                for kw in keywords {
                    self.expr_infer(&kw.value, errors);
                }
                style.propagate()
            }
        };
        self.solver().finish_quantified(&call_target.qs);
        if is_dataclass && let Type::Callable(c) = res {
            let mut kws = BoolKeywords::new();
            for kw in keywords {
                kws.set_keyword(kw.arg.as_ref(), self.expr_infer(&kw.value, errors));
            }
            Type::Function(Box::new(Function {
                signature: *c,
                metadata: FuncMetadata {
                    kind: FunctionKind::Dataclass(Box::new(kws)),
                    flags: FuncFlags::default(),
                },
            }))
        } else {
            res
        }
    }

    fn call_overloads(
        &self,
        overloads: Vec1<Callable>,
        metadata: FuncMetadata,
        self_arg: Option<CallArg>,
        args: &[CallArg],
        keywords: &[Keyword],
        range: TextRange,
        errors: &ErrorCollector,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        let mut closest_overload = None;
        let mut fewest_errors: Option<ErrorCollector> = None;
        for callable in overloads.into_iter() {
            let arg_errors = ErrorCollector::new(self.module_info().dupe(), ErrorStyle::Delayed);
            let call_errors = ErrorCollector::new(self.module_info().dupe(), ErrorStyle::Delayed);
            let res = self.callable_infer(
                callable.clone(),
                Some(metadata.kind.as_func_id()),
                self_arg.clone(),
                args,
                keywords,
                range,
                &arg_errors,
                &call_errors,
                // We intentionally drop the context here, as arg errors don't need it,
                // and if we log any call errors, we'll also log a separate
                // "No matching overloads" error with the necessary context.
                None,
            );
            if call_errors.is_empty() {
                errors.extend(arg_errors);
                return res;
            }
            match &fewest_errors {
                Some(errs) if errs.len() <= call_errors.len() => {}
                _ => {
                    closest_overload = Some(callable);
                    fewest_errors = Some(call_errors);
                }
            }
        }
        // We're guaranteed to have at least one overload.
        let closest_overload = closest_overload.unwrap();
        let fewest_errors = fewest_errors.unwrap();
        let mut signature = match self_arg {
            Some(_) => closest_overload
                .drop_first_param()
                .unwrap_or(closest_overload),
            None => closest_overload,
        };
        signature.visit_mut(&mut |x| *x = self.solver().for_display((*x).clone()));
        self.error(
            errors,
            range,
            ErrorKind::NoMatchingOverload,
            context,
            format!(
                "No matching overload found for function `{}`, reporting errors for closest overload: `{}`",
                metadata.kind.as_func_id().format(self.module_info().name()), signature,
            ),
        );
        errors.extend(fewest_errors);
        Type::any_error()
    }

    /// Helper function hide details of call synthesis from the attribute resolution code.
    pub fn call_property_getter(
        &self,
        getter_method: Type,
        range: TextRange,
        errors: &ErrorCollector,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        let call_target = self.as_call_target_or_error(
            getter_method,
            CallStyle::FreeForm,
            range,
            errors,
            context,
        );
        self.call_infer(call_target, &[], &[], range, errors, context)
    }

    /// Helper function hide details of call synthesis from the attribute resolution code.
    pub fn call_property_setter(
        &self,
        setter_method: Type,
        got: CallArg,
        range: TextRange,
        errors: &ErrorCollector,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        let call_target = self.as_call_target_or_error(
            setter_method,
            CallStyle::FreeForm,
            range,
            errors,
            context,
        );
        self.call_infer(call_target, &[got], &[], range, errors, context)
    }

    /// Helper function hide details of call synthesis from the attribute resolution code.
    pub fn call_descriptor_getter(
        &self,
        getter_method: Type,
        base: DescriptorBase,
        range: TextRange,
        errors: &ErrorCollector,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        // When a descriptor is accessed on an instance, it gets the instance and the class object as
        // the `obj` and `objtype` arguments. When it is accessed on a class, it gets `None` as `obj`
        // and the class object as `objtype`.
        let (objtype, obj) = match base {
            DescriptorBase::Instance(classtype) => (
                Type::ClassDef(classtype.class_object().dupe()),
                Type::ClassType(classtype),
            ),
            DescriptorBase::ClassDef(class) => (Type::ClassDef(class), Type::None),
        };
        let args = [CallArg::Type(&obj, range), CallArg::Type(&objtype, range)];
        let call_target = self.as_call_target_or_error(
            getter_method,
            CallStyle::FreeForm,
            range,
            errors,
            context,
        );
        self.call_infer(call_target, &args, &[], range, errors, context)
    }

    /// Helper function hide details of call synthesis from the attribute resolution code.
    pub fn call_descriptor_setter(
        &self,
        setter_method: Type,
        class_type: ClassType,
        got: CallArg,
        range: TextRange,
        errors: &ErrorCollector,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        // When a descriptor is set on an instance, it gets the instance `class_type` and the value `got` as arguments.
        // Descriptor setters cannot be called on a class (an attempt to assign will overwrite the
        // descriptor itself rather than call the setter).
        let instance = Type::ClassType(class_type);
        let args = [CallArg::Type(&instance, range), got];
        let call_target = self.as_call_target_or_error(
            setter_method,
            CallStyle::FreeForm,
            range,
            errors,
            context,
        );
        self.call_infer(call_target, &args, &[], range, errors, context)
    }

    pub fn call_getattr(
        &self,
        getattr_ty: Type,
        attr_name: Name,
        range: TextRange,
        errors: &ErrorCollector,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        let call_target =
            self.as_call_target_or_error(getattr_ty, CallStyle::FreeForm, range, errors, context);
        let attr_name_ty = Type::Literal(Lit::String(attr_name.as_str().into()));
        self.call_infer(
            call_target,
            &[CallArg::Type(&attr_name_ty, range)],
            &[],
            range,
            errors,
            context,
        )
    }
}
