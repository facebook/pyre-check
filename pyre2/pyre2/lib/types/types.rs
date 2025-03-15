/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use dupe::Dupe;
use parse_display::Display;
use pyrefly_derive::TypeEq;
use ruff_python_ast::name::Name;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use vec1::Vec1;

use crate::assert_words;
use crate::types::callable::Callable;
use crate::types::callable::FuncMetadata;
use crate::types::callable::Function;
use crate::types::callable::FunctionKind;
use crate::types::callable::Param;
use crate::types::callable::ParamList;
use crate::types::class::Class;
use crate::types::class::ClassKind;
use crate::types::class::ClassType;
use crate::types::literal::Lit;
use crate::types::module::Module;
use crate::types::param_spec::ParamSpec;
use crate::types::quantified::Quantified;
use crate::types::special_form::SpecialForm;
use crate::types::stdlib::Stdlib;
use crate::types::tuple::Tuple;
use crate::types::type_var::Restriction;
use crate::types::type_var::TypeVar;
use crate::types::type_var::Variance;
use crate::types::type_var_tuple::TypeVarTuple;
use crate::types::typed_dict::TypedDict;
use crate::util::display::commas_iter;
use crate::util::uniques::Unique;
use crate::util::uniques::UniqueFactory;
use crate::util::visit::Visit;
use crate::util::visit::VisitMut;

/// An introduced synthetic variable to range over as yet unknown types.
#[derive(Debug, Copy, Clone, Dupe, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var(Unique);

impl Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{}", self.0)
    }
}

impl Var {
    pub fn new(uniques: &UniqueFactory) -> Self {
        Self(uniques.fresh())
    }

    pub fn to_type(self) -> Type {
        Type::Var(self)
    }

    fn zero(&mut self) {
        self.0 = Unique::zero();
    }
}

/// Bundles together type param info for passing around while building TParams.
#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TParamInfo {
    pub name: Name,
    pub quantified: Quantified,
    pub restriction: Restriction,
    pub default: Option<Type>,
    /// The variance if known, or None for infer_variance=True or a scoped type parameter
    pub variance: Option<Variance>,
}

#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TParam {
    /// Display name
    pub name: Name,
    pub quantified: Quantified,
    pub restriction: Restriction,
    pub default: Option<Type>,
    pub variance: Variance,
}

impl Display for TParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

/// Wraps a vector of type parameters. The constructor ensures that
/// type parameters without defaults never follow ones with defaults.
#[derive(Debug, Clone, Default, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TParams(Vec<TParam>);

impl Display for TParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]", commas_iter(|| self.0.iter()))
    }
}

impl TParams {
    pub fn new(info: Vec<TParamInfo>) -> Result<Self, Self> {
        let mut error = false;
        let mut tparams: Vec<TParam> = Vec::with_capacity(info.len());
        for tparam in info {
            let default = if tparam.default.is_none()
                && tparams.last().is_some_and(|p| p.default.is_some())
            {
                // Missing default.
                error = true;
                Some(Type::any_error())
            } else {
                tparam.default
            };
            tparams.push(TParam {
                name: tparam.name,
                quantified: tparam.quantified,
                restriction: tparam.restriction,
                default,
                // Classes set the variance before getting here. For functions and aliases, the variance isn't meaningful;
                // it doesn't matter what we set it to as long as we make it non-None to indicate that it's not missing.
                variance: tparam.variance.unwrap_or(Variance::Invariant),
            });
        }
        if error {
            Err(Self(tparams))
        } else {
            Ok(Self(tparams))
        }
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &TParam> {
        self.0.iter()
    }

    pub fn quantified(&self) -> impl Iterator<Item = Quantified> + '_ {
        self.0.iter().map(|x| x.quantified)
    }
}

#[derive(
    Debug, Clone, Copy, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash, Display
)]
pub enum NeverStyle {
    Never,
    NoReturn,
}

#[derive(
    Debug, Clone, Copy, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash, Display
)]
pub enum AnyStyle {
    /// The user wrote `Any` literally.
    Explicit,
    /// The user didn't write a type, so we inferred `Any`.
    Implicit,
    /// There was an error, so we made up `Any`.
    /// If this `Any` is used in an error position, don't report another error.
    Error,
}

impl AnyStyle {
    pub fn propagate(self) -> Type {
        match self {
            Self::Implicit | Self::Error => Type::Any(self),
            Self::Explicit => Type::Any(Self::Implicit),
        }
    }
}

#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
pub enum TypeAliasStyle {
    /// A type alias declared with the `type` keyword
    #[display("ScopedTypeAlias")]
    Scoped,
    /// A type alias declared with a `: TypeAlias` annotation
    #[display("LegacyExplicitTypeAlias")]
    LegacyExplicit,
    /// An unannotated assignment that may be either an implicit type alias or an untyped value
    #[display("LegacyImplicitTypeAlias")]
    LegacyImplicit,
}

#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeAlias {
    pub name: Box<Name>,
    ty: Box<Type>,
    pub style: TypeAliasStyle,
}

impl TypeAlias {
    pub fn new(name: Name, ty: Type, style: TypeAliasStyle) -> Self {
        Self {
            name: Box::new(name),
            ty: Box::new(ty),
            style,
        }
    }

    /// Gets the type contained within the type alias for use in a value
    /// position - for example, for a function call or attribute access.
    pub fn as_value(&self, stdlib: &Stdlib) -> Type {
        if self.style == TypeAliasStyle::Scoped {
            stdlib.type_alias_type().to_type()
        } else {
            *self.ty.clone()
        }
    }

    /// Gets the type contained within the type alias for use in a type
    /// position - for example, in a variable type annotation. Note that
    /// the caller is still responsible for untyping the type. That is,
    /// `type X = int` is represented as `TypeAlias(X, type[int])`, and
    /// `as_type` returns `type[int]`; the caller must turn it into `int`.
    pub fn as_type(&self) -> Type {
        *self.ty.clone()
    }

    pub fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a Type)) {
        f(&self.ty);
    }

    pub fn visit_mut(&mut self, f: &mut dyn FnMut(&mut Type)) {
        f(&mut self.ty);
    }
}

assert_words!(Type, 4);

#[derive(Debug)]
pub enum CalleeKind {
    Callable,
    Function(FunctionKind),
    Class(ClassKind),
}

#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoundMethod {
    pub obj: Type,
    pub func: BoundMethodType,
}

impl BoundMethod {
    pub fn to_callable(&self) -> Option<Type> {
        self.as_bound_function().to_unbound_callable()
    }

    pub fn as_bound_function(&self) -> Type {
        self.func.as_type()
    }

    pub fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a Type)) {
        let Self { obj, func } = self;
        f(obj);
        func.visit(f);
    }

    pub fn visit_mut(&mut self, f: &mut dyn FnMut(&mut Type)) {
        let Self { obj, func } = self;
        f(obj);
        func.visit_mut(f);
    }
}

#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BoundMethodType {
    Function(Function),
    Forall(Forall<Function>),
    Overload(Overload),
}

impl BoundMethodType {
    pub fn as_type(&self) -> Type {
        match self {
            Self::Function(func) => Type::Function(Box::new(func.clone())),
            Self::Forall(forall) => {
                Forallable::Function(forall.body.clone()).forall(forall.tparams.clone())
            }
            Self::Overload(overload) => Type::Overload(overload.clone()),
        }
    }

    fn is_typeguard(&self) -> bool {
        match self {
            Self::Function(func) => func.signature.is_typeguard(),
            Self::Forall(forall) => forall.body.signature.is_typeguard(),
            Self::Overload(overload) => overload.is_typeguard(),
        }
    }

    fn is_typeis(&self) -> bool {
        match self {
            Self::Function(func) => func.signature.is_typeis(),
            Self::Forall(forall) => forall.body.signature.is_typeis(),
            Self::Overload(overload) => overload.is_typeis(),
        }
    }

    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a Type)) {
        match self {
            Self::Function(x) => x.visit(f),
            Self::Forall(x) => x.body.visit(f),
            Self::Overload(x) => x.visit(f),
        }
    }

    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut Type)) {
        match self {
            Self::Function(x) => x.visit_mut(f),
            Self::Forall(x) => x.body.visit_mut(f),
            Self::Overload(x) => x.visit_mut(f),
        }
    }
}

#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Overload {
    pub signatures: Vec1<OverloadType>,
    pub metadata: Box<FuncMetadata>,
}

impl Overload {
    fn is_typeguard(&self) -> bool {
        self.signatures.iter().any(|t| t.is_typeguard())
    }

    fn is_typeis(&self) -> bool {
        self.signatures.iter().any(|t| t.is_typeis())
    }

    fn visit<'a>(&'a self, mut f: &mut dyn FnMut(&'a Type)) {
        for t in self.signatures.iter() {
            t.visit(&mut f);
        }
        self.metadata.visit(f);
    }

    fn visit_mut(&mut self, mut f: &mut dyn FnMut(&mut Type)) {
        for t in self.signatures.iter_mut() {
            t.visit_mut(&mut f);
        }
        self.metadata.visit_mut(f);
    }
}

#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OverloadType {
    Callable(Callable),
    Forall(Forall<Function>),
}

impl OverloadType {
    pub fn as_type(&self) -> Type {
        match self {
            Self::Callable(c) => Type::Callable(Box::new(c.clone())),
            Self::Forall(forall) => {
                Forallable::Function(forall.body.clone()).forall(forall.tparams.clone())
            }
        }
    }

    fn is_typeguard(&self) -> bool {
        match self {
            Self::Callable(c) => c.is_typeguard(),
            Self::Forall(forall) => forall.body.signature.is_typeguard(),
        }
    }

    fn is_typeis(&self) -> bool {
        match self {
            Self::Callable(c) => c.is_typeis(),
            Self::Forall(forall) => forall.body.signature.is_typeis(),
        }
    }

    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a Type)) {
        match self {
            Self::Callable(c) => c.visit(f),
            Self::Forall(forall) => forall.body.signature.visit(f),
        }
    }

    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut Type)) {
        match self {
            Self::Callable(c) => c.visit_mut(f),
            Self::Forall(forall) => forall.body.signature.visit_mut(f),
        }
    }
}

#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Forall<T> {
    pub tparams: TParams,
    pub body: T,
}

/// These are things that can have Forall around them, so often you see `Forall<Forallable>`
#[derive(Debug, Clone, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Forallable {
    TypeAlias(TypeAlias),
    Function(Function),
}

impl Forallable {
    pub fn forall(self, tparams: TParams) -> Type {
        if tparams.is_empty() {
            self.as_type()
        } else {
            Type::Forall(Box::new(Forall {
                tparams,
                body: self,
            }))
        }
    }

    pub fn name(&self) -> Name {
        match self {
            Self::Function(func) => func.metadata.kind.as_func_id().func,
            Self::TypeAlias(ta) => (*ta.name).clone(),
        }
    }

    pub fn as_type(self) -> Type {
        match self {
            Self::Function(func) => Type::Function(Box::new(func)),
            Self::TypeAlias(ta) => Type::TypeAlias(ta),
        }
    }

    fn is_typeguard(&self) -> bool {
        match self {
            Self::Function(func) => func.signature.is_typeguard(),
            Self::TypeAlias(_) => false,
        }
    }

    fn is_typeis(&self) -> bool {
        match self {
            Self::Function(func) => func.signature.is_typeis(),
            Self::TypeAlias(_) => false,
        }
    }

    pub fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a Type)) {
        match self {
            Self::Function(func) => func.visit(f),
            Self::TypeAlias(ta) => ta.visit(f),
        }
    }

    pub fn visit_mut(&mut self, f: &mut dyn FnMut(&mut Type)) {
        match self {
            Self::Function(func) => func.visit_mut(f),
            Self::TypeAlias(ta) => ta.visit_mut(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, TypeEq, PartialOrd, Ord, Hash)]
pub enum Type {
    Literal(Lit),
    LiteralString,
    /// typing.Callable
    Callable(Box<Callable>),
    /// A function declared using the `def` keyword.
    /// Note that the FunctionKind metadata doesn't participate in subtyping, and thus two types with distinct metadata are still subtypes.
    Function(Box<Function>),
    /// A method of a class. The first `Box<Type>` is the self/cls argument,
    /// and the second is the function.
    BoundMethod(Box<BoundMethod>),
    /// An overloaded function.
    Overload(Overload),
    Union(Vec<Type>),
    #[expect(dead_code)] // Not currently used, but may be in the future
    Intersect(Vec<Type>),
    /// A class definition has type `Type::ClassDef(cls)`. This type
    /// has special value semantics, and can also be implicitly promoted
    /// to `Type::Type(box Type::ClassType(cls, default_targs))` by looking
    /// up the class `tparams` and setting defaults using gradual types: for
    /// example `list` in an annotation position means `list[Any]`.
    ClassDef(Class),
    /// A value that indicates a concrete, instantiated type with known type
    /// arguments that are validated against the class type parameters. If the
    /// class is not generic, the arguments are empty.
    ///
    /// Instances of classes have this type, and a term of the form `C[arg1, arg2]`
    /// would have the form `Type::Type(box Type::ClassType(C, [arg1, arg2]))`.
    ClassType(ClassType),
    /// Instances of TypedDicts have this type, and a term of the form `TD[arg1, arg2]`
    /// would have the form `Type::Type(box Type::TypedDict(TD, [arg1, arg2]))`. Note
    /// that TypedDict class definitions are still represented as `ClassDef(TD)`, just
    /// like regular classes.
    TypedDict(Box<TypedDict>),
    Tuple(Tuple),
    Module(Module),
    Forall(Box<Forall<Forallable>>),
    Var(Var),
    Quantified(Quantified),
    TypeGuard(Box<Type>),
    TypeIs(Box<Type>),
    Unpack(Box<Type>),
    TypeVar(TypeVar),
    ParamSpec(ParamSpec),
    TypeVarTuple(TypeVarTuple),
    SpecialForm(SpecialForm),
    Concatenate(Box<[Type]>, Box<Type>),
    ParamSpecValue(ParamList),
    /// Used to represent `P.args`. The spec describes it as an annotation,
    /// but it's easier to think of it as a type that can't occur in nested positions.
    Args(Quantified),
    Kwargs(Quantified),
    /// Used to represent a type that has a value representation, e.g. a class
    Type(Box<Type>),
    Ellipsis,
    Any(AnyStyle),
    Never(NeverStyle),
    TypeAlias(TypeAlias),
    /// Represents the result of a super() call. The first ClassType is the class that attribute lookup
    /// on the super instance should be done on (*not* the class passed to the super() call), and the second
    /// ClassType is the second argument (implicit or explicit) to the super() call. For example, in:
    ///   class A: ...
    ///   class B(A): ...
    ///   class C(B):
    ///     def f(self):
    ///       super(B, self)
    /// attribute lookup should be done on the class above `B` in the MRO of the type of `self` -
    /// that is, attribute lookup should be done on class `A`. And the type of `self` is class `C`.
    /// So the super instance is represented as `SuperInstance[ClassType(A), ClassType(C)]`.
    SuperInstance(Box<ClassType>, Box<ClassType>),
    None,
}

impl Visit for Type {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a Self)) {
        self.visit(f);
    }
}

impl VisitMut for Type {
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut Self)) {
        self.visit_mut(f);
    }
}

impl Type {
    pub fn arc_clone(self: Arc<Self>) -> Self {
        Arc::unwrap_or_clone(self)
    }

    pub fn never() -> Self {
        Type::Never(NeverStyle::Never)
    }

    pub fn as_module(&self) -> Option<&Module> {
        match self {
            Type::Module(m) => Some(m),
            _ => None,
        }
    }

    pub fn callable(params: Vec<Param>, ret: Type) -> Self {
        Type::Callable(Box::new(Callable::list(ParamList::new(params), ret)))
    }

    pub fn callable_ellipsis(ret: Type) -> Self {
        Type::Callable(Box::new(Callable::ellipsis(ret)))
    }

    pub fn callable_param_spec(p: Type, ret: Type) -> Self {
        Type::Callable(Box::new(Callable::param_spec(p, ret)))
    }

    pub fn is_never(&self) -> bool {
        matches!(self, Type::Never(_))
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Type::Literal(_))
    }

    pub fn is_literal_string(&self) -> bool {
        match self {
            Type::LiteralString => true,
            Type::Literal(l) if l.is_string() => true,
            _ => false,
        }
    }

    pub fn is_unpack(&self) -> bool {
        matches!(self, Type::Unpack(_))
    }

    pub fn callable_concatenate(args: Box<[Type]>, param_spec: Type, ret: Type) -> Self {
        Type::Callable(Box::new(Callable::concatenate(args, param_spec, ret)))
    }

    pub fn type_form(inner: Type) -> Self {
        Type::Type(Box::new(inner))
    }

    pub fn tuple(elts: Vec<Type>) -> Self {
        Type::Tuple(Tuple::concrete(elts))
    }

    pub fn any_tuple() -> Self {
        Type::Tuple(Tuple::Unbounded(Box::new(Type::Any(AnyStyle::Implicit))))
    }

    pub fn is_any(&self) -> bool {
        matches!(self, Type::Any(_))
    }

    pub fn is_kind_type_var_tuple(&self) -> bool {
        match self {
            Type::TypeVarTuple(_) => true,
            Type::Quantified(q) if q.is_type_var_tuple() => true,
            _ => false,
        }
    }

    pub fn is_type_variable(&self) -> bool {
        match self {
            Type::Var(_)
            | Type::Quantified(_)
            | Type::TypeVarTuple(_)
            | Type::TypeVar(_)
            | Type::ParamSpec(_) => true,
            _ => false,
        }
    }

    pub fn has_type_variable(&self) -> bool {
        if self.is_type_variable() {
            return true;
        }
        let mut has_type_var = false;
        self.visit(&mut |t| {
            if t.is_type_variable() {
                has_type_var = true;
            }
        });
        has_type_var
    }

    pub fn is_kind_param_spec(&self) -> bool {
        match self {
            Type::Ellipsis
            | Type::ParamSpec(_)
            | Type::ParamSpecValue(_)
            | Type::Concatenate(_, _) => true,
            Type::Quantified(q) if q.is_param_spec() => true,
            _ => false,
        }
    }

    pub fn is_typeguard(&self) -> bool {
        match self {
            Type::Callable(box callable)
            | Type::Function(box Function {
                signature: callable,
                metadata: _,
            }) => callable.is_typeguard(),
            Type::Forall(box forall) => forall.body.is_typeguard(),
            Type::BoundMethod(method) => method.func.is_typeguard(),
            Type::Overload(overload) => overload.is_typeguard(),
            _ => false,
        }
    }

    pub fn is_typeis(&self) -> bool {
        match self {
            Type::Callable(box callable)
            | Type::Function(box Function {
                signature: callable,
                metadata: _,
            }) => callable.is_typeis(),
            Type::Forall(box forall) => forall.body.is_typeis(),
            Type::BoundMethod(method) => method.func.is_typeis(),
            Type::Overload(overload) => overload.is_typeis(),
            _ => false,
        }
    }

    /// Convert a bound method into a callable by stripping the first argument.
    /// TODO: Does not handle generics.
    pub fn to_unbound_callable(&self) -> Option<Type> {
        match self {
            Type::Callable(callable) => callable
                .drop_first_param()
                .map(|callable| Type::Callable(Box::new(callable))),
            Type::Function(func) => func.signature.drop_first_param().map(|callable| {
                Type::Function(Box::new(Function {
                    signature: callable,
                    metadata: func.metadata.clone(),
                }))
            }),
            Type::Overload(overload) => overload
                .signatures
                .try_mapped_ref(|x| match x {
                    OverloadType::Callable(c) => c.drop_first_param().ok_or(()),
                    _ => Err(()),
                })
                .ok()
                .map(|signatures| {
                    Type::Overload(Overload {
                        signatures: signatures.mapped(OverloadType::Callable),
                        metadata: overload.metadata.clone(),
                    })
                }),
            _ => None,
        }
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Type::None)
    }

    pub fn callee_kind(&self) -> Option<CalleeKind> {
        match self {
            Type::Callable(_) => Some(CalleeKind::Callable),
            Type::Function(func) => Some(CalleeKind::Function(func.metadata.kind.clone())),
            Type::ClassDef(c) => Some(CalleeKind::Class(c.kind())),
            Type::Forall(forall) => forall.body.clone().as_type().callee_kind(),
            Type::Overload(overload) => Some(CalleeKind::Function(overload.metadata.kind.clone())),
            _ => None,
        }
    }

    pub fn subst(self, mp: &SmallMap<Quantified, Type>) -> Self {
        self.transform(&mut |ty| {
            if let Type::Quantified(x) = &ty {
                if let Some(w) = mp.get(x) {
                    *ty = w.clone();
                }
            }
        })
    }

    pub fn subst_self_type_mut(&mut self, self_type: &Type) {
        self.transform_mut(&mut |x| {
            if x == &Type::SpecialForm(SpecialForm::SelfType) {
                *x = self_type.clone()
            }
        });
    }

    pub fn for_each_quantified(&self, f: &mut impl FnMut(Quantified)) {
        self.universe(&mut |x| {
            if let Type::Quantified(x) = x {
                f(*x);
            }
        })
    }

    pub fn collect_quantifieds(&self, acc: &mut SmallSet<Quantified>) {
        self.for_each_quantified(&mut |q| {
            acc.insert(q);
        });
    }

    #[expect(dead_code)] // Not used, but might be in future
    pub fn contains(&self, x: &Type) -> bool {
        fn f(ty: &Type, x: &Type, seen: &mut bool) {
            if *seen || ty == x {
                *seen = true;
            } else {
                ty.visit(&mut |ty| f(ty, x, seen));
            }
        }
        let mut seen = false;
        f(self, x, &mut seen);
        seen
    }

    fn check_func_metadata<T: Default>(&self, check: &dyn Fn(&FuncMetadata) -> T) -> T {
        match self {
            Type::Function(box func)
            | Type::Forall(box Forall {
                tparams: _,
                body: Forallable::Function(func),
            })
            | Type::BoundMethod(box BoundMethod {
                func: BoundMethodType::Function(func),
                ..
            }) => check(&func.metadata),
            Type::Overload(overload) => check(&overload.metadata),
            _ => T::default(),
        }
    }

    pub fn is_override(&self) -> bool {
        self.check_func_metadata(&|meta| meta.flags.is_override)
    }

    pub fn has_enum_member_decoration(&self) -> bool {
        self.check_func_metadata(&|meta| meta.flags.has_enum_member_decoration)
    }

    pub fn is_property_getter(&self) -> bool {
        self.check_func_metadata(&|meta| meta.flags.is_property_getter)
    }

    pub fn is_property_setter_with_getter(&self) -> Option<Type> {
        self.check_func_metadata(&|meta| meta.flags.is_property_setter_with_getter.clone())
    }

    pub fn is_overload(&self) -> bool {
        self.check_func_metadata(&|meta| meta.flags.is_overload)
    }

    pub fn has_final_decoration(&self) -> bool {
        self.check_func_metadata(&|meta| meta.flags.has_final_decoration)
    }

    pub fn transform_func_metadata(&mut self, mut f: impl FnMut(&mut FuncMetadata)) {
        match self {
            Type::Function(box func)
            | Type::Forall(box Forall {
                tparams: _,
                body: Forallable::Function(func),
            })
            | Type::BoundMethod(box BoundMethod {
                func: BoundMethodType::Function(func),
                ..
            }) => f(&mut func.metadata),
            Type::Overload(overload) => f(&mut overload.metadata),
            _ => {}
        }
    }

    pub fn promote_literals(self, stdlib: &Stdlib) -> Type {
        self.transform(&mut |ty| match &ty {
            Type::Literal(lit) => *ty = lit.general_class_type(stdlib).to_type(),
            _ => {}
        })
    }

    pub fn any_implicit() -> Self {
        Type::Any(AnyStyle::Implicit)
    }

    pub fn any_explicit() -> Self {
        Type::Any(AnyStyle::Explicit)
    }

    pub fn any_error() -> Self {
        Type::Any(AnyStyle::Error)
    }

    pub fn explicit_any(self) -> Self {
        self.transform(&mut |ty| {
            if let Type::Any(style) = ty {
                *style = AnyStyle::Explicit;
            }
        })
    }

    pub fn anon_callables(self) -> Self {
        self.transform(&mut |ty| {
            if let Type::Function(func) = ty {
                *ty = Type::Callable(Box::new(func.signature.clone()));
            }
        })
    }

    /// Used prior to display to ensure unique variables don't leak out non-deterministically.
    pub fn deterministic_printing(self) -> Self {
        self.transform(&mut |ty| {
            match ty {
                Type::Var(v) => {
                    // FIXME: Should mostly be forcing these before printing
                    v.zero();
                }
                _ => {}
            }
        })
    }

    pub fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a Type)) {
        match self {
            Type::Callable(box c) => c.visit(f),
            Type::Function(box x) => x.visit(f),
            Type::BoundMethod(box b) => b.visit(f),
            Type::Union(xs) | Type::Intersect(xs) => xs.iter().for_each(f),
            Type::Overload(overload) => overload.visit(f),
            Type::ClassType(x) => x.visit(f),
            Type::TypedDict(x) => x.visit(f),
            Type::Tuple(t) => t.visit(f),
            Type::Forall(forall) => forall.body.visit(f),
            Type::Concatenate(args, pspec) => {
                for a in args {
                    f(a)
                }
                f(pspec);
            }
            Type::ParamSpecValue(x) => x.visit(f),
            Type::Type(x)
            | Type::TypeGuard(x)
            | Type::TypeIs(x)
            | Type::Unpack(x)
            | Type::TypeAlias(TypeAlias { ty: x, .. }) => f(x),
            Type::SuperInstance(cls1, cls2) => {
                cls1.visit(f);
                cls2.visit(f)
            }
            Type::Literal(_)
            | Type::Never(_)
            | Type::LiteralString
            | Type::Any(_)
            | Type::ClassDef(_)
            | Type::Var(_)
            | Type::None
            | Type::Module(_)
            | Type::SpecialForm(_)
            | Type::Quantified(_)
            | Type::TypeVar(_)
            | Type::ParamSpec(_)
            | Type::Args(_)
            | Type::Kwargs(_)
            | Type::TypeVarTuple(_)
            | Type::Ellipsis => {}
        }
    }

    pub fn visit_mut(&mut self, mut f: &mut dyn FnMut(&mut Type)) {
        match self {
            Type::Callable(box c) => c.visit_mut(f),
            Type::Function(box x) => x.visit_mut(f),
            Type::BoundMethod(box b) => b.visit_mut(f),
            Type::Union(xs) | Type::Intersect(xs) => xs.iter_mut().for_each(f),
            Type::Overload(overload) => overload.visit_mut(f),
            Type::ClassType(x) => x.visit_mut(f),
            Type::TypedDict(x) => x.visit_mut(f),
            Type::Tuple(t) => t.visit_mut(f),
            Type::Forall(forall) => forall.body.visit_mut(f),
            Type::Concatenate(args, pspec) => {
                for a in args {
                    f(a)
                }
                f(pspec);
            }
            Type::ParamSpecValue(x) => x.visit_mut(f),
            Type::Type(x)
            | Type::TypeGuard(x)
            | Type::TypeIs(x)
            | Type::Unpack(x)
            | Type::TypeAlias(TypeAlias { ty: x, .. }) => f(x),
            Type::SuperInstance(cls1, cls2) => {
                cls1.visit_mut(&mut f);
                cls2.visit_mut(f);
            }
            Type::Literal(_)
            | Type::Never(_)
            | Type::LiteralString
            | Type::Any(_)
            | Type::ClassDef(_)
            | Type::None
            | Type::Var(_)
            | Type::Module(_)
            | Type::SpecialForm(_)
            | Type::Quantified(_)
            | Type::TypeVar(_)
            | Type::ParamSpec(_)
            | Type::Args(_)
            | Type::Kwargs(_)
            | Type::TypeVarTuple(_)
            | Type::Ellipsis => {}
        }
    }

    /// Visit every type, with the guarantee you will have seen included types before the parent.
    pub fn universe<'a>(&'a self, f: &mut dyn FnMut(&'a Type)) {
        fn g<'a>(ty: &'a Type, f: &mut dyn FnMut(&'a Type)) {
            ty.visit(&mut |ty| g(ty, f));
            f(ty);
        }
        g(self, f);
    }

    /// Visit every type, with the guarantee you will have seen included types before the parent.
    pub fn transform_mut(&mut self, f: &mut dyn FnMut(&mut Type)) {
        fn g(ty: &mut Type, f: &mut dyn FnMut(&mut Type)) {
            ty.visit_mut(&mut |ty| g(ty, f));
            f(ty);
        }
        g(self, f);
    }

    pub fn transform(mut self, f: &mut dyn FnMut(&mut Type)) -> Self {
        self.transform_mut(f);
        self
    }

    pub fn as_quantified(&self) -> Option<Quantified> {
        match self {
            Type::Quantified(q) => Some(*q),
            _ => None,
        }
    }

    // The result of calling bool() on a value of this type if we can get a definitive answer, None otherwise.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Type::Literal(Lit::Bool(x)) => Some(*x),
            Type::Literal(Lit::Int(x)) => Some(*x != 0),
            Type::Literal(Lit::Bytes(x)) => Some(!x.is_empty()),
            Type::Literal(Lit::String(x)) => Some(!x.is_empty()),
            Type::None => Some(false),
            Type::Tuple(Tuple::Concrete(elements)) => Some(!elements.is_empty()),
            Type::Union(options) => {
                let mut answer = None;
                for option in options {
                    let option_bool = option.as_bool();
                    option_bool?;
                    if answer.is_none() {
                        answer = option_bool;
                    } else if answer != option_bool {
                        return None;
                    }
                }
                answer
            }
            _ => None,
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Type::Any(AnyStyle::Error))
    }
}

#[cfg(test)]
mod tests {
    use crate::types::literal::Lit;
    use crate::types::types::Type;

    #[test]
    fn test_as_bool() {
        let true_lit = Type::Literal(Lit::Bool(true));
        let false_lit = Type::Literal(Lit::Bool(false));
        let none = Type::None;
        let s = Type::LiteralString;

        assert_eq!(true_lit.as_bool(), Some(true));
        assert_eq!(false_lit.as_bool(), Some(false));
        assert_eq!(none.as_bool(), Some(false));
        assert_eq!(s.as_bool(), None);
    }

    #[test]
    fn test_as_bool_union() {
        let s = Type::LiteralString;
        let false_lit = Type::Literal(Lit::Bool(false));
        let none = Type::None;

        let str_opt = Type::Union(vec![s, none.clone()]);
        let false_opt = Type::Union(vec![false_lit, none]);

        assert_eq!(str_opt.as_bool(), None);
        assert_eq!(false_opt.as_bool(), Some(false));
    }
}
