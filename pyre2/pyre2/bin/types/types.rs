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
use ruff_python_ast::name::Name;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use static_assertions::assert_eq_size;

use crate::types::callable::Arg;
use crate::types::callable::Callable;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::literal::Lit;
use crate::types::module::Module;
use crate::types::param_spec::ParamSpec;
use crate::types::special_form::SpecialForm;
use crate::types::stdlib::Stdlib;
use crate::types::tuple::Tuple;
use crate::types::type_var::Restriction;
use crate::types::type_var::TypeVar;
use crate::types::type_var::Variance;
use crate::types::type_var_tuple::TypeVarTuple;
use crate::util::display::commas_iter;
use crate::util::uniques::Unique;
use crate::util::uniques::UniqueFactory;

/// An introduced synthetic variable to range over as yet unknown types.
#[derive(Debug, Copy, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Quantified {
    /// Unique identifier
    unique: Unique,
    kind: QuantifiedKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QuantifiedKind {
    TypeVar,
    ParamSpec,
    TypeVarTuple,
}

impl Display for Quantified {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?_")
    }
}

impl Quantified {
    pub fn new(uniques: &UniqueFactory, kind: QuantifiedKind) -> Self {
        Quantified {
            unique: uniques.fresh(),
            kind,
        }
    }

    pub fn type_var(uniques: &UniqueFactory) -> Self {
        Quantified::new(uniques, QuantifiedKind::TypeVar)
    }

    pub fn param_spec(uniques: &UniqueFactory) -> Self {
        Quantified::new(uniques, QuantifiedKind::ParamSpec)
    }

    pub fn type_var_tuple(uniques: &UniqueFactory) -> Self {
        Quantified::new(uniques, QuantifiedKind::TypeVarTuple)
    }

    pub fn to_type(self) -> Type {
        Type::Quantified(self)
    }

    pub fn as_value(&self, stdlib: &Stdlib) -> ClassType {
        match self.kind {
            QuantifiedKind::TypeVar => stdlib.type_var(),
            QuantifiedKind::ParamSpec => stdlib.param_spec(),
            QuantifiedKind::TypeVarTuple => stdlib.type_var_tuple(),
        }
    }

    pub fn is_param_spec(&self) -> bool {
        matches!(self.kind, QuantifiedKind::ParamSpec)
    }

    pub fn id(&self) -> Unique {
        self.unique
    }
}

/// Bundles together type param info for passing around while building TParams.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TParamInfo {
    pub name: Name,
    pub quantified: Quantified,
    pub restriction: Restriction,
    pub default: Option<Type>,
    /// The variance if known, or None for infer_variance=True or a scoped type parameter
    pub variance: Option<Variance>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
                && tparams.last().map_or(false, |p| p.default.is_some())
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

// Python's legacy (pre-PEP 695) type variable syntax is not syntactic at all, it requires
// name resolution of global variables plus multiple sets of rules for when a global that
// is a type variable placeholder is allowed to be used as a type parameter.
//
// This type represents the result of such a lookup: given a name appearing in a function or
// a class, we either determine that the name is *not* a type variable and return the type
// for the name, or we determine that it is one and create a `Quantified` that
// represents that variable as a type parameter.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LegacyTypeParameterLookup {
    Parameter(TParamInfo),
    NotParameter(Type),
}

impl Display for LegacyTypeParameterLookup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(p) => write!(f, "{}", p.name),
            Self::NotParameter(ty) => write!(f, "{ty}"),
        }
    }
}

impl LegacyTypeParameterLookup {
    pub fn parameter(&self) -> Option<&TParamInfo> {
        match self {
            Self::Parameter(p) => Some(p),
            Self::NotParameter(_) => None,
        }
    }

    pub fn not_parameter_mut(&mut self) -> Option<&mut Type> {
        match self {
            Self::Parameter(_) => None,
            Self::NotParameter(ty) => Some(ty),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
pub enum NeverStyle {
    Never,
    NoReturn,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeAliasStyle {
    /// A type alias declared with the `type` keyword
    Scoped,
    /// A type alias declared with a `: TypeAlias` annotation
    LegacyExplicit,
    /// An unannotated assignment that may be either an implicit type alias or an untyped value
    LegacyImplicit,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
}

// We have a lot of types, want to make sure they stay a reasonable size
assert_eq_size!(Type, [usize; 4]);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Literal(Lit),
    LiteralString,
    Callable(Box<Callable>),
    /// A method of a class. The first `Box<Type>` is the self/cls argument,
    /// and the second is the function.
    BoundMethod(Box<Type>, Box<Type>),
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
    Tuple(Tuple),
    Module(Module),
    Forall(TParams, Box<Type>),
    Var(Var),
    Quantified(Quantified),
    TypeGuard(Box<Type>),
    TypeIs(Box<Type>),
    Unpack(Box<Type>),
    TypeVar(TypeVar),
    ParamSpec(ParamSpec),
    TypeVarTuple(TypeVarTuple),
    SpecialForm(SpecialForm),
    /// Used to represent `P.args`. The spec describes it as an annotation,
    /// but it's easier to think of it as a type that can't occur in nested positions.
    Args(Unique),
    Kwargs(Unique),
    /// Used to represent a type that has a value representation, e.g. a class
    Type(Box<Type>),
    Ellipsis,
    Any(AnyStyle),
    Never(NeverStyle),
    TypeAlias(TypeAlias),
    None,
}

#[allow(dead_code)] // Some of these utilities will come and go
impl Type {
    pub fn arc_clone(self: Arc<Self>) -> Self {
        Arc::unwrap_or_clone(self)
    }

    pub fn as_union(&self) -> &[Type] {
        match self {
            Type::Union(types) => types,
            _ => std::slice::from_ref(self),
        }
    }

    pub fn as_intersect(&self) -> &[Type] {
        match self {
            Type::Intersect(types) => types,
            _ => std::slice::from_ref(self),
        }
    }

    pub fn never() -> Self {
        Type::Never(NeverStyle::Never)
    }

    pub fn is_never(&self) -> bool {
        match self {
            Type::Never(_) => true,
            _ => false,
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Type::Var(_))
    }

    pub fn is_callable(&self) -> bool {
        matches!(self, Type::Callable(_))
    }

    pub fn as_var(&self) -> Option<Var> {
        match self {
            Type::Var(v) => Some(*v),
            _ => None,
        }
    }

    pub fn as_module(&self) -> Option<&Module> {
        match self {
            Type::Module(m) => Some(m),
            _ => None,
        }
    }

    pub fn callable(args: Vec<Arg>, ret: Type) -> Self {
        Type::Callable(Box::new(Callable::list(args, ret)))
    }

    pub fn callable_ellipsis(ret: Type) -> Self {
        Type::Callable(Box::new(Callable::ellipsis(ret)))
    }

    pub fn callable_param_spec(p: Type, ret: Type) -> Self {
        Type::Callable(Box::new(Callable::param_spec(p, ret)))
    }

    pub fn forall(self, tparams: TParams) -> Self {
        if tparams.is_empty() {
            self
        } else {
            Type::Forall(tparams, Box::new(self))
        }
    }

    pub fn type_form(inner: Type) -> Self {
        Type::Type(Box::new(inner))
    }

    pub fn tuple(elts: Vec<Type>) -> Self {
        Type::Tuple(Tuple::concrete(elts))
    }

    pub fn is_any(&self) -> bool {
        matches!(self, Type::Any(_))
    }

    pub fn is_forall(&self) -> bool {
        matches!(self, Type::Forall(_, _))
    }

    pub fn is_tvar_declaration(&self, name: &Name) -> bool {
        match self {
            Type::TypeVar(x) => x.qname().id() == name,
            Type::TypeVarTuple(x) => x.qname().id() == name,
            Type::ParamSpec(x) => x.qname().id() == name,
            _ => false,
        }
    }

    pub fn is_none(&self) -> bool {
        matches!(self, Type::None)
    }

    pub fn subst(self, mp: &SmallMap<Quantified, Type>) -> Self {
        self.transform(|ty| {
            if let Type::Quantified(x) = &ty {
                if let Some(w) = mp.get(x) {
                    *ty = w.clone();
                }
            }
        })
    }

    pub fn subst_self_type_mut(&mut self, self_type: &Type) {
        self.transform_mut(|x| {
            if x == &Type::SpecialForm(SpecialForm::SelfType) {
                *x = self_type.clone()
            }
        });
    }

    pub fn instantiate_fresh(
        self,
        gargs: &[Quantified],
        uniques: &UniqueFactory,
    ) -> (Vec<Var>, Self) {
        let mp: SmallMap<Quantified, Type> = gargs
            .iter()
            .map(|x| (*x, Var::new(uniques).to_type()))
            .collect();
        let res = self.subst(&mp);
        (mp.into_values().map(|x| x.as_var().unwrap()).collect(), res)
    }

    pub fn for_each_quantified(&self, f: &mut impl FnMut(Quantified)) {
        self.universe(|x| {
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

    pub fn contains(&self, x: &Type) -> bool {
        fn f(ty: &Type, x: &Type, seen: &mut bool) {
            if *seen || ty == x {
                *seen = true;
            } else {
                ty.visit(|ty| f(ty, x, seen));
            }
        }
        let mut seen = false;
        f(self, x, &mut seen);
        seen
    }

    pub fn promote_literals(self, stdlib: &Stdlib) -> Type {
        self.transform(|ty| match &ty {
            Type::Literal(lit) => *ty = lit.general_class_type(stdlib).to_type(),
            Type::LiteralString => *ty = stdlib.str().to_type(),
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
        self.transform(|ty| {
            if let Type::Any(style) = ty {
                *style = AnyStyle::Explicit;
            }
        })
    }

    /// Used prior to display to ensure unique variables don't leak out non-deterministically.
    pub fn deterministic_printing(self) -> Self {
        self.transform(|ty| {
            match ty {
                Type::Var(v) => {
                    // FIXME: Should mostly be forcing these before printing
                    v.zero();
                }
                Type::Args(id) | Type::Kwargs(id) => {
                    *id = Unique::zero();
                }
                _ => {}
            }
        })
    }

    pub fn visit<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        match self {
            Type::Callable(c) => c.visit(f),
            Type::BoundMethod(obj, func) => {
                f(obj);
                f(func);
            }
            Type::Union(xs) | Type::Intersect(xs) => xs.iter().for_each(f),
            Type::ClassType(x) => x.visit(f),
            Type::Tuple(t) => t.visit(f),
            Type::Forall(_, x) => f(x),
            Type::Type(x)
            | Type::TypeGuard(x)
            | Type::TypeIs(x)
            | Type::Unpack(x)
            | Type::TypeAlias(TypeAlias { ty: x, .. }) => f(x),
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

    pub fn visit_mut<'a>(&'a mut self, mut f: impl FnMut(&'a mut Type)) {
        match self {
            Type::Callable(c) => c.visit_mut(f),
            Type::BoundMethod(obj, func) => {
                f(obj);
                f(func);
            }
            Type::Union(xs) | Type::Intersect(xs) => xs.iter_mut().for_each(f),
            Type::ClassType(x) => x.visit_mut(f),
            Type::Tuple(t) => t.visit_mut(f),
            Type::Forall(_, x) => f(x),
            Type::Type(x)
            | Type::TypeGuard(x)
            | Type::TypeIs(x)
            | Type::Unpack(x)
            | Type::TypeAlias(TypeAlias { ty: x, .. }) => f(x),
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
    pub fn universe<'a>(&'a self, mut f: impl FnMut(&'a Type)) {
        fn g<'a>(ty: &'a Type, f: &mut impl FnMut(&'a Type)) {
            ty.visit(|ty| g(ty, f));
            f(ty);
        }
        g(self, &mut f);
    }

    /// Visit every type, with the guarantee you will have seen included types before the parent.
    pub fn transform_mut(&mut self, mut f: impl FnMut(&mut Type)) {
        fn g(ty: &mut Type, f: &mut impl FnMut(&mut Type)) {
            ty.visit_mut(|ty| g(ty, f));
            f(ty);
        }
        g(self, &mut f);
    }

    pub fn transform(mut self, mut f: impl FnMut(&mut Type)) -> Self {
        self.transform_mut(&mut f);
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
