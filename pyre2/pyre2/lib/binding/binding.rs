/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use dupe::Dupe;
use itertools::Either;
use pyrefly_derive::TypeEq;
use pyrefly_derive::VisitMut;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprSubscript;
use ruff_python_ast::ExprYield;
use ruff_python_ast::ExprYieldFrom;
use ruff_python_ast::Identifier;
use ruff_python_ast::StmtAugAssign;
use ruff_python_ast::StmtClassDef;
use ruff_python_ast::StmtFunctionDef;
use ruff_python_ast::TypeParams;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::alt::class::class_field::ClassField;
use crate::alt::class::class_metadata::BaseClass;
use crate::alt::solve::TypeFormContext;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::ClassSynthesizedFields;
use crate::alt::types::decorated_function::DecoratedFunction;
use crate::alt::types::legacy_lookup::LegacyTypeParameterLookup;
use crate::alt::types::yields::YieldFromResult;
use crate::alt::types::yields::YieldResult;
use crate::assert_bytes;
use crate::assert_words;
use crate::binding::bindings::Bindings;
use crate::binding::narrow::NarrowOp;
use crate::dunder;
use crate::graph::index::Idx;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::annotation::Annotation;
use crate::types::class::Class;
use crate::types::class::ClassFieldProperties;
use crate::types::class::ClassIndex;
use crate::types::equality::TypeEq;
use crate::types::quantified::Quantified;
use crate::types::types::Type;
use crate::types::types::Var;
use crate::util::display::commas_iter;
use crate::util::display::DisplayWith;
use crate::util::display::DisplayWithCtx;
use crate::util::visit::VisitMut;

assert_words!(Key, 5);
assert_words!(KeyExpect, 1);
assert_words!(KeyExport, 3);
assert_words!(KeyClass, 1);
assert_words!(KeyClassField, 4);
assert_bytes!(KeyClassSynthesizedFields, 4);
assert_bytes!(KeyAnnotation, 12);
assert_bytes!(KeyClassMetadata, 4);
assert_words!(KeyLegacyTypeParam, 1);
assert_words!(KeyYield, 1);
assert_words!(KeyYieldFrom, 1);
assert_words!(KeyFunction, 1);

assert_words!(Binding, 9);
assert_words!(BindingExpect, 8);
assert_words!(BindingAnnotation, 13);
assert_words!(BindingClass, 22);
assert_words!(BindingClassMetadata, 8);
assert_words!(BindingClassField, 23);
assert_bytes!(BindingClassSynthesizedFields, 4);
assert_bytes!(BindingLegacyTypeParam, 4);
assert_words!(BindingYield, 3);
assert_words!(BindingYieldFrom, 3);
assert_words!(BindingFunction, 21);

pub trait Keyed: Hash + Eq + Clone + DisplayWith<ModuleInfo> + Debug + Ranged + 'static {
    const EXPORTED: bool = false;
    type Value: Debug + DisplayWith<Bindings>;
    type Answer: Clone + Debug + Display + TypeEq + VisitMut<Type>;
}

impl Keyed for Key {
    type Value = Binding;
    type Answer = Type;
}
impl Keyed for KeyExpect {
    type Value = BindingExpect;
    type Answer = EmptyAnswer;
}
impl Keyed for KeyClass {
    type Value = BindingClass;
    type Answer = NoneIfRecursive<Class>;
}
impl Keyed for KeyClassField {
    const EXPORTED: bool = true;
    type Value = BindingClassField;
    type Answer = ClassField;
}
impl Keyed for KeyClassSynthesizedFields {
    const EXPORTED: bool = true;
    type Value = BindingClassSynthesizedFields;
    type Answer = ClassSynthesizedFields;
}
impl Keyed for KeyExport {
    const EXPORTED: bool = true;
    type Value = Binding;
    type Answer = Type;
}
impl Keyed for KeyFunction {
    type Value = BindingFunction;
    type Answer = DecoratedFunction;
}
impl Keyed for KeyAnnotation {
    type Value = BindingAnnotation;
    type Answer = AnnotationWithTarget;
}
impl Keyed for KeyClassMetadata {
    const EXPORTED: bool = true;
    type Value = BindingClassMetadata;
    type Answer = ClassMetadata;
}
impl Keyed for KeyLegacyTypeParam {
    type Value = BindingLegacyTypeParam;
    type Answer = LegacyTypeParameterLookup;
}
impl Keyed for KeyYield {
    type Value = BindingYield;
    type Answer = YieldResult;
}
impl Keyed for KeyYieldFrom {
    type Value = BindingYieldFrom;
    type Answer = YieldFromResult;
}

/// Keys that refer to a `Type`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Key {
    /// I am an `import` at this location with this name.
    /// Used for `import foo.x` (the `foo` might not be literally present with `.` modules),
    /// and `from foo import *` (the names are injected from the exports)
    Import(Name, TextRange),
    /// I am defined in this module at this location.
    Definition(ShortIdentifier),
    /// The type at a specific return point.
    ReturnExplicit(TextRange),
    /// The implicit return type of a function, either Type::None or Type::Never.
    ReturnImplicit(ShortIdentifier),
    /// The actual type of the return for a function.
    ReturnType(ShortIdentifier),
    /// I am a use in this module at this location.
    Usage(ShortIdentifier),
    /// I am an expression that does not have a simple name but needs its type inferred.
    /// For example, an attribute access.
    Anon(TextRange),
    /// I am an expression that appears in a statement. The range for this key is the range of the expr itself, which is different than the range of the stmt expr.
    StmtExpr(TextRange),
    /// I am an expression that appears in a `with` context.
    ContextExpr(TextRange),
    /// I am the result of joining several branches.
    Phi(Name, TextRange),
    /// I am the result of narrowing a type. The two ranges are the range at which the operation is
    /// defined and the one at which it is used. For example, in:
    ///   if x is None:
    ///       pass
    ///   else:
    ///       pass
    /// The `x is None` operation is defined once in the `if` test but generates two key/binding
    /// pairs, when it is used to narrow `x` in the `if` and the `else`, respectively.
    Narrow(Name, TextRange, TextRange),
    /// The binding definition site, anywhere it occurs
    Anywhere(Name, TextRange),
    /// Result of a super() call
    SuperInstance(TextRange),
}

impl Ranged for Key {
    fn range(&self) -> TextRange {
        match self {
            Self::Import(_, r) => *r,
            Self::Definition(x) => x.range(),
            Self::ReturnExplicit(r) => *r,
            Self::ReturnImplicit(x) => x.range(),
            Self::ReturnType(x) => x.range(),
            Self::Usage(x) => x.range(),
            Self::Anon(r) => *r,
            Self::StmtExpr(r) => *r,
            Self::ContextExpr(r) => *r,
            Self::Phi(_, r) => *r,
            Self::Narrow(_, r, _) => *r,
            Self::Anywhere(_, r) => *r,
            Self::SuperInstance(r) => *r,
        }
    }
}

impl DisplayWith<ModuleInfo> for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        match self {
            Self::Import(n, r) => write!(f, "import {n} {r:?}"),
            Self::Definition(x) => write!(f, "{} {:?}", ctx.display(x), x.range()),
            Self::Usage(x) => write!(f, "use {} {:?}", ctx.display(x), x.range()),
            Self::Anon(r) => write!(f, "anon {r:?}"),
            Self::StmtExpr(r) => write!(f, "stmt expr {r:?}"),
            Self::ContextExpr(r) => write!(f, "context expr {r:?}"),
            Self::Phi(n, r) => write!(f, "phi {n} {r:?}"),
            Self::Narrow(n, r1, r2) => write!(f, "narrow {n} {r1:?} {r2:?}"),
            Self::Anywhere(n, r) => write!(f, "anywhere {n} {r:?}"),
            Self::ReturnType(x) => write!(f, "return {} {:?}", ctx.display(x), x.range()),
            Self::ReturnExplicit(r) => write!(f, "return {r:?}"),
            Self::ReturnImplicit(x) => {
                write!(f, "return implicit {} {:?}", ctx.display(x), x.range())
            }
            Self::SuperInstance(r) => write!(f, "super {r:?}"),
        }
    }
}

impl DisplayWith<Bindings> for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        write!(f, "{}", ctx.module_info().display(self))
    }
}

/// An expectation to be checked. For example, that a sequence is of an expected length.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct KeyExpect(pub TextRange);

impl Ranged for KeyExpect {
    fn range(&self) -> TextRange {
        self.0
    }
}

impl DisplayWith<ModuleInfo> for KeyExpect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _: &ModuleInfo) -> fmt::Result {
        write!(f, "expect {:?}", self.0)
    }
}

#[derive(Clone, Debug)]
pub enum BindingExpect {
    /// The expected number of values in an unpacked iterable expression.
    UnpackedLength(Box<Binding>, TextRange, SizeExpectation),
    /// An exception and its cause from a raise statement.
    CheckRaisedException(RaisedException),
    /// An expectation that the types are identical, with an associated name for error messages.
    Eq(Idx<KeyAnnotation>, Idx<KeyAnnotation>, Name),
    /// Verify that an attribute assignment or annotation is legal, given an expr for the
    /// assignment (use this when an expr is available, to get bidirectional typing).
    CheckAssignExprToAttribute(Box<(ExprAttribute, Expr)>),
    /// Verify that an attribute assignment or annotation is legal, given a type for the
    /// assignment (use this when no expr is available).
    CheckAssignTypeToAttribute(Box<(ExprAttribute, Binding)>),
    /// `del` statement
    Delete(Box<Expr>),
}

impl DisplayWith<Bindings> for BindingExpect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        let m = ctx.module_info();
        match self {
            Self::Delete(box x) => {
                write!(f, "del {}", m.display(x))
            }
            Self::UnpackedLength(x, range, expect) => {
                let expectation = match expect {
                    SizeExpectation::Eq(n) => n.to_string(),
                    SizeExpectation::Ge(n) => format!(">={n}"),
                };
                write!(
                    f,
                    "expect length {} for {} {:?}",
                    expectation,
                    x.display_with(ctx),
                    range
                )
            }
            Self::CheckRaisedException(RaisedException::WithoutCause(exc)) => {
                write!(f, "raise {}", m.display(exc))
            }
            Self::CheckRaisedException(RaisedException::WithCause(box (exc, cause))) => {
                write!(f, "raise {} from {}", m.display(exc), m.display(cause))
            }
            Self::Eq(k1, k2, name) => write!(
                f,
                "{} == {} on {}",
                ctx.display(*k1),
                ctx.display(*k2),
                name
            ),
            Self::CheckAssignExprToAttribute(box (attr, value)) => {
                write!(
                    f,
                    "check assign expr to attr {}.{} {}",
                    m.display(attr.value.as_ref()),
                    attr.attr,
                    m.display(value),
                )
            }
            Self::CheckAssignTypeToAttribute(box (attr, binding)) => {
                write!(
                    f,
                    "check assign type to attr {}.{} ({})",
                    m.display(attr.value.as_ref()),
                    attr.attr,
                    binding.display_with(ctx)
                )
            }
        }
    }
}

#[derive(Debug, Clone, TypeEq, VisitMut, PartialEq, Eq)]
pub struct EmptyAnswer;

impl Display for EmptyAnswer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "()")
    }
}

#[derive(Debug, Clone, TypeEq, VisitMut, PartialEq, Eq)]
pub struct NoneIfRecursive<T>(pub Option<T>);

impl<T> Display for NoneIfRecursive<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.0 {
            Some(x) => x.fmt(f),
            None => write!(f, "recursive"),
        }
    }
}

/// The binding definition site, at the end of the module (used for export).
/// If it has an annotation, only the annotation will be returned.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct KeyExport(pub Name);

impl Ranged for KeyExport {
    fn range(&self) -> TextRange {
        TextRange::default()
    }
}

impl DisplayWith<ModuleInfo> for KeyExport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _: &ModuleInfo) -> fmt::Result {
        write!(f, "export {}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct KeyFunction(pub ShortIdentifier);

impl Ranged for KeyFunction {
    fn range(&self) -> TextRange {
        self.0.range()
    }
}

impl DisplayWith<ModuleInfo> for KeyFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        write!(f, "{} {:?}", ctx.display(&self.0), self.0.range())
    }
}

/// A reference to a class.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct KeyClass(pub ShortIdentifier);

impl Ranged for KeyClass {
    fn range(&self) -> TextRange {
        self.0.range()
    }
}

impl DisplayWith<ModuleInfo> for KeyClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        write!(f, "class {} {:?}", ctx.display(&self.0), self.0.range())
    }
}

/// A reference to a field in a class.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct KeyClassField(pub ClassIndex, pub Name);

impl Ranged for KeyClassField {
    fn range(&self) -> TextRange {
        TextRange::default()
    }
}

impl DisplayWith<ModuleInfo> for KeyClassField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _ctx: &ModuleInfo) -> fmt::Result {
        write!(f, "field {} . {}", self.0, self.1)
    }
}

/// Keys that refer to fields synthesized by a class, such as a dataclass's `__init__` method. This
/// has to be its own key/binding type because of the dependencies between the various pieces of
/// information about a class: ClassDef -> ClassMetadata -> ClassField -> ClassSynthesizedFields.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct KeyClassSynthesizedFields(pub ClassIndex);

impl Ranged for KeyClassSynthesizedFields {
    fn range(&self) -> TextRange {
        TextRange::default()
    }
}

impl DisplayWith<ModuleInfo> for KeyClassSynthesizedFields {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _ctx: &ModuleInfo) -> fmt::Result {
        write!(f, "synthesized fields of {}", self.0)
    }
}

/// Keys that refer to an `Annotation`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum KeyAnnotation {
    /// I am the annotation for this instance of a name.
    Annotation(ShortIdentifier),
    /// The return type annotation for a function.
    ReturnAnnotation(ShortIdentifier),
    /// I am the annotation for the attribute at this range.
    AttrAnnotation(TextRange),
}

impl Ranged for KeyAnnotation {
    fn range(&self) -> TextRange {
        match self {
            Self::Annotation(x) => x.range(),
            Self::ReturnAnnotation(x) => x.range(),
            Self::AttrAnnotation(r) => *r,
        }
    }
}

impl DisplayWith<ModuleInfo> for KeyAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        match self {
            Self::Annotation(x) => write!(f, "annot {} {:?}", ctx.display(x), x.range()),
            Self::ReturnAnnotation(x) => write!(f, "return {} {:?}", ctx.display(x), x.range()),
            Self::AttrAnnotation(r) => write!(f, "attr {:?}", r),
        }
    }
}

/// Keys that refer to a class's `Mro` (which tracks its ancestors, in method
/// resolution order).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct KeyClassMetadata(pub ClassIndex);

impl Ranged for KeyClassMetadata {
    fn range(&self) -> TextRange {
        TextRange::default()
    }
}

impl DisplayWith<ModuleInfo> for KeyClassMetadata {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _ctx: &ModuleInfo) -> fmt::Result {
        write!(f, "mro {}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct KeyLegacyTypeParam(pub ShortIdentifier);

impl Ranged for KeyLegacyTypeParam {
    fn range(&self) -> TextRange {
        self.0.range()
    }
}

impl DisplayWith<ModuleInfo> for KeyLegacyTypeParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        write!(
            f,
            "legacy_type_param {} {:?}",
            ctx.display(&self.0),
            self.0.range()
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct KeyYield(pub TextRange);

impl Ranged for KeyYield {
    fn range(&self) -> TextRange {
        self.0
    }
}

impl DisplayWith<ModuleInfo> for KeyYield {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        write!(f, "{} {:?}", ctx.display(&self.0), self.0.range())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct KeyYieldFrom(pub TextRange);

impl Ranged for KeyYieldFrom {
    fn range(&self) -> TextRange {
        self.0
    }
}

impl DisplayWith<ModuleInfo> for KeyYieldFrom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        write!(f, "{} {:?}", ctx.display(&self.0), self.0.range())
    }
}

#[derive(Clone, Copy, Dupe, Debug)]
pub enum UnpackedPosition {
    /// Zero-based index
    Index(usize),
    /// A negative index, counting from the back
    ReverseIndex(usize),
    /// Slice represented as an index from the front to an index from the back.
    /// Note that even though the second index is conceptually negative, we can
    /// represent it as a usize because it is always negative.
    Slice(usize, usize),
}

#[derive(Clone, Debug)]
pub enum SizeExpectation {
    Eq(usize),
    Ge(usize),
}

#[derive(Clone, Debug)]
pub enum RaisedException {
    WithoutCause(Expr),
    WithCause(Box<(Expr, Expr)>),
}

#[derive(Clone, Dupe, Copy, Debug, Eq, PartialEq)]
pub enum IsAsync {
    Sync,
    Async,
}

impl IsAsync {
    pub fn new(is_async: bool) -> Self {
        if is_async { Self::Async } else { Self::Sync }
    }

    pub fn is_async(self) -> bool {
        matches!(self, Self::Async)
    }

    pub fn context_exit_dunder(self) -> Name {
        match self {
            Self::Sync => dunder::EXIT,
            Self::Async => dunder::AEXIT,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FunctionSource {
    Stub,
    Impl,
}

#[derive(Clone, Debug)]
pub struct BindingFunction {
    /// A function definition, but with the return/body stripped out.
    pub def: StmtFunctionDef,
    pub source: FunctionSource,
    pub class_key: Option<Idx<KeyClass>>,
    pub decorators: Box<[Idx<Key>]>,
    pub legacy_tparams: Box<[Idx<KeyLegacyTypeParam>]>,
    pub successor: Option<Idx<KeyFunction>>,
}

impl DisplayWith<Bindings> for BindingFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _ctx: &Bindings) -> fmt::Result {
        write!(f, "def {}", self.def.name.id)
    }
}

#[derive(Clone, Debug)]
pub struct ClassBinding {
    pub def: StmtClassDef,
    pub index: ClassIndex,
    pub fields: SmallMap<Name, ClassFieldProperties>,
    pub bases: Box<[Expr]>,
    pub legacy_tparams: Box<[Idx<KeyLegacyTypeParam>]>,
}

#[derive(Clone, Debug)]
pub struct ReturnExplicit {
    pub annot: Option<Idx<KeyAnnotation>>,
    pub expr: Option<Box<Expr>>,
    pub is_generator: bool,
    pub is_async: bool,
}

#[derive(Clone, Debug)]
pub struct ReturnType {
    /// The annotation for the return type.
    pub annot: Option<(TextRange, Idx<KeyAnnotation>)>,
    /// The returns from the function.
    pub returns: Box<[Idx<Key>]>,
    pub implicit_return: Idx<Key>,
    /// The explicit yields from the function. Left for `yield`, Right for `yield from`.
    /// If this is non-empty, the function is a generator.
    pub yields: Box<[Either<Idx<KeyYield>, Idx<KeyYieldFrom>>]>,
    pub is_async: bool,
}

#[derive(Clone, Dupe, Copy, Debug)]
pub enum LastStmt {
    /// The last statement is a expression
    Expr,
    /// The last statement is a `with`, with the following context,
    /// which might (if exit is true) catch an exception
    With(IsAsync),
}

#[derive(Clone, Debug)]
pub struct ReturnImplicit {
    /// Terminal statements in the function control flow, used to determine whether the
    /// function has an implicit `None` return.
    /// When `None`, the function always has an implicit `None` return. When `Some(xs)`,
    /// the function has an implicit `None` return if there exists a non-`Never` in this
    /// list.
    pub last_exprs: Option<Box<[(LastStmt, Idx<Key>)]>>,
    /// Ignore the implicit return type for stub functions (returning `...`). This is
    /// unsafe, but is convenient and matches Pyright's behavior.
    pub function_source: FunctionSource,
}

#[derive(Clone, Debug)]
pub enum SuperStyle {
    /// A `super(cls, obj)` call. The keys are the arguments.
    ExplicitArgs(Idx<Key>, Idx<Key>),
    /// A no-argument `super()` call. The key is the `Self` type of the class we are in.
    /// The name is the method we are in.
    ImplicitArgs(Idx<KeyClass>, Identifier),
    /// `super(Any, Any)`. Useful when we encounter an error.
    Any,
}

#[derive(Clone, Debug, Copy, Dupe, PartialEq, Eq)]
pub enum AnnotationStyle {
    /// Annotated assignment, x: MyType = my_value
    Direct,
    /// Forwarded annotation, x: MyType; x = my_value
    Forwarded,
}

#[derive(Clone, Debug)]
pub enum Binding {
    /// An expression, optionally with a Key saying what the type must be.
    /// The Key must be a type of types, e.g. `Type::Type`.
    Expr(Option<Idx<KeyAnnotation>>, Expr),
    /// TypeVar, ParamSpec, or TypeVarTuple
    TypeVar(Option<Idx<KeyAnnotation>>, Identifier, Box<ExprCall>),
    ParamSpec(Option<Idx<KeyAnnotation>>, Identifier, Box<ExprCall>),
    TypeVarTuple(Option<Idx<KeyAnnotation>>, Identifier, Box<ExprCall>),
    /// An expression returned from a function.
    ReturnExplicit(ReturnExplicit),
    /// The implicit return from a function.
    ReturnImplicit(ReturnImplicit),
    /// The return type of a function.
    ReturnType(ReturnType),
    /// A value in an iterable expression, e.g. IterableValue(\[1\]) represents 1.
    /// The second argument is the expression being iterated.
    /// The third argument indicates whether iteration is async or not.
    IterableValue(Option<Idx<KeyAnnotation>>, Expr, IsAsync),
    /// A value produced by entering a context manager.
    /// The second argument is the expression of the context manager and its range.
    /// The fourth argument indicates whether the context manager is async or not.
    ContextValue(Option<Idx<KeyAnnotation>>, Idx<Key>, TextRange, IsAsync),
    /// A value at a specific position in an unpacked iterable expression.
    /// Example: UnpackedValue(('a', 'b')), 1) represents 'b'.
    UnpackedValue(Box<Binding>, TextRange, UnpackedPosition),
    /// A subscript expression and the value assigned to it
    SubscriptValue(Box<Binding>, ExprSubscript),
    /// A type where we have an annotation, but also a type we computed.
    /// If the annotation has a type inside it (e.g. `int` then use the annotation).
    /// If the annotation doesn't (e.g. it's `Final`), then use the binding.
    AnnotatedType(Idx<KeyAnnotation>, Box<Binding>),
    /// A record of an "augmented assignment" statement like `x -= _`
    /// or `a.b *= _`. These desugar to special method calls.
    AugAssign(Option<Idx<KeyAnnotation>>, StmtAugAssign),
    /// An explicit type.
    Type(Type),
    /// The str type.
    StrType,
    /// A type parameter.
    TypeParameter(Quantified),
    /// The type of a function. Stores an optional reference to the predecessor of this function.
    /// If the function is defined in a class scope, stores a reference to the class metadata.
    Function(
        Idx<KeyFunction>,
        Option<Idx<Key>>,
        Option<Idx<KeyClassMetadata>>,
    ),
    /// An import statement, typically with Self::Import.
    Import(ModuleName, Name),
    /// A class definition, points to a BindingClass and any decorators.
    ClassDef(Idx<KeyClass>, Box<[Idx<Key>]>),
    /// A forward reference to another binding.
    Forward(Idx<Key>),
    /// A phi node, representing the union of several alternative keys.
    /// Optionally contain a value to default if you end up being recursive.
    Phi(SmallSet<Idx<Key>>, Option<Idx<Key>>),
    /// A narrowed type.
    Narrow(Idx<Key>, Box<NarrowOp>, TextRange),
    /// An import of a module.
    /// Also contains the path along the module to bind, and optionally a key
    /// with the previous import to this binding (in which case merge the modules).
    Module(ModuleName, Vec<Name>, Option<Idx<Key>>),
    /// A name that might be a legacy type parameter. Solving this gives the Quantified type if so.
    /// The TextRange is optional and should be set at most once per identifier
    /// to avoid duplicate type errors (this is not type safe, because we might
    /// produce multiple `CheckLegacyTypeParam` bindings for the same
    /// identifier).
    /// It controls whether to produce an error saying there are scoped type parameters for this
    /// function / class, and therefore the use of legacy type parameters is invalid.
    CheckLegacyTypeParam(Idx<KeyLegacyTypeParam>, Option<TextRange>),
    /// An assignment to a name.
    NameAssign(
        Name,
        Option<(AnnotationStyle, Idx<KeyAnnotation>)>,
        Box<Expr>,
    ),
    /// A type alias declared with the `type` soft keyword
    ScopedTypeAlias(Name, Option<TypeParams>, Box<Expr>),
    /// An entry in a MatchMapping. The Key looks up the value being matched, the Expr is the key we're extracting.
    PatternMatchMapping(Expr, Idx<Key>),
    /// An entry in a MatchClass. The Key looks up the value being matched, the Expr is the class name.
    /// Positional patterns index into __match_args__, and keyword patterns match an attribute name.
    PatternMatchClassPositional(Box<Expr>, usize, Idx<Key>, TextRange),
    PatternMatchClassKeyword(Box<Expr>, Identifier, Idx<Key>),
    /// Binding for an `except` (if the boolean flag is false) or `except*` (if the boolean flag is true) clause
    ExceptionHandler(Box<Expr>, bool),
    /// Binding for an `@decorator` decoration on a function or class
    Decorator(Expr),
    /// Binding for a lambda parameter.
    LambdaParameter(Var),
    /// Binding for a function parameter. We either have an annotation, or we will determine the
    /// parameter type when solving the function type. To ensure the parameter is solved before it
    /// can be observed as a Var, we include the function key and force it to be solved first.
    FunctionParameter(Either<Idx<KeyAnnotation>, (Var, Idx<KeyFunction>, AnnotationTarget)>),
    /// The result of a `super()` call.
    SuperInstance(SuperStyle, TextRange),
}

impl Binding {
    /// Helper function that turns trivial Phi nodes into a forward.
    pub fn phi(xs: SmallSet<Idx<Key>>) -> Self {
        if xs.len() == 1 {
            Self::Forward(xs.into_iter().next().unwrap())
        } else {
            Self::Phi(xs, None)
        }
    }

    /// Like `phi`, uses the first element as the default.
    pub fn phi_first_default(xs: SmallSet<Idx<Key>>) -> Self {
        if xs.len() == 1 {
            Self::Forward(xs.into_iter().next().unwrap())
        } else {
            let default = xs.iter().next().copied();
            Self::Phi(xs, default)
        }
    }
}

impl DisplayWith<Bindings> for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        let m = ctx.module_info();
        match self {
            Self::Expr(None, x) => write!(f, "{}", m.display(x)),
            Self::Expr(Some(k), x) => {
                write!(f, "{}: {}", ctx.display(*k), m.display(x))
            }
            Self::TypeVar(_, name, x) => {
                write!(f, "typevar {} = {}", name, m.display(x))
            }
            Self::ParamSpec(_, name, x) => {
                write!(f, "paramspec {} = {}", name, m.display(x))
            }
            Self::TypeVarTuple(_, name, x) => {
                write!(f, "typevartuple {} = {}", name, m.display(x))
            }
            Self::ReturnExplicit(x) => {
                if let Some(annot) = x.annot {
                    write!(f, "{} ", ctx.display(annot))?
                }
                if let Some(expr) = x.expr.as_ref() {
                    write!(f, "{}", m.display(expr))
                } else {
                    write!(f, "return")
                }
            }
            Self::ReturnImplicit(_) => write!(f, "implicit return"),
            Self::ReturnType(_) => write!(f, "return type"),
            Self::IterableValue(None, x, IsAsync::Async) => {
                write!(f, "async iter {}", m.display(x))
            }
            Self::IterableValue(Some(k), x, IsAsync::Async) => {
                write!(f, "async iter {}: {}", ctx.display(*k), m.display(x))
            }
            Self::IterableValue(None, x, IsAsync::Sync) => write!(f, "iter {}", m.display(x)),
            Self::IterableValue(Some(k), x, IsAsync::Sync) => {
                write!(f, "iter {}: {}", ctx.display(*k), m.display(x))
            }
            Self::ExceptionHandler(box x, true) => write!(f, "except* {}", m.display(x)),
            Self::ExceptionHandler(box x, false) => write!(f, "except {}", m.display(x)),
            Self::ContextValue(_ann, x, _, kind) => {
                let name = match kind {
                    IsAsync::Sync => "context",
                    IsAsync::Async => "async context",
                };
                write!(f, "{name} {}", ctx.display(*x))
            }
            Self::SubscriptValue(x, subscript) => {
                write!(
                    f,
                    "subscript {}[{}] = {}",
                    m.display(&subscript.value),
                    m.display(&subscript.slice),
                    x.display_with(ctx),
                )
            }
            Self::UnpackedValue(x, range, pos) => {
                let pos = match pos {
                    UnpackedPosition::Index(i) => i.to_string(),
                    UnpackedPosition::ReverseIndex(i) => format!("-{i}"),
                    UnpackedPosition::Slice(i, j) => {
                        let end = match j {
                            0 => "".to_owned(),
                            _ => format!("-{j}"),
                        };
                        format!("{}:{}", i, end)
                    }
                };
                write!(f, "unpack {} {:?} @ {}", x.display_with(ctx), range, pos)
            }
            Self::Function(x, _pred, _class) => write!(f, "{}", ctx.display(*x)),
            Self::Import(m, n) => write!(f, "import {m}.{n}"),
            Self::ClassDef(x, _) => write!(f, "{}", ctx.display(*x)),
            Self::Forward(k) => write!(f, "{}", ctx.display(*k)),
            Self::AugAssign(_, s) => write!(f, "augmented_assign {:?}", s),
            Self::Type(t) => write!(f, "type {t}"),
            Self::StrType => write!(f, "strtype"),
            Self::TypeParameter(q) => write!(f, "type_parameter {q}"),
            Self::CheckLegacyTypeParam(k, _) => {
                write!(f, "check_legacy_type_param {}", ctx.display(*k))
            }
            Self::AnnotatedType(k1, k2) => {
                write!(f, "({}): {}", k2.display_with(ctx), ctx.display(*k1))
            }
            Self::Module(m, path, key) => {
                write!(
                    f,
                    "module {}({}){}",
                    path.join("."),
                    m,
                    match key {
                        None => String::new(),
                        Some(k) => format!("+ {}", ctx.display(*k)),
                    }
                )
            }
            Self::Phi(xs, default) => {
                write!(f, "phi(")?;
                for (i, x) in xs.iter().enumerate() {
                    if i != 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", ctx.display(*x))?;
                }
                write!(f, ")")?;
                if let Some(x) = default {
                    write!(f, " default {}", ctx.display(*x))?;
                }
                Ok(())
            }
            Self::Narrow(k, op, _) => {
                write!(f, "narrow({}, {op:?})", ctx.display(*k))
            }
            Self::NameAssign(name, None, expr) => {
                write!(f, "{} = {}", name, expr.display_with(ctx.module_info()))
            }
            Self::NameAssign(name, Some((_, annot)), expr) => {
                write!(
                    f,
                    "{}: {} = {}",
                    name,
                    ctx.display(*annot),
                    expr.display_with(ctx.module_info())
                )
            }
            Self::ScopedTypeAlias(name, None, expr) => {
                write!(
                    f,
                    "type {} = {}",
                    name,
                    expr.display_with(ctx.module_info())
                )
            }
            Self::ScopedTypeAlias(name, Some(params), expr) => {
                write!(
                    f,
                    "type {}[{}] = {}",
                    name,
                    commas_iter(|| params.iter().map(|p| format!("{}", p.name()))),
                    expr.display_with(ctx.module_info())
                )
            }
            Self::PatternMatchMapping(mapping_key, binding_key) => {
                write!(
                    f,
                    "PatternMatchMapping {} = {}",
                    m.display(mapping_key),
                    ctx.display(*binding_key),
                )
            }
            Self::PatternMatchClassPositional(class, idx, key, range) => {
                write!(
                    f,
                    "PatternMatchClassPositional {}[{}] = {} {:?}",
                    m.display(class),
                    idx,
                    ctx.display(*key),
                    range
                )
            }
            Self::PatternMatchClassKeyword(class, attr, key) => {
                write!(
                    f,
                    "PatternMatchClassKeyword {}.{} = {}",
                    m.display(class),
                    attr,
                    ctx.display(*key),
                )
            }
            Self::Decorator(e) => write!(f, "decorator {}", m.display(e)),
            Self::LambdaParameter(_) => write!(f, "lambda parameter"),
            Self::FunctionParameter(_) => write!(f, "function parameter"),
            Self::SuperInstance(SuperStyle::ExplicitArgs(cls, obj), _range) => {
                write!(f, "super({}, {})", ctx.display(*cls), ctx.display(*obj))
            }
            Self::SuperInstance(SuperStyle::ImplicitArgs(_, _), _range) => write!(f, "super()"),
            Self::SuperInstance(SuperStyle::Any, _range) => write!(f, "super(Any, Any)"),
        }
    }
}

#[derive(Debug, Clone, Copy, VisitMut, TypeEq, PartialEq, Eq)]
pub enum Initialized {
    Yes,
    No,
}

#[derive(Debug, Clone, VisitMut, TypeEq, PartialEq, Eq)]
pub struct AnnotationWithTarget {
    pub target: AnnotationTarget,
    pub annotation: Annotation,
}

impl AnnotationWithTarget {
    pub fn ty(&self) -> Option<&Type> {
        self.annotation.ty.as_ref()
    }
}

impl Display for AnnotationWithTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.target, self.annotation)
    }
}

#[derive(Debug, Clone, VisitMut, TypeEq, PartialEq, Eq)]
pub enum AnnotationTarget {
    /// A function parameter with a type annotation
    Param(Name),
    ArgsParam(Name),
    KwargsParam(Name),
    /// A return type annotation on a function. The name is that of the function
    Return(Name),
    /// An annotated assignment. For attribute assignments, the name is the attribute name ("attr" in "x.attr")
    /// Does the annotated assignment have an initial value?
    Assign(Name, Initialized),
    /// A member of a class
    ClassMember(Name),
}

impl Display for AnnotationTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Param(name) => write!(f, "param {name}"),
            Self::ArgsParam(name) => write!(f, "args {name}"),
            Self::KwargsParam(name) => write!(f, "kwargs {name}"),
            Self::Return(name) => write!(f, "{name} return"),
            Self::Assign(name, _initialized) => write!(f, "var {name}"),
            Self::ClassMember(name) => write!(f, "attr {name}"),
        }
    }
}

impl AnnotationTarget {
    pub fn type_form_context(&self) -> TypeFormContext {
        match self {
            Self::Param(_) => TypeFormContext::ParameterAnnotation,
            Self::ArgsParam(_) => TypeFormContext::ParameterArgsAnnotation,
            Self::KwargsParam(_) => TypeFormContext::ParameterKwargsAnnotation,
            Self::Return(_) => TypeFormContext::ReturnAnnotation,
            Self::Assign(_, is_initialized) => TypeFormContext::VarAnnotation(*is_initialized),
            Self::ClassMember(_) => TypeFormContext::ClassVarAnnotation,
        }
    }
}

/// Values that return an annotation.
#[derive(Clone, Debug)]
pub enum BindingAnnotation {
    /// The type is annotated to be this key, will have the outer type removed.
    /// Optionally occurring within a class, in which case Self refers to this class.
    AnnotateExpr(AnnotationTarget, Expr, Option<Idx<KeyClass>>),
    /// A literal type we know statically.
    Type(AnnotationTarget, Type),
}

impl DisplayWith<Bindings> for BindingAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        match self {
            Self::AnnotateExpr(_, x, class_key) => write!(
                f,
                "_: {}{}",
                ctx.module_info().display(x),
                match class_key {
                    None => String::new(),
                    Some(t) => format!(" (self {})", ctx.display(*t)),
                }
            ),
            Self::Type(_, t) => write!(f, "type {t}"),
        }
    }
}

/// Binding for a class.
#[derive(Clone, Debug)]
pub enum BindingClass {
    ClassDef(ClassBinding),
    FunctionalClassDef(ClassIndex, Identifier, SmallMap<Name, ClassFieldProperties>),
}

impl DisplayWith<Bindings> for BindingClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, _ctx: &Bindings) -> fmt::Result {
        match self {
            Self::ClassDef(c) => write!(f, "class {}", c.def.name),
            Self::FunctionalClassDef(_, id, _) => write!(f, "class {}", id),
        }
    }
}

/// Binding for a class field, which is any attribute (including methods) of a class defined in
/// either the class body or in method (like `__init__`) that we recognize as
/// defining instance attributes.
#[derive(Clone, Debug)]
pub struct BindingClassField {
    pub class: Idx<KeyClass>,
    pub name: Name,
    pub value: Binding,
    pub annotation: Option<Idx<KeyAnnotation>>,
    pub range: TextRange,
    pub initial_value: ClassFieldInitialValue,
    pub is_function_without_return_annotation: bool,
}

impl DisplayWith<Bindings> for BindingClassField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        write!(f, "class field {}", self.value.display_with(ctx))
    }
}

/// The value that the class field is initialized to.
#[derive(Clone, Debug)]
pub enum ClassFieldInitialValue {
    /// The field does not have an initial value. If a name is provided, it is the name of the
    /// method where the field was inferred (which will be None for fields declared but not
    /// initialized in the class body, or for instance-only fields of synthesized classes)
    Instance(Option<Name>),
    /// The field has an initial value.
    ///
    /// If the value is from an assignment, stores the expression that the field is assigned to,
    /// which is needed for some cases like dataclass fields. The `None` case is for fields that
    /// have values which don't come from assignment (e.g. function defs, imports in a class body)
    Class(Option<Expr>),
}

/// Bindings for fields synthesized by a class, such as a dataclass's `__init__` method. This
/// has to be its own key/binding type because of the dependencies between the various pieces of
/// information about a class: ClassDef -> ClassMetadata -> ClassField -> ClassSynthesizedFields.
#[derive(Clone, Debug)]
pub struct BindingClassSynthesizedFields(pub Idx<KeyClass>);

impl DisplayWith<Bindings> for BindingClassSynthesizedFields {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        write!(f, "synthesized fields of {}", ctx.display(self.0))
    }
}

/// Binding for the class's metadata (type level information derived from the class header - this
/// includes the MRO, the class keywords, and the metaclass).
///
/// The `Key` points to the definition of the class.
/// The `[Expr]` contains the base classes from the class header.
/// The `[(Name, Expr)]` contains the class keywords from the class header.
/// The `[Idx<Key>]` points to the class's decorators.
#[derive(Clone, Debug)]
pub struct BindingClassMetadata {
    pub def: Idx<KeyClass>,
    pub bases: Box<[Expr]>,
    pub keywords: Box<[(Name, Expr)]>,
    pub decorators: Box<[Idx<Key>]>,
    pub is_new_type: bool,
    pub special_base: Option<Box<BaseClass>>,
}

impl DisplayWith<Bindings> for BindingClassMetadata {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        write!(f, "metadata {}", ctx.display(self.def))
    }
}

#[derive(Clone, Debug)]
pub struct BindingLegacyTypeParam(pub Idx<Key>);

impl DisplayWith<Bindings> for BindingLegacyTypeParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        write!(f, "legacy_type_param {}", ctx.display(self.0))
    }
}

#[derive(Clone, Debug)]
pub enum BindingYield {
    Yield(Option<Idx<KeyAnnotation>>, ExprYield),
    Invalid(ExprYield),
}

impl BindingYield {
    fn expr(&self) -> &ExprYield {
        match self {
            Self::Yield(_, x) => x,
            Self::Invalid(x) => x,
        }
    }
}

impl DisplayWith<Bindings> for BindingYield {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        let m = ctx.module_info();
        write!(f, "{}", m.display(&self.expr()))
    }
}

#[derive(Clone, Debug)]
pub enum BindingYieldFrom {
    YieldFrom(Option<Idx<KeyAnnotation>>, ExprYieldFrom),
    Invalid(ExprYieldFrom),
}

impl BindingYieldFrom {
    fn expr(&self) -> &ExprYieldFrom {
        match self {
            Self::YieldFrom(_, x) => x,
            Self::Invalid(x) => x,
        }
    }
}

impl DisplayWith<Bindings> for BindingYieldFrom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        let m = ctx.module_info();
        write!(f, "{}", m.display(&self.expr()))
    }
}
