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
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprSubscript;
use ruff_python_ast::Identifier;
use ruff_python_ast::StmtAugAssign;
use ruff_python_ast::StmtClassDef;
use ruff_python_ast::StmtFunctionDef;
use ruff_python_ast::TypeParams;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use static_assertions::assert_eq_size;

use crate::alt::classes::ClassField;
use crate::binding::bindings::Bindings;
use crate::binding::narrow::NarrowOp;
use crate::graph::index::Idx;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::annotation::Annotation;
use crate::types::class_metadata::ClassMetadata;
use crate::types::types::AnyStyle;
use crate::types::types::LegacyTypeParameterLookup;
use crate::types::types::Quantified;
use crate::types::types::Type;
use crate::util::display::DisplayWith;

assert_eq_size!(Key, [usize; 5]);
assert_eq_size!(KeyExport, [usize; 3]);
assert_eq_size!(KeyClassField, [usize; 4]);
assert_eq_size!(KeyAnnotation, [u8; 12]); // Equivalent to 1.5 usize
assert_eq_size!(KeyClassMetadata, [usize; 1]);
assert_eq_size!(KeyLegacyTypeParam, [usize; 1]);

assert_eq_size!(Binding, [usize; 9]);
assert_eq_size!(BindingAnnotation, [usize; 9]);
assert_eq_size!(BindingClassMetadata, [usize; 8]);
assert_eq_size!(BindingClassField, [usize; 10]);
assert_eq_size!(BindingLegacyTypeParam, [u32; 1]);

pub trait Keyed: Hash + Eq + Clone + DisplayWith<ModuleInfo> + Debug + Ranged + 'static {
    const EXPORTED: bool = false;
    type Value: Debug + DisplayWith<Bindings>;
    type Answer: Clone + Debug + Display;
}

impl Keyed for Key {
    type Value = Binding;
    type Answer = Type;
}
impl Keyed for KeyClassField {
    const EXPORTED: bool = true;
    type Value = BindingClassField;
    type Answer = ClassField;
}
impl Keyed for KeyExport {
    const EXPORTED: bool = true;
    type Value = Binding;
    type Answer = Type;
}
impl Keyed for KeyAnnotation {
    type Value = BindingAnnotation;
    type Answer = Annotation;
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

/// Keys that refer to a `Type`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Key {
    /// I am an `import` at this location with this name.
    /// Used for `import foo.x` (the `foo` might not be literally present with `.` modules),
    /// and `from foo import *` (the names are injected from the exports)
    Import(Name, TextRange),
    /// I am defined in this module at this location.
    Definition(ShortIdentifier),
    /// I am the self type for a particular class.
    SelfType(ShortIdentifier),
    /// The type at a specific return point.
    ReturnExpression(ShortIdentifier, TextRange),
    /// The type of a yield value at a specific point in the program.
    YieldExpression(ShortIdentifier, TextRange),
    /// The type of the yield value.
    YieldType(ShortIdentifier),
    /// The actual type of the return for a function.
    ReturnType(ShortIdentifier),
    /// I am a use in this module at this location.
    Usage(ShortIdentifier),
    /// I am an expression that does not have a simple name but needs its type inferred.
    /// For example, an attribute access.
    Anon(TextRange),
    /// An expectation to be checked. For example, that a sequence is of an expected length.
    Expect(TextRange),
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
}

impl Ranged for Key {
    fn range(&self) -> TextRange {
        match self {
            Self::Import(_, r) => *r,
            Self::Definition(x) => x.range(),
            Self::SelfType(x) => x.range(),
            Self::ReturnExpression(_, r) => *r,
            Self::YieldExpression(_, r) => *r,
            Self::YieldType(x) => x.range(),
            Self::ReturnType(x) => x.range(),
            Self::Usage(x) => x.range(),
            Self::Anon(r) => *r,
            Self::Expect(r) => *r,
            Self::Phi(_, r) => *r,
            Self::Narrow(_, r, _) => *r,
            Self::Anywhere(_, r) => *r,
        }
    }
}

impl DisplayWith<ModuleInfo> for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        match self {
            Self::Import(n, r) => write!(f, "import {n} {r:?}"),
            Self::Definition(x) => write!(f, "{} {:?}", ctx.display(x), x.range()),
            Self::SelfType(x) => write!(f, "self {} {:?}", ctx.display(x), x.range()),
            Self::Usage(x) => write!(f, "use {} {:?}", ctx.display(x), x.range()),
            Self::Anon(r) => write!(f, "anon {r:?}"),
            Self::Expect(r) => write!(f, "expect {r:?}"),
            Self::Phi(n, r) => write!(f, "phi {n} {r:?}"),
            Self::Narrow(n, r1, r2) => write!(f, "narrow {n} {r1:?} {r2:?}"),
            Self::Anywhere(n, r) => write!(f, "anywhere {n} {r:?}"),
            Self::ReturnType(x) => write!(f, "return {} {:?}", ctx.display(x), x.range()),
            Self::ReturnExpression(x, i) => {
                write!(f, "return {} {:?} @ {i:?}", ctx.display(x), x.range())
            }
            Self::YieldExpression(x, i) => {
                write!(f, "yield {} {:?} @ {i:?}", ctx.display(x), x.range())
            }
            Self::YieldType(x) => write!(f, "yield {} {:?}", ctx.display(x), x.range()),
        }
    }
}

impl DisplayWith<Bindings> for Key {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        write!(f, "{}", ctx.module_info().display(self))
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

/// A reference to a field in a class.
/// The range is the range of the class name, not the field name.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct KeyClassField(pub ShortIdentifier, pub Name);

impl Ranged for KeyClassField {
    fn range(&self) -> TextRange {
        self.0.range()
    }
}

impl DisplayWith<ModuleInfo> for KeyClassField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        write!(
            f,
            "field {} {:?} . {}",
            ctx.display(&self.0),
            self.0.range(),
            self.1
        )
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
pub struct KeyClassMetadata(pub ShortIdentifier);

impl Ranged for KeyClassMetadata {
    fn range(&self) -> TextRange {
        self.0.range()
    }
}

impl DisplayWith<ModuleInfo> for KeyClassMetadata {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        write!(f, "mro {} {:?}", ctx.display(&self.0), self.0.range())
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

#[derive(Clone, Copy, Debug)]
pub enum ContextManagerKind {
    Sync,
    Async,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FunctionKind {
    Stub,
    Impl,
}

#[derive(Clone, Debug)]
pub enum Binding {
    /// An expression, optionally with a Key saying what the type must be.
    /// The Key must be a type of types, e.g. `Type::Type`.
    Expr(Option<Idx<KeyAnnotation>>, Expr),
    /// A value in an iterable expression, e.g. IterableValue(\[1\]) represents 1.
    IterableValue(Option<Idx<KeyAnnotation>>, Expr),
    /// A value produced by entering a context manager.
    /// The second argument is the expression of the context manager. The third argument
    /// indicates whether the context manager is async or not.
    ContextValue(Option<Idx<KeyAnnotation>>, Expr, ContextManagerKind),
    /// A value at a specific position in an unpacked iterable expression.
    /// Example: UnpackedValue(('a', 'b')), 1) represents 'b'.
    UnpackedValue(Box<Binding>, TextRange, UnpackedPosition),
    /// The expected number of values in an unpacked iterable expression.
    UnpackedLength(Box<Binding>, TextRange, SizeExpectation),
    /// A subscript expression and the value assigned to it
    SubscriptValue(Box<Binding>, ExprSubscript),
    /// A type where we have an annotation, but also a type we computed.
    /// If the annotation has a type inside it (e.g. `int` then use the annotation).
    /// If the annotation doesn't (e.g. it's `Final`), then use the binding.
    AnnotatedType(Idx<KeyAnnotation>, Box<Binding>),
    /// A record of an "augmented assignment" statement like `x -= _`
    /// or `a.b *= _`. These desugar to special method calls.
    AugAssign(StmtAugAssign),
    /// The Any type.
    AnyType(AnyStyle),
    /// The str type.
    StrType,
    /// A type parameter.
    TypeParameter(Quantified),
    /// A function definition, but with the return/body stripped out.
    /// The `Vec<Idx<LegacyTypeParam>>` contains binding information for possible legacy type params.
    Function(
        Box<StmtFunctionDef>,
        FunctionKind,
        Box<[Idx<KeyLegacyTypeParam>]>,
    ),
    /// An import statement, typically with Self::Import.
    Import(ModuleName, Name),
    /// A class definition, but with the body stripped out.
    /// The field names should be used to build `ClassField` keys for lookup.
    /// The `Vec<Expr>` contains the base classes from the class header.
    /// The `Vec<Idx<KeyLegacyTypeParam>>` contains binding information for possible legacy type params.
    ClassDef(
        Box<(StmtClassDef, SmallSet<Name>)>,
        Box<[Expr]>,
        Box<[Idx<KeyLegacyTypeParam>]>,
    ),
    /// The Self type for a class, must point at a class.
    SelfType(Idx<Key>),
    /// A forward reference to another binding.
    Forward(Idx<Key>),
    /// A phi node, representing the union of several alternative keys.
    Phi(SmallSet<Idx<Key>>),
    /// A narrowed type.
    Narrow(Idx<Key>, NarrowOp),
    /// An import of a module.
    /// Also contains the path along the module to bind, and optionally a key
    /// with the previous import to this binding (in which case merge the modules).
    /// FIXME: Once we fix on alt, we the Module type will be ModuleName+Vec<Name>,
    /// so people using the None option will be able to use Self::Type instead.
    Module(ModuleName, Vec<Name>, Option<Idx<Key>>),
    /// An exception and its cause from a raise statement.
    CheckRaisedException(RaisedException),
    /// A name that might be a legacy type parameter. Solving this gives the Quantified type if so.
    /// The TextRange is optional and should be set at most once per identifier
    /// to avoid duplicate type errors (this is not type safe, because we might
    /// produce multiple `CheckLegacyTypeParam` bindings for the same
    /// identifier).
    /// It controls whether to produce an error saying there are scoped type parameters for this
    /// function / class, and therefore the use of legacy type parameters is invalid.
    CheckLegacyTypeParam(Idx<KeyLegacyTypeParam>, Option<TextRange>),
    /// An expectation that the types are identical, with an associated name for error messages.
    Eq(Idx<KeyAnnotation>, Idx<KeyAnnotation>, Name),
    /// An assignment to a name. The text range is the range of the RHS, and is used so that we
    /// can error on bad type forms in type aliases.
    NameAssign(Name, Option<Idx<KeyAnnotation>>, Box<Binding>, TextRange),
    /// A type alias declared with the `type` soft keyword
    ScopedTypeAlias(Name, Option<Box<TypeParams>>, Box<Binding>, TextRange),
    /// An entry in a MatchMapping. The Key looks up the value being matched, the Expr is the key we're extracting.
    PatternMatchMapping(Expr, Idx<Key>),
    /// An entry in a MatchClass. The Key looks up the value being matched, the Expr is the class name.
    /// Positional patterns index into __match_args__, and keyword patterns match an attribute name.
    PatternMatchClassPositional(Box<Expr>, usize, Idx<Key>, TextRange),
    PatternMatchClassKeyword(Box<Expr>, Identifier, Idx<Key>),
    /// Binding for an `except` (if the boolean flag is false) or `except*` (if the boolean flag is true) clause
    ExceptionHandler(Box<Expr>, bool),
}

impl Binding {
    /// Helper function that turns trivial Phi nodes into a forward.
    pub fn phi(xs: SmallSet<Idx<Key>>) -> Self {
        if xs.len() == 1 {
            Self::Forward(xs.into_iter().next().unwrap())
        } else {
            Self::Phi(xs)
        }
    }
}

impl DisplayWith<Bindings> for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        let m = ctx.module_info();
        match self {
            Self::Expr(None, x) => write!(f, "{}", m.display(x)),
            Self::Expr(Some(k), x) => write!(f, "{}: {}", ctx.display(*k), m.display(x)),
            Self::IterableValue(None, x) => write!(f, "iter {}", m.display(x)),
            Self::IterableValue(Some(k), x) => {
                write!(f, "iter {}: {}", ctx.display(*k), m.display(x))
            }
            Self::ExceptionHandler(box x, true) => write!(f, "except* {}", m.display(x)),
            Self::ExceptionHandler(box x, false) => write!(f, "except {}", m.display(x)),
            Self::ContextValue(_ann, x, kind) => {
                let name = match kind {
                    ContextManagerKind::Sync => "context",
                    ContextManagerKind::Async => "async context",
                };
                write!(f, "{name} {}", m.display(x))
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
            Self::Function(x, _, _) => write!(f, "def {}", x.name.id),
            Self::Import(m, n) => write!(f, "import {m}.{n}"),
            Self::ClassDef(box (c, _), _, _) => write!(f, "class {}", c.name.id),
            Self::SelfType(k) => write!(f, "self {}", ctx.display(*k)),
            Self::Forward(k) => write!(f, "{}", ctx.display(*k)),
            Self::AugAssign(s) => write!(f, "augmented_assign {:?}", s),
            Self::AnyType(s) => write!(f, "anytype {s}"),
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
            Self::CheckRaisedException(RaisedException::WithoutCause(exc)) => {
                write!(f, "raise {}", m.display(exc))
            }
            Self::CheckRaisedException(RaisedException::WithCause(box (exc, cause))) => {
                write!(f, "raise {} from {}", m.display(exc), m.display(cause))
            }
            Self::Phi(xs) => {
                write!(f, "phi(")?;
                for (i, x) in xs.iter().enumerate() {
                    if i != 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}", ctx.display(*x))?;
                }
                write!(f, ")")
            }
            Self::Narrow(k, op) => {
                write!(f, "narrow({}, {op:?})", ctx.display(*k))
            }
            Self::Eq(k1, k2, name) => write!(
                f,
                "{} == {} on {}",
                ctx.display(*k1),
                ctx.display(*k2),
                name
            ),
            Self::NameAssign(name, None, binding, _) => {
                write!(f, "{} = {}", name, binding.display_with(ctx))
            }
            Self::NameAssign(name, Some(annot), binding, _) => {
                write!(
                    f,
                    "{}: {} = {}",
                    name,
                    ctx.display(*annot),
                    binding.display_with(ctx)
                )
            }
            Self::ScopedTypeAlias(name, None, binding, _r) => {
                write!(f, "type {} = {}", name, binding.display_with(ctx))
            }
            Self::ScopedTypeAlias(name, Some(params), binding, _r) => {
                write!(
                    f,
                    "type {}[{}] = {}",
                    name,
                    params
                        .iter()
                        .map(|p| format!("{}", p.name()))
                        .collect::<Vec<_>>()
                        .join(", "),
                    binding.display_with(ctx)
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
        }
    }
}

/// Values that return an annotation.
#[derive(Clone, Debug)]
pub enum BindingAnnotation {
    /// The type is annotated to be this key, will have the outer type removed.
    /// Optionally occuring within a class, in which case Self refers to this class.
    AnnotateExpr(Expr, Option<Idx<Key>>),
    /// A literal type we know statically.
    Type(Type),
    /// Type of an attribute.
    AttrType(ExprAttribute),
    /// A forward reference to another binding.
    Forward(Idx<Key>),
}

impl DisplayWith<Bindings> for BindingAnnotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        match self {
            Self::AnnotateExpr(x, self_type) => write!(
                f,
                "_: {}{}",
                ctx.module_info().display(x),
                match self_type {
                    None => String::new(),
                    Some(t) => format!(" (self {})", ctx.display(*t)),
                }
            ),
            Self::Forward(k) => write!(f, "{}", ctx.display(*k)),
            Self::Type(t) => write!(f, "type {t}"),
            Self::AttrType(attr) => write!(f, "type {attr:?}"),
        }
    }
}

/// Correctly analyzing which attributes are visible on class objects, as well
/// as handling method binding correctly, requires distinguishing which fields
/// are assigned values in the class body.
#[derive(Clone, Copy, Debug)]
pub enum ClassFieldInitialization {
    Class,
    Instance,
}

/// Binding for a class field, which is any attribute of a class defined in
/// either the class body or in method (like `__init__`) that we recognize as
/// defining instance attributes.
#[derive(Clone, Debug)]
pub struct BindingClassField(
    pub Binding,
    pub Option<Idx<KeyAnnotation>>,
    pub ClassFieldInitialization,
);

impl DisplayWith<Bindings> for BindingClassField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        write!(f, "class field {}", self.0.display_with(ctx))
    }
}

/// Binding for the class's metadata (type level information derived from the class header - this
/// includes the MRO, the class keywords, and the metaclass).
///
/// The `Key` points to the definition of the class.
/// The `Vec<Expr>` contains the base classes from the class header.
/// The `SmallMap<Name, Expr>` contains the class keywords from the class header.
#[derive(Clone, Debug)]
pub struct BindingClassMetadata(pub Idx<Key>, pub Vec<Expr>, pub SmallMap<Name, Expr>);

impl DisplayWith<Bindings> for BindingClassMetadata {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        write!(f, "mro {}", ctx.display(self.0))
    }
}

#[derive(Clone, Debug)]
pub struct BindingLegacyTypeParam(pub Idx<Key>);

impl DisplayWith<Bindings> for BindingLegacyTypeParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &Bindings) -> fmt::Result {
        write!(f, "legacy_type_param {}", ctx.display(self.0))
    }
}
