/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;

use crate::binding::binding::AnnotationTarget;
use crate::error::kind::ErrorKind;
use crate::types::callable::FuncId;
use crate::types::types::Type;

/// General context for an error. For many errors, the root cause is some steps removed from what
/// the user sees. For example:
///   class C:
///     def __lt__(self, other: C) -> bool:
///   C() < 0  # ERROR: expected C, got 0
/// The root cause is `C.__lt__` being called with the wrong type, but the user sees a `<`
/// comparison. ErrorContext stores this context that the user sees, to make it easier to connect
/// it back to the root cause.
#[derive(Clone, Debug)]
pub enum ErrorContext {
    /// with x: ...
    BadContextManager(Type),
    /// Unary operation like `+x`
    UnaryOp(String, Type),
    /// Binary operation like `x + y`
    BinaryOp(String, Type, Type),
    /// In-place binary operation like `x += y`
    InplaceBinaryOp(String, Type, Type),
    /// for x in y: ...
    Iteration(Type),
    /// async for x in y: ...
    AsyncIteration(Type),
    /// await x
    Await(Type),
    /// x[y]
    Index(Type),
    /// x[y] = ...
    SetItem(Type),
    /// del x[y]
    DelItem(Type),
    /// match x: case Foo(y): ...
    MatchPositional(Type),
}

/// The context in which a got <: want type check occurs. This differs from ErrorContext in that
/// TypeCheckContext applies specifically to type mismatches. For example:
///   class C:
///     def __lt__(self, other: C) -> bool:
///   C() < 0  # ERROR: expected C, got 0
/// The TypeCheckContext contains a TypeCheckKind::CallArgument, recording that
/// the mismatch is in the `other` parameter of `C.__lt__`, and an
/// ErrorContext::BinaryOp, recording that the type mismatch occurs in the context of a `<` comparison.
#[derive(Debug)]
pub struct TypeCheckContext {
    pub kind: TypeCheckKind,
    pub context: Option<ErrorContext>,
}

impl TypeCheckContext {
    pub fn of_kind(kind: TypeCheckKind) -> Self {
        Self {
            kind,
            context: None,
        }
    }

    /// Temporary helper to label errors as Unknown before we properly classify them.
    pub fn unknown() -> Self {
        Self::of_kind(TypeCheckKind::Unknown)
    }
}

#[derive(Debug)]
pub enum TypeCheckKind {
    /// Check on a magic method that is expected to return a particular type; e.g., a context
    /// manager's `__exit__` method must return `bool | None`.
    MagicMethodReturn(Type, Name),
    /// Check that the return of an augmented assignment method call is still assignable to the original value
    AugmentedAssignment,
    /// Implicit return via a path with no explicit return statement. The bool indicates whether
    /// the function has *any* explicit return.
    ImplicitFunctionReturn(bool),
    /// Explicit return statement in a function body.
    ExplicitFunctionReturn,
    /// Return in a type guard function.
    TypeGuardReturn,
    /// Function call argument against parameter type.
    CallArgument(Option<Name>, Option<FuncId>),
    /// Function call argument against *arg parameter type.
    CallVarArgs(Option<Name>, Option<FuncId>),
    /// Unpacked argument against *args type.
    CallUnpackVarArgs(Option<Name>, Option<FuncId>),
    /// Keyword argument against parameter or **kwargs type, as (argument name, parameter name, function name).
    CallKwArgs(Option<Name>, Option<Name>, Option<FuncId>),
    /// Check of a parameter's default value against its type annotation.
    FunctionParameterDefault(Name),
    /// Check against type of a TypedDict key.
    TypedDictKey(Name),
    /// Check an unpacked dict against a TypedDict, e.g., `x: MyTypedDict = {**unpacked_dict}`.
    TypedDictUnpacking,
    /// Check of an attribute assignment against its type.
    Attribute(Name),
    /// A check against a user-declared type annotation on a variable name.
    AnnotatedName(Name),
    /// Check the type of an iteration variable (the `x` in `for x in seq`) against the iterable.
    // When checking iteration variables (the `x` in `for x in seq`), we transform the type annotation of the variable if it exists.
    // We need to carry around the actual un-transformed type of `x` to avoid a confusing error message.
    IterationVariableMismatch(Name, Type),
    /// var: SomeType = some_value check. This is separate from AnnotatedName because we can
    /// emit a more compact error message for this case.
    AnnAssign,
    /// Class used in an `except C` clause.
    ExceptionClass,
    /// Yielding a value that conflicts with the return annotation.
    YieldValue,
    /// Yielding from an iterator that conflicts with the return annotation.
    YieldFrom,
    /// Bare yield when the return annotation expects an actual value.
    UnexpectedBareYield,
    // TODO: categorize all type checks and remove Unknown and Test designations
    Unknown,
}

impl TypeCheckKind {
    pub fn from_annotation_target(target: &AnnotationTarget) -> Self {
        match target {
            AnnotationTarget::Param(_)
            | AnnotationTarget::ArgsParam(_)
            | AnnotationTarget::KwargsParam(_) => Self::Unknown,
            AnnotationTarget::Return(_func) => Self::ExplicitFunctionReturn,
            AnnotationTarget::Assign(name, _is_initialized) => Self::AnnotatedName(name.clone()),
            AnnotationTarget::ClassMember(member) => Self::Attribute(member.clone()),
        }
    }

    pub fn as_error_kind(&self) -> ErrorKind {
        match self {
            Self::MagicMethodReturn(..) => ErrorKind::BadReturn,
            Self::AugmentedAssignment => ErrorKind::BadAssignment,
            Self::ImplicitFunctionReturn(..) => ErrorKind::BadReturn,
            Self::ExplicitFunctionReturn => ErrorKind::BadReturn,
            Self::TypeGuardReturn => ErrorKind::BadReturn,
            Self::CallArgument(..) => ErrorKind::BadArgumentType,
            Self::CallVarArgs(..) => ErrorKind::BadArgumentType,
            Self::CallUnpackVarArgs(..) => ErrorKind::BadArgumentType,
            Self::CallKwArgs(..) => ErrorKind::BadArgumentType,
            Self::FunctionParameterDefault(..) => ErrorKind::BadFunctionDefinition,
            Self::TypedDictKey(..) => ErrorKind::TypedDictKeyError,
            Self::TypedDictUnpacking => ErrorKind::BadUnpacking,
            Self::Attribute(..) => ErrorKind::BadAssignment,
            Self::AnnotatedName(..) => ErrorKind::BadAssignment,
            Self::IterationVariableMismatch(..) => ErrorKind::BadAssignment,
            Self::AnnAssign => ErrorKind::BadAssignment,
            Self::ExceptionClass => ErrorKind::Unknown,
            Self::YieldValue => ErrorKind::InvalidYield,
            Self::YieldFrom => ErrorKind::InvalidYield,
            Self::UnexpectedBareYield => ErrorKind::InvalidYield,
            Self::Unknown => ErrorKind::Unknown,
        }
    }
}
