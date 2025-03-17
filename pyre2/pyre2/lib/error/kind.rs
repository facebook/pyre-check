/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::LazyLock;

use convert_case::Case;
use convert_case::Casing;
use dupe::Dupe;
use enum_iterator::Sequence;
use parse_display::Display;

/// ErrorKind categorizes an error by the part of the spec the error is related to.
/// They are used in suppressions to identify which error should be suppressed.
//
// Keep ErrorKind sorted lexographically, except for Unsupported and Unknown.
// There are broad categories of error kinds, based on the word used in the name.
// "Bad": Specific, straightforward type errors. Could be a disagreement with a source
//    of truth, e.g. a function definition is how we determine a call has errors.
// "Missing": Same as "Bad" but we know specifically that something is missing.
// "Invalid": Something is being used incorrectly, such as a typing construct or language feature.
// "SomethingError": Generally targeted on very specific error conditions. The "Error"
//    part may be dropped, e.g. in NotAType.
// These categories are flexible; use them for guidance when naming new ErrorKinds, but
// go with what feels right.
#[derive(
    Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Dupe, Display, Sequence
)]
pub enum ErrorKind {
    /// Attempting to annotate a name with incompatible annotations.
    /// e.g. when a name is annotated in multiple branches of an if statement
    AnnotationMismatch,
    /// Raised when an assert_type() call fails.
    AssertType,
    /// An error raised when async is not used when it should be, or perhaps used when it shouldn't be.
    AsyncError,
    /// Attempting to call a function with the wrong number of arguments.
    BadArgumentCount,
    /// Attempting to call a function with an argument that does not match the parameter's type.
    BadArgumentType,
    /// Assigning a value of the wrong type to a variable.
    BadAssignment,
    /// A class definition has some typing-related error.
    /// e.g. multiple fields with the same name.
    /// Errors related specifically to inheritance should use InvalidInheritance.
    BadClassDefinition,
    /// A function definition has some typing-related error.
    /// e.g. putting a non-default argument after a default argument.
    BadFunctionDefinition,
    /// Attempting to call a function with an incorrect keyword argument.
    /// e.g. f(x=1, x=2), or perhaps f(y=1) (where `f` has no parameter `y`).
    BadKeywordArgument,
    /// A subclass field or method incorrectly overrides a field/method of a parent class.
    BadOverride,
    /// Attempting to return a value that does not match the function's return type.
    /// Can also arise when returning values from generators.
    BadReturn,
    /// Attempting to specialize a generic class with incorrect type arguments.
    /// e.g. `type[int, str]` is an error because `type` accepts only 1 type arg.
    BadSpecialization,
    /// An error caused by unpacking.
    /// e.g. attempting to unpack an iterable into the wrong number of variables.
    BadUnpacking,
    /// Attempting to `del` something that cannot be deleted
    DeleteError,
    /// An error related to the import machinery.
    /// e.g. failed to import a module.
    ImportError,
    /// Attempting to access a container with an incorrect index.
    /// This only occurs when pyre can statically verify that the index is incorrect.
    IndexError,
    /// Internal Pyre error.
    InternalError,
    /// Attempting to write an annotation that is invalid for some reason.
    InvalidAnnotation,
    /// Passing an argument that is invalid for reasons besides type.
    InvalidArgument,
    /// An error caused by incorrect inheritance in a class or type definition.
    /// e.g. a metaclass that is not a subclass of `type`.
    InvalidInheritance,
    /// Attempting to use a value that is not a valid kind of Literal.
    InvalidLiteral,
    /// An error caused by incorrect usage of the @overload decorator.
    /// e.g. not defining multiple variants for an overlaoded function.
    InvalidOverload,
    /// An error related to ParamSpec definition or usage.
    InvalidParamSpec,
    /// Attempting to call `super()` in a way that is not allowed.
    /// e.g. calling `super(Y, x)` on an object `x` that does not match the class `Y`.
    InvalidSuperCall,
    /// An error caused by incorrect usage or definition of a TypeVar.
    InvalidTypeVar,
    /// An error caused by incorrect usage or definition of a TypeVarTuple.
    InvalidTypeVarTuple,
    /// Attempting to use `yield` in a way that is not allowed.
    /// e.g. `yield from` with something that's not an iterable.
    InvalidYield,
    /// An error caused by a bad match statement.
    /// e.g. Writing a Foo(x, y, z) pattern when Foo only matches on (x, y).
    MatchError,
    /// An error caused by calling a function without all the required arguments.
    /// Should be used when we can name the specific arguments that are missing.
    MissingArgument,
    /// Attempting to access an attribute that does not exist.
    MissingAttribute,
    /// Accessing an attribute that does not exist on a module.
    MissingModuleAttribute,
    /// The attribute exists but does not support this access pattern.
    NoAccess,
    /// Attempting to call an overloaded function, but none of the signatures match.
    NoMatchingOverload,
    /// Attempting to use something that isn't a type where a type is expected.
    /// This is a very general error and should be used sparingly.
    NotAType,
    /// Attempting to call a value that is not a callable.
    NotCallable,
    /// Attempting to use a non-iterable value as an iterable.
    NotIterable,
    /// An error related to parsing or syntax.
    ParseError,
    /// The attribute exists but cannot be modified.
    ReadOnly,
    /// Raised by a call to reveal_type().
    RevealType,
    /// An error related to type alias usage or definition.
    TypeAliasError,
    /// An error related to TypedDict keys.
    /// e.g. attempting to access a TypedDict with a key that does not exist.
    TypedDictKeyError,
    /// An error raised when one type is expected but another is found instead.
    TypeMismatch,
    /// An error caused by a keyword argument used in the wrong place.
    UnexpectedKeyword,
    /// Attempting to use a name that may be unbound or uninitialized
    UnboundName,
    /// Attempting to use a name that is not defined.
    UnknownName,
    /// Attempting to apply an operator to arguments that do not support it.
    UnsupportedOperand,
    /// Attempting to use a feature that is not yet supported.
    #[allow(dead_code)]
    Unsupported,
    /// Unknown or not-yet-defined error.
    #[allow(dead_code)]
    Unknown,
}

/// Computing the error kinds is disturbingly expensive, so cache the results.
/// Also means we can grab error code names without allocation, which is nice.
static ERROR_KIND_CACHE: LazyLock<Vec<String>> = LazyLock::new(ErrorKind::cache);

impl ErrorKind {
    fn cache() -> Vec<String> {
        enum_iterator::all::<ErrorKind>()
            .map(|x| x.to_string().to_case(Case::Kebab))
            .collect()
    }

    pub fn to_name(self) -> &'static str {
        ERROR_KIND_CACHE[self as usize].as_str()
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_error_kind_name() {
        assert_eq!(ErrorKind::Unknown.to_name(), "unknown");
        assert_eq!(ErrorKind::ParseError.to_name(), "parse-error");
    }
}
