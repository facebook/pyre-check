/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use convert_case::Case;
use convert_case::Casing;
use dupe::Dupe;
use parse_display::Display;

// Keep ErrorKind sorted lexographically, except for Unsupported and Unknown.
#[derive(
    Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Dupe, Display
)]
pub enum ErrorKind {
    /// Attempting to annotate a name with incompatible annotations.
    /// e.g. when a name is annotated in multiple branches of an if statement
    AnnotationMismatch,
    /// An error raised when async is not used when it should be, or perhaps used when it shouldn't be.
    AsyncError,
    /// Assigning a value of the wrong type to a variable.
    BadAssignment,
    /// A function definition has some typing-related error.
    /// e.g. putting a non-default argument after a default argument.
    BadFunctionDefinition,
    /// An error related to ParamSpec definition or usage.
    BadParamSpec,
    /// Attempting to return a value that does not match the function's return type.
    /// Can also arise when returning values from generators.
    BadReturn,
    /// Attempting to specialize a generic class with incorrect type arguments.
    /// e.g. `type[int, str]` is an error because `type` accepts only 1 type arg.
    BadSpecialization,
    /// An error casued by unpacking.
    /// e.g. attempting to unpack an iterable into the wrong number of variables.
    BadUnpacking,
    /// An error caused by a bad match statement.
    /// e.g. Writing a Foo(x, y, z) pattern when Foo only matches on (x, y).
    MatchError,
    /// Attempting to access an attribute that does not exist.
    MissingAttribute,
    /// Attemping to access a container with an incorrect index.
    /// This only occurs when pyre can statically verify that the index is incorrect.
    IndexError,
    /// Internal Pyre error.
    InternalError,
    /// Attempting to write an annotation that is invalid for some reason.
    InvalidAnnotation,
    /// Passing an argument that is invalid for reasons besides type.
    InvalidArgument,
    /// Attempting to use a value that is not a valid kind of Literal.
    InvalidLiteral,
    /// An error caused by incorrect inheritance in a class or type definition.
    /// e.g. a metaclass that is not a subclass of `type`.
    InvalidInheritance,
    /// Attempting to call `super()` in a way that is not allowed.
    /// e.g. calling `super(Y, x)` on an object `x` that does not match the class `Y`.
    InvalidSuperCall,
    /// An error caused by incorrect usage of the @overload decorator.
    /// e.g. not defining multiple variants for an overlaoded function.
    InvalidOverload,
    /// An error caused by incorrect usage or definition of a TypeVar.
    InvalidTypeVar,
    /// An error caused by incorrect usage or definition of a TypeVarTuple.
    InvalidTypeVarTuple,
    /// Attempting to use `yield` in a way that is not allowed.
    /// e.g. `yield from` with something that's not an iterable.
    InvalidYield,
    /// An error caused by calling a function without all the required arguments.
    MissingArgument,
    /// Accessing an attribute that does not exist on a module.
    MissingModuleAttribute,
    /// Attempting to access a TypedDict with a key that does not exist.
    MissingTypedDictKey,
    /// The attribute exists but does not support this access pattern.
    NoAccess,
    /// Attempting to use something that isn't a type where a type is expected.
    /// This is a very general error and should be used sparingly.
    NotAType,
    /// Attempting to use a non-iterable value as an iterable.
    NotIterable,
    /// An error related to parsing or syntax.
    ParseError,
    /// The attribute exists but cannot be modified.
    ReadOnly,
    /// An error related to type alias usage or definition.
    TypeAliasError,
    /// An error caused by a keyword argument used in the wrong place.
    UnexpectedKeyword,
    /// Attemping to apply an operator to arguments that do not support it.
    UnsupportedOperand,
    /// Attempting to use a feature that is not yet supported.
    #[allow(dead_code)]
    Unsupported,
    /// Unknown or not-yet-defined error.
    Unknown,
}

impl ErrorKind {
    pub fn to_name(self) -> String {
        self.to_string().to_case(Case::Kebab)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_error_kind_name() {
        assert_eq!(&ErrorKind::Unknown.to_name(), "unknown");
        assert_eq!(&ErrorKind::ParseError.to_name(), "parse-error");
    }
}
