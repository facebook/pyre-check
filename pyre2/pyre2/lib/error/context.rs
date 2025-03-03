/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;

use crate::types::types::Type;

/// The context in which a got <: want type check occurs
pub enum TypeCheckContext {
    /// Return type check on a named function. `Option<Type>` is the type that the function is
    /// defined on, if it is a method of a class.
    FunctionReturn(Name, Option<Type>),
    // TODO: categorize all type checks and remove Unknown designation
    Unknown,
}
