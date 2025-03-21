/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A proc-macro for pieces of Pyrefly.
//! Should not be used outside of Pyrefly.

#[allow(unused_extern_crates)] // proc_macro is very special
extern crate proc_macro;

use proc_macro::TokenStream;

mod type_eq;
mod visit;

/// Generate `TypeEq` traits.
#[proc_macro_derive(TypeEq)]
pub fn derive_type_eq(input: TokenStream) -> TokenStream {
    type_eq::derive_type_eq(input)
}

/// Generate `Visit` traits.
#[proc_macro_derive(Visit)]
pub fn derive_visit(input: TokenStream) -> TokenStream {
    visit::derive_visit(input)
}

/// Generate `VisitMut` traits.
#[proc_macro_derive(VisitMut)]
pub fn derive_visit_mut(input: TokenStream) -> TokenStream {
    visit::derive_visit_mut(input)
}
