/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use quote::format_ident;
use quote::quote;
use quote::quote_spanned;
use quote::ToTokens;
use syn::parse_macro_input;
use syn::spanned::Spanned;
use syn::Data;
use syn::DeriveInput;
use syn::Fields;
use syn::GenericParam;
use syn::Generics;

pub(crate) fn derive_type_eq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive_type_eq_impl(&input) {
        Ok(x) => x.into_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn generics(
    generics: &Generics,
) -> syn::Result<(proc_macro2::TokenStream, proc_macro2::TokenStream)> {
    let mut ts = Vec::new();
    for param in &generics.params {
        match param {
            GenericParam::Type(t) if t.bounds.is_empty() => {
                ts.push(&t.ident);
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    param,
                    "Unsupported generic parameter",
                ));
            }
        }
    }
    let before = quote_spanned! { generics.span() => < #(#ts: crate::types::equality::TypeEq),* > };
    let after = quote_spanned! { generics.span() => < #(#ts),* > };
    Ok((before, after))
}

fn derive_type_eq_impl(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let (generics_before, generics_after) = generics(&input.generics)?;
    let type_eq = quote! { crate::types::equality::TypeEq };
    let type_eq_ctx = quote! { crate::types::equality::TypeEqCtx };
    let body = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => {
                let comparisons = fields_named.named.iter().map(|field| {
                    let field_name = &field.ident;
                    quote_spanned! { field_name.span() => #type_eq::type_eq(&self.#field_name, &other.#field_name, ctx) }
                });
                quote_spanned! { fields_named.span() => #(#comparisons)&&* }
            }
            Fields::Unnamed(fields_unnamed) => {
                let comparisons = fields_unnamed.unnamed.iter().enumerate().map(|(i, _)| {
                    let index = syn::Index::from(i);
                    quote! { #type_eq::type_eq(&self.#index, &other.#index, ctx) }
                });
                quote_spanned! { fields_unnamed.span() => #(#comparisons)&&* }
            }
            Fields::Unit => quote_spanned! { data_struct.struct_token.span() => true },
        },
        Data::Enum(data_enum) => {
            let variants = data_enum.variants.iter().map(|variant| {
                let variant_name = &variant.ident;
                match &variant.fields {
                    Fields::Named(fields_named) => {
                        let field_names: Vec<_> = fields_named.named.iter().map(|f| &f.ident).collect();
                        let lhs: Vec<_> = (0..fields_named.named.len()).map(|i| format_ident!("lhs{i}")).collect();
                        let rhs: Vec<_> = (0..fields_named.named.len()).map(|i| format_ident!("rhs{i}")).collect();
                        let bind_lhs = field_names.iter().zip(&lhs).map(|(name, var)| quote! {#name: #var});
                        let bind_rhs = field_names.iter().zip(&rhs).map(|(name, var)| quote! {#name: #var});
                        let comparisons = lhs.iter().zip(&rhs).map(|(lhs, rhs)| {
                            quote! { #type_eq::type_eq(#lhs, #rhs, ctx) }
                        });
                        quote_spanned! { variant.span() =>
                            (#name::#variant_name { #(#bind_lhs),*  }, #name::#variant_name {  #(#bind_rhs),*  }) => {
                                #(#comparisons)&&*
                            }
                        }
                    }
                    Fields::Unnamed(fields_unnamed) => {
                        let field_indices: Vec<_> = (0..fields_unnamed.unnamed.len()).collect();
                        let lhs: Vec<_> = field_indices.iter().map(|i| format_ident!("lhs{i}")).collect();
                        let rhs: Vec<_> = field_indices.iter().map(|i| format_ident!("rhs{i}")).collect();
                        let comparisons = lhs.iter().zip(&rhs).map(|(lhs, rhs)| {
                            quote! { #type_eq::type_eq(#lhs, #rhs, ctx) }
                        });
                        quote_spanned! { variant.span() =>
                            (#name::#variant_name(#(#lhs),*), #name::#variant_name(#(#rhs),*)) => {
                                #(#comparisons)&&*
                            }
                        }
                    }
                    Fields::Unit => {
                        quote_spanned! { variant.span() =>
                            (#name::#variant_name, #name::#variant_name) => true
                        }
                    }
                }
            });
            quote! {
                match (self, other) {
                    #(#variants),*,
                    _ => false
                }
            }
        }
        _ => {
            return Err(syn::Error::new_spanned(
                input,
                "Can't deal with this type of data",
            ));
        }
    };
    Ok(quote! {
        impl #generics_before #type_eq for #name #generics_after {
            fn type_eq(&self, other: &Self, ctx: &mut #type_eq_ctx) -> bool {
                #body
            }
        }
    })
}
