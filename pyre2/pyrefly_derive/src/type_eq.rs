/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use quote::format_ident;
use quote::quote;
use quote::ToTokens;
use syn::parse_macro_input;
use syn::Data;
use syn::DeriveInput;
use syn::Fields;

pub(crate) fn derive_type_eq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive_type_eq_impl(&input) {
        Ok(x) => x.into_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn derive_type_eq_impl(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let body = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => {
                let comparisons = fields_named.named.iter().map(|field| {
                    let field_name = &field.ident;
                    quote! { TypeEq::type_eq(&self.#field_name, &other.#field_name) }
                });
                quote! { #(#comparisons)&&* }
            }
            Fields::Unnamed(fields_unnamed) => {
                let comparisons = fields_unnamed.unnamed.iter().enumerate().map(|(i, _)| {
                    let index = syn::Index::from(i);
                    quote! { TypeEq::type_eq(&self.#index, &other.#index) }
                });
                quote! { #(#comparisons)&&* }
            }
            Fields::Unit => quote! { true },
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
                            quote! { TypeEq::type_eq(#lhs, #rhs) }
                        });
                        quote! {
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
                            quote! { TypeEq::type_eq(#lhs, #rhs) }
                        });
                        quote! {
                            (#name::#variant_name(#(#lhs),*), #name::#variant_name(#(#rhs),*)) => {
                                #(#comparisons)&&*
                            }
                        }
                    }
                    Fields::Unit => {
                        quote! {
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
        impl #impl_generics TypeEq for #name #ty_generics #where_clause {
            fn type_eq(&self, other: &Self) -> bool {
                #body
            }
        }
    })
}
