/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use proc_macro2::TokenStream;
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

pub(crate) fn derive_visit(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    deriver(
        input,
        quote! { & },
        quote! { crate::util::visit::Visit },
        quote! { visit },
        quote! { fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a To)) },
    )
}

pub(crate) fn derive_visit_mut(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    deriver(
        input,
        quote! { &mut },
        quote! { crate::util::visit::VisitMut },
        quote! { visit_mut },
        quote! { fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut To)) },
    )
}

pub(crate) fn deriver(
    input: proc_macro::TokenStream,
    reff: TokenStream,
    trait_name: TokenStream,
    method_name: TokenStream,
    signature: TokenStream,
) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive_visit_impl(&input, reff, trait_name, method_name, signature) {
        Ok(x) => x.into_token_stream().into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn generics(
    generics: &Generics,
    trait_name: &TokenStream,
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
    let before = quote_spanned! { generics.span() => #(#ts: #trait_name<To>),* };
    let after = quote_spanned! { generics.span() => #(#ts),* };
    Ok((before, after))
}

fn derive_visit_impl(
    input: &DeriveInput,
    reff: TokenStream,
    trait_name: TokenStream,
    method_name: TokenStream,
    signature: TokenStream,
) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let (generics_before, generics_after) = generics(&input.generics, &trait_name)?;
    let (body, types) = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => {
                let fields = fields_named.named.iter().map(|x| &x.ident);
                (
                    quote_spanned! {fields_named.span() =>
                        #( #trait_name::#method_name(#reff self.#fields, f); )*
                    },
                    fields_named.named.iter().map(|x| &x.ty).collect(),
                )
            }
            Fields::Unnamed(fields_unnamed) => {
                let indicies = fields_unnamed
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, _)| syn::Index::from(i));
                (
                    quote_spanned! {fields_unnamed.span() =>
                        #( #trait_name::#method_name(#reff self.#indicies, f); )*
                    },
                    fields_unnamed.unnamed.iter().map(|x| &x.ty).collect(),
                )
            }
            Fields::Unit => (quote! {}, Vec::new()),
        },
        Data::Enum(data_enum) => {
            let (variants, types): (Vec<_>, Vec<_>) = data_enum
                .variants
                .iter()
                .map(|variant| {
                    let variant_name = &variant.ident;
                    match &variant.fields {
                        Fields::Named(fields_named) => {
                            let fields: Vec<_> =
                                fields_named.named.iter().map(|x| &x.ident).collect();
                            (
                                quote_spanned! { variant.span() =>
                                    #name::#variant_name { #(#fields),*  } => {
                                        #( #trait_name::#method_name(#fields, f); )*
                                    }
                                },
                                fields_named.named.iter().map(|x| &x.ty).collect(),
                            )
                        }
                        Fields::Unnamed(fields_unnamed) => {
                            let fields: Vec<_> = (0..fields_unnamed.unnamed.len())
                                .map(|i| format_ident!("x{i}"))
                                .collect();
                            (
                                quote_spanned! { variant.span() =>
                                    #name::#variant_name(#(#fields),*) => {
                                        #( #trait_name::#method_name(#fields, f); )*
                                    }
                                },
                                fields_unnamed.unnamed.iter().map(|x| &x.ty).collect(),
                            )
                        }
                        Fields::Unit => (
                            quote_spanned! { variant.span() => #name::#variant_name => {} },
                            Vec::new(),
                        ),
                    }
                })
                .unzip();
            (
                quote! {
                    match self {
                        #(#variants)*
                    }
                },
                types.into_iter().flatten().collect(),
            )
        }
        _ => {
            return Err(syn::Error::new_spanned(
                input,
                "Can't deal with this type of data",
            ));
        }
    };
    let contains = quote! { #(<#types as #trait_name<To>>::VISIT_CONTAINS || )* false };

    Ok(quote! {
        impl <To: 'static, #generics_before > #trait_name<To> for #name < #generics_after > where
            #(#types : #trait_name<To>,)*
         {
            const RECURSE_CONTAINS: bool = #contains;

            #signature {
                #body
            }
        }
    })
}
