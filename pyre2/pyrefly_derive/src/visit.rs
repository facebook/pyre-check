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

pub(crate) fn derive_visit(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match derive_visit_impl(&input) {
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
    let before = quote_spanned! { generics.span() => #(#ts: crate::util::visit::Visit<To>),* };
    let after = quote_spanned! { generics.span() => #(#ts),* };
    Ok((before, after))
}

fn derive_visit_impl(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = &input.ident;
    let (generics_before, generics_after) = generics(&input.generics)?;
    let visit = quote! { crate::util::visit::Visit };
    let body = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => {
                let fields = fields_named.named.iter().map(|x| &x.ident);
                quote_spanned! {fields_named.span() =>
                    #( #visit::visit0(&self.#fields, f); )*
                }
            }
            Fields::Unnamed(fields_unnamed) => {
                let indicies = fields_unnamed
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, _)| syn::Index::from(i));
                quote_spanned! {fields_unnamed.span() =>
                    #( #visit::visit0(&self.#indicies, f); )*
                }
            }
            Fields::Unit => quote! {},
        },
        Data::Enum(data_enum) => {
            let variants = data_enum.variants.iter().map(|variant| {
                let variant_name = &variant.ident;
                match &variant.fields {
                    Fields::Named(fields_named) => {
                        let fields: Vec<_> = fields_named.named.iter().map(|x| &x.ident).collect();
                        quote_spanned! { variant.span() =>
                            #name::#variant_name { #(#fields),*  } => {
                                #( #visit::visit0(#fields, f); )*
                            }
                        }
                    }
                    Fields::Unnamed(fields_unnamed) => {
                        let fields: Vec<_> = (0..fields_unnamed.unnamed.len())
                            .map(|i| format_ident!("x{i}"))
                            .collect();
                        quote_spanned! { variant.span() =>
                            #name::#variant_name(#(#fields),*) => {
                                #( #visit::visit0(#fields, f); )*
                            }
                        }
                    }
                    Fields::Unit => {
                        quote_spanned! { variant.span() => #name::#variant_name => {} }
                    }
                }
            });
            quote! {
                match self {
                    #(#variants)*
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
        impl <To: 'static, #generics_before > #visit<To> for #name < #generics_after > {
            fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
                #body
            }
        }
    })
}
