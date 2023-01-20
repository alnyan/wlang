use proc_macro::TokenStream;
use proc_macro2::{Ident, Literal, TokenStream as TokenStream2};
use quote::quote;
use syn::{parse::Parse, parse_macro_input, spanned::Spanned, Attribute, Data, DeriveInput};

#[derive(Debug, Clone)]
enum ToFromAttributes {
    Character(char),
    String(String),
}

impl Parse for ToFromAttributes {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let value = input.parse::<Literal>()?;
        if input.is_empty() {
            let literal = litrs::Literal::from(&value);

            match literal {
                litrs::Literal::Char(c) => Ok(Self::Character(c.value())),
                litrs::Literal::String(s) => Ok(Self::String(s.value().to_owned())),
                _ => Err(syn::Error::new(
                    value.span(),
                    "to_from is not implemented for this kind of value",
                )),
            }
        } else {
            Err(syn::Error::new(
                input.span(),
                "to_from only accepts one argument",
            ))
        }
    }
}

impl ToFromAttributes {
    fn as_character(&self) -> char {
        match self {
            Self::Character(c) => *c,
            _ => panic!("Not a character: {self:?}"),
        }
    }
    fn as_string(&self) -> String {
        match self {
            Self::String(s) => s.clone(),
            _ => panic!("Not a string: {self:?}"),
        }
    }

    fn parse(attr: &Attribute) -> Option<syn::Result<Self>> {
        let Some(ident) = attr.path.get_ident() else {
            return None;
        };
        if ident != "to_from" {
            return None;
        }

        Some(attr.parse_args())
    }
}

fn impl_from_items(variants: &[(Ident, ToFromAttributes)]) -> (Option<TokenStream2>, TokenStream2) {
    // TODO check that the values are same type
    if variants.is_empty() {
        return (None, TokenStream2::new());
    }
    let (_, first) = &variants[0];

    match first {
        ToFromAttributes::Character(_) => (
            Some(quote! { char }),
            variants
                .iter()
                .map(|(name, value)| {
                    let value = value.as_character();

                    quote! {
                        #value => Ok(Self::#name),
                    }
                })
                .collect(),
        ),
        ToFromAttributes::String(_) => (
            Some(quote! { &str }),
            variants
                .iter()
                .map(|(name, value)| {
                    let value = value.as_string();

                    quote! {
                        #value => Ok(Self::#name),
                    }
                })
                .collect(),
        ),
    }
}

fn impl_to_items(variants: &[(Ident, ToFromAttributes)]) -> (Option<TokenStream2>, TokenStream2) {
    // TODO check that the values are same type
    if variants.is_empty() {
        return (None, TokenStream2::new());
    }
    let (_, first) = &variants[0];

    match first {
        ToFromAttributes::Character(_) => (
            Some(quote! { char }),
            variants
                .iter()
                .map(|(name, value)| {
                    let value = value.as_character();

                    quote! {
                        Self::#name => #value,
                    }
                })
                .collect(),
        ),
        ToFromAttributes::String(_) => (
            Some(quote! { &'static str }),
            variants
                .iter()
                .map(|(name, value)| {
                    let value = value.as_string();

                    quote! {
                        Self::#name => #value,
                    }
                })
                .collect(),
        ),
    }
}

// TODO impl "to"
fn impl_to_from(ident: &Ident, variants: &[(Ident, ToFromAttributes)]) -> TokenStream2 {
    let (from_ty, from_items) = impl_from_items(variants);
    let (to_ty, to_items) = impl_to_items(variants);

    let mut result = TokenStream2::new();

    if let Some(from_ty) = from_ty {
        result.extend(quote! {
            impl ::core::convert::TryFrom<#from_ty> for #ident {
                type Error = ();

                fn try_from(value: #from_ty) -> Result<Self, <Self as ::core::convert::TryFrom<#from_ty>>::Error> {
                    match value {
                        #from_items
                        _ => Err(())
                    }
                }
            }
        });
    }

    if let Some(to_ty) = to_ty {
        result.extend(quote! {
            impl ::core::convert::Into<#to_ty> for #ident {
                fn into(self) -> #to_ty {
                    match self {
                        #to_items
                    }
                }
            }
        });
    }

    result
}

#[proc_macro_derive(ToFromEnum, attributes(to_from))]
pub fn to_from_enum(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, data, .. } = parse_macro_input!(input);

    let Data::Enum(data) = data else {
        todo!()
    };

    let mut output = TokenStream2::new();
    let mut errors = TokenStream2::new();
    let mut variants = vec![];
    for variant in data.variants {
        let to_from = variant
            .attrs
            .iter()
            .filter_map(ToFromAttributes::parse)
            .collect::<Result<Vec<_>, _>>();

        let to_from = match to_from {
            Ok(items) => {
                if items.is_empty() {
                    errors.extend(
                        syn::Error::new(variant.span(), "No to_from attributes specified")
                            .to_compile_error(),
                    );
                    continue;
                }
                if items.len() != 1 {
                    errors.extend(
                        syn::Error::new(variant.span(), "Multiple to_from attributes")
                            .to_compile_error(),
                    );
                    continue;
                }

                items[0].clone()
            }
            Err(e) => {
                errors.extend(e.to_compile_error());
                continue;
            }
        };

        variants.push((variant.ident, to_from));
    }

    if errors.is_empty() {
        output.extend(impl_to_from(&ident, &variants));
    }
    output.extend(errors);

    output.into()
}
