use proc_macro::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Serialize, attributes(declio))]
pub fn derive_serialize(input: TokenStream) -> TokenStream {
    let DeriveInput {
        ident,
        generics,
        data,
        ..
    } = parse_macro_input!(input as DeriveInput);

    let serialize_body = match &data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => {
            let field_pats = fields.iter().enumerate().map(|(index, field)| {
                if let Some(ident) = &field.ident {
                    quote! { #ident, }
                } else {
                    let ident = format_ident!("field_{}", index);
                    quote! { #ident, }
                }
            });

            let pat = match fields {
                syn::Fields::Named(..) => quote! { { #( #field_pats )* } },
                syn::Fields::Unnamed(..) => quote! { ( #( #field_pats )* ) },
                syn::Fields::Unit => quote! {},
            };

            let field_writes = fields.iter().enumerate().map(|(index, field)| {
                let syn::Field { ident, ty, .. } = field;
                let span = field.span();

                let accessor = if let Some(ident) = ident {
                    quote! { #ident }
                } else {
                    let ident = format_ident!("field_{}", index);
                    quote! { #ident }
                };

                quote_spanned! {span=>
                    <#ty as declio::Serialize>::serialize(#accessor, (), writer)?;
                }
            });

            quote! {
                let Self #pat = self;
                #( #field_writes )*
                Ok(())
            }
        }
        syn::Data::Enum(syn::DataEnum { variants, .. }) => {
            let variant_arms = variants.iter().map(|variant| {
                let syn::Variant { ident, fields, .. } = variant;
                let span = variant.span();

                let field_pats = fields.iter().enumerate().map(|(index, field)| {
                    if let Some(ident) = &field.ident {
                        quote! { #ident, }
                    } else {
                        let ident = format_ident!("field_{}", index);
                        quote! { #ident, }
                    }
                });

                let pat = match fields {
                    syn::Fields::Named(..) => quote! { { #( #field_pats )* } },
                    syn::Fields::Unnamed(..) => quote! { ( #( #field_pats )* ) },
                    syn::Fields::Unit => quote! {},
                };

                let field_writes = fields.iter().enumerate().map(|(index, field)| {
                    let syn::Field { ident, ty, .. } = field;
                    let span = field.span();

                    let accessor = if let Some(ident) = ident {
                        quote! { #ident }
                    } else {
                        let ident = format_ident!("field_{}", index);
                        quote! { #ident }
                    };

                    quote_spanned! {span=>
                        <#ty as declio::Serialize>::serialize(#accessor, (), writer)?;
                    }
                });

                quote_spanned! {span=>
                    Self::#ident #pat => {
                        #( #field_writes )*
                        Ok(())
                    }
                }
            });

            quote! {
                match self {
                    #( #variant_arms )*
                }
            }
        }
        syn::Data::Union(..) => todo!(),
    };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    TokenStream::from(quote! {
        impl #impl_generics ::declio::Serialize for #ident #ty_generics #where_clause {
            fn serialize<W>(&self, _: (), writer: &mut W) -> Result<(), ::std::io::Error>
            where
                W: ::std::io::Write,
            {
                #serialize_body
            }
        }
    })
}

#[proc_macro_derive(Deserialize, attributes(declio))]
pub fn derive_deserialize(inp: TokenStream) -> TokenStream {
    todo!()
}
