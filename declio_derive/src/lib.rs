use darling::{ast, Error, FromDeriveInput, FromField, FromVariant};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::parse_quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Encode, attributes(declio))]
pub fn derive_encode(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    Container::from_derive_input(&input)
        .and_then(|c| c.impl_encode())
        .map(ToTokens::into_token_stream)
        .unwrap_or_else(Error::write_errors)
        .into()
}

#[proc_macro_derive(Decode, attributes(declio))]
pub fn derive_decode(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    Container::from_derive_input(&input)
        .and_then(|c| c.impl_decode())
        .map(ToTokens::into_token_stream)
        .unwrap_or_else(Error::write_errors)
        .into()
}

#[derive(FromDeriveInput)]
#[darling(attributes(declio))]
struct Container {
    ident: syn::Ident,
    generics: syn::Generics,
    data: ast::Data<Variant, Field>,

    #[darling(default)]
    crate_path: Option<syn::Path>,

    #[darling(default)]
    id: Option<syn::LitStr>,

    #[darling(default)]
    id_type: Option<syn::LitStr>,
}

impl Container {
    fn impl_encode(&self) -> Result<syn::ItemImpl, Error> {
        let Self {
            ident,
            generics,
            data,
            crate_path,
            ..
        } = self;

        let crate_path = crate_path.clone().unwrap_or_else(|| parse_quote!(declio));
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        let writer_binding: syn::Ident = parse_quote!(writer);

        let variants = match data {
            ast::Data::Enum(variants) => variants.clone(),
            ast::Data::Struct(fields) => vec![Variant::from_struct(fields.clone())],
        };
        let id_type: TokenStream;
        if data.is_struct() {
            id_type = match &self.id_type {
                None => quote!(()),
                _ => Error::unknown_field("id_type").write_errors(),
            };
        } else {
            id_type = match &self.id_type {
                Some(id_type) => id_type.parse().unwrap_or_else(|e| e.to_compile_error()),
                _ => Error::missing_field("id_type").write_errors(),
            };
        }
        let variant_arms = variants.into_iter().map(|var| {
            var.generate_encode_arm(&crate_path, &id_type, &writer_binding)
                .map(ToTokens::into_token_stream)
                .unwrap_or_else(Error::write_errors)
        });

        Ok(parse_quote! {
            impl #impl_generics #crate_path::Encode<()> for #ident #ty_generics
                #where_clause
            {
                fn encode<W>(&self, _: (), #writer_binding: &mut W)
                    -> Result<(), #crate_path::export::io::Error>
                where
                    W: #crate_path::export::io::Write,
                {
                    match self {
                        #( #variant_arms )*
                    }
                }
            }
        })
    }

    fn impl_decode(&self) -> Result<syn::ItemImpl, Error> {
        let Self {
            ident,
            generics,
            data,
            crate_path,
            ..
        } = self;

        let crate_path = crate_path.clone().unwrap_or_else(|| parse_quote!(declio));
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        let reader_binding: syn::Ident = parse_quote!(reader);

        let id_expr: TokenStream;
        let id_type: TokenStream;
        if data.is_struct() {
            id_expr = match &self.id {
                None => quote!(()),
                _ => Error::unknown_field("id").write_errors(),
            };
        } else {
            id_type = match &self.id_type {
                Some(id_type) => id_type.parse().unwrap_or_else(|e| e.to_compile_error()),
                _ => Error::missing_field("id_type").write_errors(),
            };
            id_expr = match &self.id {
                Some(id) => id.parse().unwrap_or_else(|e| e.to_compile_error()),
                None => {
                    quote! {
                        <#id_type as #crate_path::Decode>::decode((), #reader_binding)?
                    }
                }
            };
        }

        let variants = match data {
            ast::Data::Enum(variants) => variants.clone(),
            ast::Data::Struct(fields) => vec![Variant::from_struct(fields.clone())],
        };
        let variant_arms = variants.into_iter().map(|var| {
            var.generate_decode_arm(&crate_path, &reader_binding)
                .map(ToTokens::into_token_stream)
                .unwrap_or_else(Error::write_errors)
        });

        Ok(parse_quote! {
            impl #impl_generics #crate_path::Decode<()> for #ident #ty_generics
                #where_clause
        {
                fn decode<R>(_: (), #reader_binding: &mut R)
                    -> Result<Self, #crate_path::export::io::Error>
                where
                    R: #crate_path::export::io::Read,
                {
                    let id = #id_expr;
                    match id {
                        #( #variant_arms )*
                        _ => Err(#crate_path::export::io::Error::new(
                            #crate_path::export::io::ErrorKind::InvalidData,
                            "unknown id"
                        ))
                    }
                }
            }
        })
    }
}

#[derive(Clone, FromVariant)]
#[darling(attributes(declio))]
struct Variant {
    ident: syn::Ident,
    fields: ast::Fields<Field>,

    #[darling(skip)]
    from_struct: bool,

    id: syn::LitStr,
}

impl Variant {
    fn from_struct(fields: ast::Fields<Field>) -> Self {
        Self {
            ident: parse_quote!(__declio_unused),
            fields,
            from_struct: true,
            id: syn::LitStr::new("()", Span::call_site()),
        }
    }

    fn id_expr(&self) -> Result<syn::Expr, Error> {
        self.id.parse().map_err(Error::custom)
    }

    fn generate_encode_arm(
        &self,
        crate_path: &syn::Path,
        id_type: &TokenStream,
        writer_binding: &syn::Ident,
    ) -> Result<syn::Arm, Error> {
        let Self {
            ident,
            fields,
            from_struct,
            ..
        } = self;

        let id_expr = self
            .id_expr()
            .map(ToTokens::into_token_stream)
            .unwrap_or_else(Error::write_errors);

        let path: syn::Path;
        if *from_struct {
            path = parse_quote!(Self);
        } else {
            path = parse_quote!(Self::#ident);
        }

        let pat_fields_inner = fields
            .iter()
            .enumerate()
            .map(|(index, field)| field.ident(index));
        let pat_fields = match fields.style {
            ast::Style::Tuple => quote! {
                ( #( #pat_fields_inner, )* )
            },
            ast::Style::Struct => quote! {
                { #( #pat_fields_inner, )* }
            },
            ast::Style::Unit => quote! {},
        };

        let field_encoders = fields.iter().enumerate().map(|(index, field)| {
            field
                .generate_encoder(&crate_path, &field.ident(index), writer_binding)
                .map(ToTokens::into_token_stream)
                .unwrap_or_else(Error::write_errors)
        });

        Ok(parse_quote! {
            #path #pat_fields => {
                <#id_type as #crate_path::Encode>::encode(&(#id_expr), (), #writer_binding)?;
                #( #field_encoders ?; )*
                Ok(())
            }
        })
    }

    fn generate_decode_arm(
        &self,
        crate_path: &syn::Path,
        reader_binding: &syn::Ident,
    ) -> Result<syn::Arm, Error> {
        let Self {
            ident,
            fields,
            from_struct,
            ..
        } = self;

        let id_expr = self
            .id_expr()
            .map(ToTokens::into_token_stream)
            .unwrap_or_else(Error::write_errors);

        let path: syn::Path;
        // XXX: would be syn::Pat, but guards aren't first-class patterns,
        // only supported as part of a match arm.
        let id_pat: TokenStream;
        if *from_struct {
            path = parse_quote!(Self);
            id_pat = parse_quote!(());
        } else {
            path = parse_quote!(Self::#ident);
            id_pat = parse_quote!(id if id == #id_expr)
        }

        let cons_fields_inner = fields
            .iter()
            .enumerate()
            .map(|(index, field)| field.ident(index));
        let cons_fields = match fields.style {
            ast::Style::Tuple => quote! {
                ( #( #cons_fields_inner, )* )
            },
            ast::Style::Struct => quote! {
                { #( #cons_fields_inner, )* }
            },
            ast::Style::Unit => quote! {},
        };

        let field_decoders = fields.iter().enumerate().map(|(index, field)| {
            let binding = field.ident(index);
            let decoder = field
                .generate_decoder(&crate_path, reader_binding)
                .map(ToTokens::into_token_stream)
                .unwrap_or_else(Error::write_errors);
            quote! { let #binding = #decoder ?; }
        });

        Ok(parse_quote! {
            #id_pat => {
                #( #field_decoders )*
                Ok(#path #cons_fields)
            }
        })
    }
}

#[derive(Clone, FromField)]
#[darling(attributes(declio))]
struct Field {
    ident: Option<syn::Ident>,
    ty: syn::Type,

    #[darling(default)]
    with: Option<syn::Path>,

    #[darling(default)]
    encode_with: Option<syn::Path>,

    #[darling(default)]
    decode_with: Option<syn::Path>,
}

impl Field {
    fn ident(&self, index: usize) -> syn::Ident {
        self.ident
            .clone()
            .unwrap_or_else(|| format_ident!("field_{}", index))
    }

    fn generate_encoder(
        &self,
        crate_path: &syn::Path,
        binding: &syn::Ident,
        writer_binding: &syn::Ident,
    ) -> Result<syn::Expr, Error> {
        let Self {
            ty,
            with,
            encode_with,
            ..
        } = self;
        match (with, encode_with) {
            (None, None) => Ok(
                parse_quote! { <#ty as #crate_path::Encode>::encode(#binding, (), #writer_binding) },
            ),
            (Some(with), None) => {
                Ok(parse_quote!({
                    // sanitize scope
                    fn __encode<__W>(__val: &#ty, _: (), __writer: &mut __W)
                        -> Result<(), #crate_path::export::io::Error>
                    where
                        __W: #crate_path::export::io::Write,
                    {
                        #with::encode(__val, (), __writer)
                    }
                    __encode(#binding, (), #writer_binding)
                }))
            }
            (None, Some(encode_with)) => {
                Ok(parse_quote!({
                    // sanitize scope
                    fn __encode<__W>(__val: &#ty, _: (), __writer: &mut __W)
                        -> Result<(), #crate_path::export::io::Error>
                    where
                        __W: #crate_path::export::io::Write,
                    {
                        #encode_with(__val, (), __writer)
                    }
                    __encode(#binding, (), #writer_binding)
                }))
            }
            _ => Err(Error::custom(
                "conflicting fields: `with` and `encode_with`",
            )),
        }
    }

    fn generate_decoder(
        &self,
        crate_path: &syn::Path,
        reader_binding: &syn::Ident,
    ) -> Result<syn::Expr, Error> {
        let Self {
            ty,
            with,
            decode_with,
            ..
        } = self;
        match (with, decode_with) {
            (None, None) => {
                Ok(parse_quote! { <#ty as #crate_path::Decode>::decode((), #reader_binding) })
            }
            (Some(with), None) => {
                Ok(parse_quote!({
                    // sanitize scope
                    fn __decode<__R>(_: (), __reader: &mut __R)
                        -> Result<#ty, #crate_path::export::io::Error>
                    where
                        __R: #crate_path::export::io::Read,
                    {
                        #with::decode((), __reader)
                    }
                    __decode((), #reader_binding)
                }))
            }
            (None, Some(decode_with)) => {
                Ok(parse_quote!({
                    // sanitize scope
                    fn __decode<__R>(_: (), __reader: &mut __R)
                        -> Result<#ty, #crate_path::export::io::Error>
                    where
                        __R: #crate_path::export::io::Read,
                    {
                        #decode_with((), __reader)
                    }
                    __decode((), #reader_binding)
                }))
            }
            _ => Err(Error::custom(
                "conflicting fields: `with` and `decode_with`",
            )),
        }
    }
}
