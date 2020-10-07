use darling::{ast, Error, FromDeriveInput, FromField, FromMeta, FromVariant};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, parse_quote, DeriveInput, Token};

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
    ctx: Asym<syn::LitStr>,

    #[darling(default)]
    id: Option<syn::LitStr>,

    #[darling(default)]
    id_type: Option<syn::LitStr>,

    #[darling(default)]
    id_ctx: Asym<syn::LitStr>,
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

        let ctx_parts: Punctuated<syn::FnArg, Token![,]> = self
            .ctx
            .encode()
            .map(|lit| lit.parse_with(Punctuated::parse_terminated))
            .transpose()
            .map_err(Error::custom)?
            .unwrap_or_else(Punctuated::new);

        let ctx_parts: Vec<syn::PatType> = ctx_parts
            .into_iter()
            .map(|pat| {
                if let syn::FnArg::Typed(pat_type) = pat {
                    Ok(pat_type)
                } else {
                    Err(Error::custom("expected a typed pattern, like `foo: i64`"))
                }
            })
            .collect::<Result<_, _>>()?;

        let ctx_pats = ctx_parts.iter().map(|pat_type| &pat_type.pat);
        let ctx_pat: syn::Pat = parse_quote! {
            ( #( #ctx_pats , )* )
        };

        let ctx_types = ctx_parts.iter().map(|pat_type| &pat_type.ty);
        let ctx_type: syn::Type = parse_quote! {
            ( #( #ctx_types , )* )
        };

        let variants = match data {
            ast::Data::Enum(variants) => variants.clone(),
            ast::Data::Struct(fields) => vec![Variant::from_struct(fields.clone())],
        };
        let id_type: TokenStream;
        let id_ctx: TokenStream;
        if data.is_struct() {
            id_type = match &self.id_type {
                None => quote!(()),
                _ => Error::unknown_field("id_type").write_errors(),
            };
            id_ctx = match &self.id_ctx.encode() {
                Some(_) => Error::unknown_field("id_ctx").write_errors(),
                _ => quote!(()),
            };
        } else {
            id_type = match &self.id_type {
                Some(id_type) => id_type.parse().unwrap_or_else(|e| e.to_compile_error()),
                _ => Error::missing_field("id_type").write_errors(),
            };
            id_ctx = match &self.id_ctx.encode() {
                Some(id_ctx) => id_ctx.parse().unwrap_or_else(|e| e.to_compile_error()),
                None => quote!(()),
            };
        }
        let variant_arms = variants.into_iter().map(|var| {
            var.generate_encode_arm(&crate_path, &id_type, &id_ctx, &writer_binding)
                .map(ToTokens::into_token_stream)
                .unwrap_or_else(Error::write_errors)
        });

        Ok(parse_quote! {
            impl #impl_generics #crate_path::Encode<#ctx_type> for #ident #ty_generics
                #where_clause
            {
                fn encode<W>(&self, #ctx_pat: #ctx_type, #writer_binding: &mut W)
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

        let ctx_parts: Punctuated<syn::FnArg, Token![,]> = self
            .ctx
            .decode()
            .map(|lit| lit.parse_with(Punctuated::parse_terminated))
            .transpose()
            .map_err(Error::custom)?
            .unwrap_or_else(Punctuated::new);

        let ctx_parts: Vec<syn::PatType> = ctx_parts
            .into_iter()
            .map(|pat| {
                if let syn::FnArg::Typed(pat_type) = pat {
                    Ok(pat_type)
                } else {
                    Err(Error::custom("expected a typed pattern, like `foo: i64`"))
                }
            })
            .collect::<Result<_, _>>()?;

        let ctx_pats = ctx_parts.iter().map(|pat_type| &pat_type.pat);
        let ctx_pat: syn::Pat = parse_quote! {
            ( #( #ctx_pats , )* )
        };

        let ctx_types = ctx_parts.iter().map(|pat_type| &pat_type.ty);
        let ctx_type: syn::Type = parse_quote! {
            ( #( #ctx_types , )* )
        };

        let id_expr: TokenStream;
        if data.is_struct() {
            id_expr = match &self.id {
                None => quote!(()),
                _ => Error::unknown_field("id").write_errors(),
            };
            if self.id_ctx.decode().is_some() {
                return Err(Error::unknown_field("id_ctx"));
            }
        } else {
            let id_type: TokenStream = match &self.id_type {
                Some(id_type) => id_type.parse().unwrap_or_else(|e| e.to_compile_error()),
                _ => Error::missing_field("id_type").write_errors(),
            };
            let id_ctx: TokenStream = match self.id_ctx.decode() {
                Some(id_ctx) => id_ctx.parse().unwrap_or_else(|e| e.to_compile_error()),
                None => quote!(()),
            };
            id_expr = match &self.id {
                Some(id) => id.parse().unwrap_or_else(|e| e.to_compile_error()),
                None => {
                    quote! {
                        <#id_type as #crate_path::Decode<_>>::decode(#id_ctx, #reader_binding)?
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
            impl #impl_generics #crate_path::Decode<#ctx_type> for #ident #ty_generics
                #where_clause
        {
                fn decode<R>(#ctx_pat: #ctx_type, #reader_binding: &mut R)
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
        id_ctx: &TokenStream,
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
                <#id_type as #crate_path::Encode<_>>::encode(&(#id_expr), #id_ctx, #writer_binding)?;
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
    ctx: Asym<syn::LitStr>,

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
        let Self { ty, .. } = self;

        let ctx: TokenStream = self
            .ctx
            .encode()
            .map(|lit| lit.parse().unwrap_or_else(|e| e.to_compile_error()))
            .unwrap_or_else(|| quote!(()));

        match (&self.with, &self.encode_with) {
            (None, None) => Ok(
                parse_quote! { <#ty as #crate_path::Encode<_>>::encode(#binding, #ctx, #writer_binding) },
            ),
            (Some(with), None) => {
                Ok(parse_quote! { #with::encode(#binding, #ctx, #writer_binding) })
            }
            (None, Some(encode_with)) => {
                Ok(parse_quote! { #encode_with(#binding, #ctx, #writer_binding) })
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
        let Self { ty, .. } = self;

        let ctx: TokenStream = self
            .ctx
            .decode()
            .map(|lit| lit.parse().unwrap_or_else(|e| e.to_compile_error()))
            .unwrap_or_else(|| quote!(()));

        match (&self.with, &self.decode_with) {
            (None, None) => {
                Ok(parse_quote! { <#ty as #crate_path::Decode<_>>::decode(#ctx, #reader_binding) })
            }
            (Some(with), None) => Ok(parse_quote! { #with::decode(#ctx, #reader_binding) }),
            (None, Some(decode_with)) => Ok(parse_quote! { #decode_with(#ctx, #reader_binding) }),
            _ => Err(Error::custom(
                "conflicting fields: `with` and `decode_with`",
            )),
        }
    }
}

#[derive(Debug, Clone)]
enum Asym<T> {
    Single(T),
    Multi {
        encode: Option<T>,
        decode: Option<T>,
    },
}

impl<T> Asym<T> {
    fn encode(&self) -> Option<&T> {
        match self {
            Self::Single(val) => Some(val),
            Self::Multi { encode, .. } => encode.as_ref(),
        }
    }

    fn decode(&self) -> Option<&T> {
        match self {
            Self::Single(val) => Some(val),
            Self::Multi { decode, .. } => decode.as_ref(),
        }
    }
}

impl<T> FromMeta for Asym<T>
where
    T: FromMeta + std::fmt::Debug,
{
    fn from_meta(item: &syn::Meta) -> Result<Self, Error> {
        match item {
            syn::Meta::List(value) => {
                Self::from_list(&value.nested.iter().cloned().collect::<Vec<_>>())
            }
            _ => T::from_meta(item).map(Self::Single),
        }
    }

    fn from_list(items: &[syn::NestedMeta]) -> Result<Self, Error> {
        let mut encode = None;
        let mut decode = None;

        let encode_path: syn::Path = parse_quote!(encode);
        let decode_path: syn::Path = parse_quote!(decode);

        for item in items {
            match item {
                syn::NestedMeta::Meta(meta) => match meta.path() {
                    path if *path == encode_path => {
                        if encode.is_none() {
                            encode = Some(T::from_meta(meta)?);
                        } else {
                            return Err(Error::duplicate_field_path(path));
                        }
                    }
                    path if *path == decode_path => {
                        if decode.is_none() {
                            decode = Some(T::from_meta(meta)?);
                        } else {
                            return Err(Error::duplicate_field_path(path));
                        }
                    }
                    other => return Err(Error::unknown_field_path(other)),
                },
                syn::NestedMeta::Lit(..) => return Err(Error::unsupported_format("literal")),
            }
        }
        Ok(Self::Multi { encode, decode })
    }
}

impl<T> Default for Asym<T> {
    fn default() -> Self {
        Self::Multi {
            encode: None,
            decode: None,
        }
    }
}
