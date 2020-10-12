use darling::{ast, Error, FromDeriveInput, FromField, FromMeta, FromVariant};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, parse_quote, DeriveInput, Token};

#[proc_macro_derive(Encode, attributes(declio))]
pub fn derive_encode(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    ContainerReceiver::from_derive_input(&input)
        .and_then(|receiver| receiver.validate())
        .map(|data| data.encode_impl().into_token_stream())
        .unwrap_or_else(|error| error.write_errors())
        .into()
}

#[proc_macro_derive(Decode, attributes(declio))]
pub fn derive_decode(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    ContainerReceiver::from_derive_input(&input)
        .and_then(|receiver| receiver.validate())
        .map(|data| data.decode_impl().into_token_stream())
        .unwrap_or_else(|error| error.write_errors())
        .into()
}

#[derive(FromDeriveInput)]
#[darling(attributes(declio))]
struct ContainerReceiver {
    ident: syn::Ident,
    generics: syn::Generics,
    data: ast::Data<VariantReceiver, FieldReceiver>,

    #[darling(default)]
    crate_path: Option<syn::Path>,

    #[darling(default)]
    ctx: Asym<syn::LitStr>,

    #[darling(default)]
    id_expr: Option<syn::LitStr>,

    #[darling(default)]
    id_type: Option<syn::LitStr>,

    #[darling(default)]
    id_ctx: Asym<syn::LitStr>,
}

struct ContainerData {
    ident: syn::Ident,
    generics: syn::Generics,
    crate_path: syn::Path,
    encode_ctx_pat: TokenStream,
    decode_ctx_pat: TokenStream,
    encode_ctx_type: TokenStream,
    decode_ctx_type: TokenStream,
    id_encode_ctx: TokenStream,
    id_decode_ctx: TokenStream,
    id_encoder: Option<TokenStream>,
    id_decoder: Option<TokenStream>,
    id_decode_expr: Option<TokenStream>,
    variants: Vec<VariantData>,
}

impl ContainerReceiver {
    fn validate(&self) -> Result<ContainerData, Error> {
        let mut errors = Vec::new();

        let ident = self.ident.clone();
        let generics = self.generics.clone();
        let crate_path = self
            .crate_path
            .clone()
            .unwrap_or_else(|| parse_quote!(declio));

        let mut parse_ctx = |arg: Option<&syn::LitStr>| match arg {
            None => (quote!(_), quote!(())),
            Some(lit) => {
                let parts: Punctuated<syn::FnArg, Token![,]> =
                    match lit.parse_with(Punctuated::parse_terminated) {
                        Ok(punct) => punct,
                        Err(error) => {
                            errors.push(from_syn_error(error));
                            Punctuated::new()
                        }
                    };

                let parts: Vec<_> = parts
                    .into_iter()
                    .flat_map(|fn_arg| match fn_arg {
                        syn::FnArg::Typed(pat_type) => Some((pat_type.pat, pat_type.ty)),
                        _ => {
                            errors.push(
                                Error::custom("expected name and type, like `foo: i64`")
                                    .with_span(&fn_arg),
                            );
                            None
                        }
                    })
                    .collect();

                let pats = parts.iter().map(|(pat, _)| pat);
                let types = parts.iter().map(|(_, ty)| ty);

                // Special case: single context variable gets to be not-a-tuple.
                if parts.len() == 1 {
                    (quote!( #( #pats )* ), quote!( #( #types )* ))
                } else {
                    (quote!( ( #( #pats , )* ) ), quote!( ( #( #types , )* ) ))
                }
            }
        };

        let (encode_ctx_pat, encode_ctx_type) = parse_ctx(self.ctx.encode());
        let (decode_ctx_pat, decode_ctx_type) = parse_ctx(self.ctx.decode());

        let (id_encoder, id_decoder, id_decode_expr) = match (&self.id_expr, &self.id_type) {
            (None, None) => (None, None, Some(quote!(()))),
            (Some(lit), None) => {
                let expr = match lit.parse() {
                    Ok(expr) => expr,
                    Err(error) => {
                        errors.push(from_syn_error(error));
                        quote!(unreachable!("compile error"))
                    }
                };
                (None, None, Some(expr))
            }
            (None, Some(lit)) => {
                let ty = match lit.parse() {
                    Ok(ty) => ty,
                    Err(error) => {
                        errors.push(from_syn_error(error));
                        quote!(())
                    }
                };
                (
                    Some(quote!( <#ty as #crate_path::Encode<_>>::encode )),
                    Some(quote!( <#ty as #crate_path::Decode<_>>::decode )),
                    None,
                )
            }
            (Some(..), Some(..)) => {
                errors.push(Error::custom(
                    "`id_expr` and `id_type` are incompatible with each other",
                ));
                (None, None, None)
            }
        };

        let mut parse_id_ctx = |arg: Option<&syn::LitStr>| match arg {
            None => quote!(()),
            Some(lit) => match lit.parse() {
                Ok(expr) => expr,
                Err(error) => {
                    errors.push(from_syn_error(error));
                    quote!(unreachable!("compile error"))
                }
            },
        };

        let id_encode_ctx = parse_id_ctx(self.id_ctx.encode());
        let id_decode_ctx = parse_id_ctx(self.id_ctx.decode());

        if self.data.is_struct() && self.id_expr.is_some() {
            errors.push(Error::unknown_field("id_expr"));
        }
        if self.data.is_struct() && self.id_type.is_some() {
            errors.push(Error::unknown_field("id_type"));
        }
        if self.data.is_enum() && self.id_expr.is_none() && self.id_type.is_none() {
            errors.push(Error::custom(
                "either `id_expr` or `id_type` is required for enums",
            ));
        }

        let variants = match &self.data {
            ast::Data::Enum(variants) => variants
                .iter()
                .flat_map(|variant| match variant.validate(&crate_path) {
                    Ok(data) => Some(data),
                    Err(error) => {
                        errors.push(error);
                        None
                    }
                })
                .collect(),
            ast::Data::Struct(fields) => match VariantData::from_struct(fields, &crate_path) {
                Ok(data) => vec![data],
                Err(error) => {
                    errors.push(error);
                    vec![]
                }
            },
        };

        if errors.is_empty() {
            Ok(ContainerData {
                ident,
                generics,
                crate_path,
                encode_ctx_pat,
                decode_ctx_pat,
                encode_ctx_type,
                decode_ctx_type,
                id_encode_ctx,
                id_decode_ctx,
                id_encoder,
                id_decoder,
                id_decode_expr,
                variants,
            })
        } else {
            Err(Error::multiple(errors))
        }
    }
}

impl ContainerData {
    fn encode_impl(&self) -> TokenStream {
        let Self {
            ident,
            crate_path,
            encode_ctx_pat,
            encode_ctx_type,
            ..
        } = self;
        let (impl_generics, ident_generics, where_clause) = self.generics.split_for_impl();
        let writer_binding = quote!(__declio_writer);

        let variant_arm = self.variants.iter().map(|variant| {
            variant.encode_arm(
                self.id_encoder.as_ref(),
                &self.id_encode_ctx,
                &self.crate_path,
                &writer_binding,
            )
        });

        quote! {
            #[allow(non_shorthand_field_patterns)]
            impl #impl_generics #crate_path::Encode<#encode_ctx_type> for #ident #ident_generics
                #where_clause
            {
                fn encode<W>(&self, #encode_ctx_pat: #encode_ctx_type, #writer_binding: &mut W)
                    -> Result<(), #crate_path::Error>
                where
                    W: #crate_path::export::io::Write,
                {
                    match self {
                        #( #variant_arm, )*
                    }
                }
            }
        }
    }

    fn decode_impl(&self) -> TokenStream {
        let Self {
            ident,
            crate_path,
            decode_ctx_pat,
            decode_ctx_type,
            ..
        } = self;
        let Self {
            id_decoder,
            id_decode_ctx,
            id_decode_expr,
            ..
        } = self;
        let (impl_generics, ident_generics, where_clause) = self.generics.split_for_impl();
        let reader_binding: TokenStream = quote!(__declio_reader);

        let variant_arm = self
            .variants
            .iter()
            .map(|variant| variant.decode_arm(&self.crate_path, &reader_binding));

        let id_decode_expr = match (id_decoder, id_decode_expr) {
            (Some(decoder), None) => quote! {
                #decoder(#id_decode_ctx, #reader_binding)
                    .map_err(|e| #crate_path::Error::with_context("error decoding enum id", e))?
            },
            (None, Some(decode_expr)) => quote!(#decode_expr),
            _ => unreachable!(),
        };

        quote! {
            impl #impl_generics #crate_path::Decode<#decode_ctx_type> for #ident #ident_generics
                #where_clause
            {
                fn decode<R>(#decode_ctx_pat: #decode_ctx_type, #reader_binding: &mut R)
                    -> Result<Self, #crate_path::Error>
                where
                    R: #crate_path::export::io::Read,
                {
                    match #id_decode_expr {
                        #( #variant_arm )*
                        _ => Err(#crate_path::Error::new("unknown id value")),
                    }
                }
            }
        }
    }
}

#[derive(FromVariant)]
#[darling(attributes(declio))]
struct VariantReceiver {
    ident: syn::Ident,
    fields: ast::Fields<FieldReceiver>,

    id: syn::LitStr,
}

struct VariantData {
    ident: Option<syn::Ident>,
    id_expr: TokenStream,
    id_pat: TokenStream,
    style: ast::Style,
    fields: Vec<FieldData>,
}

impl VariantReceiver {
    fn validate(&self, crate_path: &syn::Path) -> Result<VariantData, Error> {
        let mut errors = Vec::new();

        let ident = Some(self.ident.clone());

        let id_expr = match self.id.parse() {
            Ok(expr) => expr,
            Err(error) => {
                errors.push(from_syn_error(error));
                quote!(unreachable!("compile error"))
            }
        };

        let id_pat = quote!(__declio_id if __declio_id == #id_expr);

        let style = self.fields.style;

        let fields = self
            .fields
            .iter()
            .enumerate()
            .flat_map(|(index, field)| match field.validate(crate_path, index) {
                Ok(field) => Some(field),
                Err(error) => {
                    errors.push(error);
                    None
                }
            })
            .collect();

        if errors.is_empty() {
            Ok(VariantData {
                ident,
                id_expr,
                id_pat,
                style,
                fields,
            })
        } else {
            Err(Error::multiple(errors))
        }
    }
}

impl VariantData {
    fn from_struct(
        fields: &ast::Fields<FieldReceiver>,
        crate_path: &syn::Path,
    ) -> Result<VariantData, Error> {
        let mut errors = Vec::new();

        let ident = None;
        let id_expr = quote!(());
        let id_pat = quote!(_);
        let style = fields.style;

        let fields = fields
            .iter()
            .enumerate()
            .flat_map(|(index, field)| match field.validate(crate_path, index) {
                Ok(field) => Some(field),
                Err(error) => {
                    errors.push(error);
                    None
                }
            })
            .collect();

        if errors.is_empty() {
            Ok(VariantData {
                ident,
                id_expr,
                id_pat,
                style,
                fields,
            })
        } else {
            Err(Error::multiple(errors))
        }
    }

    fn encode_arm(
        &self,
        id_encoder: Option<&TokenStream>,
        id_encode_ctx: &TokenStream,
        crate_path: &syn::Path,
        writer_binding: &TokenStream,
    ) -> TokenStream {
        let Self { id_expr, .. } = self;

        let path = match &self.ident {
            Some(ident) => quote!(Self::#ident),
            None => quote!(Self),
        };

        let field_pat = self.fields.iter().map(|field| {
            let FieldData {
                stored_ident,
                public_ref_ident,
                ..
            } = field;
            match stored_ident {
                Some(stored_ident) => quote!(#stored_ident: #public_ref_ident),
                None => quote!(#public_ref_ident),
            }
        });
        let pat_fields = match self.style {
            ast::Style::Tuple => quote!( ( #( #field_pat, )* ) ),
            ast::Style::Struct => quote!( { #( #field_pat, )* } ),
            ast::Style::Unit => quote!(),
        };

        let id_encode_stmt = id_encoder.map(|encoder| {
            quote! {
                #encoder(&(#id_expr), #id_encode_ctx, #writer_binding)
                    .map_err(|e| #crate_path::Error::with_context("error encoding enum id", e))?;
            }
        });

        let field_encode_expr = self
            .fields
            .iter()
            .map(|field| field.encode_expr(crate_path, writer_binding));

        quote! {
            #path #pat_fields => {
                #id_encode_stmt
                #( #field_encode_expr; )*
                Ok(())
            }
        }
    }

    fn decode_arm(&self, crate_path: &syn::Path, reader_binding: &TokenStream) -> TokenStream {
        let Self { id_pat, .. } = self;

        let private_owned_ident = self.fields.iter().map(|field| &field.private_owned_ident);
        let public_ref_ident = self.fields.iter().map(|field| &field.public_ref_ident);
        let field_decode_expr = self
            .fields
            .iter()
            .map(|field| field.decode_expr(crate_path, reader_binding));

        let path = match &self.ident {
            Some(ident) => quote!(Self::#ident),
            None => quote!(Self),
        };

        let field_cons = self.fields.iter().map(|field| {
            let FieldData {
                stored_ident,
                private_owned_ident,
                ..
            } = field;
            match stored_ident {
                Some(stored_ident) => quote!(#stored_ident: #private_owned_ident),
                None => quote!(#private_owned_ident),
            }
        });
        let cons_fields = match self.style {
            ast::Style::Tuple => quote!( ( #( #field_cons, )* ) ),
            ast::Style::Struct => quote!( { #( #field_cons, )* } ),
            ast::Style::Unit => quote!(),
        };

        quote! {
            #id_pat => {
                #(
                    let #private_owned_ident = #field_decode_expr;
                    #[allow(unused_variables)]
                    let #public_ref_ident = &#private_owned_ident;
                )*
                Ok(#path #cons_fields)
            }
        }
    }
}

#[derive(FromField)]
#[darling(attributes(declio))]
struct FieldReceiver {
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

    #[darling(default)]
    skip_if: Option<syn::LitStr>,
}

struct FieldData {
    stored_ident: Option<syn::Ident>,
    public_ref_ident: syn::Ident,
    private_owned_ident: syn::Ident,
    encode_ctx: TokenStream,
    decode_ctx: TokenStream,
    encoder: TokenStream,
    decoder: TokenStream,
    skip_if: Option<TokenStream>,
}

impl FieldReceiver {
    fn validate(&self, crate_path: &syn::Path, index: usize) -> Result<FieldData, Error> {
        let Self { ty, .. } = self;
        let mut errors = Vec::new();

        let stored_ident = self.ident.clone();
        let public_ref_ident = match &self.ident {
            Some(ident) => ident.clone(),
            None => format_ident!("field_{}", index),
        };
        let private_owned_ident = format_ident!("__declio_owned_{}", public_ref_ident);

        let encode_ctx = match self.ctx.encode() {
            Some(lit) => match lit.parse() {
                Ok(expr) => expr,
                Err(err) => {
                    errors.push(from_syn_error(err));
                    quote!(unreachable!("compile error"))
                }
            },
            None => quote!(()),
        };

        let decode_ctx = match self.ctx.decode() {
            Some(lit) => match lit.parse() {
                Ok(expr) => expr,
                Err(err) => {
                    errors.push(from_syn_error(err));
                    quote!(unreachable!("compile error"))
                }
            },
            None => quote!(()),
        };

        let encoder = match (&self.encode_with, &self.with) {
            (None, None) => quote!(<#ty as #crate_path::Encode<_>>::encode),
            (Some(encode_with), None) => quote!(#encode_with),
            (None, Some(with)) => quote!(#with::encode),
            _ => {
                errors.push(Error::custom(
                    "`encode_with` and `with` are incompatible with each other",
                ));
                quote!(__compile_error_throwaway)
            }
        };

        let decoder = match (&self.decode_with, &self.with) {
            (None, None) => quote!(<#ty as #crate_path::Decode<_>>::decode),
            (Some(decode_with), None) => quote!(#decode_with),
            (None, Some(with)) => quote!(#with::decode),
            _ => {
                errors.push(Error::custom(
                    "`decode_with` and `with` are incompatible with each other",
                ));
                quote!(__compile_error_throwaway)
            }
        };

        let skip_if = match &self.skip_if {
            Some(lit) => match lit.parse() {
                Ok(expr) => Some(expr),
                Err(error) => {
                    errors.push(from_syn_error(error));
                    Some(quote!(unreachable!("compile error")))
                }
            },
            None => None,
        };

        if errors.is_empty() {
            Ok(FieldData {
                stored_ident,
                public_ref_ident,
                private_owned_ident,
                encode_ctx,
                decode_ctx,
                encoder,
                decoder,
                skip_if,
            })
        } else {
            Err(Error::multiple(errors))
        }
    }
}

impl FieldData {
    fn encode_expr(&self, crate_path: &syn::Path, writer_binding: &TokenStream) -> TokenStream {
        let Self {
            public_ref_ident,
            encoder,
            encode_ctx,
            ..
        } = self;
        let error_context = format!("error encoding field {}", public_ref_ident);
        let raw_encoder = quote! {
            #encoder(#public_ref_ident, #encode_ctx, #writer_binding)
                .map_err(|e| #crate_path::Error::with_context(#error_context, e))?
        };
        match &self.skip_if {
            Some(skip_if) => quote! {
                if #skip_if {
                    ()
                } else {
                    #raw_encoder
                }
            },
            None => raw_encoder,
        }
    }

    fn decode_expr(&self, crate_path: &syn::Path, reader_binding: &TokenStream) -> TokenStream {
        let Self {
            public_ref_ident,
            decode_ctx,
            decoder,
            ..
        } = self;
        let error_context = format!("error decoding field {}", public_ref_ident);
        let raw_decoder = quote! {
            #decoder(#decode_ctx, #reader_binding)
                .map_err(|e| #crate_path::Error::with_context(#error_context, e))?
        };
        match &self.skip_if {
            Some(skip_if) => quote! {
                if #skip_if {
                    Default::default()
                } else {
                    #raw_decoder
                }
            },
            None => raw_decoder,
        }
    }
}

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

fn from_syn_error(err: syn::Error) -> Error {
    Error::custom(&err).with_span(&err.span())
}
