use darling::util::Override;
use darling::{FromDeriveInput, FromField, FromVariant};

#[derive(FromDeriveInput)]
#[darling(attributes(declio), supports(struct_any, enum_any))]
struct Container {
    ident: syn::Ident,
    generics: syn::Generics,
    data: darling::ast::Data<Variant, Field>,

    #[darling(default, rename = "crate")]
    crate_path: Option<syn::Path>,

    #[darling(default)]
    ctx: Option<syn::LitStr>,

    #[darling(default)]
    default: Option<Override<syn::LitStr>>,

    #[darling(default)]
    bound: Option<Vec<syn::WherePredicate>>,

    #[darling(default)]
    id: Option<syn::LitStr>,

    #[darling(default)]
    id_type: Option<syn::Path>,
}

#[derive(FromVariant)]
#[darling(attributes(declio))]
struct Variant {
    ident: syn::Ident,

    #[darling(default)]
    id: Option<syn::LitStr>,

    #[darling(default)]
    id_pat: Option<syn::LitStr>,

    #[darling(default)]
    from: Option<syn::Path>,

    #[darling(default)]
    into: Option<syn::Path>,

    #[darling(default)]
    serialize_with: Option<syn::Path>,

    #[darling(default)]
    deserialize_with: Option<syn::Path>,

    #[darling(default)]
    with: Option<syn::Path>,

    #[darling(default)]
    ctx: Option<syn::LitStr>,
}

#[derive(FromField)]
#[darling(attributes(declio))]
struct Field {
    ident: Option<syn::Ident>,
    ty: syn::Type,

    #[darling(default)]
    skip_if: Option<syn::LitStr>,

    #[darling(default)]
    default: Option<syn::LitStr>,

    #[darling(default)]
    from: Option<syn::Path>,

    #[darling(default)]
    into: Option<syn::Path>,

    #[darling(default)]
    serialize_with: Option<syn::Path>,

    #[darling(default)]
    deserialize_with: Option<syn::Path>,

    #[darling(default)]
    with: Option<syn::Path>,

    #[darling(default)]
    ctx: Option<syn::LitStr>,
}

#[proc_macro_derive(Serialize, attributes(declio))]
pub fn derive_serialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    todo!()
}

#[proc_macro_derive(Deserialize, attributes(declio))]
pub fn derive_deserialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    todo!()
}
