mod data;

use self::data::Container;
use darling::{Error, FromDeriveInput};
use quote::ToTokens;
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
