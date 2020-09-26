use proc_macro::TokenStream;

#[proc_macro_derive(Serialize, attributes(declio))]
pub fn derive_serialize(inp: TokenStream) -> TokenStream {
    todo!()
}

#[proc_macro_derive(Deserialize, attributes(declio))]
pub fn derive_deserialize(inp: TokenStream) -> TokenStream {
    todo!()
}
