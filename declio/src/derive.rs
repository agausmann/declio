//! Derive macros.
//!
//! **Note:** The macros themselves are not contained in this module; they are at the top level of
//! the crate. This module is used to document them.
//!
//! The `Encode` and `Decode` macros generate implementations of their respective traits.
//! For structs, each field of the struct is encoded or decoded in the order they are listed.
//! For enums, each variant is encoded and decoded as if it were a struct, but some additional
//! attributes are required to know which variant to pick when decoding. Specifically, either
//! `id_type` or `id_expr` must be specified outside the enum, and an `id` expression must be
//! specified for each variant. See below for more information.
//!
//! # Attributes
//!
//! The implementation can be modified by attributes at several levels:
//!
//! - **Container** attributes are applied to the outside of the struct or enum.
//! - **Variant** attributes are applied to the outside of an enum variant.
//! - **Field** attributes are prepended to the field declarartion.
//!
//! Some attributes can be _asymmetric_, meaning different values may be specified for `Encode` and
//! `Decode`. To do this, instead of providing a single value like `#[declio(name = "value")]`, the
//! syntax `#[declio(name(encode = "value1", decode = "value2"))]` will also work. Either of the
//! `encode` or `decode` values may be omitted, and the default will be used, as if the attribute
//! is not present.
//!
//! Unless otherwise specified, the attributes listed are optional.
//!
//! ## Attribute Expressions
//!
//! Some attributes accept values in the form of _expressions_. These should be provided
//! as a string literal (surrounded by quotes), and they have access to the context bindings
//! defined in the container-level `ctx` attribute, as well as the values (by reference) of any of
//! the fields declared _before_ the attribute. Field values can be accessed by the field's name,
//! or by `field_0`, `field_1`, etc. if it is a tuple struct or variant. It may also use the try
//! operator `?`, provided that the error type can be converted into a `declio::Error`.
//!
//! The classic example of this is using an integer field adjacent to a `Vec` to provide the length
//! context required by the `Vec`:
//!
//! ```
//! use declio::{Encode, Decode};
//! use declio::ctx::{Len, Endian};
//! use std::convert::TryInto;
//!
//! #[derive(Encode, Decode)]
//! struct LengthPrefixedBytes {
//!     #[declio(ctx = "Endian::Big")]
//!     len: u16,
//!     #[declio(ctx(decode = "Len((*len).try_into()?)"))]
//!     bytes: Vec<u8>,
//! }
//! ```
//!
//! ## Container Attributes
//!
//! - **`crate_path`** - Specify a custom path to the `declio` crate. If you use the `declio` crate
//! under a different name, this must be set to that path for the `derive` to successfully compile.
//!
//! - **`ctx`** (Asymmetric) - A comma-separated list of context fields, specified by `$ident:
//! $type` (e.g. `tag: i32`). The `Ctx` type parameter of the resulting `Encode` or `Decode` impl
//! will be a _n_-tuple of the given types if n > 1, or the given type itself if n = 1, and the
//! context values will be bound to the given `ident`s to be used in attribute expressions.
//! When not present, the context type is the unit type `()`. Example:
//!
//! ```
//! use declio::{Encode, Decode};
//! use declio::ctx::Len;
//!
//! /// Accepts a context
//! #[derive(Encode, Decode)]
//! #[declio(ctx = "len: usize")]
//! struct UnknownLength<T: Encode + Decode> {
//!     #[declio(ctx(decode = "Len(len)"))]
//!     vec: Vec<T>,
//! }
//! ```
//!
//! - **`id_expr`** (Required for enums, conflicts with `id_type`) - Use the given expression as
//! the variant ID when decoding. Unlike `id_type`, the variant ID is not encoded or decoded as
//! part of the enum. Useful for specifying a variant ID via a `ctx` field.
//!
//! - **`id_type`** (Required for enums, conflicts with `id_expr`) - Encode or decode the variant ID
//! as the given type before encoding/decoding the fields.
//!
//! - **`id_ctx`** (Asymmetric, conflicts with `id_expr`) - If encoding or decoding a variant ID
//! with `id_type`, this attribute will set the context used by the ID encoder or decoder.
//!
//! ## Variant Attributes
//!
//! - **`id`** - An expression used to match the variant ID when decoding, and to encode the variant
//! when `id_type` is being used.
//!
//! ## Field Attributes
//!
//! - **`ctx`** (Asymmetric) The context value to be passed to the field's encoder or decoder. When
//! not present, the passed context is the unit context.
//!
//! - **`with`** (Conflicts with `encode_with` and `decode_with`) - Uses the given helper functions
//! to encode or decode the field instead of the field type's `Encode` or `Decode` implementation.
//! Should be a path to a module with these definitions:
//!
//! ```
//! # type T = ();
//! # type Ctx = ();
//! fn encode<W>(val: &T, ctx: Ctx, writer: &mut W) -> Result<(), declio::Error>
//! where
//!     W: std::io::Write,
//! {
//! # todo!()
//!     /* ... */
//! }
//!
//! fn decode<R>(ctx: Ctx, reader: &mut R) -> Result<T, declio::Error>
//! where
//!     R: std::io::Read,
//! {
//! # todo!()
//!     /* ... */
//! }
//! ```
//!
//! where `T` is the field type and `Ctx` is the type of the context provided by `ctx` (or the
//! unit type `()` if not specified).
//!
//! - **`encode_with`** (Conflicts with `with`) - Uses the given helper function to encode the field
//! instead of the field type's `Encode` implementation. Should be a path to a function with the
//! signature `fn<W: std::io::Write>(&T, Ctx, &mut W) -> Result<(), declio::Error>`, where `T` is
//! the field type and `Ctx` is the type of the context provided by `ctx` (or the unit type `()` if
//! not specified).
//!
//! - **`decode_with`** (Conflicts with `with`) - Uses the given helper function to decode the field
//! instead of the field type's `Decode` implementation. Should be a path to a function with the
//! signature `fn<R: std::io::Read>(Ctx, &mut R) -> Result<T, declio::Error>`, where `T` is the
//! field type and `Ctx` is the type of the context provided by `ctx` (or the unit type `()` if not
//! specified).
//!
//! - **`skip_if`** - If the given expression evaluates true, the field will not be encoded or
//! decoded. When decoding, the field will be given the value of `Default::default()` instead.
//!
//!   For example, this is useful for optionally encoding or decoding a field based on the value of
//!   a previous field. In particular, it is impossible to get `None` from `Option::decode` without
//!   using `skip_if`, since it assumes that the inner value is present:
//!
//! ```
//! use declio::{Encode, Decode};
//! use declio::ctx::Endian;
//!
//! #[derive(Encode, Decode)]
//! struct OptionalExtraData {
//!     tag: u8,
//!
//!     #[declio(ctx = "Endian::Big", skip_if = "*tag != 2")]
//!     extra_data: Option<u32>,
//! }
//! ```
