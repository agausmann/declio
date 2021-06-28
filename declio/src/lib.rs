//! A declarative I/O serialization library.
//!
//! `declio` provides a pair of traits, [`Encode`] and [`Decode`], that facilitate bidirectional
//! conversions between binary data streams (the `std::io` traits) and arbitrary data types.
//!
//! These traits are implemented for many of the types in `std`; primitives can be encoded and
//! decoded as their big- or little-endian binary representations, and collections and other
//! container types encode and decode the data they contain.
//!
//! This crate also provides a pair of derive macros, via the default feature `derive`, that can
//! implement `Encode` and `Decode` for arbitrary compound data types. By default it will encode
//! and decode all of its fields in order, but it is highly configurable, intended to target the
//! many different patterns found in binary formats.
//!
//! Inspiration for this crate largely comes from [`deku`], but incorporating some changes based on
//! my own opinions and preferences. For example, `declio` uses byte-wise data streams from
//! `std::io` instead of the bit-wise `BitVec`s used by `deku`.
//!
//! # Examples
//!
//! Let's start with a simple example - encoding a single integer into a byte buffer:
//!
//! ```
//! use declio::Encode;
//! use declio::ctx::Endian;
//!
//! let mut buf: Vec<u8> = Vec::new();
//! u32::encode(&0xdeadbeef, Endian::Big, &mut buf)
//!     .expect("encode failed");
//!
//! assert_eq!(buf, [0xde, 0xad, 0xbe, 0xef]);
//! ```
//!
//! In this example, [`Endian::Big`] is a "context" value. `declio` provides these as an easy way to
//! configure or alter an implementation of `Encode` or `Decode` at runtime, instead of using
//! wrapper types or helper functions. In this case, it tells the implementation of `Encode` for
//! `u32` to encode the bytes in big-endian order. It can instead be set to [`Endian::Little`] to
//! reverse the byte order, and in general, [`Endian`] can be passed to any of the integer and
//! floating-point primitive types for similar effects.
//!
//! Context can also be used to abstract some fields that are necessary to encode and decode some
//! types. For example, containers with variable length like `Vec` accept a [`Len`] context value
//! during decoding, which tells the `Decode` implementation how many values it should decode.
//! This can be a compile-time constant like `Len(1024)`, or it can be created from another value
//! at runtime, like this:
//!
//! ```
//! use declio::Decode;
//! use declio::ctx::Len;
//!
//! let mut bytes = &[
//!     // len
//!     0x00, 0x02,
//!
//!     // words[0]
//!     0xde, 0xad,
//!
//!     // words[1]
//!     0xbe, 0xef,
//! ][..];
//!
//! let len = u16::decode((), &mut bytes)
//!     .expect("decode len failed");
//!
//! let words: Vec<u16> = Vec::decode(Len(len as usize), &mut bytes)
//!     .expect("decode bytes failed");
//!
//! assert!(bytes.is_empty()); // did we consume the whole buffer?
//! assert_eq!(words, [0xdead, 0xbeef]);
//! ```
//!
//! The reason for this is that the `Decode` implementation for `Vec` does not know how to read the
//! length value. It doesn't know what integer size or byte order the binary format uses to encode
//! the length; it doesn't even know if the length is encoded at all! It might be some fixed length
//! defined as part of the format.
//!
//! Also note that we are decoding integers in this example, but I've omitted the `Endian` context,
//! instead passing `()` to `u16::decode`.  In the case of `u16` and other primitives, providing
//! `()` as context defaults to `Endian::Big`.
//!
//! `Vec::decode` can also accept an additional context value to pass to the element decoder, using
//! a 2-tuple like `(Len(len as usize), Endian::Big)`. However, in this example, only a `Len` is
//! passed, which is also valid and will pass `()` as context to the element decoder.
//!
//! ## Deriving
//!
//! Here is an example which makes use of derive macros to encode and decode a
//! user-defined data type. This is not a complete demonstration of the features of the derive
//! macros; for a more complete reference, see the [`mod@derive`] module docs.
//!
//! ```
//! use declio::{Encode, Decode};
//! use declio::ctx::Endian;
//! use std::convert::TryInto;
//!
//! #[derive(Debug, PartialEq, Encode, Decode)]
//! struct WithLength {
//!     // Context can be passed to the field decoder with a `ctx` attribute.
//!     #[declio(ctx = "Endian::Little")]
//!     len: u16,
//!
//!     // Context may be different for encode and decode,
//!     // though they should generally be as symmetric as possible.
//!     // For example, `Vec` doesn't accept a `Len` when encoding.
//!     //
//!     // Fields declared before this one can be accessed by name
//!     // (or by `field_0`, `field_1`, etc for tuple structs):
//!     #[declio(ctx(encode = "Endian::Little", decode = "(len.try_into()?, Endian::Little)"))]
//!     bytes: Vec<u8>,
//! }
//!
//! let bytes: Vec<u8> = vec![0xde, 0xad, 0xbe, 0xef];
//!
//! let with_length = WithLength {
//!     len: bytes.len().try_into().expect("length out of range"),
//!     bytes,
//! };
//!
//! let mut encoded: Vec<u8> = Vec::new();
//! with_length.encode((), &mut encoded)
//!     .expect("encode failed");
//! assert_eq!(encoded, [0x04, 0x00, 0xde, 0xad, 0xbe, 0xef]);
//!
//! let mut decode_reader: &[u8] = encoded.as_slice();
//! let decoded = WithLength::decode((), &mut decode_reader)
//!     .expect("decode failed");
//!
//! assert!(decode_reader.is_empty());
//! assert_eq!(decoded, with_length);
//! ```
//!
//! [`deku`]: https://crates.io/crates/deku

#![warn(missing_docs)]

mod error;
mod macros;

pub mod ctx;
pub mod derive;

pub use self::error::Error;
#[doc(hidden)]
pub use std as export;

#[cfg(feature = "derive")]
/// Implements [`Decode`] for a given type. For more information, see [`derive`](derive/index.html).
pub use declio_derive::Decode;

#[cfg(feature = "derive")]
/// Implements [`Encode`] for a given type. For more information, see [`derive`](derive/index.html).
pub use declio_derive::Encode;

use self::ctx::{Endian, Len};
use std::borrow::Cow;
use std::{io, mem};

/// A type that can be encoded into a byte stream.
pub trait Encode<Ctx = ()> {
    /// Encodes `&self` to the given writer.
    fn encode<W>(&self, ctx: Ctx, writer: &mut W) -> Result<(), Error>
    where
        W: io::Write;
}

/// A type that can be decoded from a byte stream.
pub trait Decode<Ctx = ()>: Sized {
    /// Decodes a value from the given reader.
    fn decode<R>(ctx: Ctx, reader: &mut R) -> Result<Self, Error>
    where
        R: io::Read;
}

impl<T, Ctx> Encode<Ctx> for &T
where
    T: Encode<Ctx>,
{
    fn encode<W>(&self, ctx: Ctx, writer: &mut W) -> Result<(), Error>
    where
        W: io::Write,
    {
        (*self).encode(ctx, writer)
    }
}

impl<T, Ctx> Encode<Ctx> for [T]
where
    T: Encode<Ctx>,
    Ctx: Clone,
{
    /// Encodes each element of the slice in order.
    ///
    /// If length is also to be encoded, it has to be done separately.
    fn encode<W>(&self, inner_ctx: Ctx, writer: &mut W) -> Result<(), Error>
    where
        W: io::Write,
    {
        for elem in self {
            elem.encode(inner_ctx.clone(), writer)?;
        }
        Ok(())
    }
}

impl<T, Ctx, const N: usize> Encode<Ctx> for [T; N]
where
    T: Encode<Ctx>,
    Ctx: Clone,
{
    fn encode<W>(&self, inner_ctx: Ctx, writer: &mut W) -> Result<(), Error>
    where
        W: io::Write,
    {
        for elem in self {
            elem.encode(inner_ctx.clone(), writer)?;
        }
        Ok(())
    }
}

impl<T, Ctx, const N: usize> Decode<Ctx> for [T; N]
where
    T: Decode<Ctx> + Copy + Default,
    Ctx: Clone,
{
    fn decode<R>(inner_ctx: Ctx, reader: &mut R) -> Result<Self, Error>
    where
        R: io::Read,
    {
        //TODO: Use MaybeUninit when stabilized with arrays
        let mut arr = [Default::default(); N];
        for slot in &mut arr {
            *slot = Decode::decode(inner_ctx.clone(), reader)?;
        }
        Ok(arr)
    }
}

impl<T, Ctx> Encode<Ctx> for Vec<T>
where
    T: Encode<Ctx>,
    Ctx: Clone,
{
    /// Encodes each element of the vector in order.
    ///
    /// If length is also to be encoded, it has to be done separately.
    fn encode<W>(&self, inner_ctx: Ctx, writer: &mut W) -> Result<(), Error>
    where
        W: io::Write,
    {
        self.as_slice().encode(inner_ctx, writer)
    }
}

impl<T, Ctx> Decode<(Len, Ctx)> for Vec<T>
where
    T: Decode<Ctx>,
    Ctx: Clone,
{
    /// Decodes multiple values of type `T`, collecting them in a `Vec`.
    ///
    /// The length of the vector / number of elements decoded is equal to the value of the
    /// `Len` context.
    fn decode<R>((Len(len), inner_ctx): (Len, Ctx), reader: &mut R) -> Result<Self, Error>
    where
        R: io::Read,
    {
        let mut acc = Self::with_capacity(len);
        for _ in 0..len {
            acc.push(T::decode(inner_ctx.clone(), reader)?);
        }
        Ok(acc)
    }
}

impl<T> Decode<Len> for Vec<T>
where
    T: Decode,
{
    /// Decodes multiple values of type `T`, collecting them in a `Vec`.
    ///
    /// The length of the vector / number of elements decoded is equal to the value of the
    /// `Len` context.
    fn decode<R>(len: Len, reader: &mut R) -> Result<Self, Error>
    where
        R: io::Read,
    {
        Self::decode((len, ()), reader)
    }
}

impl<T, Ctx> Encode<Ctx> for Option<T>
where
    T: Encode<Ctx>,
{
    /// If `Some`, then the inner value is encoded, otherwise, nothing is written.
    fn encode<W>(&self, inner_ctx: Ctx, writer: &mut W) -> Result<(), Error>
    where
        W: io::Write,
    {
        if let Some(inner) = self {
            inner.encode(inner_ctx, writer)
        } else {
            Ok(())
        }
    }
}

impl<T, Ctx> Decode<Ctx> for Option<T>
where
    T: Decode<Ctx>,
{
    /// Decodes a value of type `T` and wraps it in `Some`.
    ///
    /// Detecting and deserializing a `None` should be done outside of this function by
    /// checking the relevant conditions in other decoded values and skipping this call if a
    /// `None` is expected.
    ///
    /// Since serializing a `None` writes nothing, deserialization is also a no-op; just construct
    /// a value of `None`.
    fn decode<R>(inner_ctx: Ctx, reader: &mut R) -> Result<Self, Error>
    where
        R: io::Read,
    {
        T::decode(inner_ctx, reader).map(Some)
    }
}

impl<'a, T, Ctx> Encode<Ctx> for Cow<'a, T>
where
    T: Encode<Ctx> + ToOwned + ?Sized,
{
    /// Borrows a value of type `T` and encodes it.
    fn encode<W>(&self, inner_ctx: Ctx, writer: &mut W) -> Result<(), Error>
    where
        W: io::Write,
    {
        T::encode(&*self, inner_ctx, writer)
    }
}

impl<'a, T, Ctx> Decode<Ctx> for Cow<'a, T>
where
    T: ToOwned + ?Sized,
    T::Owned: Decode<Ctx>,
{
    /// Decodes a value of type `T::Owned`.
    fn decode<R>(inner_ctx: Ctx, reader: &mut R) -> Result<Self, Error>
    where
        R: io::Read,
    {
        T::Owned::decode(inner_ctx, reader).map(Self::Owned)
    }
}

impl<T, Ctx> Encode<Ctx> for Box<T>
where
    T: Encode<Ctx>,
{
    /// Encodes the boxed value.
    fn encode<W>(&self, inner_ctx: Ctx, writer: &mut W) -> Result<(), Error>
    where
        W: io::Write,
    {
        T::encode(&*self, inner_ctx, writer)
    }
}

impl<T, Ctx> Decode<Ctx> for Box<T>
where
    T: Decode<Ctx>,
{
    /// Decodes a value of type `T` and boxes it.
    fn decode<R>(inner_ctx: Ctx, reader: &mut R) -> Result<Self, Error>
    where
        R: io::Read,
    {
        T::decode(inner_ctx, reader).map(Self::new)
    }
}

impl Encode for () {
    /// No-op.
    fn encode<W>(&self, _: (), _: &mut W) -> Result<(), Error>
    where
        W: io::Write,
    {
        Ok(())
    }
}

impl Decode for () {
    /// No-op.
    fn decode<R>(_: (), _: &mut R) -> Result<Self, Error>
    where
        R: io::Read,
    {
        Ok(())
    }
}

impl Encode for bool {
    /// Encodes `true` as a 1-byte, `false` as a 0-byte.
    fn encode<W>(&self, _: (), writer: &mut W) -> Result<(), Error>
    where
        W: io::Write,
    {
        let byte = match self {
            false => 0,
            true => 1,
        };
        u8::encode(&byte, (), writer)
    }
}

impl Decode for bool {
    /// Decodes a 1-byte as `true`, a 0-byte as `false`, and any other byte value as an error.
    fn decode<R>(_: (), reader: &mut R) -> Result<Self, Error>
    where
        R: io::Read,
    {
        let byte = u8::decode((), reader)?;
        match byte {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(Error::new("invalid byte value for boolean")),
        }
    }
}

macro_rules! impl_primitive {
    ($($t:ty)*) => {$(
        impl Encode<Endian> for $t {
            fn encode<W>(&self, endian: Endian, writer: &mut W) -> Result<(), Error>
            where
                W: io::Write,
            {
                let bytes = match endian {
                    Endian::Big => self.to_be_bytes(),
                    Endian::Little => self.to_le_bytes(),
                };
                writer.write_all(&bytes)?;
                Ok(())
            }
        }

        impl Encode for $t {
            fn encode<W>(&self, _: (), writer: &mut W) -> Result<(), Error>
            where
                W: io::Write,
            {
                self.encode(Endian::default(), writer)
            }
        }

        impl Decode<Endian> for $t {
            fn decode<R>(endian: Endian, reader: &mut R) -> Result<Self, Error>
            where
                R: io::Read,
            {
                let mut bytes = [0u8; mem::size_of::<$t>()];
                reader.read_exact(&mut bytes)?;
                match endian {
                    Endian::Big => Ok(Self::from_be_bytes(bytes)),
                    Endian::Little => Ok(Self::from_le_bytes(bytes)),
                }
            }
        }

        impl Decode for $t {
            fn decode<R>(_: (), reader: &mut R) -> Result<Self, Error>
            where
                R: io::Read,
            {
                Self::decode(Endian::default(), reader)
            }
        }
    )*}
}

impl_primitive! {
    u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64
}
