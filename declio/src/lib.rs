//! A declarative I/O serialization library.

#![warn(missing_docs)]

mod error;

pub mod ctx;

pub use self::error::Error;
#[doc(hidden)]
pub use std as export;

#[cfg(feature = "derive")]
pub use declio_derive::{Decode, Encode};

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
