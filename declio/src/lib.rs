//! A declarative I/O serialization library.

#![warn(missing_docs)]

pub mod ctx;

#[cfg(feature = "derive")]
pub use declio_derive::{Deserialize, Serialize};

use self::ctx::{Endian, Len};
use std::{io, mem};

/// A type that can be serialized into a byte stream.
pub trait Serialize<Ctx = ()> {
    /// Serializes `&self` to the given writer.
    fn serialize<W>(&self, ctx: Ctx, writer: &mut W) -> Result<(), io::Error>
    where
        W: io::Write;
}

/// A type that can be deserialized from a byte stream.
pub trait Deserialize<Ctx = ()>: Sized {
    /// Deserializes a value from the given reader.
    fn deserialize<R>(ctx: Ctx, reader: &mut R) -> Result<Self, io::Error>
    where
        R: io::Read;
}

impl<T, Ctx> Serialize<Ctx> for &T
where
    T: Serialize<Ctx>,
{
    fn serialize<W>(&self, ctx: Ctx, writer: &mut W) -> Result<(), io::Error>
    where
        W: io::Write,
    {
        (*self).serialize(ctx, writer)
    }
}

impl<T, Ctx> Serialize<Ctx> for [T]
where
    T: Serialize<Ctx>,
    Ctx: Clone,
{
    /// Serializes each element of the slice in order.
    ///
    /// If length is also to be serialized, it has to be done separately.
    fn serialize<W>(&self, inner_ctx: Ctx, writer: &mut W) -> Result<(), io::Error>
    where
        W: io::Write,
    {
        for elem in self {
            elem.serialize(inner_ctx.clone(), writer)?;
        }
        Ok(())
    }
}

impl<T, Ctx> Serialize<Ctx> for Vec<T>
where
    T: Serialize<Ctx>,
    Ctx: Clone,
{
    /// Serializes each element of the vector in order.
    ///
    /// If length is also to be serialized, it has to be done separately.
    fn serialize<W>(&self, inner_ctx: Ctx, writer: &mut W) -> Result<(), io::Error>
    where
        W: io::Write,
    {
        self.as_slice().serialize(inner_ctx, writer)
    }
}

impl<T, Ctx> Deserialize<(Len, Ctx)> for Vec<T>
where
    T: Deserialize<Ctx>,
    Ctx: Clone,
{
    /// Deserializes multiple values of type `T`, collecting them in a `Vec`.
    ///
    /// The length of the vector / number of elements deserialized is equal to the value of the
    /// `Len` context.
    fn deserialize<R>((Len(len), inner_ctx): (Len, Ctx), reader: &mut R) -> Result<Self, io::Error>
    where
        R: io::Read,
    {
        let mut acc = Self::with_capacity(len);
        for _ in 0..len {
            acc.push(T::deserialize(inner_ctx.clone(), reader)?);
        }
        Ok(acc)
    }
}

impl<T, Ctx> Serialize<Ctx> for Option<T>
where
    T: Serialize<Ctx>,
{
    /// If `Some`, then the inner value is serialized, otherwise, nothing is written.
    fn serialize<W>(&self, inner_ctx: Ctx, writer: &mut W) -> Result<(), io::Error>
    where
        W: io::Write,
    {
        if let Some(inner) = self {
            inner.serialize(inner_ctx, writer)
        } else {
            Ok(())
        }
    }
}

impl<T, Ctx> Deserialize<Ctx> for Option<T>
where
    T: Deserialize<Ctx>,
{
    /// Deserializes a value of type `T` and wraps it in `Some`.
    ///
    /// Detecting and deserializing a `None` should be done outside of this function by
    /// checking the relevant conditions in other deserialized values and skipping this call if a
    /// `None` is expected.
    ///
    /// Since serializing a `None` writes nothing, deserialization is also a no-op; just construct
    /// a value of `None`.
    fn deserialize<R>(inner_ctx: Ctx, reader: &mut R) -> Result<Self, io::Error>
    where
        R: io::Read,
    {
        T::deserialize(inner_ctx, reader).map(Some)
    }
}

macro_rules! impl_primitive {
    ($($t:ty)*) => {$(
        impl Serialize<Endian> for $t {
            fn serialize<W>(&self, endian: Endian, writer: &mut W) -> Result<(), io::Error>
            where
                W: io::Write,
            {
                let bytes = match endian {
                    Endian::Big => self.to_be_bytes(),
                    Endian::Little => self.to_le_bytes(),
                };
                writer.write_all(&bytes)
            }
        }

        impl Serialize for $t {
            fn serialize<W>(&self, _: (), writer: &mut W) -> Result<(), io::Error>
            where
                W: io::Write,
            {
                self.serialize(Endian::default(), writer)
            }
        }

        impl Deserialize<Endian> for $t {
            fn deserialize<R>(endian: Endian, reader: &mut R) -> Result<Self, io::Error>
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

        impl Deserialize for $t {
            fn deserialize<R>(_: (), reader: &mut R) -> Result<Self, io::Error>
            where
                R: io::Read,
            {
                Self::deserialize(Endian::default(), reader)
            }
        }
    )*}
}

impl_primitive! {
    u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 f32 f64
}
