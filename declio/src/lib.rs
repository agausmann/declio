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
