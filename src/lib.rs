//! A declarative I/O serialization library.

#![warn(missing_docs)]

pub mod ctx;

use self::ctx::Endian;
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
