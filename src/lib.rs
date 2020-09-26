//! A declarative I/O serialization library.

#![warn(missing_docs)]

use std::io;

/// A type that can be serialized into a byte stream.
pub trait Serialize<Ctx> {
    /// Serializes `&self` to the given writer.
    fn serialize<W>(&self, ctx: Ctx, writer: &mut W) -> Result<(), io::Error>
    where
        W: io::Write;
}

/// A type that can be deserialized from a byte stream.
pub trait Deserialize<Ctx>: Sized {
    /// Deserializes a value from the given reader.
    fn deserialize<R>(ctx: Ctx, reader: &mut R) -> Result<Self, io::Error>
    where
        R: io::Read;
}
