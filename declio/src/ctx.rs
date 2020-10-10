//! Types that give context to encoders and decoders.

use std::convert::{TryFrom, TryInto};
use std::num::TryFromIntError;

/// The endianness, or byte order, of primitive types.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Endian {
    /// Big-endian (most-significant-byte first).
    Big,
    /// Little-endian (least-significant-byte first).
    Little,
}

impl Endian {
    /// The native endianness of the target architecture.
    ///
    /// **Warning** - This should not be used for cross-platform I/O in general. While dealing with
    /// native-endian bytes is marginally more efficient, it may cause incompatibilities if the
    /// data is shared between multiple devices where the native byte orders are different.
    pub const fn native() -> Self {
        #[cfg(target_endian = "big")]
        let endian = Self::Big;

        #[cfg(target_endian = "little")]
        let endian = Self::Little;

        endian
    }

    /// "Network-endian", an alias for big-endian, the default endianness.
    pub const fn network() -> Self {
        Self::Big
    }
}

impl Default for Endian {
    /// (Network- / Big-endian) The default endianness used to encode primitives.
    fn default() -> Self {
        Self::network()
    }
}

/// The number of elements in variable-sized containers.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Len(pub usize);

impl From<usize> for Len {
    fn from(x: usize) -> Self {
        Len(x)
    }
}

impl From<Len> for usize {
    fn from(Len(x): Len) -> Self {
        x
    }
}

macro_rules! convert_len {
    ($($t:ty)*) => {$(
        impl TryFrom<$t> for Len {
            type Error = TryFromIntError;

            fn try_from(x: $t) -> Result<Self, Self::Error> {
                Ok(Self(usize::try_from(x)?))
            }
        }

        // unfortunate workaround, because type inference and derefs fail sometimes.
        //
        // I want to make using `try_into` painless in derive impls,
        // like `#[declio(ctx = "len.try_into()?")]`,
        // but rust insists on looking for `<&T as TryInto<Len>>::try_into` without derefing.
        // without this impl, and without working dereferencing,
        // the attribute would look like `#[declio(ctx = "(*len).try_into()?")]`.
        //
        // making this a generic impl over all &T also fails due to conflicts, and might otherwise
        // be undesirable.
        impl TryFrom<&$t> for Len {
            type Error = TryFromIntError;

            fn try_from(&x: &$t) -> Result<Self, Self::Error> {
                x.try_into()
            }
        }

        impl TryFrom<Len> for $t {
            type Error = TryFromIntError;

            fn try_from(Len(x): Len) -> Result<Self, Self::Error> {
                Ok(Self::try_from(x)?)
            }
        }
    )*};
}

convert_len! {
    u8 u16 u32 u64 u128 i8 i16 i32 i64 i128
}
