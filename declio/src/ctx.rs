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

    /// "Network-endian", an alias for big-endian.
    pub const fn network() -> Self {
        Self::Big
    }
}

/// The number of elements in variable-sized containers.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Len(pub usize);
