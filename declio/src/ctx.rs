//! Types that give context to serializers.

/// The endianness, or byte order, of primitive types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Endian {
    /// Big-endian (most-significant-byte first).
    Big,
    /// Little-endian (least-significant-byte first).
    Little,
}

impl Endian {
    /// The native endianness of the target architecture.
    pub const fn native() -> Self {
        #[cfg(target_endian = "big")]
        let endian = Self::Big;

        #[cfg(target_endian = "little")]
        let endian = Self::Little;

        endian
    }
}

impl Default for Endian {
    fn default() -> Self {
        Self::native()
    }
}

/// The number of elements in variable-sized containers.
pub struct Len(pub usize);
