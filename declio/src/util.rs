//! Utilities that aren't part of the "core" of declio, but may be useful in reducing boilerplate.

use crate::{Encode, Decode, Error};
use crate::ctx::Endian;

#[doc(inline)]
pub use crate::magic_bytes;

macro_rules! endian_wrappers {
    ($($(#[$attr:meta])* $name:ident: $endian:expr,)*) => {$(
        $(#[$attr])*
        #[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name<T>(pub T);

        impl<T> Encode<()> for $name<T>
        where
            T: Encode<Endian>,
        {
            fn encode<W>(&self, _ctx: (), writer: &mut W) -> Result<(), Error>
            where
                W: std::io::Write,
            {
                self.0.encode($endian, writer)
            }
        }

        impl<T> Decode<()> for $name<T>
        where
            T: Decode<Endian>,
        {
            fn decode<R>(_ctx: (), reader: &mut R) -> Result<Self, Error>
            where
                R: std::io::Read,
            {
                T::decode($endian, reader).map(Self)
            }
        }

        impl<T> From<T> for $name<T> {
            fn from(value: T) -> Self {
                Self(value)
            }
        }

        /* NB: Orphan rules prohibit something like this:
        impl<T> From<$name<T>> for T {
            fn from(wrapper: $name<T>) -> Self {
                wrapper.0
            }
        }
        */

        impl<T> $name<T> {
            /// Unwraps and returns the inner `T` value.
            pub fn into_inner(self) -> T {
                self.0
            }
        }
    )*}
}

endian_wrappers! {
    /// Encodes and decodes the inner type in little-endian format.
    ///
    /// # Example
    ///
    /// ```
    /// use declio::Encode;
    /// use declio::util::LittleEndian;
    ///
    /// type Uint = LittleEndian<u32>;
    ///
    /// let x: Uint = 0xdeadbeef.into();
    ///
    /// let mut bytes = Vec::new();
    /// x.encode((), &mut bytes).unwrap();
    ///
    /// assert_eq!(bytes, &[0xef, 0xbe, 0xad, 0xde]);
    /// ```
    LittleEndian: Endian::Little,

    /// Encodes and decodes the inner type in big-endian format.
    ///
    /// # Example
    ///
    /// ```
    /// use declio::Encode;
    /// use declio::util::BigEndian;
    ///
    /// type Uint = BigEndian<u32>;
    ///
    /// let x: Uint = 0xdeadbeef.into();
    ///
    /// let mut bytes = Vec::new();
    /// x.encode((), &mut bytes).unwrap();
    ///
    /// assert_eq!(bytes, &[0xde, 0xad, 0xbe, 0xef]);
    /// ```
    BigEndian: Endian::Big,
}
