/// Defines a type that encodes and decodes as a constant byte string.
///
/// When decoding, the bytes read will be compared against the given string,
/// and an error will be returned if there is a mismatch.
///
/// # Example
///
/// ```
/// use declio::{Encode, Decode};
/// use declio::util::magic_bytes;
///
/// // creates a `pub struct Foo;` and a `struct Bar;`:
/// magic_bytes! {
///     #[derive(Debug)]
///     pub Foo(b"FOO");
///
///     #[derive(Debug)]
///     Bar(b"BAR");
/// }
///
/// let mut bytes = Vec::new();
/// Foo.encode((), &mut bytes).unwrap();
/// assert_eq!(bytes, b"FOO");
///
/// let mut reader = bytes.as_slice();
/// assert!(Foo::decode((), &mut reader).is_ok());
///
/// let mut reader = bytes.as_slice();
/// assert!(Bar::decode((), &mut reader).is_err());
/// ```
#[macro_export]
macro_rules! magic_bytes {
    ($($(#[$attr:meta])* $vis:vis $name:ident($bytes:expr);)*) => {$(
        $(#[$attr])*
        $vis struct $name;

        impl $crate::Encode<()> for $name {
            fn encode<W>(&self, _ctx: (), writer: &mut W) -> Result<(), $crate::Error>
            where
                W: std::io::Write,
            {
                ($bytes).encode((), writer)
            }
        }

        impl $crate::Decode<()> for $name {
            fn decode<R>(_ctx: (), reader: &mut R) -> Result<Self, $crate::Error>
            where
                R: std::io::Read,
            {
                let bytes: [u8; ($bytes).len()] = $crate::Decode::decode((), reader)?;
                if &bytes != $bytes {
                    return Err($crate::Error::new(format!(
                        "magic bytes mismatch: expected {:x?}, got {:x?}",
                        $bytes, bytes,
                    )));
                }
                Ok(Self)
            }
        }
    )*}
}
