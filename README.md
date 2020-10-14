# declio

A declarative I/O serialization library.

`declio` provides a pair of traits, [`Encode`] and [`Decode`], that facilitate bidirectional
conversions between binary data streams (the `std::io` traits) and arbitrary data types.

These traits are implemented for many of the types in `std`; primitives can be encoded and
decoded as their big- or little-endian binary representations, and collections and other
container types encode and decode the data they contain.

This crate also provides a pair of derive macros, via the default feature `derive`, that can
implement `Encode` and `Decode` for arbitrary compound data types. By default it will encode
and decode all of its fields in order, but it is highly configurable, intended to target the
many different patterns found in binary formats.

Inspiration for this crate largely comes from [`deku`], but incorporating some changes based on
my own opinions and preferences. For example, `declio` uses byte-wise data streams from
`std::io` instead of the bit-wise `BitVec`s used by `deku`.

## Examples

Let's start with a simple example - encoding a single integer into a byte buffer:

```rust
use declio::Encode;
use declio::ctx::Endian;

let mut buf: Vec<u8> = Vec::new();
u32::encode(&0xdeadbeef, Endian::Big, &mut buf)
    .expect("encode failed");

assert_eq!(buf, [0xde, 0xad, 0xbe, 0xef]);
```

In this example, [`Endian::Big`] is a "context" value. `declio` provides these as an easy way to
configure or alter an implementation of `Encode` or `Decode` at runtime, instead of using
wrapper types or helper functions. In this case, it tells the implementation of `Encode` for
`u32` to encode the bytes in big-endian order. It can instead be set to [`Endian::Little`] to
reverse the byte order, and in general, [`Endian`] can be passed to any of the integer and
floating-point primitive types for similar effects.

Context can also be used to abstract some fields that are necessary to encode and decode some
types. For example, containers with variable length like `Vec` accept a [`Len`] context value
during decoding, which tells the `Decode` implementation how many values it should decode.
This can be a compile-time constant like `Len(1024)`, or it can be created from another value
at runtime, like this:

```rust
use declio::Decode;
use declio::ctx::Len;

let mut bytes = &[
    // len
    0x00, 0x02,

    // words[0]
    0xde, 0xad,

    // words[1]
    0xbe, 0xef,
][..];

let len = u16::decode((), &mut bytes)
    .expect("decode len failed");

let words: Vec<u16> = Vec::decode(Len(len as usize), &mut bytes)
    .expect("decode bytes failed");

assert!(bytes.is_empty()); // did we consume the whole buffer?
assert_eq!(words, [0xdead, 0xbeef]);
```

The reason for this is that the `Decode` implementation for `Vec` does not know how to read the
length value. It doesn't know what integer size or byte order the binary format uses to encode
the length; it doesn't even know if the length is encoded at all! It might be some fixed length
defined as part of the format.

Also note that we are decoding integers in this example, but I've omitted the `Endian` context,
instead passing `()` to `u16::decode`.  In the case of `u16` and other primitives, providing
`()` as context defaults to `Endian::Big`.

`Vec::decode` can also accept an additional context value to pass to the element decoder, using
a 2-tuple like `(Len(len as usize), Endian::Big)`. However, in this example, only a `Len` is
passed, which is also valid and will pass `()` as context to the element decoder.

### Deriving

Here is an example which makes use of derive macros to encode and decode a
user-defined data type. This is not a complete demonstration of the features of the derive
macros; for a more complete reference, see [TODO].

```rust
use declio::{Encode, Decode};
use declio::ctx::Endian;
use std::convert::TryInto;

#[derive(Debug, PartialEq, Encode, Decode)]
struct WithLength {
    // Context can be passed to the field decoder with a `ctx` attribute.
    #[declio(ctx = "Endian::Little")]
    len: u16,

    // Context may be different for encode and decode,
    // though they should generally be as symmetric as possible.
    // For example, `Vec` doesn't accept a `Len` when encoding.
    //
    // Fields declared before this one can be accessed by name
    // (or by `field_0`, `field_1`, etc for tuple structs):
    #[declio(ctx(encode = "Endian::Little", decode = "(len.try_into()?, Endian::Little)"))]
    bytes: Vec<u8>,
}

let bytes: Vec<u8> = vec![0xde, 0xad, 0xbe, 0xef];

let with_length = WithLength {
    len: bytes.len().try_into().expect("length out of range"),
    bytes,
};

let mut encoded: Vec<u8> = Vec::new();
with_length.encode((), &mut encoded)
    .expect("encode failed");
assert_eq!(encoded, [0x04, 0x00, 0xde, 0xad, 0xbe, 0xef]);

let mut decode_reader: &[u8] = encoded.as_slice();
let decoded = WithLength::decode((), &mut decode_reader)
    .expect("decode failed");

assert!(decode_reader.is_empty());
assert_eq!(decoded, with_length);
```

[`deku`]: https://crates.io/crates/deku
