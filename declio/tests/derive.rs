use declio::{ctx, Decode, Encode};
use std::fmt::Debug;
use std::io;

#[derive(Debug, PartialEq, Encode, Decode)]
struct UnitStruct;

#[derive(Debug, PartialEq, Encode, Decode)]
struct TupleStruct(u8, u32);

#[derive(Debug, PartialEq, Encode, Decode)]
struct Struct {
    x: u8,
    y: u32,
}

#[derive(Debug, PartialEq, Encode, Decode)]
#[declio(id_type = "u8")]
enum Enum {
    #[declio(id = "0")]
    Unit,
    #[declio(id = "1")]
    Tuple(u8, u32),
    #[declio(id = "2")]
    Struct { x: u8, y: u32 },
}

#[derive(Debug, PartialEq, Encode, Decode)]
struct With {
    #[declio(with = "big_endian")]
    y: u32,
}

#[derive(Debug, PartialEq, Encode, Decode)]
struct WithSeparate {
    #[declio(encode_with = "big_endian::encode", decode_with = "big_endian::decode")]
    y: u32,
}

mod big_endian {
    use super::*;

    pub fn encode<W>(x: &u32, _: (), writer: &mut W) -> Result<(), io::Error>
    where
        W: io::Write,
    {
        x.encode(ctx::Endian::Big, writer)
    }

    pub fn decode<R>(_: (), reader: &mut R) -> Result<u32, io::Error>
    where
        R: io::Read,
    {
        u32::decode(ctx::Endian::Big, reader)
    }
}

fn test_encode<T>(input: T, expected: &[u8])
where
    T: Encode,
{
    let mut output = Vec::new();
    input.encode((), &mut output).unwrap();
    assert_eq!(output, expected);
}

fn test_decode<T>(mut input: &[u8], expected: T)
where
    T: Decode + Debug + PartialEq,
{
    let output = T::decode((), &mut input).unwrap();
    assert_eq!(output, expected);
}

fn test_bidir<T>(val: T, bytes: &[u8])
where
    T: Encode + Decode + Debug + PartialEq,
{
    test_encode(&val, bytes);
    test_decode(bytes, val);
}

#[test]
fn unit_struct() {
    test_bidir(UnitStruct, &[]);
}

#[test]
fn tuple_struct() {
    test_bidir(
        TupleStruct(0xab, 0xdeadbeef),
        &[0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}

#[test]
fn struct_encode() {
    test_bidir(
        Struct {
            x: 0xab,
            y: 0xdeadbeef,
        },
        &[0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}

#[test]
fn unit_enum() {
    test_bidir(Enum::Unit, &[0x00]);
}

#[test]
fn tuple_enum() {
    test_bidir(
        Enum::Tuple(0xab, 0xdeadbeef),
        &[0x01, 0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}

#[test]
fn struct_enum() {
    test_bidir(
        Enum::Struct {
            x: 0xab,
            y: 0xdeadbeef,
        },
        &[0x02, 0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}

#[test]
fn with() {
    test_bidir(With { y: 0xdeadbeef }, &[0xde, 0xad, 0xbe, 0xef]);
}

#[test]
fn with_separate() {
    test_bidir(WithSeparate { y: 0xdeadbeef }, &[0xde, 0xad, 0xbe, 0xef]);
}
