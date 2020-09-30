use declio::{Decode, Encode};
use std::fmt::Debug;

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
fn test_unit_struct() {
    test_bidir(UnitStruct, &[]);
}

#[test]
fn test_tuple_struct() {
    test_bidir(
        TupleStruct(0xab, 0xdeadbeef),
        &[0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}

#[test]
fn test_struct_encode() {
    test_bidir(
        Struct {
            x: 0xab,
            y: 0xdeadbeef,
        },
        &[0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}

#[test]
fn test_unit_enum() {
    test_bidir(Enum::Unit, &[0x00]);
}

#[test]
fn test_tuple_enum() {
    test_bidir(
        Enum::Tuple(0xab, 0xdeadbeef),
        &[0x01, 0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}

#[test]
fn test_struct_enum() {
    test_bidir(
        Enum::Struct {
            x: 0xab,
            y: 0xdeadbeef,
        },
        &[0x02, 0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}
