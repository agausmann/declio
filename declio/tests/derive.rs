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
    #[declio(with = "little_endian")]
    y: u32,
}

#[derive(Debug, PartialEq, Encode, Decode)]
struct WithSeparate {
    #[declio(
        encode_with = "little_endian::encode",
        decode_with = "little_endian::decode"
    )]
    y: u32,
}

#[derive(Debug, PartialEq, Encode, Decode)]
struct FieldCtx {
    #[declio(ctx = "ctx::Endian::Little")]
    y: u32,
}

#[derive(Debug, PartialEq, Encode, Decode)]
#[declio(ctx = "endian: ctx::Endian")]
struct ContainerCtx {
    #[declio(ctx = "endian")]
    y: u32,
}

#[derive(Debug, PartialEq, Encode, Decode)]
#[declio(id_type = "u16", id_ctx = "ctx::Endian::Little")]
enum IdCtx {
    #[declio(id = "1")]
    Bar,
}

#[derive(Debug, PartialEq, Encode, Decode)]
struct SkipIf {
    x: u8,
    #[declio(skip_if = "*x == 8")]
    y: Option<u32>,
}

mod little_endian {
    use super::*;

    pub fn encode<W>(x: &u32, _: (), writer: &mut W) -> Result<(), declio::Error>
    where
        W: io::Write,
    {
        x.encode(ctx::Endian::Little, writer)
    }

    pub fn decode<R>(_: (), reader: &mut R) -> Result<u32, declio::Error>
    where
        R: io::Read,
    {
        u32::decode(ctx::Endian::Little, reader)
    }
}

fn test_encode<T, Ctx>(input: T, expected: &[u8], ctx: Ctx)
where
    T: Encode<Ctx>,
{
    let mut output = Vec::new();
    input.encode(ctx, &mut output).unwrap();
    assert_eq!(output, expected);
}

fn test_decode<T, Ctx>(mut input: &[u8], expected: &T, ctx: Ctx)
where
    T: Decode<Ctx> + Debug + PartialEq,
{
    let output = T::decode(ctx, &mut input).unwrap();
    assert_eq!(output, *expected);
}

fn test_bidir<T>(val: T, bytes: &[u8])
where
    T: Encode + Decode + Debug + PartialEq,
{
    test_bidir_ctx(val, bytes, ());
}

fn test_bidir_ctx<T, Ctx>(val: T, bytes: &[u8], ctx: Ctx)
where
    T: Encode<Ctx> + Decode<Ctx> + Debug + PartialEq,
    Ctx: Copy,
{
    test_encode(&val, bytes, ctx);
    test_decode(bytes, &val, ctx);
}

#[test]
fn unit_struct() {
    test_bidir(UnitStruct, &[]);
}

#[test]
fn tuple_struct() {
    test_bidir(
        TupleStruct(0xab, 0xdeadbeef),
        &[0xab, 0xde, 0xad, 0xbe, 0xef],
    );
}

#[test]
fn struct_encode() {
    test_bidir(
        Struct {
            x: 0xab,
            y: 0xdeadbeef,
        },
        &[0xab, 0xde, 0xad, 0xbe, 0xef],
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
        &[0x01, 0xab, 0xde, 0xad, 0xbe, 0xef],
    );
}

#[test]
fn struct_enum() {
    test_bidir(
        Enum::Struct {
            x: 0xab,
            y: 0xdeadbeef,
        },
        &[0x02, 0xab, 0xde, 0xad, 0xbe, 0xef],
    );
}

#[test]
fn with() {
    test_bidir(With { y: 0xdeadbeef }, &[0xef, 0xbe, 0xad, 0xde]);
}

#[test]
fn with_separate() {
    test_bidir(WithSeparate { y: 0xdeadbeef }, &[0xef, 0xbe, 0xad, 0xde]);
}

#[test]
fn field_ctx() {
    test_bidir(FieldCtx { y: 0xdeadbeef }, &[0xef, 0xbe, 0xad, 0xde]);
}

#[test]
fn container_ctx() {
    test_bidir_ctx(
        ContainerCtx { y: 0xdeadbeef },
        &[0xef, 0xbe, 0xad, 0xde],
        ctx::Endian::Little,
    );
}

#[test]
fn id_ctx() {
    test_bidir(IdCtx::Bar, &[0x01, 0x00]);
}

#[test]
fn skip_if() {
    test_bidir(SkipIf { x: 8, y: None }, &[0x08]);
    test_bidir(SkipIf { x: 7, y: Some(2) }, &[0x07, 0x00, 0x00, 0x00, 0x02]);
}
