use declio::Encode;

fn test_encode<T>(input: T, expected: &[u8])
where
    T: Encode,
{
    let mut output = Vec::new();
    input.encode((), &mut output).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn test_unit_struct_encode() {
    #[derive(Encode)]
    #[declio(bound = "UnitStruct: Encode")]
    struct UnitStruct;

    test_encode(UnitStruct, &[]);
}

#[test]
fn test_tuple_struct_encode() {
    #[derive(Encode)]
    struct TupleStruct(u8, u32);

    test_encode(
        TupleStruct(0xab, 0xdeadbeef),
        &[0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}

#[test]
fn test_struct_encode() {
    #[derive(Encode)]
    struct Struct {
        x: u8,
        y: u32,
    }

    test_encode(
        Struct {
            x: 0xab,
            y: 0xdeadbeef,
        },
        &[0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}
