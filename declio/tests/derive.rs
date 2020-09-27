use declio::Serialize;

fn test_serialize<T>(input: T, expected: &[u8])
where
    T: Serialize,
{
    let mut output = Vec::new();
    input.serialize((), &mut output).unwrap();
    assert_eq!(output, expected);
}

#[test]
fn test_unit_struct_serialize() {
    #[derive(Serialize)]
    struct UnitStruct;

    test_serialize(UnitStruct, &[]);
}

#[test]
fn test_tuple_struct_serialize() {
    #[derive(Serialize)]
    struct TupleStruct(u8, u32);

    test_serialize(
        TupleStruct(0xab, 0xdeadbeef),
        &[0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}

#[test]
fn test_struct_serialize() {
    #[derive(Serialize)]
    struct Struct {
        x: u8,
        y: u32,
    }

    test_serialize(
        Struct {
            x: 0xab,
            y: 0xdeadbeef,
        },
        &[0xab, 0xef, 0xbe, 0xad, 0xde],
    );
}
