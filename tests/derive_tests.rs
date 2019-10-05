use std::borrow::Cow;
use std::fmt::Debug;

use speedy::{Readable, Writable};

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStruct {
    /// A doc comment.
    a: u8,
    b: u16,
    c: u32
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedTupleStruct( u8, u16, u32 );

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedUnitStruct;

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedEmptyStruct {}

#[derive(PartialEq, Debug, Readable, Writable)]
enum DerivedSimpleEnum {
    /// A doc comment.
    A,
    B = 10,
    C
}

#[derive(PartialEq, Debug, Readable, Writable)]
enum DerivedEnum {
    A,
    B( u8, u16, u32 ),
    C {
        /// A doc comment.
        a: u8,
        b: u16,
        c: u32
    }
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithLifetimeBytes< 'a > {
    bytes: Cow< 'a, [u8] >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithLifetimeStr< 'a > {
    inner: Cow< 'a, str >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithGenericCow< 'a, T: 'a + ToOwned + ?Sized > where <T as ToOwned>::Owned: Debug {
    inner: Cow< 'a, T >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithGenericRef< 'a, T: 'a + ?Sized > {
    inner: &'a T
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithGeneric< T > {
    inner: T
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithDefaultOnEof {
    a: u8,
    #[speedy(default_on_eof)]
    b: u16,
    #[speedy(default_on_eof)]
    c: u32
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedRecursiveStruct {
    inner: Vec< DerivedRecursiveStruct >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithVecWithCount {
    length: u8,
    #[speedy(count = length * 2)]
    data: Vec< bool >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithCowWithCount< 'a > {
    length: u8,
    #[speedy(count = length * 2)]
    data: Cow< 'a, [bool] >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedTupleStructWithVecWithCount(
    u8,
    #[speedy(count = t0 * 2)]
    Vec< bool >
);

mod inner {
    use speedy::{Readable, Writable};

    #[derive(Readable, Writable)]
    struct Private {
    }

    // This is here only to make sure it compiles.
    #[derive(Readable, Writable)]
    pub struct Public {
        field: Vec< Private >
    }
}

macro_rules! define_test {
    ($($name:ident: $value:expr, $serialized:expr)*) => { $(
        #[test]
        fn $name() {
            use speedy::{
                Readable,
                Writable,
                Endianness
            };

            let original = $value;
            let serialized = original.write_to_vec( Endianness::LittleEndian ).unwrap();
            assert_eq!( serialized, $serialized );

            let deserialized = Readable::read_from_buffer( Endianness::LittleEndian, &serialized ).unwrap();
            assert_eq!( original, deserialized );
        }
    )* }
}

define_test!(
    test_derived_struct:
        DerivedStruct { a: 1, b: 2, c: 3 },
        &[1, 2, 0, 3, 0, 0, 0]

    test_derived_tuple_struct:
        DerivedTupleStruct( 1, 2, 3 ),
        &[1, 2, 0, 3, 0, 0, 0]

    test_derived_unit_struct:
        DerivedUnitStruct,
        &[]

    test_derived_empty_struct:
        DerivedEmptyStruct {},
        &[]

    test_derived_simple_enum_a:
        DerivedSimpleEnum::A,
        &[0, 0, 0, 0]

    test_derived_simple_enum_b:
        DerivedSimpleEnum::B,
        &[10, 0, 0, 0]

    test_derived_simple_enum_c:
        DerivedSimpleEnum::C,
        &[11, 0, 0, 0]

    test_derived_enum_unit_variant:
        DerivedEnum::A,
        &[0, 0, 0, 0]

    test_derived_enum_tuple_variant:
        DerivedEnum::B( 10, 20, 30 ),
        &[1, 0, 0, 0, 10, 20, 0, 30, 0, 0, 0]

    test_derived_enum_struct_variant:
        DerivedEnum::C { a: 100, b: 200, c: 300 },
        &[2, 0, 0, 0, 100, 200, 0, 44, 1, 0, 0]

    test_derived_struct_with_lifetime_bytes:
        DerivedStructWithLifetimeBytes { bytes: Cow::Borrowed( &[2, 4, 8] ) },
        &[3, 0, 0, 0, 2, 4, 8]

    test_derived_struct_with_lifetime_str:
        DerivedStructWithLifetimeStr { inner: Cow::Borrowed( "ABC" ) },
        &[3, 0, 0, 0, 0x41, 0x42, 0x43]

    test_derived_struct_with_generic_cow:
        DerivedStructWithGenericCow { inner: Cow::Borrowed( "ABC" ) },
        &[3, 0, 0, 0, 0x41, 0x42, 0x43]

    test_derived_struct_with_generic:
        DerivedStructWithGeneric { inner: Cow::Borrowed( &[1_u8, 2_u8, 3_u8][..] ) },
        &[3, 0, 0, 0, 1, 2, 3]

    test_derived_recursive_struct_empty:
        DerivedRecursiveStruct { inner: Vec::new() },
        &[0, 0, 0, 0]

    test_derived_recursive_struct_one_element:
        DerivedRecursiveStruct { inner: vec![ DerivedRecursiveStruct { inner: Vec::new() } ] },
        &[1, 0, 0, 0, 0, 0, 0, 0]

    test_derived_struct_with_vec_with_count:
        DerivedStructWithVecWithCount { length: 2, data: vec![ true, false, false, true ] },
        &[2, 1, 0, 0, 1]

    test_derived_struct_with_cow_with_count:
        DerivedStructWithCowWithCount { length: 2, data: vec![ true, false, false, true ].into() },
        &[2, 1, 0, 0, 1]

    test_derived_tuple_struct_with_vec_with_count:
        DerivedTupleStructWithVecWithCount( 2, vec![ true, false, false, true ] ),
        &[2, 1, 0, 0, 1]
);

#[test]
fn test_derived_struct_with_default_on_eof() {
    use speedy::{
       Readable,
       Endianness
    };

    let deserialized: DerivedStructWithDefaultOnEof = Readable::read_from_buffer( Endianness::LittleEndian, &[0xAA] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithDefaultOnEof { a: 0xAA, b: 0, c: 0 } );

    let deserialized: DerivedStructWithDefaultOnEof = Readable::read_from_buffer( Endianness::LittleEndian, &[0xAA, 0xBB] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithDefaultOnEof { a: 0xAA, b: 0, c: 0 } );

    let deserialized: DerivedStructWithDefaultOnEof = Readable::read_from_buffer( Endianness::LittleEndian, &[0xAA, 0xBB, 0xCC] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithDefaultOnEof { a: 0xAA, b: 0xCCBB, c: 0 } );
}

#[test]
fn test_length_mismatch_with_count_attribute() {
    use speedy::{
        Endianness,
        Writable
    };

    let err = DerivedStructWithVecWithCount {
        length: 0,
        data: vec![ true ]
    }.write_to_vec( Endianness::LittleEndian ).unwrap_err();

    assert_eq!(
        err.to_string(),
        "the length of 'data' is not the same as its 'count' attribute"
    );
}

#[test]
fn test_minimum_bytes_needed() {
    use speedy::{
        Readable,
        Endianness
    };

    assert_eq!( <DerivedStruct as Readable< Endianness >>::minimum_bytes_needed(), 7 );
    assert_eq!( <DerivedTupleStruct as Readable< Endianness >>::minimum_bytes_needed(), 7 );
    assert_eq!( <DerivedUnitStruct as Readable< Endianness >>::minimum_bytes_needed(), 0 );
    assert_eq!( <DerivedEmptyStruct as Readable< Endianness >>::minimum_bytes_needed(), 0 );
    assert_eq!( <DerivedSimpleEnum as Readable< Endianness >>::minimum_bytes_needed(), 4 );
    assert_eq!( <DerivedEnum as Readable< Endianness >>::minimum_bytes_needed(), 4 );
    assert_eq!( <DerivedStructWithLifetimeBytes as Readable< Endianness >>::minimum_bytes_needed(), 4 );
    assert_eq!( <DerivedStructWithLifetimeStr as Readable< Endianness >>::minimum_bytes_needed(), 4 );
    assert_eq!( <DerivedStructWithDefaultOnEof as Readable< Endianness >>::minimum_bytes_needed(), 1 );
}
