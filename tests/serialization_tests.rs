use std::borrow::Cow;
use std::ops::Range;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Debug;

#[allow(unused_imports)]
use speedy::{Readable, Writable, Endianness};

macro_rules! symmetric_tests {
    ($(
        $name:ident for $type:ty {
            in = $value:expr,
            le = $le_bytes:expr,
            be = $be_bytes:expr,
            minimum_bytes = $minimum_bytes:expr
        }
    )*) => { $(
        mod $name {
            use super::*;

            #[test]
            fn round_trip_le_borrowed_aligned() {
                let original: $type = $value;
                let serialized = original.write_to_vec( Endianness::LittleEndian ).unwrap();
                let deserialized: $type = Readable::read_from_buffer( Endianness::LittleEndian, &serialized ).unwrap();
                assert_eq!( original, deserialized );
            }

            #[test]
            fn round_trip_le_borrowed_unaligned() {
                let original: (u8, $type) = (1, $value);
                let serialized = original.write_to_vec( Endianness::LittleEndian ).unwrap();
                let deserialized: (u8, $type) = Readable::read_from_buffer( Endianness::LittleEndian, &serialized ).unwrap();
                assert_eq!( original, deserialized );
            }

            #[test]
            fn round_trip_le_owned() {
                let original: $type = $value;
                let serialized = original.write_to_vec( Endianness::LittleEndian ).unwrap();
                let deserialized: $type = Readable::read_from_buffer_owned( Endianness::LittleEndian, &serialized ).unwrap();
                assert_eq!( original, deserialized );
            }

            #[test]
            fn round_trip_be_borrowed_aligned() {
                let original: $type = $value;
                let serialized = original.write_to_vec( Endianness::BigEndian ).unwrap();
                let deserialized: $type = Readable::read_from_buffer( Endianness::BigEndian, &serialized ).unwrap();
                assert_eq!( original, deserialized );
            }

            #[test]
            fn round_trip_be_borrowed_unaligned() {
                let original: (u8, $type) = (1, $value);
                let serialized = original.write_to_vec( Endianness::BigEndian ).unwrap();
                let deserialized: (u8, $type) = Readable::read_from_buffer( Endianness::BigEndian, &serialized ).unwrap();
                assert_eq!( original, deserialized );
            }

            #[test]
            fn round_trip_be_owned() {
                let original: $type = $value;
                let serialized = original.write_to_vec( Endianness::BigEndian ).unwrap();
                let deserialized: $type = Readable::read_from_buffer_owned( Endianness::BigEndian, &serialized ).unwrap();
                assert_eq!( original, deserialized );
            }

            #[test]
            fn serialized_bytes_le() {
                let original: $type = $value;
                let serialized = original.write_to_vec( Endianness::LittleEndian ).unwrap();
                assert_eq!( serialized, $le_bytes );
            }

            #[test]
            fn serialized_bytes_be() {
                let original: $type = $value;
                let serialized = original.write_to_vec( Endianness::BigEndian ).unwrap();
                assert_eq!( serialized, $be_bytes );
            }

            #[test]
            fn serialize_to_buffer_le() {
                let original: $type = $value;
                let mut buffer = Vec::new();
                buffer.resize( Writable::< Endianness >::bytes_needed( &original ).unwrap(), 0xff );
                original.write_to_buffer( Endianness::LittleEndian, &mut buffer ).unwrap();
                assert_eq!( buffer, $le_bytes );

                let serialized = original.write_to_vec( Endianness::LittleEndian ).unwrap();
                assert_eq!( buffer, serialized );
            }

            #[test]
            fn serialize_to_buffer_be() {
                let original: $type = $value;
                let mut buffer = Vec::new();
                buffer.resize( Writable::< Endianness >::bytes_needed( &original ).unwrap(), 0xff );
                original.write_to_buffer( Endianness::BigEndian, &mut buffer ).unwrap();
                assert_eq!( buffer, $be_bytes );

                let serialized = original.write_to_vec( Endianness::BigEndian ).unwrap();
                assert_eq!( buffer, serialized );
            }

            #[test]
            fn minimum_bytes() {
                assert_eq!( <$type as Readable< Endianness >>::minimum_bytes_needed(), $minimum_bytes );
            }
        }
    )* }
}

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
#[speedy(tag_type = u8)]
enum DerivedSimpleEnumTagTypeU8 {
    A,
    B = 0xab,
    C
}

#[derive(PartialEq, Debug, Readable, Writable)]
#[speedy(tag_type = u16)]
enum DerivedSimpleEnumTagTypeU16 {
    A,
    B = 0xabcd,
    C
}

#[derive(PartialEq, Debug, Readable, Writable)]
#[speedy(tag_type = u32)]
enum DerivedSimpleEnumTagTypeU32 {
    A,
    B = 0xabcdef01,
    C
}

#[derive(PartialEq, Debug, Readable, Writable)]
#[speedy(tag_type = u64)]
#[repr(u64)]
enum DerivedSimpleEnumTagTypeU64 {
    A,
    B = 0xabcdef0123456789_u64,
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
struct DerivedStructWithStringWithCount {
    length: u8,
    #[speedy(count = length * 2)]
    data: String
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithCowSliceWithCount< 'a > {
    length: u8,
    #[speedy(count = length * 2)]
    data: Cow< 'a, [bool] >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithCowStrWithCount< 'a > {
    length: u8,
    #[speedy(count = length * 2)]
    data: Cow< 'a, str >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithHashMapWithCount {
    length: u8,
    #[speedy(count = length / 4)]
    data: HashMap< u8, u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithBTreeMapWithCount {
    length: u8,
    #[speedy(count = length / 4)]
    data: BTreeMap< u8, u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithHashSetWithCount {
    length: u8,
    #[speedy(count = length / 4)]
    data: HashSet< u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithBTreeSetWithCount {
    length: u8,
    #[speedy(count = length / 4)]
    data: BTreeSet< u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedTupleStructWithVecWithCount(
    u8,
    #[speedy(count = t0 * 2)]
    Vec< bool >
);

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithVecWithDefaultOnEof {
    #[speedy(default_on_eof)]
    data: Vec< u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithVecWithCountWithDefaultOnEof {
    length: u8,
    #[speedy(count = length, default_on_eof)]
    data: Vec< u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithVecWithLengthTypeU8 {
    #[speedy(length_type = u8)]
    data: Vec< u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithVecWithLengthTypeU16 {
    #[speedy(length_type = u16)]
    data: Vec< u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithVecWithLengthTypeU32 {
    #[speedy(length_type = u32)]
    data: Vec< u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithVecWithLengthTypeU64 {
    #[speedy(length_type = u64)]
    data: Vec< u8 >
}

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

#[derive(Clone, PartialEq, Debug, Readable, Writable)]
struct Newtype( u16 );

symmetric_tests! {
    vec_u8 for Vec< u8 > {
        in = vec![ 10, 11 ],
        le = [
            2, 0, 0, 0,
            10,
            11
        ],
        be = [
            0, 0, 0, 2,
            10,
            11
        ],
        minimum_bytes = 4
    }
    cow_u8 for Cow< [u8] > {
        in = Cow::Owned( vec![ 10, 11 ] ),
        le = [
            2, 0, 0, 0,
            10,
            11
        ],
        be = [
            0, 0, 0, 2,
            10,
            11
        ],
        minimum_bytes = 4
    }
    vec_u16 for Vec< u16 > {
        in = vec![ 10, 11 ],
        le = [
            2, 0, 0, 0,
            10, 0,
            11, 0
        ],
        be = [
            0, 0, 0, 2,
            0, 10,
            0, 11
        ],
        minimum_bytes = 4
    }
    cow_u16 for Cow< [u16] > {
        in = Cow::Owned( vec![ 10, 11 ] ),
        le = [
            2, 0, 0, 0,
            10, 0,
            11, 0
        ],
        be = [
            0, 0, 0, 2,
            0, 10,
            0, 11
        ],
        minimum_bytes = 4
    }
    vec_u32 for Vec< u32 > {
        in = vec![ 10, 11 ],
        le = [
            2, 0, 0, 0,
            10, 0, 0, 0,
            11, 0, 0, 0
        ],
        be = [
            0, 0, 0, 2,
            0, 0, 0, 10,
            0, 0, 0, 11
        ],
        minimum_bytes = 4
    }
    cow_u32 for Cow< [u32] > {
        in = Cow::Owned( vec![ 10, 11 ] ),
        le = [
            2, 0, 0, 0,
            10, 0, 0, 0,
            11, 0, 0, 0
        ],
        be = [
            0, 0, 0, 2,
            0, 0, 0, 10,
            0, 0, 0, 11
        ],
        minimum_bytes = 4
    }
    vec_u64 for Vec< u64 > {
        in = vec![ 10, 11 ],
        le = [
            2, 0, 0, 0,
            10, 0, 0, 0, 0, 0, 0, 0,
            11, 0, 0, 0, 0, 0, 0, 0
        ],
        be = [
            0, 0, 0, 2,
            0, 0, 0, 0, 0, 0, 0, 10,
            0, 0, 0, 0, 0, 0, 0, 11
        ],
        minimum_bytes = 4
    }
    cow_u64 for Cow< [u64] > {
        in = Cow::Owned( vec![ 10, 11 ] ),
        le = [
            2, 0, 0, 0,
            10, 0, 0, 0, 0, 0, 0, 0,
            11, 0, 0, 0, 0, 0, 0, 0
        ],
        be = [
            0, 0, 0, 2,
            0, 0, 0, 0, 0, 0, 0, 10,
            0, 0, 0, 0, 0, 0, 0, 11
        ],
        minimum_bytes = 4
    }
    vec_newtype for Vec< Newtype > {
        in = vec![ Newtype( 10 ), Newtype( 11 ) ],
        le = [
            2, 0, 0, 0,
            10, 0,
            11, 0
        ],
        be = [
            0, 0, 0, 2,
            0, 10,
            0, 11
        ],
        minimum_bytes = 4
    }
    cow_newtype for Cow< [Newtype] > {
        in = Cow::Owned( vec![ Newtype( 10 ), Newtype( 11 ) ] ),
        le = [
            2, 0, 0, 0,
            10, 0,
            11, 0
        ],
        be = [
            0, 0, 0, 2,
            0, 10,
            0, 11
        ],
        minimum_bytes = 4
    }
    bool_false for bool {
        in = false,
        le = [0],
        be = [0],
        minimum_bytes = 1
    }
    bool_true for bool {
        in = true,
        le = [1],
        be = [1],
        minimum_bytes = 1
    }
    u8 for u8 {
        in = 33,
        le = [33],
        be = [33],
        minimum_bytes = 1
    }
    i8 for i8 {
        in = -33,
        le = [223],
        be = [223],
        minimum_bytes = 1
    }
    u16 for u16 {
        in = 33,
        le = [33, 0],
        be = [0, 33],
        minimum_bytes = 2
    }
    i16 for i16 {
        in = -33,
        le = [223, 255],
        be = [255, 223],
        minimum_bytes = 2
    }
    u32 for u32 {
        in = 33,
        le = [33, 0, 0, 0],
        be = [0, 0, 0, 33],
        minimum_bytes = 4
    }
    i32 for i32 {
        in = -33,
        le = [223, 255, 255, 255],
        be = [255, 255,255, 223],
        minimum_bytes = 4
    }
    u64 for u64 {
        in = 33,
        le = [33, 0, 0, 0, 0, 0, 0, 0],
        be = [0, 0, 0, 0, 0, 0, 0, 33],
        minimum_bytes = 8
    }
    i64 for i64 {
        in = -33,
        le = [223, 255, 255, 255, 255, 255, 255, 255],
        be = [255, 255, 255, 255, 255, 255, 255, 223],
        minimum_bytes = 8
    }
    usize for usize {
        in = 33,
        le = [33, 0, 0, 0, 0, 0, 0, 0],
        be = [0, 0, 0, 0, 0, 0, 0, 33],
        minimum_bytes = 8
    }
    string for String {
        in = "Hello".to_owned(),
        le = [5, 0, 0, 0, 72, 101, 108, 108, 111],
        be = [0, 0, 0, 5, 72, 101, 108, 108, 111],
        minimum_bytes = 4
    }
    cow_str for Cow< str > {
        in = Cow::Owned( "Hello".to_owned() ),
        le = [5, 0, 0, 0, 72, 101, 108, 108, 111],
        be = [0, 0, 0, 5, 72, 101, 108, 108, 111],
        minimum_bytes = 4
    }
    range_u16 for Range< u16 > {
        in = 10..11,
        le = [10, 0, 11, 0],
        be = [0, 10, 0, 11],
        minimum_bytes = 4
    }
    unit for () {
        in = (),
        le = [],
        be = [],
        minimum_bytes = 0
    }
    tuple_u16 for (u16,) {
        in = (10,),
        le = [10, 0],
        be = [0, 10],
        minimum_bytes = 2
    }
    tuple_u16_u16 for (u16, u16) {
        in = (10, 11),
        le = [10, 0, 11, 0],
        be = [0, 10, 0, 11],
        minimum_bytes = 4
    }
    option_u16_some for Option< u16 > {
        in = Some( 10 ),
        le = [1, 10, 0],
        be = [1, 0, 10],
        minimum_bytes = 1
    }
    option_u16_none for Option< u16 > {
        in = None,
        le = [0],
        be = [0],
        minimum_bytes = 1
    }
    hashmap for HashMap< u16, bool > {
        in = vec![ (10, true) ].into_iter().collect(),
        le = [1, 0, 0, 0, 10, 0, 1],
        be = [0, 0, 0, 1, 0, 10, 1],
        minimum_bytes = 4
    }
    hashset for HashSet< u16 > {
        in = vec![ 10 ].into_iter().collect(),
        le = [1, 0, 0, 0, 10, 0],
        be = [0, 0, 0, 1, 0, 10],
        minimum_bytes = 4
    }
    btreemap for BTreeMap< u16, bool > {
        in = vec![ (10, true), (20, false) ].into_iter().collect(),
        le = [2, 0, 0, 0, 10, 0, 1, 20, 0, 0],
        be = [0, 0, 0, 2, 0, 10, 1, 0, 20, 0],
        minimum_bytes = 4
    }
    btreeset for BTreeSet< u16 > {
        in = vec![ 10, 20 ].into_iter().collect(),
        le = [2, 0, 0, 0, 10, 0, 20, 0],
        be = [0, 0, 0, 2, 0, 10, 0, 20],
        minimum_bytes = 4
    }
    char for char {
        in = '妹',
        le = [0xb9, 0x59, 0, 0],
        be = [0, 0, 0x59, 0xb9],
        minimum_bytes = 4
    }
    derived_struct for DerivedStruct {
        in = DerivedStruct { a: 1, b: 2, c: 3 },
        le = [1, 2, 0, 3, 0, 0, 0],
        be = [1, 0, 2, 0, 0, 0, 3],
        minimum_bytes = 7
    }
    derived_tuple_struct for DerivedTupleStruct {
        in = DerivedTupleStruct( 1, 2, 3 ),
        le = [1, 2, 0, 3, 0, 0, 0],
        be = [1, 0, 2, 0, 0, 0, 3],
        minimum_bytes = 7
    }
    derived_unit_struct for DerivedUnitStruct {
        in = DerivedUnitStruct,
        le = [],
        be = [],
        minimum_bytes = 0
    }
    derived_empty_struct for DerivedEmptyStruct {
        in = DerivedEmptyStruct {},
        le = [],
        be = [],
        minimum_bytes = 0
    }
    derived_simple_enum_a for DerivedSimpleEnum {
        in = DerivedSimpleEnum::A,
        le = [0, 0, 0, 0],
        be = [0, 0, 0, 0],
        minimum_bytes = 4
    }
    derived_simple_enum_b for DerivedSimpleEnum {
        in = DerivedSimpleEnum::B,
        le = [10, 0, 0, 0],
        be = [0, 0, 0, 10],
        minimum_bytes = 4
    }
    derived_simple_enum_c for DerivedSimpleEnum {
        in = DerivedSimpleEnum::C,
        le = [11, 0, 0, 0],
        be = [0, 0, 0, 11],
        minimum_bytes = 4
    }
    derived_simple_enum_tag_type_u8 for DerivedSimpleEnumTagTypeU8 {
        in = DerivedSimpleEnumTagTypeU8::B,
        le = [0xab],
        be = [0xab],
        minimum_bytes = 1
    }
    derived_simple_enum_tag_type_u16 for DerivedSimpleEnumTagTypeU16 {
        in = DerivedSimpleEnumTagTypeU16::B,
        le = [0xcd, 0xab],
        be = [0xab, 0xcd],
        minimum_bytes = 2
    }
    derived_simple_enum_tag_type_u32 for DerivedSimpleEnumTagTypeU32 {
        in = DerivedSimpleEnumTagTypeU32::B,
        le = [0x01, 0xef, 0xcd, 0xab],
        be = [0xab, 0xcd, 0xef, 0x01],
        minimum_bytes = 4
    }
    derived_simple_enum_tag_type_u64 for DerivedSimpleEnumTagTypeU64 {
        in = DerivedSimpleEnumTagTypeU64::B,
        le = [0x89, 0x67, 0x45, 0x23, 0x01, 0xef, 0xcd, 0xab],
        be = [0xab, 0xcd, 0xef, 0x01, 0x23, 0x45, 0x67, 0x89],
        minimum_bytes = 8
    }
    derived_enum_unit_variant for DerivedEnum {
        in = DerivedEnum::A,
        le = [0, 0, 0, 0],
        be = [0, 0, 0, 0],
        minimum_bytes = 4
    }
    derived_enum_tuple_variant for DerivedEnum {
        in = DerivedEnum::B( 10, 20, 30 ),
        le = [1, 0, 0, 0, 10, 20, 0, 30, 0, 0, 0],
        be = [0, 0, 0, 1, 10, 0, 20, 0, 0, 0, 30],
        minimum_bytes = 4
    }
    derived_enum_struct_variant for DerivedEnum {
        in = DerivedEnum::C { a: 100, b: 200, c: 300 },
        le = [2, 0, 0, 0, 100, 200, 0, 44, 1, 0, 0],
        be = [0, 0, 0, 2, 100, 0, 200, 0, 0, 1, 44],
        minimum_bytes = 4
    }
    derived_struct_with_lifetime_bytes for DerivedStructWithLifetimeBytes {
        in = DerivedStructWithLifetimeBytes { bytes: Cow::Borrowed( &[2, 4, 8] ) },
        le = [3, 0, 0, 0, 2, 4, 8],
        be = [0, 0, 0, 3, 2, 4, 8],
        minimum_bytes = 4
    }
    derived_struct_with_lifetime_str for DerivedStructWithLifetimeStr {
        in = DerivedStructWithLifetimeStr { inner: Cow::Borrowed( "ABC" ) },
        le = [3, 0, 0, 0, 0x41, 0x42, 0x43],
        be = [0, 0, 0, 3, 0x41, 0x42, 0x43],
        minimum_bytes = 4
    }
    derived_struct_with_generic_cow for DerivedStructWithGenericCow< str > {
        in = DerivedStructWithGenericCow { inner: Cow::Borrowed( "ABC" ) },
        le = [3, 0, 0, 0, 0x41, 0x42, 0x43],
        be = [0, 0, 0, 3, 0x41, 0x42, 0x43],
        minimum_bytes = 4
    }
    derived_struct_with_generic for DerivedStructWithGeneric< Cow< [u8] > > {
        in = DerivedStructWithGeneric { inner: Cow::Borrowed( &[1_u8, 2_u8, 3_u8][..] ) },
        le = [3, 0, 0, 0, 1, 2, 3],
        be = [0, 0, 0, 3, 1, 2, 3],
        minimum_bytes = 4
    }
    derived_recursive_struct_empty for DerivedRecursiveStruct {
        in = DerivedRecursiveStruct { inner: Vec::new() },
        le = [0, 0, 0, 0],
        be = [0, 0, 0, 0],
        minimum_bytes = 4
    }
    derived_recursive_struct_one_element for DerivedRecursiveStruct {
        in = DerivedRecursiveStruct { inner: vec![ DerivedRecursiveStruct { inner: Vec::new() } ] },
        le = [1, 0, 0, 0, 0, 0, 0, 0],
        be = [0, 0, 0, 1, 0, 0, 0, 0],
        minimum_bytes = 4
    }
    derived_struct_with_vec_with_count for DerivedStructWithVecWithCount {
        in = DerivedStructWithVecWithCount { length: 2, data: vec![ true, false, false, true ] },
        le = [2, 1, 0, 0, 1],
        be = [2, 1, 0, 0, 1],
        minimum_bytes = 1
    }
    derived_struct_with_string_with_count for DerivedStructWithStringWithCount {
        in = DerivedStructWithStringWithCount { length: 2, data: "ABCD".into() },
        le = [2, b'A', b'B', b'C', b'D'],
        be = [2, b'A', b'B', b'C', b'D'],
        minimum_bytes = 1
    }
    derived_struct_with_cow_slice_with_count for DerivedStructWithCowSliceWithCount {
        in = DerivedStructWithCowSliceWithCount { length: 2, data: vec![ true, false, false, true ].into() },
        le = [2, 1, 0, 0, 1],
        be = [2, 1, 0, 0, 1],
        minimum_bytes = 1
    }
    derived_struct_with_cow_str_with_count for DerivedStructWithCowStrWithCount {
        in = DerivedStructWithCowStrWithCount { length: 2, data: "ABCD".into() },
        le = [2, b'A', b'B', b'C', b'D'],
        be = [2, b'A', b'B', b'C', b'D'],
        minimum_bytes = 1
    }
    derived_struct_with_hash_map_with_count for DerivedStructWithHashMapWithCount {
        in = DerivedStructWithHashMapWithCount { length: 4, data: vec![ (50, 60) ].into_iter().collect() },
        le = [4, 50, 60],
        be = [4, 50, 60],
        minimum_bytes = 1
    }
    derived_struct_with_btree_map_with_count for DerivedStructWithBTreeMapWithCount {
        in = DerivedStructWithBTreeMapWithCount { length: 4, data: vec![ (50, 60) ].into_iter().collect() },
        le = [4, 50, 60],
        be = [4, 50, 60],
        minimum_bytes = 1
    }
    derived_struct_with_hash_set_with_count for DerivedStructWithHashSetWithCount {
        in = DerivedStructWithHashSetWithCount { length: 4, data: vec![ 50 ].into_iter().collect() },
        le = [4, 50],
        be = [4, 50],
        minimum_bytes = 1
    }
    derived_struct_with_btree_set_with_count for DerivedStructWithBTreeSetWithCount {
        in = DerivedStructWithBTreeSetWithCount { length: 4, data: vec![ 50 ].into_iter().collect() },
        le = [4, 50],
        be = [4, 50],
        minimum_bytes = 1
    }
    derived_tuple_struct_with_vec_with_count for DerivedTupleStructWithVecWithCount {
        in = DerivedTupleStructWithVecWithCount( 2, vec![ true, false, false, true ] ),
        le = [2, 1, 0, 0, 1],
        be = [2, 1, 0, 0, 1],
        minimum_bytes = 1
    }
    derived_struct_with_vec_with_length_type_u8 for DerivedStructWithVecWithLengthTypeU8 {
        in = DerivedStructWithVecWithLengthTypeU8 { data: vec![ 100, 101 ] },
        le = [2, 100, 101],
        be = [2, 100, 101],
        minimum_bytes = 1
    }
    derived_struct_with_vec_with_length_type_u16 for DerivedStructWithVecWithLengthTypeU16 {
        in = DerivedStructWithVecWithLengthTypeU16 { data: vec![ 100, 101 ] },
        le = [2, 0, 100, 101],
        be = [0, 2, 100, 101],
        minimum_bytes = 2
    }
    derived_struct_with_vec_with_length_type_u32 for DerivedStructWithVecWithLengthTypeU32 {
        in = DerivedStructWithVecWithLengthTypeU32 { data: vec![ 100, 101 ] },
        le = [2, 0, 0, 0, 100, 101],
        be = [0, 0, 0, 2, 100, 101],
        minimum_bytes = 4
    }
    derived_struct_with_vec_with_length_type_u64 for DerivedStructWithVecWithLengthTypeU64 {
        in = DerivedStructWithVecWithLengthTypeU64 { data: vec![ 100, 101 ] },
        le = [2, 0, 0, 0, 0, 0, 0, 0, 100, 101],
        be = [0, 0, 0, 0, 0, 0, 0, 2, 100, 101],
        minimum_bytes = 8
    }
}

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

    let deserialized: DerivedStructWithVecWithDefaultOnEof = Readable::read_from_buffer( Endianness::LittleEndian, &[] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithVecWithDefaultOnEof { data: vec![] } );

    let deserialized: DerivedStructWithVecWithCountWithDefaultOnEof = Readable::read_from_buffer( Endianness::LittleEndian, &[2, 0xAA, 0xBB] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithVecWithCountWithDefaultOnEof { length: 2, data: vec![0xAA, 0xBB] } );

    let deserialized: DerivedStructWithVecWithCountWithDefaultOnEof = Readable::read_from_buffer( Endianness::LittleEndian, &[2, 0xAA] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithVecWithCountWithDefaultOnEof { length: 2, data: vec![] } );
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

    assert_eq!( <DerivedStructWithDefaultOnEof as Readable< Endianness >>::minimum_bytes_needed(), 1 );
}
