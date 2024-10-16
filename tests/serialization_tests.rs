extern crate alloc;

use alloc::borrow::Cow;
use core::ops::{Range, RangeInclusive};
use alloc::collections::{BTreeMap, BTreeSet};
use std::collections::{HashMap, HashSet};
use core::fmt::Debug;
use core::num::NonZeroU32;

#[allow(unused_imports)]
use speedy::{Readable, Writable, Endianness};

macro_rules! symmetric_tests {
    ($(
        $name:ident for $type:ty {
            kind = common,
            in = $value:expr,
            le = $le_bytes:expr,
            be = $be_bytes:expr,
            minimum_bytes = $minimum_bytes:expr
        }
    )*) => { paste::paste! { $(
        mod [<$name _common_tests>] {
            use super::*;

            #[test]
            fn serialized_bytes_le() {
                let original: $type = $value;
                let serialized = original.write_to_vec_with_ctx( Endianness::LittleEndian ).unwrap();
                assert_eq!( serialized, $le_bytes );
            }

            #[test]
            fn serialized_bytes_be() {
                let original: $type = $value;
                let serialized = original.write_to_vec_with_ctx( Endianness::BigEndian ).unwrap();
                assert_eq!( serialized, $be_bytes );
            }

            #[test]
            fn serialize_to_buffer_le() {
                let original: $type = $value;
                let mut buffer = Vec::new();
                buffer.resize( Writable::< Endianness >::bytes_needed( &original ).unwrap(), 0xff );
                original.write_to_buffer_with_ctx( Endianness::LittleEndian, &mut buffer ).unwrap();
                assert_eq!( buffer, $le_bytes );

                let serialized = original.write_to_vec_with_ctx( Endianness::LittleEndian ).unwrap();
                assert_eq!( buffer, serialized );
            }

            #[test]
            fn serialize_to_buffer_be() {
                let original: $type = $value;
                let mut buffer = Vec::new();
                buffer.resize( Writable::< Endianness >::bytes_needed( &original ).unwrap(), 0xff );
                original.write_to_buffer_with_ctx( Endianness::BigEndian, &mut buffer ).unwrap();
                assert_eq!( buffer, $be_bytes );

                let serialized = original.write_to_vec_with_ctx( Endianness::BigEndian ).unwrap();
                assert_eq!( buffer, serialized );
            }

            #[test]
            fn minimum_bytes() {
                assert_eq!( <$type as Readable< Endianness >>::minimum_bytes_needed(), $minimum_bytes );
            }

            #[cfg(feature = "std")]
            #[cfg(not(miri))]
            #[test]
            fn write_to_file_le() {
                let file = tempfile::NamedTempFile::new().unwrap();
                let path = file.path();
                let original: $type = $value;
                original.write_to_file_with_ctx( Endianness::LittleEndian, &path ).unwrap();
                assert_eq!( std::fs::read( &path ).unwrap(), $le_bytes );
            }

            #[cfg(feature = "std")]
            #[cfg(not(miri))]
            #[test]
            fn write_to_file_be() {
                let file = tempfile::NamedTempFile::new().unwrap();
                let path = file.path();
                let original: $type = $value;
                original.write_to_file_with_ctx( Endianness::BigEndian, &path ).unwrap();
                assert_eq!( std::fs::read( &path ).unwrap(), $be_bytes );
            }
        }
    )* } };

    ($(
        $name:ident for $type:ty {
            kind = round_trip,
            in = $value:expr,
            le = $le_bytes:expr,
            be = $be_bytes:expr
        }
    )*) => { paste::paste! { $(
        mod [<$name _round_trip_tests>] {
            use super::*;

            #[test]
            fn round_trip_le_borrowed_aligned() {
                let original: $type = $value;
                let serialized = original.write_to_vec_with_ctx( Endianness::LittleEndian ).unwrap();
                let deserialized: Result< $type, _ > = Readable::read_from_buffer_with_ctx( Endianness::LittleEndian, &serialized );
                assert_eq!( original, deserialized.unwrap() );
            }

            #[test]
            fn round_trip_le_borrowed_unaligned() {
                let original: (u8, $type) = (1, $value);
                let serialized = original.write_to_vec_with_ctx( Endianness::LittleEndian ).unwrap();
                let deserialized: (u8, $type) = Readable::read_from_buffer_with_ctx( Endianness::LittleEndian, &serialized ).unwrap();
                assert_eq!( original.0, deserialized.0 );
                assert_eq!( original.1, deserialized.1 );
            }

            #[test]
            fn round_trip_be_borrowed_aligned() {
                let original: $type = $value;
                let serialized = original.write_to_vec_with_ctx( Endianness::BigEndian ).unwrap();
                let deserialized: $type = Readable::read_from_buffer_with_ctx( Endianness::BigEndian, &serialized ).unwrap();
                assert_eq!( original, deserialized );
            }

            #[test]
            fn round_trip_be_borrowed_unaligned() {
                let original: (u8, $type) = (1, $value);
                let serialized = original.write_to_vec_with_ctx( Endianness::BigEndian ).unwrap();
                let deserialized: (u8, $type) = Readable::read_from_buffer_with_ctx( Endianness::BigEndian, &serialized ).unwrap();
                assert_eq!( original.0, deserialized.0 );
                assert_eq!( original.1, deserialized.1 );
            }
        }
    )* } };

    ($(
        $name:ident for $type:ty {
            kind = round_trip_native_endian,
            in = $value:expr,
            le = $le_bytes:expr,
            be = $be_bytes:expr
        }
    )*) => { paste::paste! { $(
        mod [<$name _round_trip_native_endian_tests>] {
            use super::*;

            #[test]
            fn round_trip_le_borrowed_aligned() {
                let original: $type = $value;
                let serialized = original.write_to_vec_with_ctx( Endianness::LittleEndian ).unwrap();
                let deserialized: Result< $type, _ > = Readable::read_from_buffer_with_ctx( Endianness::LittleEndian, &serialized );
                if Endianness::LittleEndian.conversion_necessary() {
                    match speedy::private::get_error_kind( &deserialized.unwrap_err() ) {
                        speedy::private::ErrorKind::EndiannessMismatch => {},
                        error => panic!( "unexpected error: {:?}", error )
                    }
                } else {
                    assert_eq!( original, deserialized.unwrap() );
                }
            }

            #[test]
            fn round_trip_le_borrowed_unaligned() {
                let original: (u8, $type) = (1, $value);
                let serialized = original.write_to_vec_with_ctx( Endianness::LittleEndian ).unwrap();
                let deserialized: Result< (u8, $type), _ > = Readable::read_from_buffer_with_ctx( Endianness::LittleEndian, &serialized );
                if Endianness::LittleEndian.conversion_necessary() {
                    match speedy::private::get_error_kind( &deserialized.unwrap_err() ) {
                        speedy::private::ErrorKind::EndiannessMismatch => {},
                        error => panic!( "unexpected error: {:?}", error )
                    }
                } else {
                    let deserialized = deserialized.unwrap();
                    assert_eq!( original.0, deserialized.0 );
                    assert_eq!( original.1, deserialized.1 );
                }
            }

            #[test]
            fn round_trip_be_borrowed_aligned() {
                let original: $type = $value;
                let serialized = original.write_to_vec_with_ctx( Endianness::BigEndian ).unwrap();
                let deserialized: Result< $type, _ > = Readable::read_from_buffer_with_ctx( Endianness::BigEndian, &serialized );
                if Endianness::BigEndian.conversion_necessary() {
                    match speedy::private::get_error_kind( &deserialized.unwrap_err() ) {
                        speedy::private::ErrorKind::EndiannessMismatch => {},
                        error => panic!( "unexpected error: {:?}", error )
                    }
                } else {
                    assert_eq!( original, deserialized.unwrap() );
                }
            }

            #[test]
            fn round_trip_be_borrowed_unaligned() {
                let original: (u8, $type) = (1, $value);
                let serialized = original.write_to_vec_with_ctx( Endianness::BigEndian ).unwrap();
                let deserialized: Result< (u8, $type), _ > = Readable::read_from_buffer_with_ctx( Endianness::BigEndian, &serialized );
                if Endianness::BigEndian.conversion_necessary() {
                    match speedy::private::get_error_kind( &deserialized.unwrap_err() ) {
                        speedy::private::ErrorKind::EndiannessMismatch => {},
                        error => panic!( "unexpected error: {:?}", error )
                    }
                } else {
                    let deserialized = deserialized.unwrap();
                    assert_eq!( original.0, deserialized.0 );
                    assert_eq!( original.1, deserialized.1 );
                }
            }
        }
    )* } };

    ($(
        $name:ident for $type:ty {
            kind = sized,
            in = $value:expr,
            le = $le_bytes:expr,
            be = $be_bytes:expr
        }
    )*) => { paste::paste! { $(
        mod [<$name _sized_tests>] {
            use super::*;

            #[test]
            fn round_trip_le_owned() {
                let original: $type = $value;
                let serialized = original.write_to_vec_with_ctx( Endianness::LittleEndian ).unwrap();
                let deserialized: $type = Readable::read_from_buffer_copying_data_with_ctx( Endianness::LittleEndian, &serialized ).unwrap();
                assert_eq!( original, deserialized );
            }

            #[test]
            fn round_trip_be_owned() {
                let original: $type = $value;
                let serialized = original.write_to_vec_with_ctx( Endianness::BigEndian ).unwrap();
                let deserialized: $type = Readable::read_from_buffer_copying_data_with_ctx( Endianness::BigEndian, &serialized ).unwrap();
                assert_eq!( original, deserialized );
            }

            #[cfg(feature = "std")]
            #[test]
            fn read_from_stream_unbuffered_only_reads_what_is_necessary() {
                let original: $type = $value;
                let mut serialized = original.write_to_vec_with_ctx( Endianness::LittleEndian ).unwrap();
                let message_length = serialized.len();
                serialized.extend_from_slice( &[0, 1, 2, 3, 4, 5, 6, 7, 8] );

                let mut cursor = std::io::Cursor::new( serialized );
                let deserialized: $type = Readable::read_from_stream_unbuffered_with_ctx( Endianness::LittleEndian, &mut cursor ).unwrap();
                assert_eq!( original, deserialized );
                assert_eq!( cursor.position(), message_length as u64 );
            }

            #[cfg(feature = "std")]
            #[test]
            fn read_from_stream_buffered_reads_as_much_as_possible() {
                let original: $type = $value;
                let mut serialized = original.write_to_vec_with_ctx( Endianness::LittleEndian ).unwrap();
                let is_zero_length = serialized.is_empty();

                serialized.extend_from_slice( &[0, 1, 2, 3, 4, 5, 6, 7, 8] );
                let total_length = serialized.len();

                let mut cursor = std::io::Cursor::new( serialized );
                let deserialized: $type = Readable::read_from_stream_buffered_with_ctx( Endianness::LittleEndian, &mut cursor ).unwrap();
                assert_eq!( original, deserialized );

                if is_zero_length {
                    // If the type is zero length no buffering should be done.
                    assert_eq!( cursor.position(), 0 );
                } else {
                    assert_eq!( cursor.position(), total_length as u64 );
                }
            }

            #[cfg(feature = "std")]
            #[cfg(not(miri))]
            #[test]
            fn round_trip_file_le() {
                let file = tempfile::NamedTempFile::new().unwrap();
                let path = file.path();
                let original: $type = $value;
                original.write_to_file_with_ctx( Endianness::LittleEndian, &path ).unwrap();
                let deserialized: $type = Readable::read_from_file_with_ctx( Endianness::LittleEndian, &path ).unwrap();
                assert_eq!( original, deserialized );
            }

            #[cfg(feature = "std")]
            #[cfg(not(miri))]
            #[test]
            fn round_trip_file_be() {
                let file = tempfile::NamedTempFile::new().unwrap();
                let path = file.path();
                let original: $type = $value;
                original.write_to_file_with_ctx( Endianness::BigEndian, &path ).unwrap();
                let deserialized: $type = Readable::read_from_file_with_ctx( Endianness::BigEndian, &path ).unwrap();
                assert_eq!( original, deserialized );
            }
        }
    )* } };

    ($(
        $name:ident for $type:ty {
            in = $value:expr,
            le = $le_bytes:expr,
            be = $be_bytes:expr,
            minimum_bytes = $minimum_bytes:expr
        }
    )*) => { $(
        symmetric_tests! {
            $name for $type {
                kind = common,
                in = $value,
                le = $le_bytes,
                be = $be_bytes,
                minimum_bytes = $minimum_bytes
            }
        }

        symmetric_tests! {
            $name for $type {
                kind = round_trip,
                in = $value,
                le = $le_bytes,
                be = $be_bytes
            }
        }

        symmetric_tests! {
            $name for $type {
                kind = sized,
                in = $value,
                le = $le_bytes,
                be = $be_bytes
            }
        }
    )* };
}

macro_rules! symmetric_tests_unsized {
    ($(
        $name:ident for $type:ty {
            in = $value:expr,
            le = $le_bytes:expr,
            be = $be_bytes:expr,
            minimum_bytes = $minimum_bytes:expr
        }
    )*) => { $(
        symmetric_tests! {
            $name for $type {
                kind = common,
                in = $value,
                le = $le_bytes,
                be = $be_bytes,
                minimum_bytes = $minimum_bytes
            }
        }

        symmetric_tests! {
            $name for $type {
                kind = round_trip,
                in = $value,
                le = $le_bytes,
                be = $be_bytes
            }
        }
    )* };
}

macro_rules! symmetric_tests_unsized_native_endian {
    ($(
        $name:ident for $type:ty {
            in = $value:expr,
            le = $le_bytes:expr,
            be = $be_bytes:expr,
            minimum_bytes = $minimum_bytes:expr
        }
    )*) => { $(
        symmetric_tests! {
            $name for $type {
                kind = common,
                in = $value,
                le = $le_bytes,
                be = $be_bytes,
                minimum_bytes = $minimum_bytes
            }
        }

        symmetric_tests! {
            $name for $type {
                kind = round_trip_native_endian,
                in = $value,
                le = $le_bytes,
                be = $be_bytes
            }
        }
    )* };
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
#[speedy(tag_type = u7)]
enum DerivedSimpleEnumTagTypeU7 {
    A,
    B,
    C = 0x7f
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
#[speedy(tag_type = u64_varint)]
enum DerivedSimpleEnumTagTypeVarInt64 {
    A,
    B = 0x1234
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
struct DerivedStructWithGenericDefault< T = u8 > {
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
    #[speedy(length = length * 2)]
    data: Vec< bool >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithStringWithCount {
    length: u8,
    #[speedy(length = length * 2)]
    data: String
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithCowSliceWithCount< 'a > {
    length: u8,
    #[speedy(length = length * 2)]
    data: Cow< 'a, [bool] >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithCowStrWithCount< 'a > {
    length: u8,
    #[speedy(length = length * 2)]
    data: Cow< 'a, str >
}

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedRefSliceU8WithCount< 'a > {
    length: u8,
    #[speedy(length = length * 2)]
    data: &'a [u8]
}

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedRefStrWithCount< 'a > {
    length: u8,
    #[speedy(length = length * 2)]
    data: &'a str
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithHashMapWithCount {
    length: u8,
    #[speedy(length = length / 4)]
    data: HashMap< u8, u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithBTreeMapWithCount {
    length: u8,
    #[speedy(length = length / 4)]
    data: BTreeMap< u8, u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithHashSetWithCount {
    length: u8,
    #[speedy(length = length / 4)]
    data: HashSet< u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithBTreeSetWithCount {
    length: u8,
    #[speedy(length = length / 4)]
    data: BTreeSet< u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithCowHashMapWithCount< 'a > {
    length: u8,
    #[speedy(length = length / 4)]
    data: Cow< 'a, HashMap< u8, u8 > >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithCowBTreeMapWithCount< 'a > {
    length: u8,
    #[speedy(length = length / 4)]
    data: Cow< 'a, BTreeMap< u8, u8 > >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithCowHashSetWithCount< 'a > {
    length: u8,
    #[speedy(length = length / 4)]
    data: Cow< 'a, HashSet< u8 > >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithCowBTreeSetWithCount< 'a > {
    length: u8,
    #[speedy(length = length / 4)]
    data: Cow< 'a, BTreeSet< u8 > >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedTupleStructWithVecWithCount(
    u8,
    #[speedy(length = t0 * 2)]
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
    #[speedy(length = length, default_on_eof)]
    data: Vec< u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithVecWithLengthTypeU7 {
    #[speedy(length_type = u7)]
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

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithVecWithLengthTypeVarInt64 {
    #[speedy(length_type = u64_varint)]
    data: Vec< u8 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithOptionVecWithLengthTypeU16 {
    #[speedy(length_type = u16)]
    data: Option< Vec< u8 > >
}

#[derive(PartialEq, Debug, Readable, Writable)]
#[speedy(tag_type = u8)]
enum DerivedEnumWithCustomTag {
    #[speedy(tag = 20)]
    A = 10,
    B
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithOptionU16 {
    data: Option< u16 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithResultU16 {
    data: Result< u16, u16 >
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithSkippedField {
    a: u8,
    #[speedy(skip)]
    _b: u8,
    c: u8
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

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithArray< T > {
    data: [T; 4]
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithConstantPrefixString {
    #[speedy(constant_prefix = "ABC")]
    value: ()
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithConstantPrefixByteString {
    #[speedy(constant_prefix = b"ABC")]
    value: ()
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithConstantPrefixChar {
    #[speedy(constant_prefix = '苺')]
    value: ()
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithConstantPrefixByte {
    #[speedy(constant_prefix = b'A')]
    value: ()
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithConstantPrefixBoolTrue {
    #[speedy(constant_prefix = true)]
    value: ()
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithConstantPrefixBoolFalse {
    #[speedy(constant_prefix = false)]
    value: ()
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithConstantPrefixU8 {
    #[speedy(constant_prefix = 10_u8)]
    value: ()
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithConstantPrefixI8 {
    #[speedy(constant_prefix = -1_i8)]
    value: ()
}

#[derive(PartialEq, Debug, Readable, Writable)]
#[speedy(tag_type = u8)]
#[speedy(peek_tag)]
enum DerivedEnumWithPeekTagU8 {
    #[speedy(tag = 1)]
    One( u8 ),
    #[speedy(tag = 2)]
    Two( u8 )
}

#[derive(PartialEq, Debug, Readable, Writable)]
#[speedy(tag_type = u8)]
enum DerivedRecursiveEnum {
    None,
    Some( Box< DerivedRecursiveEnum > )
}

// This is here only to make sure it compiles.
#[derive(PartialEq, Debug, Readable, Writable)]
struct DerivedStructWithTwoDifferentCows< 'a > {
    a: Cow< 'a, BTreeMap< u8, u8 > >,
    b: Cow< 'a, [u8] >
}

#[derive(Readable, Writable)]
struct DerivedTupleStructWithGenericVec< T >( Vec< T > );

#[cfg(feature = "std")]
#[derive(Readable, Writable)]
struct DerivedTupleStructWithGenericHashMap< K, V >( HashMap< K, V > ) where K: std::hash::Hash + Eq;

#[cfg(feature = "std")]
#[derive(Readable, Writable)]
struct DerivedTupleStructWithGenericHashSet< T >( HashSet< T > ) where T: std::hash::Hash + Eq;

#[derive(Readable, Writable)]
struct DerivedTupleStructWithGenericBTreeMap< K, V >( BTreeMap< K, V > ) where K: Ord;

#[derive(Readable, Writable)]
struct DerivedTupleStructWithGenericBTreeSet< T >( BTreeSet< T > ) where T: Ord;

#[cfg(feature = "std")]
#[derive(Readable, Writable)]
struct DerivedTupleStructWithGenericCowHashMap< 'a, K, V >( Cow< 'a, HashMap< K, V > > ) where K: std::hash::Hash + Eq + Clone, V: Clone;

#[cfg(feature = "std")]
#[derive(Readable, Writable)]
struct DerivedTupleStructWithGenericCowHashSet< 'a, T >( Cow< 'a, HashSet< T > > ) where T: std::hash::Hash + Eq + Clone;

#[derive(Readable, Writable)]
struct DerivedTupleStructWithGenericCowBTreeMap< 'a, K, V >( Cow< 'a, BTreeMap< K, V > > ) where K: Ord + Clone, V: Clone;

#[derive(Readable, Writable)]
struct DerivedTupleStructWithGenericCowBTreeSet< 'a, T >( Cow< 'a, BTreeSet< T > > ) where T: Ord + Clone;

macro_rules! atomic_wrapper {
    ($name:ident, $base_ty:ident) => {
        #[derive(Debug, Readable, Writable)]
        struct $name( core::sync::atomic::$name );

        impl $name {
            pub fn new( value: $base_ty ) -> Self {
                $name( value.into() )
            }
        }

        impl PartialEq for $name {
            fn eq( &self, rhs: &$name ) -> bool {
                self.0.load( core::sync::atomic::Ordering::SeqCst ) ==
                rhs.0.load( core::sync::atomic::Ordering::SeqCst )
            }
        }
    }
}

atomic_wrapper!( AtomicI8, i8 );
atomic_wrapper!( AtomicU8, u8 );
atomic_wrapper!( AtomicI16, i16 );
atomic_wrapper!( AtomicU16, u16 );
atomic_wrapper!( AtomicI32, i32 );
atomic_wrapper!( AtomicU32, u32 );
atomic_wrapper!( AtomicI64, i64 );
atomic_wrapper!( AtomicU64, u64 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(transparent)]
struct TransparentU8( u8 );

#[derive(Copy, Clone, Readable, Writable, PartialEq, Eq, Debug)]
#[repr(transparent)]
struct TransparentU16( u16 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(transparent)]
struct TransparentU32( u32 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(transparent)]
struct TransparentU128( u128 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedRefStr< 'a >( &'a str );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedRefSliceU8< 'a >( &'a [u8] );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedStringUntilEof(
    #[speedy(length = ..)]
    String
);

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedVecUntilEof(
    #[speedy(length = ..)]
    Vec< u8 >
);

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedCowSliceUntilEof< 'a >(
    #[speedy(length = ..)]
    Cow< 'a, [u8] >
);

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedCowStrUntilEof< 'a >(
    #[speedy(length = ..)]
    Cow< 'a, str >
);

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedBTreeSetUntilEof(
    #[speedy(length = ..)]
    BTreeSet< u16 >
);

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedRefSliceU8UntilEof< 'a >(
    #[speedy(length = ..)]
    &'a [u8]
);

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedRefStrUntilEof< 'a >(
    #[speedy(length = ..)]
    &'a str
);

#[derive(Copy, Clone, Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedRefSlicePackedTuple< 'a >(
    &'a [DerivedPackedTuple]
);

#[derive(Copy, Clone, Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedRefSlicePackedTupleUntilEof< 'a >(
    #[speedy(length = ..)]
    &'a [DerivedPackedTuple]
);

#[derive(Copy, Clone, Readable, Writable, PartialEq, Eq, Debug)]
#[repr(packed)]
struct DerivedPackedTuple( u16, u16 );

#[derive(Copy, Clone, Readable, Writable, PartialEq, Eq, Debug)]
#[repr(packed)]
struct DerivedPackedRecursiveTuple( DerivedPackedTuple, DerivedPackedTuple );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(C)]
struct DeriveCWithU8( u8 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(C)]
struct DeriveCWithU16( u16 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(C)]
struct DeriveCWithU16U16( u16, u16 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(C)]
struct DeriveCWithU16U8( u16, u8 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(C)]
struct DeriveCWithDeriveC( DeriveCWithU16 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(packed)]
struct DerivePackedDummy;

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(transparent)]
struct DeriveTransparentDummy;

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(C)]
struct DeriveCDummy;

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(transparent)]
pub struct DeriveVecGenericTransparent< T >( Vec< T > );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(transparent)]
pub struct DeriveArrayTransparent( [u8; 16] );

#[derive(Copy, Clone, Readable, Writable, PartialEq, Eq, Debug)]
#[repr(packed)]
struct PackedU16( u16 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct DerivedStructWithVarInt {
    #[speedy(varint)]
    value: u64
}

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
    transparent_u8 for TransparentU8 {
        in = TransparentU8(33),
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
    packed_tuple_u16 for DerivedPackedTuple {
        in = DerivedPackedTuple( 33, 66 ),
        le = [33, 0, 66, 0],
        be = [0, 33, 0, 66],
        minimum_bytes = 4
    }
    packed_recursive_tuple_u16 for DerivedPackedRecursiveTuple {
        in = DerivedPackedRecursiveTuple( DerivedPackedTuple( 33, 66 ), DerivedPackedTuple( 34, 67 ) ),
        le = [33, 0, 66, 0, 34, 0, 67, 0],
        be = [0, 33, 0, 66, 0, 34, 0, 67],
        minimum_bytes = 8
    }
    u32 for u32 {
        in = 33,
        le = [33, 0, 0, 0],
        be = [0, 0, 0, 33],
        minimum_bytes = 4
    }
    transparent_u32 for TransparentU32 {
        in = TransparentU32(33),
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
    f32 for f32 {
        in = 8388610.0,
        le = [2, 0, 0, 75],
        be = [75, 0, 0, 2],
        minimum_bytes = 4
    }
    f64 for f64 {
        in = 8388610.0,
        le = [0, 0, 0, 64, 0, 0, 96, 65],
        be = [65, 96, 0, 0, 64, 0, 0, 0],
        minimum_bytes = 8
    }
    transparent_u128 for TransparentU128 {
        in = TransparentU128(33),
        le = [33, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        be = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 33],
        minimum_bytes = 16
    }
    u128 for u128 {
        in = 33,
        le = [33, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        be = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 33],
        minimum_bytes = 16
    }
    i128 for i128 {
        in = -33,
        le = [223, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],
        be = [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 223],
        minimum_bytes = 16
    }
    atomic_u8 for AtomicU8 {
        in = AtomicU8::new( 33 ),
        le = [33],
        be = [33],
        minimum_bytes = 1
    }
    atomic_i8 for AtomicI8 {
        in = AtomicI8::new( -33 ),
        le = [223],
        be = [223],
        minimum_bytes = 1
    }
    atomic_u16 for AtomicU16 {
        in = AtomicU16::new( 33 ),
        le = [33, 0],
        be = [0, 33],
        minimum_bytes = 2
    }
    atomic_i16 for AtomicI16 {
        in = AtomicI16::new( -33 ),
        le = [223, 255],
        be = [255, 223],
        minimum_bytes = 2
    }
    atomic_u32 for AtomicU32 {
        in = AtomicU32::new( 33 ),
        le = [33, 0, 0, 0],
        be = [0, 0, 0, 33],
        minimum_bytes = 4
    }
    atomic_i32 for AtomicI32 {
        in = AtomicI32::new( -33 ),
        le = [223, 255, 255, 255],
        be = [255, 255,255, 223],
        minimum_bytes = 4
    }
    atomic_u64 for AtomicU64 {
        in = AtomicU64::new( 33 ),
        le = [33, 0, 0, 0, 0, 0, 0, 0],
        be = [0, 0, 0, 0, 0, 0, 0, 33],
        minimum_bytes = 8
    }
    atomic_i64 for AtomicI64 {
        in = AtomicI64::new( -33 ),
        le = [223, 255, 255, 255, 255, 255, 255, 255],
        be = [255, 255, 255, 255, 255, 255, 255, 223],
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
    range_inc_u16 for RangeInclusive< u16 > {
        in = 10..=11,
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
    unit_struct_repr_packed for DerivePackedDummy {
        in = DerivePackedDummy,
        le = [],
        be = [],
        minimum_bytes = 0
    }
    unit_struct_repr_c for DeriveCDummy {
        in = DeriveCDummy,
        le = [],
        be = [],
        minimum_bytes = 0
    }
    unit_struct_repr_transparent for DeriveTransparentDummy {
        in = DeriveTransparentDummy,
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
    derived_struct_with_option_u16_some for DerivedStructWithOptionU16 {
        in = DerivedStructWithOptionU16 { data: Some( 10 ) },
        le = [1, 10, 0],
        be = [1, 0, 10],
        minimum_bytes = 1
    }
    derived_struct_with_option_u16_none for DerivedStructWithOptionU16 {
        in = DerivedStructWithOptionU16 { data: None },
        le = [0],
        be = [0],
        minimum_bytes = 1
    }
    result_u16_ok for Result< u16, u16 > {
        in = Ok( 10 ),
        le = [1, 10, 0],
        be = [1, 0, 10],
        minimum_bytes = 1
    }
    result_u16_err for Result< u16, u16 > {
        in = Err( 10 ),
        le = [0, 10, 0],
        be = [0, 0, 10],
        minimum_bytes = 1
    }
    derived_struct_with_result_u16_ok for DerivedStructWithResultU16 {
        in = DerivedStructWithResultU16 { data: Ok( 10 ) },
        le = [1, 10, 0],
        be = [1, 0, 10],
        minimum_bytes = 1
    }
    derived_struct_with_result_u16_err for DerivedStructWithResultU16 {
        in = DerivedStructWithResultU16 { data: Err( 10 ) },
        le = [0, 10, 0],
        be = [0, 0, 10],
        minimum_bytes = 1
    }
    btreemap_u16_bool for BTreeMap< u16, bool > {
        in = vec![ (10, true), (20, false) ].into_iter().collect(),
        le = [2, 0, 0, 0, 10, 0, 1, 20, 0, 0],
        be = [0, 0, 0, 2, 0, 10, 1, 0, 20, 0],
        minimum_bytes = 4
    }
    btreemap_u16_u8 for BTreeMap< u16, u8 > {
        in = vec![ (10, 1), (20, 0) ].into_iter().collect(),
        le = [2, 0, 0, 0, 10, 0, 1, 20, 0, 0],
        be = [0, 0, 0, 2, 0, 10, 1, 0, 20, 0],
        minimum_bytes = 4
    }
    cow_btreemap_u16_u8 for Cow< BTreeMap< u16, u8 > > {
        in = Cow::Owned( vec![ (10, 1), (20, 0) ].into_iter().collect() ),
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
    cow_btreeset for Cow< BTreeSet< u16 > > {
        in = Cow::Owned( vec![ 10, 20 ].into_iter().collect() ),
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
    ipv4 for core::net::Ipv4Addr {
        in = core::net::Ipv4Addr::new( 127, 0, 0, 1 ),
        le = [1, 0, 0, 127],
        be = [127, 0, 0, 1],
        minimum_bytes = 4
    }
    ipv6 for core::net::Ipv6Addr {
        in = core::net::Ipv6Addr::new( 0x2001, 0x720, 0x1500, 0x1, 0, 0, 0, 0xa100 ),
        le = [0x00, 0xa1, 0, 0, 0, 0, 0, 0, 0x01, 0x00, 0x00, 0x15, 0x20, 0x07, 0x01, 0x20],
        be = [0x20, 0x01, 0x07, 0x20, 0x15, 0x00, 0x00, 0x01, 0, 0, 0, 0, 0, 0, 0xa1, 0x00],
        minimum_bytes = 16
    }
    ipaddr_v4 for core::net::IpAddr {
        in = core::net::IpAddr::V4( core::net::Ipv4Addr::new( 127, 0, 0, 1 ) ),
        le = [0, 1, 0, 0, 127],
        be = [0, 127, 0, 0, 1],
        minimum_bytes = 5
    }
    ipaddr_v6 for core::net::IpAddr {
        in = core::net::IpAddr::V6( core::net::Ipv6Addr::new( 0x2001, 0x720, 0x1500, 0x1, 0, 0, 0, 0xa100 ) ),
        le = [1, 0x00, 0xa1, 0, 0, 0, 0, 0, 0, 0x01, 0x00, 0x00, 0x15, 0x20, 0x07, 0x01, 0x20],
        be = [1, 0x20, 0x01, 0x07, 0x20, 0x15, 0x00, 0x00, 0x01, 0, 0, 0, 0, 0, 0, 0xa1, 0x00],
        minimum_bytes = 5
    }
    duration for core::time::Duration {
        in = core::time::Duration::new( 1, 2 ),
        le = [
            1, 0, 0, 0, 0, 0, 0, 0,
            2, 0, 0, 0
        ],
        be = [
            0, 0, 0, 0, 0, 0, 0, 1,
            0, 0, 0, 2
        ],
        minimum_bytes = 12
    }
    boxed for Box< u8 > {
        in = Box::new( 10 ),
        le = [10],
        be = [10],
        minimum_bytes = 1
    }
    boxed_slice for Box< [u8] > {
        in = vec![ 10, 11 ].into(),
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
    boxed_str for Box< str > {
        in = "Hello".into(),
        le = [5, 0, 0, 0, 72, 101, 108, 108, 111],
        be = [0, 0, 0, 5, 72, 101, 108, 108, 111],
        minimum_bytes = 4
    }
    array_primitive_small for [u16; 2] {
        in = [1, 1024],
        le = [1, 0, 0, 4],
        be = [0, 1, 4, 0],
        minimum_bytes = 4
    }
    array_primitive_big for [u8; 32] {
        in = [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41],
        le = [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41],
        be = [10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41],
        minimum_bytes = 32
    }
    array_string_small for [String; 2] {
        in = [String::from("A"), String::from("BB")],
        le = [1, 0, 0, 0, b'A', 2, 0, 0, 0, b'B', b'B'],
        be = [0, 0, 0, 1, b'A', 0, 0, 0, 2, b'B', b'B'],
        minimum_bytes = 8
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
    derived_simple_enum_tag_type_u7 for DerivedSimpleEnumTagTypeU7 {
        in = DerivedSimpleEnumTagTypeU7::C,
        le = [0x7f],
        be = [0x7f],
        minimum_bytes = 1
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
    derived_simple_enum_tag_type_varint64_a for DerivedSimpleEnumTagTypeVarInt64 {
        in = DerivedSimpleEnumTagTypeVarInt64::A,
        le = [0x00],
        be = [0x00],
        minimum_bytes = 1
    }
    derived_simple_enum_tag_type_varint64_b for DerivedSimpleEnumTagTypeVarInt64 {
        in = DerivedSimpleEnumTagTypeVarInt64::B,
        le = [0b10000000 | 0x12, 0x34],
        be = [0b10000000 | 0x12, 0x34],
        minimum_bytes = 1
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
    derived_struct_with_generic_default for DerivedStructWithGenericDefault {
        in = DerivedStructWithGenericDefault { inner: 1_u8 },
        le = [1],
        be = [1],
        minimum_bytes = 1
    }
    derived_struct_with_generic_default_provided for DerivedStructWithGenericDefault<u16> {
        in = DerivedStructWithGenericDefault { inner: 1_u16 },
        le = [1, 0],
        be = [0, 1],
        minimum_bytes = 2
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
    derived_struct_with_vec_with_length for DerivedStructWithVecWithCount {
        in = DerivedStructWithVecWithCount { length: 2, data: vec![ true, false, false, true ] },
        le = [2, 1, 0, 0, 1],
        be = [2, 1, 0, 0, 1],
        minimum_bytes = 1
    }
    derived_struct_with_string_with_length for DerivedStructWithStringWithCount {
        in = DerivedStructWithStringWithCount { length: 2, data: "ABCD".into() },
        le = [2, b'A', b'B', b'C', b'D'],
        be = [2, b'A', b'B', b'C', b'D'],
        minimum_bytes = 1
    }
    derived_struct_with_cow_slice_with_length for DerivedStructWithCowSliceWithCount {
        in = DerivedStructWithCowSliceWithCount { length: 2, data: vec![ true, false, false, true ].into() },
        le = [2, 1, 0, 0, 1],
        be = [2, 1, 0, 0, 1],
        minimum_bytes = 1
    }
    derived_struct_with_cow_str_with_length for DerivedStructWithCowStrWithCount {
        in = DerivedStructWithCowStrWithCount { length: 2, data: "ABCD".into() },
        le = [2, b'A', b'B', b'C', b'D'],
        be = [2, b'A', b'B', b'C', b'D'],
        minimum_bytes = 1
    }
    derived_struct_with_hash_map_with_length for DerivedStructWithHashMapWithCount {
        in = DerivedStructWithHashMapWithCount { length: 4, data: vec![ (50, 60) ].into_iter().collect() },
        le = [4, 50, 60],
        be = [4, 50, 60],
        minimum_bytes = 1
    }
    derived_struct_with_btree_map_with_length for DerivedStructWithBTreeMapWithCount {
        in = DerivedStructWithBTreeMapWithCount { length: 4, data: vec![ (50, 60) ].into_iter().collect() },
        le = [4, 50, 60],
        be = [4, 50, 60],
        minimum_bytes = 1
    }
    derived_struct_with_hash_set_with_length for DerivedStructWithHashSetWithCount {
        in = DerivedStructWithHashSetWithCount { length: 4, data: vec![ 50 ].into_iter().collect() },
        le = [4, 50],
        be = [4, 50],
        minimum_bytes = 1
    }
    derived_struct_with_btree_set_with_length for DerivedStructWithBTreeSetWithCount {
        in = DerivedStructWithBTreeSetWithCount { length: 4, data: vec![ 50 ].into_iter().collect() },
        le = [4, 50],
        be = [4, 50],
        minimum_bytes = 1
    }
    derived_struct_with_cow_hash_map_with_length for DerivedStructWithCowHashMapWithCount {
        in = DerivedStructWithCowHashMapWithCount { length: 4, data: Cow::Owned( vec![ (50, 60) ].into_iter().collect() ) },
        le = [4, 50, 60],
        be = [4, 50, 60],
        minimum_bytes = 1
    }
    derived_struct_with_cow_btree_map_with_length for DerivedStructWithCowBTreeMapWithCount {
        in = DerivedStructWithCowBTreeMapWithCount { length: 4, data: Cow::Owned( vec![ (50, 60) ].into_iter().collect() ) },
        le = [4, 50, 60],
        be = [4, 50, 60],
        minimum_bytes = 1
    }
    derived_struct_with_cow_hash_set_with_length for DerivedStructWithCowHashSetWithCount {
        in = DerivedStructWithCowHashSetWithCount { length: 4, data: Cow::Owned( vec![ 50 ].into_iter().collect() ) },
        le = [4, 50],
        be = [4, 50],
        minimum_bytes = 1
    }
    derived_struct_with_cow_btree_set_with_length for DerivedStructWithCowBTreeSetWithCount {
        in = DerivedStructWithCowBTreeSetWithCount { length: 4, data: Cow::Owned( vec![ 50 ].into_iter().collect() ) },
        le = [4, 50],
        be = [4, 50],
        minimum_bytes = 1
    }
    derived_tuple_struct_with_vec_with_length for DerivedTupleStructWithVecWithCount {
        in = DerivedTupleStructWithVecWithCount( 2, vec![ true, false, false, true ] ),
        le = [2, 1, 0, 0, 1],
        be = [2, 1, 0, 0, 1],
        minimum_bytes = 1
    }
    derived_struct_with_vec_with_length_type_u7 for DerivedStructWithVecWithLengthTypeU7 {
        in = DerivedStructWithVecWithLengthTypeU7 { data: vec![ 100, 101 ] },
        le = [2, 100, 101],
        be = [2, 100, 101],
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
    derived_struct_with_vec_with_length_type_varint64 for DerivedStructWithVecWithLengthTypeVarInt64 {
        in = DerivedStructWithVecWithLengthTypeVarInt64 { data: vec![ 100, 101 ] },
        le = [2, 100, 101],
        be = [2, 100, 101],
        minimum_bytes = 1
    }
    derived_struct_with_option_vec_with_length_type_u16_none for DerivedStructWithOptionVecWithLengthTypeU16 {
        in = DerivedStructWithOptionVecWithLengthTypeU16 { data: None },
        le = [0],
        be = [0],
        minimum_bytes = 1
    }
    derived_struct_with_option_vec_with_length_type_u16_some_empty for DerivedStructWithOptionVecWithLengthTypeU16 {
        in = DerivedStructWithOptionVecWithLengthTypeU16 { data: Some( vec![] ) },
        le = [1, 0, 0],
        be = [1, 0, 0],
        minimum_bytes = 1
    }
    derived_struct_with_option_vec_with_length_type_u16_some_non_empty for DerivedStructWithOptionVecWithLengthTypeU16 {
        in = DerivedStructWithOptionVecWithLengthTypeU16 { data: Some( vec![ 100, 101 ] ) },
        le = [1, 2, 0, 100, 101],
        be = [1, 0, 2, 100, 101],
        minimum_bytes = 1
    }
    derived_enum_with_custom_tag_a for DerivedEnumWithCustomTag {
        in = DerivedEnumWithCustomTag::A,
        le = [20],
        be = [20],
        minimum_bytes = 1
    }
    derived_enum_with_custom_tag_b for DerivedEnumWithCustomTag {
        in = DerivedEnumWithCustomTag::B,
        le = [21],
        be = [21],
        minimum_bytes = 1
    }
    derived_struct_with_array for DerivedStructWithArray< u8 > {
        in = DerivedStructWithArray { data: [1, 2, 3, 4] },
        le = [1, 2, 3, 4],
        be = [1, 2, 3, 4],
        minimum_bytes = 4
    }
    non_zero_u32 for NonZeroU32 {
        in = NonZeroU32::new( 33 ).unwrap(),
        le = [33, 0, 0, 0],
        be = [0, 0, 0, 33],
        minimum_bytes = 4
    }
    derived_struct_with_skipped_field for DerivedStructWithSkippedField {
        in =  DerivedStructWithSkippedField { a: 1, _b: 0, c: 3 },
        le = [1, 3],
        be = [1, 3],
        minimum_bytes = 2
    }
    derived_struct_with_constant_prefix_string for DerivedStructWithConstantPrefixString {
        in = DerivedStructWithConstantPrefixString { value: () },
        le = [0x41, 0x42, 0x43],
        be = [0x41, 0x42, 0x43],
        minimum_bytes = 3
    }
    derived_struct_with_constant_prefix_byte_string for DerivedStructWithConstantPrefixByteString {
        in = DerivedStructWithConstantPrefixByteString { value: () },
        le = [0x41, 0x42, 0x43],
        be = [0x41, 0x42, 0x43],
        minimum_bytes = 3
    }
    derived_struct_with_constant_prefix_char for DerivedStructWithConstantPrefixChar {
        in = DerivedStructWithConstantPrefixChar { value: () },
        le = [0xe8, 0x8b, 0xba],
        be = [0xe8, 0x8b, 0xba],
        minimum_bytes = 3
    }
    derived_struct_with_constant_prefix_byte for DerivedStructWithConstantPrefixByte {
        in = DerivedStructWithConstantPrefixByte { value: () },
        le = [0x41],
        be = [0x41],
        minimum_bytes = 1
    }
    derived_struct_with_constant_prefix_bool_true for DerivedStructWithConstantPrefixBoolTrue {
        in = DerivedStructWithConstantPrefixBoolTrue { value: () },
        le = [0x01],
        be = [0x01],
        minimum_bytes = 1
    }
    derived_struct_with_constant_prefix_bool_false for DerivedStructWithConstantPrefixBoolFalse {
        in = DerivedStructWithConstantPrefixBoolFalse { value: () },
        le = [0x00],
        be = [0x00],
        minimum_bytes = 1
    }
    derived_struct_with_constant_prefix_u8 for DerivedStructWithConstantPrefixU8 {
        in = DerivedStructWithConstantPrefixU8 { value: () },
        le = [10],
        be = [10],
        minimum_bytes = 1
    }
    derived_struct_with_constant_prefix_i8 for DerivedStructWithConstantPrefixI8 {
        in = DerivedStructWithConstantPrefixI8 { value: () },
        le = [255],
        be = [255],
        minimum_bytes = 1
    }
    derived_enum_with_peek_tag_u8 for DerivedEnumWithPeekTagU8 {
        in = DerivedEnumWithPeekTagU8::One( 1 ),
        le = [1],
        be = [1],
        minimum_bytes = 1
    }
    derived_recursive_enum for DerivedRecursiveEnum {
        in = DerivedRecursiveEnum::Some( Box::new( DerivedRecursiveEnum::None ) ),
        le = [1, 0],
        be = [1, 0],
        minimum_bytes = 1
    }
    derive_c_u8 for DeriveCWithU8 {
        in = DeriveCWithU8( 127 ),
        le = [127],
        be = [127],
        minimum_bytes = 1
    }
    derive_c_u16 for DeriveCWithU16 {
        in = DeriveCWithU16( 33 ),
        le = [33, 0],
        be = [0, 33],
        minimum_bytes = 2
    }
    derive_c_u16_u16 for DeriveCWithU16U16 {
        in = DeriveCWithU16U16( 33, 44 ),
        le = [33, 0, 44, 0],
        be = [0, 33, 0, 44],
        minimum_bytes = 4
    }
    derive_c_u16_u8 for DeriveCWithU16U8 {
        in = DeriveCWithU16U8( 33, 44 ),
        le = [33, 0, 44],
        be = [0, 33, 44],
        minimum_bytes = 3
    }
    derive_c_derive_c for DeriveCWithDeriveC {
        in = DeriveCWithDeriveC( DeriveCWithU16( 33 ) ),
        le = [33, 0],
        be = [0, 33],
        minimum_bytes = 2
    }
    derived_struct_with_varint_0 for DerivedStructWithVarInt {
        in = DerivedStructWithVarInt { value: 0 },
        le = [0],
        be = [0],
        minimum_bytes = 1
    }
    derived_struct_with_varint_127 for DerivedStructWithVarInt {
        in = DerivedStructWithVarInt { value: 127 },
        le = [127],
        be = [127],
        minimum_bytes = 1
    }
    derived_struct_with_varint_128 for DerivedStructWithVarInt {
        in = DerivedStructWithVarInt { value: 128 },
        le = [0b10000000, 0b10000000],
        be = [0b10000000, 0b10000000],
        minimum_bytes = 1
    }
}

#[cfg(feature = "std")]
symmetric_tests! {
    system_time for std::time::SystemTime {
        in = std::time::SystemTime::UNIX_EPOCH,
        le = [
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0
        ],
        be = [
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0
        ],
        minimum_bytes = 12
    }
    hashmap_u16_bool for HashMap< u16, bool > {
        in = vec![ (10, true) ].into_iter().collect(),
        le = [1, 0, 0, 0, 10, 0, 1],
        be = [0, 0, 0, 1, 0, 10, 1],
        minimum_bytes = 4
    }
    hashmap_u16_u8 for HashMap< u16, u8 > {
        in = vec![ (10, 1) ].into_iter().collect(),
        le = [1, 0, 0, 0, 10, 0, 1],
        be = [0, 0, 0, 1, 0, 10, 1],
        minimum_bytes = 4
    }
    cow_hashmap_u16_u8 for Cow< HashMap< u16, u8 > > {
        in = Cow::Owned( vec![ (10, 1) ].into_iter().collect() ),
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
    cow_hashset for Cow< HashSet< u16 > > {
        in = Cow::Owned( vec![ 10 ].into_iter().collect() ),
        le = [1, 0, 0, 0, 10, 0],
        be = [0, 0, 0, 1, 0, 10],
        minimum_bytes = 4
    }
}

symmetric_tests_unsized! {
    ref_str for &str {
        in = "Hello",
        le = [5, 0, 0, 0, 72, 101, 108, 108, 111],
        be = [0, 0, 0, 5, 72, 101, 108, 108, 111],
        minimum_bytes = 4
    }
    derived_ref_str for DerivedRefStr {
        in = DerivedRefStr( "Hello" ),
        le = [5, 0, 0, 0, 72, 101, 108, 108, 111],
        be = [0, 0, 0, 5, 72, 101, 108, 108, 111],
        minimum_bytes = 4
    }
    ref_slice_u8 for &[u8] {
        in = &[10, 11],
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
    derived_ref_slice_u8 for DerivedRefSliceU8 {
        in = DerivedRefSliceU8( &[10, 11] ),
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
    derived_struct_with_ref_slice_u8_with_length for DerivedRefSliceU8WithCount {
        in = DerivedRefSliceU8WithCount { length: 2, data: &[1, 0, 0, 1] },
        le = [2, 1, 0, 0, 1],
        be = [2, 1, 0, 0, 1],
        minimum_bytes = 1
    }
    derived_struct_with_ref_str_with_length for DerivedRefStrWithCount {
        in = DerivedRefStrWithCount { length: 2, data: "ABCD" },
        le = [2, b'A', b'B', b'C', b'D'],
        be = [2, b'A', b'B', b'C', b'D'],
        minimum_bytes = 1
    }
    derived_string_until_eof_empty for DerivedStringUntilEof {
        in = DerivedStringUntilEof( "".into() ),
        le = [],
        be = [],
        minimum_bytes = 0
    }
    derived_string_until_eof_non_empty for DerivedStringUntilEof {
        in = DerivedStringUntilEof( "Hello".into() ),
        le = [72, 101, 108, 108, 111],
        be = [72, 101, 108, 108, 111],
        minimum_bytes = 0
    }
    derived_vec_until_eof_empty for DerivedVecUntilEof {
        in = DerivedVecUntilEof( vec![] ),
        le = [],
        be = [],
        minimum_bytes = 0
    }
    derived_vec_until_eof_non_empty for DerivedVecUntilEof {
        in = DerivedVecUntilEof( b"Hello".to_vec() ),
        le = [72, 101, 108, 108, 111],
        be = [72, 101, 108, 108, 111],
        minimum_bytes = 0
    }
    derived_cow_str_until_eof_empty for DerivedCowStrUntilEof {
        in = DerivedCowStrUntilEof( "".into() ),
        le = [],
        be = [],
        minimum_bytes = 0
    }
    derived_cow_str_until_eof_non_empty for DerivedCowStrUntilEof {
        in = DerivedCowStrUntilEof( "Hello".into() ),
        le = [72, 101, 108, 108, 111],
        be = [72, 101, 108, 108, 111],
        minimum_bytes = 0
    }
    derived_cow_slice_until_eof_empty for DerivedCowSliceUntilEof {
        in = DerivedCowSliceUntilEof( Cow::Borrowed( b"" ) ),
        le = [],
        be = [],
        minimum_bytes = 0
    }
    derived_cow_slice_until_eof_non_empty for DerivedCowSliceUntilEof {
        in = DerivedCowSliceUntilEof( Cow::Borrowed( b"Hello" ) ),
        le = [72, 101, 108, 108, 111],
        be = [72, 101, 108, 108, 111],
        minimum_bytes = 0
    }
    derived_btreeset_until_eof_empty for DerivedBTreeSetUntilEof {
        in = DerivedBTreeSetUntilEof( BTreeSet::new() ),
        le = [],
        be = [],
        minimum_bytes = 0
    }
    derived_btreeset_until_eof_non_empty for DerivedBTreeSetUntilEof {
        in = DerivedBTreeSetUntilEof( vec![ 10, 20 ].into_iter().collect() ),
        le = [10, 0, 20, 0],
        be = [0, 10, 0, 20],
        minimum_bytes = 0
    }
    derived_ref_slice_u8_until_eof for DerivedRefSliceU8UntilEof {
        in = DerivedRefSliceU8UntilEof( b"Hello" ),
        le = [72, 101, 108, 108, 111],
        be = [72, 101, 108, 108, 111],
        minimum_bytes = 0
    }
    derived_ref_str_until_eof for DerivedRefStrUntilEof {
        in = DerivedRefStrUntilEof( "Hello" ),
        le = [72, 101, 108, 108, 111],
        be = [72, 101, 108, 108, 111],
        minimum_bytes = 0
    }
    vec_packed_tuple for Vec< DerivedPackedTuple > {
        in = vec![
            DerivedPackedTuple( 33, 66 ),
            DerivedPackedTuple( 100, 200 ),
        ],
        le = [
            2, 0, 0, 0,
            33, 0, 66, 0,
            100, 0, 200, 0,
        ],
        be = [
            0, 0, 0, 2,
            0, 33, 0, 66,
            0, 100, 0, 200,
        ],
        minimum_bytes = 4
    }
    vec_packed_recursive_tuple for Vec< DerivedPackedRecursiveTuple > {
        in = vec![
            DerivedPackedRecursiveTuple( DerivedPackedTuple( 33, 66 ), DerivedPackedTuple( 34, 67 ) ),
            DerivedPackedRecursiveTuple( DerivedPackedTuple( 100, 200 ), DerivedPackedTuple( 101, 201 ) ),
        ],
        le = [
            2, 0, 0, 0,
            33, 0, 66, 0, 34, 0, 67, 0,
            100, 0, 200, 0, 101, 0, 201, 0
        ],
        be = [
            0, 0, 0, 2,
            0, 33, 0, 66, 0, 34, 0, 67,
            0, 100, 0, 200, 0, 101, 0, 201
        ],
        minimum_bytes = 4
    }
    vec_transparent_u16 for Vec< TransparentU16 > {
        in = vec![
            TransparentU16( 33 ),
            TransparentU16( 100 ),
        ],
        le = [
            2, 0, 0, 0,
            33, 0,
            100, 0,
        ],
        be = [
            0, 0, 0, 2,
            0, 33,
            0, 100,
        ],
        minimum_bytes = 4
    }
    cow_slice_packed_tuple for Cow< [DerivedPackedTuple] > {
        in = Cow::Owned( vec![
            DerivedPackedTuple( 33, 66 ),
            DerivedPackedTuple( 100, 200 ),
        ]),
        le = [
            2, 0, 0, 0,
            33, 0, 66, 0,
            100, 0, 200, 0,
        ],
        be = [
            0, 0, 0, 2,
            0, 33, 0, 66,
            0, 100, 0, 200,
        ],
        minimum_bytes = 4
    }
    cow_slice_packed_recursive_tuple for Cow< [DerivedPackedRecursiveTuple] > {
        in = Cow::Owned( vec![
            DerivedPackedRecursiveTuple( DerivedPackedTuple( 33, 66 ), DerivedPackedTuple( 34, 67 ) ),
            DerivedPackedRecursiveTuple( DerivedPackedTuple( 100, 200 ), DerivedPackedTuple( 101, 201 ) ),
        ]),
        le = [
            2, 0, 0, 0,
            33, 0, 66, 0, 34, 0, 67, 0,
            100, 0, 200, 0, 101, 0, 201, 0
        ],
        be = [
            0, 0, 0, 2,
            0, 33, 0, 66, 0, 34, 0, 67,
            0, 100, 0, 200, 0, 101, 0, 201
        ],
        minimum_bytes = 4
    }
    cow_slice_transparent_u16 for Cow< [TransparentU16] > {
        in = Cow::Owned( vec![
            TransparentU16( 33 ),
            TransparentU16( 100 ),
        ]),
        le = [
            2, 0, 0, 0,
            33, 0,
            100, 0,
        ],
        be = [
            0, 0, 0, 2,
            0, 33,
            0, 100,
        ],
        minimum_bytes = 4
    }
}

symmetric_tests_unsized_native_endian! {
    derived_ref_slice_packed_tuple for DerivedRefSlicePackedTuple {
        in = DerivedRefSlicePackedTuple( &[
            DerivedPackedTuple( 33, 66 ),
            DerivedPackedTuple( 100, 200 ),
        ]),
        le = [
            2, 0, 0, 0,
            33, 0, 66, 0,
            100, 0, 200, 0,
        ],
        be = [
            0, 0, 0, 2,
            0, 33, 0, 66,
            0, 100, 0, 200,
        ],
        minimum_bytes = 4
    }
    derived_ref_slice_packed_tuple_until_eof for DerivedRefSlicePackedTupleUntilEof {
        in = DerivedRefSlicePackedTupleUntilEof( &[
            DerivedPackedTuple( 33, 66 ),
            DerivedPackedTuple( 100, 200 ),
        ]),
        le = [
            33, 0, 66, 0,
            100, 0, 200, 0,
        ],
        be = [
            0, 33, 0, 66,
            0, 100, 0, 200,
        ],
        minimum_bytes = 0
    }
    ref_slice_derived_packed_tuple for &[DerivedPackedTuple] {
        in = &[
            DerivedPackedTuple( 33, 66 ),
            DerivedPackedTuple( 100, 200 ),
        ],
        le = [
            2, 0, 0, 0,
            33, 0, 66, 0,
            100, 0, 200, 0,
        ],
        be = [
            0, 0, 0, 2,
            0, 33, 0, 66,
            0, 100, 0, 200,
        ],
        minimum_bytes = 4
    }
    ref_slice_packed_u16 for &[PackedU16] {
        in = &[PackedU16( 33 ), PackedU16( 66 )],
        le = [
            2, 0, 0, 0,
            33, 0, 66, 0,
        ],
        be = [
            0, 0, 0, 2,
            0, 33, 0, 66,
        ],
        minimum_bytes = 4
    }
    ref_slice_sized_slice_packed_u16 for &[PackedU16; 2] {
        in = &[PackedU16( 33 ), PackedU16( 66 )],
        le = [
            33, 0, 66, 0,
        ],
        be = [
            0, 33, 0, 66,
        ],
        minimum_bytes = 4
    }
}

#[cfg(feature = "chrono")]
symmetric_tests! {
    chrono_datetime_utc for chrono::DateTime< chrono::Utc > {
        in = chrono::offset::TimeZone::timestamp( &chrono::Utc, 123, 222 ),
        le = [123, 0, 0, 0, 0, 0, 0, 0, 222, 0, 0, 0],
        be = [0, 0, 0, 0, 0, 0, 0, 123, 0, 0, 0, 222],
        minimum_bytes = 12
    }
}

#[cfg(feature = "smallvec")]
symmetric_tests! {
    smallvec_u8_smaller for smallvec::SmallVec< [u8; 2] > {
        in = vec![ 111 ].into(),
        le = [1, 0, 0, 0, 111],
        be = [0, 0, 0, 1, 111],
        minimum_bytes = 4
    }
    smallvec_u8_bigger for smallvec::SmallVec< [u8; 2] > {
        in = vec![ 111, 112, 113 ].into(),
        le = [3, 0, 0, 0, 111, 112, 113],
        be = [0, 0, 0, 3, 111, 112, 113],
        minimum_bytes = 4
    }
}

#[cfg(feature = "indexmap_v1")]
symmetric_tests! {
    indexmap_v1_u16 for indexmap_v1::IndexMap< u16, bool > {
        in = vec![ (10, true) ].into_iter().collect(),
        le = [1, 0, 0, 0, 10, 0, 1],
        be = [0, 0, 0, 1, 0, 10, 1],
        minimum_bytes = 4
    }
    indexset_v1_u16 for indexmap_v1::IndexSet< u16 > {
        in = vec![ 10 ].into_iter().collect(),
        le = [1, 0, 0, 0, 10, 0],
        be = [0, 0, 0, 1, 0, 10],
        minimum_bytes = 4
    }
}

#[cfg(feature = "indexmap_v2")]
symmetric_tests! {
    indexmap_v2_u16 for indexmap_v2::IndexMap< u16, bool > {
        in = vec![ (10, true) ].into_iter().collect(),
        le = [1, 0, 0, 0, 10, 0, 1],
        be = [0, 0, 0, 1, 0, 10, 1],
        minimum_bytes = 4
    }
    indexset_v2_u16 for indexmap_v2::IndexSet< u16 > {
        in = vec![ 10 ].into_iter().collect(),
        le = [1, 0, 0, 0, 10, 0],
        be = [0, 0, 0, 1, 0, 10],
        minimum_bytes = 4
    }
}

#[cfg(feature = "regex")]
#[test]
fn test_regex() {
    use speedy::{
        Readable,
        Writable
    };

    let regex = regex::Regex::new( "[a-z]+" ).unwrap();
    let buffer = regex.write_to_vec().unwrap();
    assert_eq!( buffer, &[6, 0, 0, 0, b'[', b'a', b'-', b'z', b']', b'+'] );

    let deserialized = regex::Regex::read_from_buffer( &buffer ).unwrap();
    assert_eq!( regex.as_str(), deserialized.as_str() );
}

#[cfg(feature = "uuid")]
symmetric_tests! {
    uuid for uuid::Uuid {
        in = uuid::Uuid::parse_str("99ac1675-12ef-495b-bf56-606ebe5e3e71").unwrap(),
        le = [0x99, 0xAC, 0x16, 0x75, 0x12, 0xEF, 0x49, 0x5B, 0xBF, 0x56, 0x60, 0x6E, 0xBE, 0x5E, 0x3E, 0x71],
        be = [0x99, 0xAC, 0x16, 0x75, 0x12, 0xEF, 0x49, 0x5B, 0xBF, 0x56, 0x60, 0x6E, 0xBE, 0x5E, 0x3E, 0x71],
        minimum_bytes = 16
    }
}


#[test]
fn test_derived_struct_with_default_on_eof() {
    use speedy::{
       Readable,
       Endianness
    };

    let deserialized: DerivedStructWithDefaultOnEof = Readable::read_from_buffer_with_ctx( Endianness::LittleEndian, &[0xAA] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithDefaultOnEof { a: 0xAA, b: 0, c: 0 } );

    let deserialized: DerivedStructWithDefaultOnEof = Readable::read_from_buffer_with_ctx( Endianness::LittleEndian, &[0xAA, 0xBB] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithDefaultOnEof { a: 0xAA, b: 0, c: 0 } );

    let deserialized: DerivedStructWithDefaultOnEof = Readable::read_from_buffer_with_ctx( Endianness::LittleEndian, &[0xAA, 0xBB, 0xCC] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithDefaultOnEof { a: 0xAA, b: 0xCCBB, c: 0 } );

    let deserialized: DerivedStructWithVecWithDefaultOnEof = Readable::read_from_buffer_with_ctx( Endianness::LittleEndian, &[] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithVecWithDefaultOnEof { data: vec![] } );

    let deserialized: DerivedStructWithVecWithCountWithDefaultOnEof = Readable::read_from_buffer_with_ctx( Endianness::LittleEndian, &[2, 0xAA, 0xBB] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithVecWithCountWithDefaultOnEof { length: 2, data: vec![0xAA, 0xBB] } );

    let deserialized: DerivedStructWithVecWithCountWithDefaultOnEof = Readable::read_from_buffer_with_ctx( Endianness::LittleEndian, &[2, 0xAA] ).unwrap();
    assert_eq!( deserialized, DerivedStructWithVecWithCountWithDefaultOnEof { length: 2, data: vec![] } );
}

#[test]
fn test_length_mismatch_with_length_attribute() {
    use speedy::{
        Endianness,
        Writable
    };

    let err = DerivedStructWithVecWithCount {
        length: 0,
        data: vec![ true ]
    }.write_to_vec_with_ctx( Endianness::LittleEndian ).unwrap_err();

    assert_eq!(
        err.to_string(),
        "the length of 'data' is not the same as its 'length' attribute"
    );
}

#[test]
fn test_zero_non_zero() {
    let error = NonZeroU32::read_from_buffer( &[0, 0, 0, 0] ).unwrap_err();
    match speedy::private::get_error_kind( &error ) {
        speedy::private::ErrorKind::ZeroNonZero => {},
        error => panic!( "Unexpected error: {:?}", error )
    }
}

#[test]
fn test_vec_with_length_type_u7_read_out_of_range_length() {
    let error = DerivedStructWithVecWithLengthTypeU7::read_from_buffer( &[0x80] ).unwrap_err();
    match speedy::private::get_error_kind( &error ) {
        speedy::private::ErrorKind::OutOfRangeLength => {},
        error => panic!( "Unexpected error: {:?}", error )
    }
}

#[test]
fn test_prefix_constant_mismatch() {
    let error = DerivedStructWithConstantPrefixString::read_from_buffer( &[0x41, 0x42] ).unwrap_err();
    match speedy::private::get_error_kind( &error ) {
        speedy::private::ErrorKind::InputBufferIsTooSmall { .. } => {},
        error => panic!( "Unexpected error: {:?}", error )
    }

    let error = DerivedStructWithConstantPrefixString::read_from_buffer( &[0x41, 0x42, 0x00] ).unwrap_err();
    match speedy::private::get_error_kind( &error ) {
        speedy::private::ErrorKind::ExpectedConstant { .. } => {},
        error => panic!( "Unexpected error: {:?}", error )
    }
}

#[test]
fn test_minimum_bytes_needed() {
    use speedy::{
        Readable,
        Endianness
    };

    assert_eq!( <DerivedStructWithDefaultOnEof as Readable< Endianness >>::minimum_bytes_needed(), 1 );
}

#[test]
fn test_derive_transparent() {
    assert!( <TransparentU8 as Readable< Endianness >>::speedy_is_primitive() );
    assert!( <TransparentU16 as Readable< Endianness >>::speedy_is_primitive() );
    assert!( <NonTransparentU8 as Readable< Endianness >>::speedy_is_primitive() );
    assert!( !<ManualU8 as Readable< Endianness >>::speedy_is_primitive() );

    assert!( <TransparentU8 as Writable< Endianness >>::speedy_is_primitive() );
    assert!( <TransparentU16 as Writable< Endianness >>::speedy_is_primitive() );
    assert!( <NonTransparentU8 as Writable< Endianness >>::speedy_is_primitive() );
    assert!( !<ManualU8 as Writable< Endianness >>::speedy_is_primitive() );
}

#[test]
fn test_derive_packed() {
    assert!( !<DerivedTupleStruct as Readable< Endianness >>::speedy_is_primitive() );
    assert!( <DerivedPackedTuple as Readable< Endianness >>::speedy_is_primitive() );
    assert!( !<DerivedTupleStruct as Writable< Endianness >>::speedy_is_primitive() );
    assert!( <DerivedPackedTuple as Writable< Endianness >>::speedy_is_primitive() );

    assert!( <DerivedPackedRecursiveTuple as Readable< Endianness >>::speedy_is_primitive() );
    assert!( <DerivedPackedRecursiveTuple as Writable< Endianness >>::speedy_is_primitive() );
}

#[test]
fn test_derive_c() {
    assert!( <DeriveCWithU8 as Readable< Endianness >>::speedy_is_primitive() );
    assert!( <DeriveCWithU16 as Readable< Endianness >>::speedy_is_primitive() );
    assert!( <DeriveCWithU16U16 as Readable< Endianness >>::speedy_is_primitive() );
    assert!( <DeriveCWithDeriveC as Readable< Endianness >>::speedy_is_primitive() );
    assert!( !<DeriveCWithU16U8 as Readable< Endianness >>::speedy_is_primitive() );

    assert!( <DeriveCWithU8 as Writable< Endianness >>::speedy_is_primitive() );
    assert!( <DeriveCWithU16 as Writable< Endianness >>::speedy_is_primitive() );
    assert!( <DeriveCWithU16U16 as Writable< Endianness >>::speedy_is_primitive() );
    assert!( <DeriveCWithDeriveC as Writable< Endianness >>::speedy_is_primitive() );
    assert!( !<DeriveCWithU16U8 as Writable< Endianness >>::speedy_is_primitive() );
}

#[test]
fn test_derive_primitive() {
    assert!( <ThreeF32 as Readable< Endianness >>::speedy_is_primitive() );
    assert!( <ThreeF32 as Writable< Endianness >>::speedy_is_primitive() );
    assert!( <FourU8 as Readable< Endianness >>::speedy_is_primitive() );
    assert!( <FourU8 as Writable< Endianness >>::speedy_is_primitive() );
    assert!( <SimpleComposite as Readable< Endianness >>::speedy_is_primitive() );
    assert!( <SimpleComposite as Writable< Endianness >>::speedy_is_primitive() );

    assert!( !<DerivedStructWithVarInt as Readable< Endianness >>::speedy_is_primitive() );
    assert!( !<DerivedStructWithVarInt as Writable< Endianness >>::speedy_is_primitive() );
    assert!( !<DerivedStructWithOptionU16 as Readable< Endianness >>::speedy_is_primitive() );
    assert!( !<DerivedStructWithOptionU16 as Writable< Endianness >>::speedy_is_primitive() );

    assert!( <ForcedPrimitive as Readable< Endianness >>::speedy_is_primitive() );
    assert!( <ForcedPrimitive as Writable< Endianness >>::speedy_is_primitive() );
}

#[derive(PartialEq, Eq, Debug)]
struct ManualU8( u8 );

impl< 'a, C: speedy::Context > Readable< 'a, C > for ManualU8 {
    #[inline]
    fn read_from< R: speedy::Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        Ok( ManualU8( reader.read_u8()? ) )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        1
    }
}

impl< C: speedy::Context > speedy::Writable< C > for ManualU8 {
    #[inline]
    fn write_to< T: ?Sized + speedy::Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        writer.write_u8( self.0 )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( 1 )
    }
}

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct NonTransparentU8( u8 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(transparent)]
struct TransparentNonZeroCopyable( Vec< u8 > );

#[derive(Copy, Clone, Readable, Writable, PartialEq, Eq, Debug)]
struct NonZeroCopyable( u8 );

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
#[repr(packed)]
struct PackedNonZeroCopyable( NonZeroCopyable );

#[derive(Readable, Writable, PartialEq, Debug)]
struct ThreeF32 {
    x: f32,
    y: f32,
    z: f32,
}

#[derive(Readable, Writable, PartialEq, Eq, Debug)]
struct FourU8 {
    x0: u8,
    x1: u8,
    x2: u8,
    x3: u8,
}

#[derive(Readable, Writable, PartialEq, Debug)]
struct SimpleComposite {
    v0: ThreeF32,
    v1: ThreeF32,
    v2: ThreeF32,
    normal: ThreeF32,
}

#[derive(Readable, Writable, PartialEq, Debug)]
#[speedy(unsafe_is_primitive = always)]
struct ForcedPrimitive( char );

// Source: https://users.rust-lang.org/t/a-macro-to-assert-that-a-type-does-not-implement-trait-bounds/31179
macro_rules! assert_not_impl {
    ($x:ty, $($t:path),+ $(,)*) => {
        const _: fn() -> () = || {
            struct Check<T: ?Sized>(T);
            trait AmbiguousIfImpl<A> { fn some_item() { } }

            impl<T: ?Sized> AmbiguousIfImpl<()> for Check<T> { }
            impl<T: ?Sized $(+ $t)*> AmbiguousIfImpl<u8> for Check<T> { }

            <Check::<$x> as AmbiguousIfImpl<_>>::some_item()
        };
    };
}

macro_rules! assert_impl {
    ($x:ty, $($t:path),+ $(,)*) => {
        const _: fn() -> () = || {
            struct Check where $x: $($t),+;
        };
    };
}

#[cfg(target_endian = "little")]
pub use speedy::LittleEndian as NativeEndian;

#[cfg(target_endian = "little")]
pub use speedy::BigEndian as OtherEndian;

#[cfg(target_endian = "big")]
pub use speedy::BigEndian as NativeEndian;

#[cfg(target_endian = "big")]
pub use speedy::LittleEndian as OtherEndian;

assert_not_impl!( (u8, u8), speedy::private::ZeroCopyable< NativeEndian, () > );
assert_not_impl!( Vec< u8 >, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_not_impl!( NonTransparentU8, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_not_impl!( TransparentNonZeroCopyable, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_not_impl!( NonZeroCopyable, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_not_impl!( PackedNonZeroCopyable, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_not_impl!( DerivedRefSlicePackedTuple, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_not_impl!( DerivedRefSlicePackedTupleUntilEof, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_not_impl!( &[DerivedPackedTuple], speedy::private::ZeroCopyable< NativeEndian, () > );

assert_impl!( u16, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( u32, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( u64, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( u128, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( i16, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( i32, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( i64, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( i128, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( f32, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( f64, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( TransparentU16, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( TransparentU32, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( TransparentU128, speedy::private::ZeroCopyable< NativeEndian, () > );

assert_impl!( &'static [u16], speedy::Readable< 'static, NativeEndian > );

assert_not_impl!( u16, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( u32, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( u64, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( u128, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( i16, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( i32, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( i64, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( i128, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( f32, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( f64, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( TransparentU16, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( TransparentU32, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_not_impl!( TransparentU128, speedy::private::ZeroCopyable< OtherEndian, () > );

assert_not_impl!( &'static [u16], speedy::Readable< 'static, OtherEndian > );

assert_impl!( u8, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( i8, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( TransparentU8, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( DerivedPackedTuple, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( DerivedPackedRecursiveTuple, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( &'static [PackedU16], speedy::Readable< 'static, NativeEndian > );
assert_impl!( &'static [PackedU16], speedy::Readable< 'static, OtherEndian > );

assert_impl!( ForcedPrimitive, speedy::private::ZeroCopyable< NativeEndian, () > );
assert_impl!( ForcedPrimitive, speedy::private::ZeroCopyable< OtherEndian, () > );
assert_impl!( &'static [ForcedPrimitive], speedy::Readable< 'static, NativeEndian > );
assert_impl!( &'static [ForcedPrimitive], speedy::Readable< 'static, OtherEndian > );

#[cfg(feature = "std")]
#[test]
fn test_incomplete_read_into_vec_triggers_drop_for_already_read_items() {
    use core::sync::atomic::{AtomicU64, Ordering};

    static COUNTER: AtomicU64 = AtomicU64::new( 0 );

    #[derive(Readable, Writable, PartialEq, Eq, Debug)]
    struct WithDrop( ManualU8 );
    impl Drop for WithDrop {
        fn drop( &mut self ) {
            COUNTER.fetch_add( 1, Ordering::SeqCst );
        }
    }

    #[derive(Readable, Writable, PartialEq, Eq, Debug)]
    struct Struct( Vec< WithDrop > );

    Struct::read_from_stream_unbuffered( &mut &[0, 0, 0, 10, 1, 2][..] ).unwrap_err();
    assert_eq!( COUNTER.load( Ordering::SeqCst ), 2 );
}

#[cfg(feature = "std")]
#[test]
fn test_incomplete_read_into_vec_does_not_trigger_drop_for_already_read_items_if_they_are_primitive() {
    use core::sync::atomic::{AtomicU64, Ordering};

    static COUNTER: AtomicU64 = AtomicU64::new( 0 );

    #[derive(Readable, Writable, PartialEq, Eq, Debug)]
    struct WithDrop( u8 );
    impl Drop for WithDrop {
        fn drop( &mut self ) {
            COUNTER.fetch_add( 1, Ordering::SeqCst );
        }
    }

    #[derive(Readable, Writable, PartialEq, Eq, Debug)]
    struct Struct( Vec< WithDrop > );

    Struct::read_from_stream_unbuffered( &mut &[0, 0, 0, 10, 1, 2][..] ).unwrap_err();
    assert_eq!( COUNTER.load( Ordering::SeqCst ), 0 );
}

#[test]
fn test_zero_copy_cow_deserialization() {
    let input: Vec< u8 > = vec![
        2, 0, 0, 0,
        33, 0,
        100, 0,
    ];

    let value_borrowed: Cow< [u16] > = Readable::read_from_buffer_with_ctx( Endianness::LittleEndian, &input ).unwrap();
    let value_owned: Cow< [u16] > = Readable::read_from_buffer_copying_data_with_ctx( Endianness::LittleEndian, &input ).unwrap();

    match value_borrowed {
        Cow::Owned( _ ) => panic!(),
        Cow::Borrowed( value ) => assert_eq!( value, &[33, 100] )
    }

    core::mem::drop( input );

    match value_owned {
        Cow::Owned( value ) => assert_eq!( value, &[33, 100] ),
        Cow::Borrowed( _ ) => panic!()
    }
}

#[cfg(feature = "std")]
#[test]
fn test_partial_deserialization_of_arrays_calls_destructors() {
    use core::sync::atomic::{AtomicU64, Ordering};

    static COUNTER: AtomicU64 = AtomicU64::new( 0 );

    #[derive(Readable, Writable, PartialEq, Eq, Debug)]
    struct WithDrop( String );
    impl Drop for WithDrop {
        fn drop( &mut self ) {
            COUNTER.fetch_add( 1, Ordering::SeqCst );
        }
    }

    <[WithDrop; 2]>::read_from_stream_unbuffered( &mut &[1, 0, 0, 0, b'A', 1, 0, 0, 0][..] ).unwrap_err();
    assert_eq!( COUNTER.load( Ordering::SeqCst ), 1 );
}
