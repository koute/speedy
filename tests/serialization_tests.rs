extern crate speedy;

use std::borrow::Cow;
#[allow(unused_imports)]
use speedy::{Readable, Writable, Endianness};

macro_rules! symmetric_tests {
    ($(
        $name:ident for $type:ty {
            in = $value:expr,
            le = $le_bytes:expr,
            be = $be_bytes:expr
        }
    )*) => { $(
        mod $name {
            use super::*;

            #[test]
            fn round_trip_le() {
                let original: $type = $value;
                let serialized = original.write_to_vec( Endianness::LittleEndian ).unwrap();
                let deserialized: $type = Readable::read_from_buffer( Endianness::LittleEndian, &serialized ).unwrap();
                assert_eq!( original, deserialized );
            }

            #[test]
            fn round_trip_be() {
                let original: $type = $value;
                let serialized = original.write_to_vec( Endianness::BigEndian ).unwrap();
                let deserialized: $type = Readable::read_from_buffer( Endianness::BigEndian, &serialized ).unwrap();
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
        }
    )* }
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
        ]
    }
    cow_u8 for Cow< 'static, [u8] > {
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
        ]
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
        ]
    }
    cow_u16 for Cow< 'static, [u16] > {
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
        ]
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
        ]
    }
    cow_u32 for Cow< 'static, [u32] > {
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
        ]
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
        ]
    }
    cow_u64 for Cow< 'static, [u64] > {
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
        ]
    }
    bool_false for bool {
        in = false,
        le = [0],
        be = [0]
    }
    bool_true for bool {
        in = true,
        le = [1],
        be = [1]
    }
    u8 for u8 {
        in = 33,
        le = [33],
        be = [33]
    }
    i8 for i8 {
        in = -33,
        le = [223],
        be = [223]
    }
    u16 for u16 {
        in = 33,
        le = [33, 0],
        be = [0, 33]
    }
    i16 for i16 {
        in = -33,
        le = [223, 255],
        be = [255, 223]
    }
    u32 for u32 {
        in = 33,
        le = [33, 0, 0, 0],
        be = [0, 0, 0, 33]
    }
    i32 for i32 {
        in = -33,
        le = [223, 255, 255, 255],
        be = [255, 255,255, 223]
    }
    u64 for u64 {
        in = 33,
        le = [33, 0, 0, 0, 0, 0, 0, 0],
        be = [0, 0, 0, 0, 0, 0, 0, 33]
    }
    i64 for i64 {
        in = -33,
        le = [223, 255, 255, 255, 255, 255, 255, 255],
        be = [255, 255, 255, 255, 255, 255, 255, 223]
    }
    string for String {
        in = "Hello".to_owned(),
        le = [5, 0, 0, 0, 72, 101, 108, 108, 111],
        be = [0, 0, 0, 5, 72, 101, 108, 108, 111]
    }
}
