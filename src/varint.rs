use {
    crate::{
        Context,
        Readable,
        Reader,
        Writable,
        Writer
    }
};

// Encoding:
//   At most  7bit - 0xxxxxxx
//   At most 14bit - 10xxxxxx xxxxxxxx
//   At most 21bit - 110xxxxx xxxxxxxx xxxxxxxx
//   At most 28bit - 1110xxxx xxxxxxxx xxxxxxxx xxxxxxxx
//   At most 35bit - 11110xxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
//   At most 42bit - 111110xx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
//   At most 49bit - 1111110x xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
//   At most 56bit - 11111110 xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
//   At most 64bit - 11111111 xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
//
// The first byte always contains the most significant bits. The rest of the bytes are always
// written in a little endian order.
//
// Signed integers are transformed with a zigzag transformation before being serialized.

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct VarInt64( u64 );

impl From< u64 > for VarInt64 {
    #[inline]
    fn from( value: u64 ) -> Self {
        VarInt64( value )
    }
}

impl From< u32 > for VarInt64 {
    #[inline]
    fn from( value: u32 ) -> Self {
        VarInt64( value as u64 )
    }
}

impl From< u16 > for VarInt64 {
    #[inline]
    fn from( value: u16 ) -> Self {
        VarInt64( value as u64 )
    }
}

impl From< u8 > for VarInt64 {
    #[inline]
    fn from( value: u8 ) -> Self {
        VarInt64( value as u64 )
    }
}

impl From< i64 > for VarInt64 {
    #[inline]
    fn from( value: i64 ) -> Self {
        let value = (value << 1) ^ (value >> 63);
        VarInt64( value as u64 )
    }
}

impl From< i32 > for VarInt64 {
    #[inline]
    fn from( value: i32 ) -> Self {
        let value = (value << 1) ^ (value >> 31);
        VarInt64( value as u32 as u64 )
    }
}

impl From< i16 > for VarInt64 {
    #[inline]
    fn from( value: i16 ) -> Self {
        let value = (value << 1) ^ (value >> 15);
        VarInt64( value as u16 as u64 )
    }
}

impl From< i8 > for VarInt64 {
    #[inline]
    fn from( value: i8 ) -> Self {
        let value = (value << 1) ^ (value >> 7);
        VarInt64( value as u8 as u64 )
    }
}

impl From< VarInt64 > for u64 {
    #[inline]
    fn from( value: VarInt64 ) -> Self {
        value.0
    }
}

impl From< VarInt64 > for i64 {
    #[inline]
    fn from( value: VarInt64 ) -> Self {
        let value = value.0;
        ((value >> 1) ^ (value << 15)) as i64
    }
}

macro_rules! impl_read {
    ($reader:ident, $read_u8:ident, $read_bytes:ident) => {{
        let first_byte = $reader.$read_u8()?;
        let length = (!first_byte).leading_zeros();

        let upper_mask = 0b11111111_u64 >> length;
        let upper_bits = (upper_mask & (first_byte as u64)).wrapping_shl( length * 8 );

        macro_rules! read {
            ($count:expr) => {{
                let mut value: u64 = 0;
                {
                    let slice = unsafe { std::slice::from_raw_parts_mut( &mut value as *mut u64 as *mut u8, $count ) };
                    $reader.$read_bytes( slice )?;
                }
                value = value.to_le();
                Ok( VarInt64( upper_bits | value ) )
            }}
        }

        match length {
            0 => read! { 0 },
            1 => read! { 1 },
            2 => read! { 2 },
            3 => read! { 3 },
            4 => read! { 4 },
            5 => read! { 5 },
            6 => read! { 6 },
            7 => read! { 7 },
            8 => read! { 8 },
            _ => {
                if cfg!( debug_assertions ) {
                    unreachable!()
                } else {
                    unsafe {
                        std::hint::unreachable_unchecked();
                    }
                }
            }
        }
    }}
}

impl< 'a, C: Context > Readable< 'a, C > for VarInt64 {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        impl_read!( reader, read_u8, read_bytes )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        1
    }
}

impl VarInt64 {
    #[inline]
    pub(crate) fn peek_from< 'a, C, R >( reader: &mut R ) -> Result< Self, C::Error >
        where C: Context,
              R: Reader< 'a, C >
    {
        impl_read!( reader, peek_u8, peek_bytes )
    }
}

#[cfg(test)]
fn get_length_slow( leading_zeros: u32 ) -> u32 {
    let bits_required = 64 - leading_zeros;
    match bits_required {
        0..=7 => 0,
        8..=14 => 1,
        15..=21 => 2,
        22..=28 => 3,
        29..=35 => 4,
        36..=42 => 5,
        43..=49 => 6,
        50..=56 => 7,
        57..=64 => 8,
        _ => unreachable!()
    }
}

#[inline]
fn get_length( leading_zeros: u32 ) -> u32 {
    let bits_required = 64 - leading_zeros;
    let x = bits_required >> 3;
    ((x + bits_required) ^ x) >> 3
}

#[test]
fn test_get_length() {
    for zeros in 0..=64 {
        assert_eq!(
            get_length( zeros ),
            get_length_slow( zeros )
        );
    }
}

impl< C: Context > Writable< C > for VarInt64 {
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        let mut value = self.0;
        let length = get_length( value.leading_zeros() );

        if let Some( false ) = writer.can_write_at_least( length as usize + 1 ) {
            return Err( crate::error::error_end_of_output_buffer() );
        }

        match length {
            0 => writer.write_u8( value as u8 ),
            1 => {
                writer.write_u8( 0b10000000 | (value >> 8) as u8 )?;
                value = value.to_le();
                let slice = unsafe { std::slice::from_raw_parts( &value as *const u64 as *const u8, 1 ) };
                writer.write_bytes( slice )
            },
            2 => {
                writer.write_u8( 0b11000000 | (value >> 16) as u8 )?;
                value = value.to_le();
                let slice = unsafe { std::slice::from_raw_parts( &value as *const u64 as *const u8, 2 ) };
                writer.write_bytes( slice )
            },
            3 => {
                writer.write_u8( 0b11100000 | (value >> 24) as u8 )?;
                value = value.to_le();
                let slice = unsafe { std::slice::from_raw_parts( &value as *const u64 as *const u8, 3 ) };
                writer.write_bytes( slice )
            },
            4 => {
                writer.write_u8( 0b11110000 | (value >> 32) as u8 )?;
                value = value.to_le();
                let slice = unsafe { std::slice::from_raw_parts( &value as *const u64 as *const u8, 4 ) };
                writer.write_bytes( slice )
            },
            5 => {
                writer.write_u8( 0b11111000 | (value >> 40) as u8 )?;
                value = value.to_le();
                let slice = unsafe { std::slice::from_raw_parts( &value as *const u64 as *const u8, 5 ) };
                writer.write_bytes( slice )
            },
            6 => {
                writer.write_u8( 0b11111100 | (value >> 48) as u8 )?;
                value = value.to_le();
                let slice = unsafe { std::slice::from_raw_parts( &value as *const u64 as *const u8, 6 ) };
                writer.write_bytes( slice )
            },
            7 => {
                writer.write_u8( 0b11111110 | (value >> 56) as u8 )?;
                value = value.to_le();
                let slice = unsafe { std::slice::from_raw_parts( &value as *const u64 as *const u8, 7 ) };
                writer.write_bytes( slice )
            },
            8 => {
                writer.write_u8( 0b11111111 )?;
                value = value.to_le();
                let slice = unsafe { std::slice::from_raw_parts( &value as *const u64 as *const u8, 8 ) };
                writer.write_bytes( slice )
            },
            _ => {
                if cfg!( debug_assertions ) {
                    unreachable!()
                } else {
                    unsafe {
                        std::hint::unreachable_unchecked();
                    }
                }
            }
        }
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( get_length( self.0.leading_zeros() ) as usize + 1 )
    }
}

#[test]
fn test_varint64_serialization() {
    use crate::Endianness;

    macro_rules! check {
        ($($value:expr => $expected:expr),+) => {
            $(
                assert_eq!(
                    VarInt64( $value ).write_to_vec_with_ctx( Endianness::LittleEndian ).unwrap(),
                    &$expected
                );
            )+
            $(
                assert_eq!(
                    VarInt64( $value ).write_to_vec_with_ctx( Endianness::BigEndian ).unwrap(),
                    &$expected
                );
            )+
        }
    }

    check! {
        0b_00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000 => [0b00000000],
        0b_00000000_00000000_00000000_00000000_00000000_00000000_00000000_01111111 => [0b01111111],
        0b_00000000_00000000_00000000_00000000_00000000_00000000_00000000_10000000 => [0b10000000, 0b10000000],
        0b_00000000_00000000_00000000_00000000_00000000_00000000_00111111_11111111 => [0b10111111, 0b11111111],
        0b_00000000_00000000_00000000_00000000_00000000_00000000_01000000_00000000 => [0b11000000, 0b00000000, 0b01000000],
        0b_00000000_00000000_00000000_00000000_00000000_00011111_11111111_11111111 => [0b11011111, 0b11111111, 0b11111111],
        0b_00000000_00000000_00000000_00000000_00000000_00100000_00000000_00000000 => [0b11100000, 0b00000000, 0b00000000, 0b00100000],
        0b_00000000_00000000_00000000_00000000_00001111_11111111_11111111_11111111 => [0b11101111, 0b11111111, 0b11111111, 0b11111111],
        0b_00000000_00000000_00000000_00000000_00010000_00000000_00000000_00000000 => [0b11110000, 0b00000000, 0b00000000, 0b00000000, 0b00010000],
        0b_00000000_00000000_00000000_00000111_11111111_11111111_11111111_11111111 => [0b11110111, 0b11111111, 0b11111111, 0b11111111, 0b11111111],
        0b_00000000_00000000_00000000_00001000_00000000_00000000_00000000_00000000 => [0b11111000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00001000],
        0b_00000000_00000000_00000011_11111111_11111111_11111111_11111111_11111111 => [0b11111011, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111],
        0b_00000000_00000000_00000100_00000000_00000000_00000000_00000000_00000000 => [0b11111100, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000100],
        0b_00000000_00000001_11111111_11111111_11111111_11111111_11111111_11111111 => [0b11111101, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111],
        0b_00000000_00000010_00000000_00000000_00000000_00000000_00000000_00000000 => [0b11111110, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000010],
        0b_00000000_11111111_11111111_11111111_11111111_11111111_11111111_11111111 => [0b11111110, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111],
        0b_00000001_00000000_00000000_00000000_00000000_00000000_00000000_00000000 => [0b11111111, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000000, 0b00000001],
        0b_11111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111 => [0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111, 0b11111111]
    }
}

#[test]
fn test_varint64_bruteforce() {
    const fn hash32( x: u32 ) -> u32 {
        let mut x = x.wrapping_mul( 0xa4d94a4f );
        let a = x >> 16;
        let b = x >> 30;
        x ^= a >> b;
        x.wrapping_mul( 0xa4d94a4f )
    }

    const fn hash64( x: u64 ) -> u64 {
        (hash32( x as u32 ) as u64) | ((hash32( (x.wrapping_mul( 0xaaaaaaaa ) ^ 0xaaaaaaaa) as u32 ) as u64) << 32)
    }

    for n in 0..64 {
        let values = [
            1 << n,
            (1 << n) - 1,
            ((1 << n) - 1) << 1,
            (1_u64 << n).wrapping_mul( 0b00000010 ),
            (1_u64 << n).wrapping_mul( 0b00000101 ),
            (1_u64 << n).wrapping_mul( 0b00001010 ),
            (1_u64 << n).wrapping_mul( 0b00010101 ),
            (1_u64 << n).wrapping_mul( 0b00101010 ),
            (1_u64 << n).wrapping_mul( 0b01010101 ),
            (1_u64 << n).wrapping_mul( 0b10101010 ),
            hash64( n )
        ];

        for &value in &values {
            let value = VarInt64( value );
            let serialized_le = value.write_to_vec_with_ctx( crate::Endianness::LittleEndian ).unwrap();
            let deserialized = VarInt64::read_from_buffer_with_ctx( crate::Endianness::LittleEndian, &serialized_le ).unwrap();
            assert_eq!( deserialized, value );

            let serialized_be = value.write_to_vec_with_ctx( crate::Endianness::BigEndian ).unwrap();
            assert_eq!( serialized_be, serialized_le );
        }
    }
}
