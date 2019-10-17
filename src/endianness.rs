use std::slice;

use byteorder::{
    ByteOrder,
    LittleEndian,
    BigEndian
};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Endianness {
    LittleEndian,
    BigEndian
}

impl Endianness {
    #[cfg( target_endian = "little" )]
    pub const NATIVE: Endianness = Endianness::LittleEndian;

    #[cfg( target_endian = "big" )]
    pub const NATIVE: Endianness = Endianness::BigEndian;
}

impl Endianness {
    #[inline(always)]
    pub fn conversion_necessary( self ) -> bool {
        self != Endianness::NATIVE
    }

    #[inline(always)]
    pub fn swap_slice_u8( self, _: &mut [u8] ) {}

    #[inline(always)]
    pub fn swap_slice_i8( self, _: &mut [i8] ) {}
}

macro_rules! emit_wrapper {
    ($type:ty, $reader:ident, $swapper:ident, $slice_swapper:ident, $from_slice:ident, $write:ident) => {
        impl Endianness {
            #[inline]
            pub fn $reader( self, slice: &[u8] ) -> $type {
                match self {
                    Endianness::LittleEndian => LittleEndian::$reader( slice ),
                    Endianness::BigEndian => BigEndian::$reader( slice )
                }
            }

            #[inline]
            pub fn $swapper( self, value: &mut $type ) {
                let slice = unsafe { slice::from_raw_parts_mut( value as *mut $type, 1 ) };
                self.$slice_swapper( slice );
            }

            #[inline]
            pub fn $slice_swapper( self, slice: &mut [$type] ) {
                match self {
                    Endianness::LittleEndian => LittleEndian::$from_slice( slice ),
                    Endianness::BigEndian => BigEndian::$from_slice( slice )
                }
            }

        }
    }
}

emit_wrapper!( u16, read_u16, swap_u16, swap_slice_u16, from_slice_u16, write_u16 );
emit_wrapper!( u32, read_u32, swap_u32, swap_slice_u32, from_slice_u32, write_u32 );
emit_wrapper!( u64, read_u64, swap_u64, swap_slice_u64, from_slice_u64, write_u64 );
emit_wrapper!( i16, read_i16, swap_i16, swap_slice_i16, from_slice_i16, write_i16 );
emit_wrapper!( i32, read_i32, swap_i32, swap_slice_i32, from_slice_i32, write_i32 );
emit_wrapper!( i64, read_i64, swap_i64, swap_slice_i64, from_slice_i64, write_i64 );
emit_wrapper!( f32, read_f32, swap_f32, swap_slice_f32, from_slice_f32, write_f32 );
emit_wrapper!( f64, read_f64, swap_f64, swap_slice_f64, from_slice_f64, write_f64 );
