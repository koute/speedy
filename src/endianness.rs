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
    ($type:ty, $reader:ident, $swapper:ident, $slice_swapper:ident) => {
        impl Endianness {
            #[inline]
            pub fn $reader( self, slice: &[u8] ) -> $type {
                assert!( slice.len() == core::mem::size_of::< $type >() );

                let mut value: $type = 0;
                unsafe {
                    core::ptr::copy_nonoverlapping(
                        slice.as_ptr(),
                        &mut value as *mut $type as *mut u8,
                        core::mem::size_of::< $type >()
                    );
                }

                if self.conversion_necessary() {
                    value = value.swap_bytes();
                }

                value
            }

            #[inline]
            pub fn $swapper( self, value: &mut $type ) {
                if self.conversion_necessary() {
                    *value = value.swap_bytes();
                }
            }

            #[inline]
            pub fn $slice_swapper( self, slice: &mut [$type] ) {
                if self.conversion_necessary() {
                    for value in slice {
                        *value = value.swap_bytes();
                    }
                }
            }
        }
    }
}

emit_wrapper!( u16, read_u16, swap_u16, swap_slice_u16 );
emit_wrapper!( u32, read_u32, swap_u32, swap_slice_u32 );
emit_wrapper!( u64, read_u64, swap_u64, swap_slice_u64 );
emit_wrapper!( u128, read_u128, swap_u128, swap_slice_u128 );
emit_wrapper!( i16, read_i16, swap_i16, swap_slice_i16 );
emit_wrapper!( i32, read_i32, swap_i32, swap_slice_i32 );
emit_wrapper!( i64, read_i64, swap_i64, swap_slice_i64 );
emit_wrapper!( i128, read_i128, swap_i128, swap_slice_i128 );

impl Endianness {
    #[inline]
    pub fn read_f32( self, slice: &[u8] ) -> f32 {
        f32::from_bits( self.read_u32( slice ) )
    }

    #[inline]
    pub fn read_f64( self, slice: &[u8] ) -> f64 {
        f64::from_bits( self.read_u64( slice ) )
    }

    #[inline]
    pub fn swap_f32( self, value: &mut f32 ) {
        let value = unsafe {
            &mut *(value as *mut f32 as *mut u32)
        };

        self.swap_u32( value );
    }

    #[inline]
    pub fn swap_f64( self, value: &mut f64 ) {
        let value = unsafe {
            &mut *(value as *mut f64 as *mut u64)
        };

        self.swap_u64( value );
    }

    #[inline]
    pub fn swap_slice_f32( self, slice: &mut [f32] ) {
        let slice = unsafe {
            core::slice::from_raw_parts_mut( slice.as_mut_ptr() as *mut u32, slice.len() )
        };

        self.swap_slice_u32( slice );
    }

    #[inline]
    pub fn swap_slice_f64( self, slice: &mut [f64] ) {
        let slice = unsafe {
            core::slice::from_raw_parts_mut( slice.as_mut_ptr() as *mut u64, slice.len() )
        };

        self.swap_slice_u64( slice );
    }
}
