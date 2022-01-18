use std::mem;

use crate::context::Context;
use crate::endianness::Endianness;
use crate::writable::Writable;
use crate::varint::VarInt64;

pub trait Writer< C: Context > {
    fn write_bytes( &mut self, slice: &[u8] ) -> Result< (), C::Error >;

    fn context( &self ) -> &C;
    fn context_mut( &mut self ) -> &mut C;

    #[inline(always)]
    fn can_write_at_least( &self, _size: usize ) -> Option< bool > {
        None
    }

    #[inline(always)]
    fn write_u8( &mut self, value: u8 ) -> Result< (), C::Error > {
        let slice = unsafe { std::slice::from_raw_parts( &value, 1 ) };
        self.write_bytes( slice )
    }

    #[inline(always)]
    fn write_u16( &mut self, mut value: u16 ) -> Result< (), C::Error > {
        self.context().endianness().swap_u16( &mut value );
        let slice = unsafe { std::slice::from_raw_parts( &value as *const u16 as *const u8, 2 ) };
        self.write_bytes( slice )
    }

    #[inline(always)]
    fn write_u32( &mut self, mut value: u32 ) -> Result< (), C::Error > {
        self.context().endianness().swap_u32( &mut value );
        let slice = unsafe { std::slice::from_raw_parts( &value as *const u32 as *const u8, 4 ) };
        self.write_bytes( slice )
    }

    #[inline(always)]
    fn write_u64( &mut self, mut value: u64 ) -> Result< (), C::Error > {
        self.context().endianness().swap_u64( &mut value );
        let slice = unsafe { std::slice::from_raw_parts( &value as *const u64 as *const u8, 8 ) };
        self.write_bytes( slice )
    }

    #[inline(always)]
    fn write_u128( &mut self, mut value: u128 ) -> Result< (), C::Error > {
        self.context().endianness().swap_u128( &mut value );
        let slice = unsafe { std::slice::from_raw_parts( &value as *const u128 as *const u8, 16 ) };
        self.write_bytes( slice )
    }

    #[inline(always)]
    fn write_i8( &mut self, value: i8 ) -> Result< (), C::Error > {
        self.write_u8( value as u8 )
    }

    #[inline(always)]
    fn write_i16( &mut self, value: i16 ) -> Result< (), C::Error > {
        self.write_u16( value as u16 )
    }

    #[inline(always)]
    fn write_i32( &mut self, value: i32 ) -> Result< (), C::Error > {
        self.write_u32( value as u32 )
    }

    #[inline(always)]
    fn write_i64( &mut self, value: i64 ) -> Result< (), C::Error > {
        self.write_u64( value as u64 )
    }

    #[inline(always)]
    fn write_i128( &mut self, value: i128 ) -> Result< (), C::Error > {
        self.write_u128( value as u128 )
    }

    #[inline(always)]
    fn write_f32( &mut self, value: f32 ) -> Result< (), C::Error > {
        let value: u32 = unsafe { mem::transmute( value ) };
        self.write_u32( value )
    }

    #[inline(always)]
    fn write_f64( &mut self, value: f64 ) -> Result< (), C::Error > {
        let value: u64 = unsafe { mem::transmute( value ) };
        self.write_u64( value )
    }

    #[inline(always)]
    fn endianness( &self ) -> Endianness {
        self.context().endianness()
    }

    #[inline(always)]
    fn write_value< T: Writable< C > >( &mut self, item: &T ) -> Result< (), C::Error > {
        item.write_to( self )
    }

    #[inline]
    fn write_slice< T >( &mut self, slice: &[T] ) -> Result< (), C::Error >
        where T: Writable< C >
    {
        if T::speedy_is_primitive() && (mem::size_of::< T >() == 1 || !self.endianness().conversion_necessary()) {
            let bytes = unsafe { T::speedy_slice_as_bytes( slice ) };
            self.write_bytes( bytes )
        } else {
            for value in slice {
                self.write_value( value )?;
            }
            Ok(())
        }
    }

    #[inline]
    fn write_collection< T >( &mut self, collection: impl IntoIterator< Item = T > ) -> Result< (), C::Error >
        where T: Writable< C >
    {
        for item in collection {
            item.write_to( self )?;
        }

        Ok(())
    }

    #[inline]
    fn write_u64_varint( &mut self, value: u64 ) -> Result< (), C::Error > {
        VarInt64::from( value ).write_to( self )
    }
}
