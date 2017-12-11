use std::io;
use std::mem;

use context::Context;
use endianness::Endianness;
use writable::Writable;

pub trait Writer< 'a, C: Context > {
    fn write_bytes( &mut self, slice: &'a [u8] ) -> io::Result< () >;
    fn write_owned_bytes( &mut self, vec: Vec< u8 > ) -> io::Result< () >;
    fn write_u8( &mut self, value: u8 ) -> io::Result< () >;
    fn write_u16( &mut self, value: u16 ) -> io::Result< () >;
    fn write_u32( &mut self, value: u32 ) -> io::Result< () >;
    fn write_u64( &mut self, value: u64 ) -> io::Result< () >;

    fn context( &self ) -> &C;
    fn context_mut( &mut self ) -> &mut C;

    #[inline]
    fn write_i8( &mut self, value: i8 ) -> io::Result< () > {
        self.write_u8( value as u8 )
    }

    #[inline]
    fn write_i16( &mut self, value: i16 ) -> io::Result< () > {
        self.write_u16( value as u16 )
    }

    #[inline]
    fn write_i32( &mut self, value: i32 ) -> io::Result< () > {
        self.write_u32( value as u32 )
    }

    #[inline]
    fn write_i64( &mut self, value: i64 ) -> io::Result< () > {
        self.write_u64( value as u64 )
    }

    #[inline]
    fn write_f32( &mut self, value: f32 ) -> io::Result< () > {
        let value: u32 = unsafe { mem::transmute( value ) };
        self.write_u32( value )
    }

    #[inline]
    fn write_f64( &mut self, value: f64 ) -> io::Result< () > {
        let value: u64 = unsafe { mem::transmute( value ) };
        self.write_u64( value )
    }

    #[inline]
    fn endianness( &self ) -> Endianness {
        self.context().endianness()
    }

    #[inline]
    fn write_value< T: Writable< C > >( &mut self, item: &'a T ) -> io::Result< () > {
        item.write_to( self )
    }
}
