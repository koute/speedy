use std::io;
use std::mem;
use std::borrow::Cow;

use endianness::Endianness;
use readable::Readable;
use context::Context;

pub trait Reader< 'a, C: Context >: Sized {
    fn read_bytes( &mut self, output: &mut [u8] ) -> io::Result< () >;
    fn context( &self ) -> &C;
    fn context_mut( &mut self ) -> &mut C;

    #[inline]
    fn read_bytes_cow( &mut self, length: usize ) -> io::Result< Cow< 'a, [u8] > > {
        let mut buffer = Vec::with_capacity( length );
        unsafe { buffer.set_len( length ) };
        try!( self.read_bytes( &mut buffer ) );
        Ok( Cow::Owned( buffer ) )
    }

    #[inline(always)]
    fn read_u8( &mut self ) -> io::Result< u8 > {
        let mut slice: [u8; 1] = unsafe { mem::uninitialized() };
        try!( self.read_bytes( &mut slice ) );
        Ok( slice[0] )
    }

    #[inline(always)]
    fn read_i8( &mut self ) -> io::Result< i8 > {
        self.read_u8().map( |byte| byte as i8 )
    }

    #[inline(always)]
    fn read_u16( &mut self ) -> io::Result< u16 > {
        let mut slice: [u8; 2] = unsafe { mem::uninitialized() };
        try!( self.read_bytes( &mut slice ) );
        Ok( self.context().endianness().read_u16( &slice ) )
    }

    #[inline(always)]
    fn read_i16( &mut self ) -> io::Result< i16 > {
        let mut slice: [u8; 2] = unsafe { mem::uninitialized() };
        try!( self.read_bytes( &mut slice ) );
        Ok( self.context().endianness().read_i16( &slice ) )
    }

    #[inline(always)]
    fn read_u32( &mut self ) -> io::Result< u32 > {
        let mut slice: [u8; 4] = unsafe { mem::uninitialized() };
        try!( self.read_bytes( &mut slice ) );
        Ok( self.context().endianness().read_u32( &slice ) )
    }

    #[inline(always)]
    fn read_i32( &mut self ) -> io::Result< i32 > {
        let mut slice: [u8; 4] = unsafe { mem::uninitialized() };
        try!( self.read_bytes( &mut slice ) );
        Ok( self.context().endianness().read_i32( &slice ) )
    }

    #[inline(always)]
    fn read_u64( &mut self ) -> io::Result< u64 > {
        let mut slice: [u8; 8] = unsafe { mem::uninitialized() };
        try!( self.read_bytes( &mut slice ) );
        Ok( self.context().endianness().read_u64( &slice ) )
    }

    #[inline(always)]
    fn read_i64( &mut self ) -> io::Result< i64 > {
        let mut slice: [u8; 8] = unsafe { mem::uninitialized() };
        try!( self.read_bytes( &mut slice ) );
        Ok( self.context().endianness().read_i64( &slice ) )
    }

    #[inline(always)]
    fn read_f32( &mut self ) -> io::Result< f32 > {
        let mut slice: [u8; 4] = unsafe { mem::uninitialized() };
        try!( self.read_bytes( &mut slice ) );
        Ok( self.context().endianness().read_f32( &slice ) )
    }

    #[inline(always)]
    fn read_f64( &mut self ) -> io::Result< f64 > {
        let mut slice: [u8; 8] = unsafe { mem::uninitialized() };
        try!( self.read_bytes( &mut slice ) );
        Ok( self.context().endianness().read_f64( &slice ) )
    }

    #[inline(always)]
    fn read_value< T: Readable< 'a, C > >( &mut self ) -> io::Result< T > {
        T::read_from( self )
    }

    #[inline(always)]
    fn endianness( &self ) -> Endianness {
        self.context().endianness()
    }
}
