use std::io;
use std::mem;
use std::borrow::Cow;

use readable::Readable;
use reader::Reader;

use context::Context;
use utils::as_bytes_mut;

impl< C: Context > Readable< C > for bool {
    #[inline]
    fn read_from< R: Reader< C > >( reader: &mut R ) -> io::Result< Self > {
        let value = try!( reader.read_u8() );
        if value == 0 {
            Ok( false )
        } else {
            Ok( true )
        }
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        1
    }
}

macro_rules! impl_for_primitive {
    ($type:ty, $getter:ident) => {
        impl< C: Context > Readable< C > for $type {
            #[inline]
            fn read_from< R: Reader< C > >( reader: &mut R ) -> io::Result< Self > {
                reader.$getter()
            }

            #[inline]
            fn minimum_bytes_needed() -> usize {
                mem::size_of::< Self >()
            }
        }
    }
}

impl_for_primitive!( u8, read_u8 );
impl_for_primitive!( u16, read_u16 );
impl_for_primitive!( u32, read_u32 );
impl_for_primitive!( u64, read_u64 );
impl_for_primitive!( f32, read_f32 );
impl_for_primitive!( f64, read_f64 );

impl< C: Context > Readable< C > for Vec< u8 > {
    #[inline]
    fn read_from< R: Reader< C > >( reader: &mut R ) -> io::Result< Self > {
        let length = try!( reader.read_u32() ) as usize;
        let mut vec = Vec::with_capacity( length );
        unsafe { vec.set_len( length ); }
        try!( reader.read_bytes( &mut vec[..] ) );

        Ok( vec )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}

impl< 'a, C: Context > Readable< C > for Cow< 'a, [u8] > {
    #[inline]
    fn read_from< R: Reader< C > >( reader: &mut R ) -> io::Result< Self > {
        let bytes: Vec< u8 > = try!( reader.read_value() );
        Ok( bytes.into() )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <Vec< u8 > as Readable< C >>::minimum_bytes_needed()
    }
}

impl< 'a, C: Context > Readable< C > for Cow< 'a, [u64] > {
    #[inline]
    fn read_from< R: Reader< C > >( reader: &mut R ) -> io::Result< Self > {
        let bytes: Vec< u64 > = try!( reader.read_value() );
        Ok( bytes.into() )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <Vec< u64 > as Readable< C >>::minimum_bytes_needed()
    }
}

impl< C: Context > Readable< C > for String {
    #[inline]
    fn read_from< R: Reader< C > >( reader: &mut R ) -> io::Result< Self > {
        let bytes: Vec< u8 > = try!( reader.read_value() );
        match String::from_utf8( bytes ) {
            Err( error ) => Err( io::Error::new( io::ErrorKind::InvalidData, error ) ),
            Ok( string ) => Ok( string )
        }
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <Vec< u8 > as Readable< C >>::minimum_bytes_needed()
    }
}

impl< C: Context > Readable< C > for Vec< u64 > {
    #[inline]
    fn read_from< R: Reader< C > >( reader: &mut R ) -> io::Result< Self > {
        let length = try!( reader.read_u32() ) as usize;
        let mut vec = Vec::with_capacity( length );
        unsafe { vec.set_len( length ); }
        try!( reader.read_bytes( as_bytes_mut( &mut vec ) ) );

        Ok( vec )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <Vec< u8 > as Readable< C >>::minimum_bytes_needed()
    }
}
