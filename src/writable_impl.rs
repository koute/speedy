use std::io;

use std::mem;
use std::borrow::Cow;

use writable::Writable;
use writer::Writer;

use context::Context;
use utils::as_bytes;

impl< C: Context > Writable< C > for u8 {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        writer.write_u8( *self )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        mem::size_of::< Self >()
    }
}

macro_rules! impl_for_primitive {
    ($type:ty, $write_name:ident) => {
        impl< C: Context > Writable< C > for $type {
            #[inline]
            fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
                writer.$write_name( *self )
            }

            #[inline]
            fn bytes_needed( &self ) -> usize {
                mem::size_of::< Self >()
            }
        }
    }
}

impl_for_primitive!( u16, write_u16 );
impl_for_primitive!( u32, write_u32 );
impl_for_primitive!( u64, write_u64 );
impl_for_primitive!( f32, write_f32 );
impl_for_primitive!( f64, write_f64 );

impl< C: Context > Writable< C > for bool {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        writer.write_u8( if *self { 1 } else { 0 } )?;
        Ok(())
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        1
    }
}

impl< C: Context > Writable< C > for [u8] {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        writer.write_u32( self.len() as _ )?;
        writer.write_bytes( self )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        4 + self.len()
    }
}

impl< 'r, C: Context > Writable< C > for &'r [u8] {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        (*self).write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        Writable::< C >::bytes_needed( *self )
    }
}

impl< 'r, C: Context > Writable< C > for Cow< 'r, [u8] > {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        self.as_ref().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        Writable::< C >::bytes_needed( self.as_ref() )
    }
}

impl< C: Context > Writable< C > for String {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        self.as_bytes().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        Writable::< C >::bytes_needed( self.as_bytes() )
    }
}

impl< C: Context > Writable< C > for [u64] {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        writer.write_u32( self.len() as _ )?;
        if writer.endianness().conversion_necessary() {
            for &value in self {
                writer.write_u64( value )?;
            }
            Ok(())
        } else {
            writer.write_bytes( as_bytes( self ) )
        }
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        4 + self.len() * mem::size_of::< u64 >()
    }
}

impl< C: Context > Writable< C > for Vec< u64 > {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        self.as_slice().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        Writable::< C >::bytes_needed( self.as_slice() )
    }
}
