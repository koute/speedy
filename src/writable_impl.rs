use std::io;

use std::mem;
use std::borrow::Cow;

use writable::Writable;
use writer::Writer;

use context::Context;
use utils::as_bytes;

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

impl_for_primitive!( i8, write_i8 );
impl_for_primitive!( i16, write_i16 );
impl_for_primitive!( i32, write_i32 );
impl_for_primitive!( i64, write_i64 );
impl_for_primitive!( u8, write_u8 );
impl_for_primitive!( u16, write_u16 );
impl_for_primitive!( u32, write_u32 );
impl_for_primitive!( u64, write_u64 );
impl_for_primitive!( f32, write_f32 );
impl_for_primitive!( f64, write_f64 );

impl< C: Context > Writable< C > for bool {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        writer.write_u8( if *self { 1 } else { 0 } )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        1
    }
}

impl< C: Context > Writable< C > for [u8] {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        try!( writer.write_u32( self.len() as _ ) );
        writer.write_bytes( self )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        4 + self.len()
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

impl< C: Context > Writable< C > for Vec< u8 > {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        self.as_slice().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        Writable::< C >::bytes_needed( self.as_slice() )
    }
}

impl< C: Context > Writable< C > for [i8] {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        as_bytes( self ).write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        Writable::< C >::bytes_needed( as_bytes( self ) )
    }
}

impl< 'r, C: Context > Writable< C > for Cow< 'r, [i8] > {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        self.as_ref().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        Writable::< C >::bytes_needed( self.as_ref() )
    }
}

impl< C: Context > Writable< C > for Vec< i8 > {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        self.as_slice().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        Writable::< C >::bytes_needed( self.as_slice() )
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

impl< C: Context > Writable< C > for str {
    #[inline]
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
        self.as_bytes().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        Writable::< C >::bytes_needed( self.as_bytes() )
    }
}

macro_rules! impl_for_primitive_slice {
    ($type:ty, $writer:ident) => {
        impl< C: Context > Writable< C > for [$type] {
            #[inline]
            fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
                try!( writer.write_u32( self.len() as _ ) );
                if writer.endianness().conversion_necessary() {
                    for &value in self {
                        try!( writer.$writer( value ) );
                    }
                    Ok(())
                } else {
                    writer.write_bytes( as_bytes( self ) )
                }
            }

            #[inline]
            fn bytes_needed( &self ) -> usize {
                4 + self.len() * mem::size_of::< $type >()
            }
        }

        impl< 'r, C: Context > Writable< C > for Cow< 'r, [$type] > {
            #[inline]
            fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
                self.as_ref().write_to( writer )
            }

            #[inline]
            fn bytes_needed( &self ) -> usize {
                Writable::< C >::bytes_needed( self.as_ref() )
            }
        }

        impl< C: Context > Writable< C > for Vec< $type > {
            #[inline]
            fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
                self.as_slice().write_to( writer )
            }

            #[inline]
            fn bytes_needed( &self ) -> usize {
                Writable::< C >::bytes_needed( self.as_slice() )
            }
        }
    }
}

impl_for_primitive_slice!( i16, write_i16 );
impl_for_primitive_slice!( i32, write_i32 );
impl_for_primitive_slice!( i64, write_i64 );
impl_for_primitive_slice!( u16, write_u16 );
impl_for_primitive_slice!( u32, write_u32 );
impl_for_primitive_slice!( u64, write_u64 );
impl_for_primitive_slice!( f32, write_f32 );
impl_for_primitive_slice!( f64, write_f64 );
