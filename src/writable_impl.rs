use std::mem;
use std::borrow::{Cow, ToOwned};
use std::ops::Range;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use crate::endianness::Endianness;
use crate::writable::Writable;
use crate::writer::Writer;

use crate::context::Context;
use crate::utils::as_bytes;

use crate::private::write_length;

impl< C, K, V > Writable< C > for BTreeMap< K, V >
    where C: Context,
          K: Writable< C >,
          V: Writable< C >
{
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        write_length( self.len(), writer )?;
        writer.write_collection( self.iter() )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        let mut count = mem::size_of::< u32 >();
        for (key, value) in self {
            count += key.bytes_needed()? + value.bytes_needed()?;
        }

        Ok( count )
    }
}

impl< C, T > Writable< C > for BTreeSet< T >
    where C: Context,
          T: Writable< C >
{
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        write_length( self.len(), writer )?;
        writer.write_collection( self.iter() )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        let mut count = mem::size_of::< u32 >();
        for value in self {
            count += value.bytes_needed()?;
        }

        Ok( count )
    }
}

impl< C, K, V, S > Writable< C > for HashMap< K, V, S >
    where C: Context,
          K: Writable< C >,
          V: Writable< C >
{
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        write_length( self.len(), writer )?;
        writer.write_collection( self.iter() )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        let mut count = mem::size_of::< u32 >();
        for (key, value) in self {
            count += key.bytes_needed()? + value.bytes_needed()?;
        }

        Ok( count )
    }
}

impl< C, T, S > Writable< C > for HashSet< T, S >
    where C: Context,
          T: Writable< C >
{
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        write_length( self.len(), writer )?;
        writer.write_collection( self.iter() )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        let mut count = mem::size_of::< u32 >();
        for value in self {
            count += value.bytes_needed()?;
        }

        Ok( count )
    }
}

macro_rules! impl_for_primitive {
    ($type:ty, $write_name:ident) => {
        impl< C: Context > Writable< C > for $type {
            #[inline(always)]
            fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
                writer.$write_name( *self )
            }

            #[inline]
            fn bytes_needed( &self ) -> Result< usize, C::Error > {
                Ok( mem::size_of::< Self >() )
            }

            #[doc(hidden)]
            #[inline(always)]
            fn speedy_is_primitive() -> bool {
                true
            }

            #[doc(hidden)]
            #[inline(always)]
            unsafe fn speedy_slice_as_bytes( slice: &[Self] ) -> &[u8] where Self: Sized {
                as_bytes( slice )
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

impl< C: Context > Writable< C > for usize {
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        writer.write_u64( *self as u64 )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( mem::size_of::< u64 >() )
    }
}

impl< C: Context > Writable< C > for bool {
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        writer.write_u8( if *self { 1 } else { 0 } )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( 1 )
    }
}

impl< C: Context > Writable< C > for char {
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        writer.write_u32( *self as u32 )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( mem::size_of::< u32 >() )
    }
}

impl< C: Context > Writable< C > for String {
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        self.as_bytes().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Writable::< C >::bytes_needed( self.as_bytes() )
    }
}

impl< C: Context > Writable< C > for str {
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        self.as_bytes().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Writable::< C >::bytes_needed( self.as_bytes() )
    }
}

impl< 'r, C: Context > Writable< C > for Cow< 'r, str > {
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        self.as_bytes().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Writable::< C >::bytes_needed( self.as_bytes() )
    }
}

impl< C: Context, T: Writable< C > > Writable< C > for [T] {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        write_length( self.len(), writer )?;
        writer.write_slice( self )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        if T::speedy_is_primitive() {
            return Ok( 4 + self.len() * mem::size_of::< T >() );
        }

        let mut sum = 4;
        for element in self {
            sum += element.bytes_needed()?;
        }

        Ok( sum )
    }
}

impl< 'r, C: Context, T: Writable< C > > Writable< C > for Cow< 'r, [T] > where [T]: ToOwned {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        self.as_ref().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Writable::< C >::bytes_needed( self.as_ref() )
    }
}

impl< C: Context, T: Writable< C > > Writable< C > for Vec< T > {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        self.as_slice().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Writable::< C >::bytes_needed( self.as_slice() )
    }
}

impl< C: Context, T: Writable< C > > Writable< C > for Range< T > {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        self.start.write_to( writer )?;
        self.end.write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( Writable::< C >::bytes_needed( &self.start )? + Writable::< C >::bytes_needed( &self.end )? )
    }
}

impl< C: Context, T: Writable< C > > Writable< C > for Option< T > {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        if let Some( ref value ) = *self {
            writer.write_u8( 1 )?;
            value.write_to( writer )
        } else {
            writer.write_u8( 0 )
        }
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        if let Some( ref value ) = *self {
            Ok( 1 + Writable::< C >::bytes_needed( value )? )
        } else {
            Ok( 1 )
        }
    }
}

impl< C: Context > Writable< C > for () {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, _: &mut W ) -> Result< (), C::Error > {
        Ok(())
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( 0 )
    }
}

macro_rules! impl_for_tuple {
    ($($name:ident),+) => {
        impl< C: Context, $($name: Writable< C >),+ > Writable< C > for ($($name,)+) {
            #[inline]
            fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
                #[allow(non_snake_case)]
                let &($(ref $name,)+) = self;
                $(
                    $name.write_to( writer )?;
                )+
                Ok(())
            }

            #[inline]
            fn bytes_needed( &self ) -> Result< usize, C::Error > {
                #[allow(non_snake_case)]
                let &($(ref $name,)+) = self;
                let mut size = 0;
                $(
                    size += Writable::< C >::bytes_needed( $name )?;
                )+
                Ok( size )
            }
        }
    }
}

impl_for_tuple!( A0 );
impl_for_tuple!( A0, A1 );
impl_for_tuple!( A0, A1, A2 );
impl_for_tuple!( A0, A1, A2, A3 );
impl_for_tuple!( A0, A1, A2, A3, A4 );
impl_for_tuple!( A0, A1, A2, A3, A4, A5 );
impl_for_tuple!( A0, A1, A2, A3, A4, A5, A6 );
impl_for_tuple!( A0, A1, A2, A3, A4, A5, A6, A7 );
impl_for_tuple!( A0, A1, A2, A3, A4, A5, A6, A7, A8 );
impl_for_tuple!( A0, A1, A2, A3, A4, A5, A6, A7, A8, A9 );
impl_for_tuple!( A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10 );

impl< C: Context > Writable< C > for Endianness {
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        let value = match *self {
            Endianness::LittleEndian => 0,
            Endianness::BigEndian => 1
        };

        writer.write_u8( value )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( 1 )
    }
}

impl< 'a, C, T > Writable< C > for &'a T where C: Context, T: Writable< C > {
    #[inline(always)]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        (**self).write_to( writer )
    }

    #[inline(always)]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        (**self).bytes_needed()
    }
}

impl< 'a, C, T > Writable< C > for &'a mut T where C: Context, T: Writable< C > {
    #[inline(always)]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        (**self).write_to( writer )
    }

    #[inline(always)]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        (**self).bytes_needed()
    }
}
