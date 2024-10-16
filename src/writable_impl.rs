use core::mem;
use core::ops::{Range, RangeInclusive};

use crate::endianness::Endianness;
use crate::writable::Writable;
use crate::writer::Writer;

use crate::context::Context;

use crate::private::write_length;

#[cfg(feature = "std")]
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

#[cfg(feature = "alloc")]
use alloc::{
    borrow::{Cow, ToOwned},
    boxed::Box,
    string::String,
    vec::Vec,
    collections::{BTreeMap, BTreeSet}
};

#[cfg(feature = "alloc")]
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
        unsafe_is_length!( self.len() );

        let mut count = mem::size_of::< u32 >();
        for (key, value) in self {
            count += key.bytes_needed()? + value.bytes_needed()?;
        }

        Ok( count )
    }
}

#[cfg(feature = "alloc")]
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
        unsafe_is_length!( self.len() );

        let mut count = mem::size_of::< u32 >();
        for value in self {
            count += value.bytes_needed()?;
        }

        Ok( count )
    }
}

#[cfg(feature = "std")]
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
        unsafe_is_length!( self.len() );

        let mut count = mem::size_of::< u32 >();
        for (key, value) in self {
            count += key.bytes_needed()? + value.bytes_needed()?;
        }

        Ok( count )
    }
}

#[cfg(feature = "std")]
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
        unsafe_is_length!( self.len() );

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
                unsafe {
                    core::slice::from_raw_parts( slice.as_ptr() as *const u8, slice.len() * mem::size_of::< Self >() )
                }
            }
        }
    }
}

impl_for_primitive!( i8, write_i8 );
impl_for_primitive!( i16, write_i16 );
impl_for_primitive!( i32, write_i32 );
impl_for_primitive!( i64, write_i64 );
impl_for_primitive!( i128, write_i128 );
impl_for_primitive!( u8, write_u8 );
impl_for_primitive!( u16, write_u16 );
impl_for_primitive!( u32, write_u32 );
impl_for_primitive!( u64, write_u64 );
impl_for_primitive!( u128, write_u128 );
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

#[cfg(feature = "alloc")]
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

impl< 'a, C: Context > Writable< C > for &'a str {
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        self.as_bytes().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Writable::< C >::bytes_needed( self.as_bytes() )
    }
}

#[cfg(feature = "alloc")]
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
        unsafe_is_length!( self.len() );

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

#[cfg(feature = "alloc")]
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

#[cfg(feature = "alloc")]
impl< 'a, C: Context, T: Writable< C > > Writable< C > for &'a [T] where [T]: ToOwned {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        <[T] as Writable< C >>::write_to( self, writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        <[T] as Writable::< C >>::bytes_needed( self.as_ref() )
    }
}

#[cfg(feature = "std")]
impl< 'r, C, T > Writable< C > for Cow< 'r, HashSet< T > > where C: Context, T: Writable< C > + Clone + Hash + Eq {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        (&**self).write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Writable::< C >::bytes_needed( &**self )
    }
}

#[cfg(feature = "alloc")]
impl< 'r, C, T > Writable< C > for Cow< 'r, BTreeSet< T > > where C: Context, T: Writable< C > + Clone + Ord {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        (&**self).write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Writable::< C >::bytes_needed( &**self )
    }
}

#[cfg(feature = "std")]
impl< 'r, C, K, V > Writable< C > for Cow< 'r, HashMap< K, V > > where C: Context, K: Writable< C > + Clone + Hash + Eq, V: Writable< C > + Clone {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        (&**self).write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Writable::< C >::bytes_needed( &**self )
    }
}

#[cfg(feature = "alloc")]
impl< 'r, C, K, V > Writable< C > for Cow< 'r, BTreeMap< K, V > > where C: Context, K: Writable< C > + Clone + Ord, V: Writable< C > + Clone {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        (&**self).write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Writable::< C >::bytes_needed( &**self )
    }
}

#[cfg(feature = "alloc")]
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

impl< C: Context, T: Writable< C > > Writable< C > for RangeInclusive< T > {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        self.start().write_to( writer )?;
        self.end().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( Writable::< C >::bytes_needed( self.start() )? + Writable::< C >::bytes_needed( self.end() )? )
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

impl< C: Context, T: Writable< C >, E: Writable< C > > Writable< C > for Result< T, E > {
    #[inline]
    fn write_to< W: ?Sized + Writer< C > >( &self, writer: &mut W ) -> Result< (), C::Error > {
        match *self {
            Ok( ref value ) => {
                writer.write_u8( 1 )?;
                value.write_to( writer )
            },
            Err( ref value ) => {
                writer.write_u8( 0 )?;
                value.write_to( writer )
            }
        }
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        match *self {
            Ok( ref value ) => {
                Ok( 1 + Writable::< C >::bytes_needed( value )? )
            },
            Err( ref value ) => {
                Ok( 1 + Writable::< C >::bytes_needed( value )? )
            }
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

macro_rules! impl_for_non_zero {
    ($type:ident, $base_type:ty) => {
        impl< C > Writable< C > for core::num::$type where C: Context {
            #[inline]
            fn write_to< W >( &self, writer: &mut W ) -> Result< (), C::Error > where W: ?Sized + Writer< C > {
                self.get().write_to( writer )
            }

            #[inline]
            fn bytes_needed( &self ) -> Result< usize, C::Error > {
                Ok( mem::size_of::< $base_type >() )
            }
        }
    }
}

impl_for_non_zero!( NonZeroU8, u8 );
impl_for_non_zero!( NonZeroU16, u16 );
impl_for_non_zero!( NonZeroU32, u32 );
impl_for_non_zero!( NonZeroU64, u64 );
impl_for_non_zero!( NonZeroI8, i8 );
impl_for_non_zero!( NonZeroI16, i16 );
impl_for_non_zero!( NonZeroI32, i32 );
impl_for_non_zero!( NonZeroI64, i64 );

macro_rules! impl_for_atomic {
    ($type:ident, $base_type:ty) => {
        impl< C: Context > Writable< C > for core::sync::atomic::$type {
            #[inline(always)]
            fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
                writer.write_value( &self.load( core::sync::atomic::Ordering::SeqCst ) )
            }

            #[inline]
            fn bytes_needed( &self ) -> Result< usize, C::Error > {
                Ok( mem::size_of::< $base_type >() )
            }
        }
    }
}

#[cfg(target_has_atomic = "8")]
impl_for_atomic!( AtomicI8, i8 );

#[cfg(target_has_atomic = "16")]
impl_for_atomic!( AtomicI16, i16 );

#[cfg(target_has_atomic = "32")]
impl_for_atomic!( AtomicI32, i32 );

#[cfg(target_has_atomic = "64")]
impl_for_atomic!( AtomicI64, i64 );

#[cfg(target_has_atomic = "8")]
impl_for_atomic!( AtomicU8, u8 );

#[cfg(target_has_atomic = "16")]
impl_for_atomic!( AtomicU16, u16 );

#[cfg(target_has_atomic = "32")]
impl_for_atomic!( AtomicU32, u32 );

#[cfg(target_has_atomic = "64")]
impl_for_atomic!( AtomicU64, u64 );

impl< C > Writable< C > for core::net::Ipv4Addr where C: Context {
    #[inline]
    fn write_to< W >( &self, writer: &mut W ) -> Result< (), C::Error > where W: ?Sized + Writer< C > {
        let raw: u32 = (*self).into();
        writer.write_u32( raw )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( 4 )
    }
}

impl< C > Writable< C > for core::net::Ipv6Addr where C: Context {
    #[inline]
    fn write_to< W >( &self, writer: &mut W ) -> Result< (), C::Error > where W: ?Sized + Writer< C > {
        let mut octets = self.octets();
        if !writer.endianness().conversion_necessary() {
            octets.reverse();
        }

        writer.write_bytes( &octets )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( 16 )
    }
}

impl< C > Writable< C > for core::net::IpAddr where C: Context {
    #[inline]
    fn write_to< W >( &self, writer: &mut W ) -> Result< (), C::Error > where W: ?Sized + Writer< C > {
        match self {
            core::net::IpAddr::V4( address ) => {
                writer.write_u8( 0 )?;
                address.write_to( writer )
            },
            core::net::IpAddr::V6( address ) => {
                writer.write_u8( 1 )?;
                address.write_to( writer )
            }
        }
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        match self {
            core::net::IpAddr::V4( address ) => Writable::< C >::bytes_needed( address ).map( |count| count + 1 ),
            core::net::IpAddr::V6( address ) => Writable::< C >::bytes_needed( address ).map( |count| count + 1 )
        }
    }
}

impl< C > Writable< C > for core::time::Duration where C: Context {
    #[inline]
    fn write_to< W >( &self, writer: &mut W ) -> Result< (), C::Error > where W: ?Sized + Writer< C > {
        writer.write_u64( self.as_secs() )?;
        writer.write_u32( self.subsec_nanos() )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( 12 )
    }
}

#[cfg(feature = "std")]
impl< C > Writable< C > for std::time::SystemTime where C: Context {
    #[inline]
    fn write_to< W >( &self, writer: &mut W ) -> Result< (), C::Error > where W: ?Sized + Writer< C > {
        let duration = self.duration_since( std::time::SystemTime::UNIX_EPOCH ).map_err( |_| crate::error::error_invalid_system_time() )?;
        writer.write_value( &duration )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( 12 )
    }
}

impl< C, T, const N: usize > Writable< C > for [T; N] where C: Context, T: Writable< C > {
    #[inline]
    fn write_to< W >( &self, writer: &mut W ) -> Result< (), C::Error > where W: ?Sized + Writer< C > {
        for item in self {
            item.write_to( writer )?;
        }
        Ok(())
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        let mut size = 0;
        for item in self {
            size += Writable::< C >::bytes_needed( item )?;
        }
        Ok( size )
    }
}

#[cfg(feature = "alloc")]
impl< C, T > Writable< C > for Box< T >
    where C: Context,
          T: Writable< C >
{
    #[inline]
    fn write_to< W >( &self, writer: &mut W ) -> Result< (), C::Error > where W: ?Sized + Writer< C > {
        (**self).write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        (**self).bytes_needed()
    }
}

#[cfg(feature = "alloc")]
impl< C, T > Writable< C > for Box< [T] >
    where C: Context,
          T: Writable< C >
{
    #[inline]
    fn write_to< W >( &self, writer: &mut W ) -> Result< (), C::Error > where W: ?Sized + Writer< C > {
        (**self).write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        (**self).bytes_needed()
    }
}

#[cfg(feature = "alloc")]
impl< C > Writable< C > for Box< str >
    where C: Context
{
    #[inline]
    fn write_to< W >( &self, writer: &mut W ) -> Result< (), C::Error > where W: ?Sized + Writer< C > {
        let value: &str = &**self;
        value.write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        let value: &str = &**self;
        Writable::< C >::bytes_needed( value )
    }
}
