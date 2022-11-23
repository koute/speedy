use std::mem;
use std::borrow::{Cow, ToOwned};
use std::ops::Range;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::hash::{BuildHasher, Hash};

use crate::readable::Readable;
use crate::reader::Reader;

use crate::context::Context;
use crate::utils::SwapBytes;
use crate::endianness::Endianness;

impl< 'a, C, K, V > Readable< 'a, C > for BTreeMap< K, V >
    where C: Context,
          K: Readable< 'a, C > + Ord,
          V: Readable< 'a, C >,
{
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let length = crate::private::read_length( reader )?;
        reader.read_key_value_collection( length )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}

impl< 'a, C, T > Readable< 'a, C > for BTreeSet< T >
    where C: Context,
          T: Readable< 'a, C > + Ord
{
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let length = crate::private::read_length( reader )?;
        reader.read_collection( length )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}

impl< 'a, C, K, V, S > Readable< 'a, C > for HashMap< K, V, S >
    where C: Context,
          K: Readable< 'a, C > + Eq + Hash,
          V: Readable< 'a, C >,
          S: BuildHasher + Default
{
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let length = crate::private::read_length( reader )?;
        reader.read_key_value_collection( length )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}

impl< 'a, C, T, S > Readable< 'a, C > for HashSet< T, S >
    where C: Context,
          T: Readable< 'a, C > + Eq + Hash,
          S: BuildHasher + Default
{
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let length = crate::private::read_length( reader )?;
        reader.read_collection( length )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}

impl< 'a, C: Context > Readable< 'a, C > for bool {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let value = reader.read_u8()?;
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

impl< 'a, C: Context > Readable< 'a, C > for char {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let value = reader.read_u32()?;
        std::char::from_u32( value ).ok_or_else( crate::error::error_out_of_range_char )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}

macro_rules! impl_for_primitive {
    ($type:ty, $getter:ident, $endianness_swap:ident) => {
        impl< 'a, C: Context > Readable< 'a, C > for $type {
            #[inline(always)]
            fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
                reader.$getter()
            }

            #[inline]
            fn minimum_bytes_needed() -> usize {
                mem::size_of::< Self >()
            }

            #[doc(hidden)]
            #[inline(always)]
            fn speedy_is_primitive() -> bool {
                true
            }

            #[doc(hidden)]
            #[inline]
            unsafe fn speedy_slice_from_bytes( slice: &[u8] ) -> &[Self] {
                unsafe {
                    std::slice::from_raw_parts( slice.as_ptr() as *const $type, slice.len() / mem::size_of::< Self >() )
                }
            }

            #[doc(hidden)]
            #[inline(always)]
            unsafe fn speedy_flip_endianness( itself: *mut Self ) {
                unsafe {
                    std::ptr::write_unaligned( itself, std::ptr::read_unaligned( itself ).swap_bytes() );
                }
            }

            #[doc(hidden)]
            #[inline(always)]
            fn speedy_convert_slice_endianness( endianness: Endianness, slice: &mut [$type] ) {
                endianness.$endianness_swap( slice );
            }
        }
    }
}

impl_for_primitive!( i8, read_i8, swap_slice_i8 );
impl_for_primitive!( i16, read_i16, swap_slice_i16 );
impl_for_primitive!( i32, read_i32, swap_slice_i32 );
impl_for_primitive!( i64, read_i64, swap_slice_i64 );
impl_for_primitive!( i128, read_i128, swap_slice_i128 );
impl_for_primitive!( u8, read_u8, swap_slice_u8 );
impl_for_primitive!( u16, read_u16, swap_slice_u16 );
impl_for_primitive!( u32, read_u32, swap_slice_u32 );
impl_for_primitive!( u64, read_u64, swap_slice_u64 );
impl_for_primitive!( u128, read_u128, swap_slice_u128 );
impl_for_primitive!( f32, read_f32, swap_slice_f32 );
impl_for_primitive!( f64, read_f64, swap_slice_f64 );

impl< 'a, C: Context > Readable< 'a, C > for usize {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let value = u64::read_from( reader )?;
        if value > std::usize::MAX as u64 {
            return Err( crate::error::error_too_big_usize_for_this_architecture() );
        }
        Ok( value as usize )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <u64 as Readable< 'a, C >>::minimum_bytes_needed()
    }
}

impl< 'a, C: Context > Readable< 'a, C > for String {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let bytes: Vec< u8 > = reader.read_value()?;
        let value = crate::private::vec_to_string( bytes )?;
        Ok( value )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <Vec< u8 > as Readable< 'a, C >>::minimum_bytes_needed()
    }
}

impl< 'a, C: Context > Readable< 'a, C > for Cow< 'a, str > {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let length = crate::private::read_length( reader )?;
        let bytes: Cow< 'a, [u8] > = reader.read_cow( length )?;
        let value = crate::private::cow_bytes_to_cow_str( bytes )?;
        Ok( value )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <String as Readable< 'a, C >>::minimum_bytes_needed()
    }
}

impl< 'a, C: Context, T: Readable< 'a, C > > Readable< 'a, C > for Vec< T > {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let length = crate::private::read_length( reader )?;
        reader.read_vec( length )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}

impl< 'a, C: Context, T: Readable< 'a, C > > Readable< 'a, C > for Cow< 'a, [T] > where [T]: ToOwned< Owned = Vec< T > > {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let length = crate::private::read_length( reader )?;
        reader.read_cow( length )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <Vec< T > as Readable< 'a, C >>::minimum_bytes_needed()
    }
}

impl< 'a, C: Context, T: Readable< 'a, C > > Readable< 'a, C > for Cow< 'a, HashSet< T > > where T: Readable< 'a, C > + Clone + Hash + Eq {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        Ok( Cow::Owned( reader.read_value()? ) )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <HashSet< T > as Readable< 'a, C >>::minimum_bytes_needed()
    }
}

impl< 'a, C: Context, T: Readable< 'a, C > > Readable< 'a, C > for Cow< 'a, BTreeSet< T > > where T: Readable< 'a, C > + Clone + Ord {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        Ok( Cow::Owned( reader.read_value()? ) )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <BTreeSet< T > as Readable< 'a, C >>::minimum_bytes_needed()
    }
}

impl< 'a, C: Context, K: Readable< 'a, C >, V: Readable< 'a, C > > Readable< 'a, C > for Cow< 'a, HashMap< K, V > > where K: Readable< 'a, C > + Clone + Hash + Eq, V: Readable< 'a, C > + Clone {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        Ok( Cow::Owned( reader.read_value()? ) )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <HashMap< K, V > as Readable< 'a, C >>::minimum_bytes_needed()
    }
}

impl< 'a, C: Context, K: Readable< 'a, C >, V: Readable< 'a, C > > Readable< 'a, C > for Cow< 'a, BTreeMap< K, V > > where K: Readable< 'a, C > + Clone + Ord, V: Readable< 'a, C > + Clone {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        Ok( Cow::Owned( reader.read_value()? ) )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <BTreeMap< K, V > as Readable< 'a, C >>::minimum_bytes_needed()
    }
}

impl< 'a, C: Context, T: Readable< 'a, C > > Readable< 'a, C > for Range< T > {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let start = reader.read_value()?;
        let end = reader.read_value()?;
        Ok( start..end )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <T as Readable< 'a, C >>::minimum_bytes_needed() * 2
    }
}

impl< 'a, C: Context, T: Readable< 'a, C > > Readable< 'a, C > for Option< T > {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let flag = reader.read_value()?;
        let value = if flag {
            Some( reader.read_value()? )
        } else {
            None
        };

        Ok( value )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        1
    }
}

impl< 'a, C: Context > Readable< 'a, C > for () {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( _: &mut R ) -> Result< Self, C::Error > {
        Ok(())
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        0
    }
}

macro_rules! impl_for_tuple {
    ($($name:ident),+) => {
        impl< 'a, C: Context, $($name: Readable< 'a, C >),+ > Readable< 'a, C > for ($($name,)+) {
            #[inline]
            fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
                $(
                    #[allow(non_snake_case)]
                    let $name = reader.read_value()?;
                )+

                Ok( ($($name,)+) )
            }

            #[inline]
            fn minimum_bytes_needed() -> usize {
                let mut size = 0;
                $(
                    size += <$name as Readable< 'a, C >>::minimum_bytes_needed();
                )+
                size
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

impl< 'a, C: Context > Readable< 'a, C > for Endianness {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let value = reader.read_u8()?;
        match value {
            0 => Ok( Endianness::LittleEndian ),
            1 => Ok( Endianness::BigEndian ),
            _ => Err( crate::error::error_invalid_enum_variant() )
        }
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        1
    }
}

macro_rules! impl_for_non_zero {
    ($type:ident, $base_type:ty) => {
        impl< 'a, C: Context > Readable< 'a, C > for std::num::$type {
            #[inline]
            fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
                let value: $base_type = reader.read_value()?;
                std::num::$type::new( value ).ok_or_else( crate::error::error_zero_non_zero )
            }

            #[inline]
            fn minimum_bytes_needed() -> usize {
                mem::size_of::< $base_type >()
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
        impl< 'a, C: Context > Readable< 'a, C > for std::sync::atomic::$type {
            #[inline(always)]
            fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
                let value: $base_type = reader.read_value()?;
                Ok( value.into() )
            }

            #[inline]
            fn minimum_bytes_needed() -> usize {
                mem::size_of::< $base_type >()
            }
        }
    }
}

impl_for_atomic!( AtomicI8, i8 );
impl_for_atomic!( AtomicI16, i16 );
impl_for_atomic!( AtomicI32, i32 );
impl_for_atomic!( AtomicI64, i64 );

impl_for_atomic!( AtomicU8, u8 );
impl_for_atomic!( AtomicU16, u16 );
impl_for_atomic!( AtomicU32, u32 );
impl_for_atomic!( AtomicU64, u64 );

impl< 'a, C > Readable< 'a, C > for std::net::Ipv4Addr where C: Context {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let value = reader.read_u32()?;
        Ok( value.into() )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}

impl< 'a, C > Readable< 'a, C > for std::net::Ipv6Addr where C: Context {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let mut octets = [0; 16];
        reader.read_bytes( &mut octets )?;
        if !reader.endianness().conversion_necessary() {
            octets.reverse();
        }

        Ok( octets.into() )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        16
    }
}

impl< 'a, C > Readable< 'a, C > for std::net::IpAddr where C: Context {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let kind = reader.read_u8()?;
        match kind {
            0 => Ok( std::net::IpAddr::V4( reader.read_value()? ) ),
            1 => Ok( std::net::IpAddr::V6( reader.read_value()? ) ),
            _ => Err( crate::error::error_invalid_enum_variant() )
        }
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        5
    }
}

impl< 'a, C > Readable< 'a, C > for std::time::Duration where C: Context {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let secs = reader.read_u64()?;
        let nanos = reader.read_u32()?;
        Ok( std::time::Duration::new( secs, nanos ) )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        12
    }
}

impl< 'a, C > Readable< 'a, C > for std::time::SystemTime where C: Context {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let duration = std::time::Duration::read_from( reader )?;
        std::time::SystemTime::UNIX_EPOCH.checked_add( duration ).ok_or_else( crate::error::error_invalid_system_time )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        12
    }
}

macro_rules! repeat {
    (1, $expr:expr) => { [$expr] };
    (2, $expr:expr) => { [$expr, $expr] };
    (3, $expr:expr) => { [$expr, $expr, $expr] };
    (4, $expr:expr) => { [$expr, $expr, $expr, $expr] };
    (5, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr] };
    (6, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr, $expr] };
    (7, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr, $expr, $expr] };
    (8, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr] };
    (9, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr] };
    (10, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr] };
    (11, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr] };
    (12, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr] };
    (13, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr] };
    (14, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr] };
    (15, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr] };
    (16, $expr:expr) => { [$expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr, $expr] };
}

macro_rules! impl_for_array {
    ($count:tt) => {
        impl< 'a, C, T > Readable< 'a, C > for [T; $count] where C: Context, T: Readable< 'a, C > {
            #[inline(always)]
            fn read_from< R >( reader: &mut R ) -> Result< Self, C::Error > where R: Reader< 'a, C > {
                let array = repeat!( $count, reader.read_value()? );
                Ok( array )
            }

            #[inline]
            fn minimum_bytes_needed() -> usize {
                T::minimum_bytes_needed() * $count
            }
        }
    }
}

impl_for_array!( 1 );
impl_for_array!( 2 );
impl_for_array!( 3 );
impl_for_array!( 4 );
impl_for_array!( 5 );
impl_for_array!( 6 );
impl_for_array!( 7 );
impl_for_array!( 8 );
impl_for_array!( 9 );
impl_for_array!( 10 );
impl_for_array!( 11 );
impl_for_array!( 12 );
impl_for_array!( 13 );
impl_for_array!( 14 );
impl_for_array!( 15 );
impl_for_array!( 16 );

impl< 'a, C, T > Readable< 'a, C > for Box< T >
    where C: Context,
          T: Readable< 'a, C >
{
    #[inline]
    fn read_from< R >( reader: &mut R ) -> Result< Self, C::Error > where R: Reader< 'a, C > {
        Ok( Box::new( T::read_from( reader )? ) )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        T::minimum_bytes_needed()
    }
}

impl< 'a, C, T > Readable< 'a, C > for Box< [T] >
    where C: Context,
          T: Readable< 'a, C >
{
    #[inline]
    fn read_from< R >( reader: &mut R ) -> Result< Self, C::Error > where R: Reader< 'a, C > {
        let data = Vec::< T >::read_from( reader )?;
        Ok( data.into() )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        Vec::< T >::minimum_bytes_needed()
    }
}

impl< 'a, C > Readable< 'a, C > for Box< str >
    where C: Context
{
    #[inline]
    fn read_from< R >( reader: &mut R ) -> Result< Self, C::Error > where R: Reader< 'a, C > {
        let data = String::read_from( reader )?;
        Ok( data.into() )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <String as Readable< 'a, C >>::minimum_bytes_needed()
    }
}
