use std::mem::{self, MaybeUninit};
use std::borrow::Cow;
use std::iter::FromIterator;

use crate::endianness::Endianness;
use crate::readable::Readable;
use crate::context::Context;
use crate::varint::VarInt64;

use crate::error::error_end_of_input;
use crate::error::IsEof;
use crate::utils::SwapBytes;

struct RawCopyIter< T > {
    pointer: std::ptr::NonNull< T >,
    end: *const T
}

impl< T > Iterator for RawCopyIter< T > {
    type Item = T;
    #[inline(always)]
    fn next( &mut self ) -> Option< Self::Item > {
        if self.pointer.as_ptr() as *const T == self.end {
            None
        } else {
            unsafe {
                let old = self.pointer.as_ptr();
                self.pointer = std::ptr::NonNull::new_unchecked( old.add( 1 ) );
                Some( std::ptr::read_unaligned( old ) )
            }
        }
    }

    #[inline(always)]
    fn size_hint( &self ) -> (usize, Option< usize >) {
        let length = self.len();
        (length, Some( length ))
    }
}

impl< T > ExactSizeIterator for RawCopyIter< T > {
    #[inline(always)]
    fn len(&self) -> usize {
        let bytesize = self.end as usize - self.pointer.as_ptr() as usize;
        bytesize / std::mem::size_of::< T >()
    }
}
impl< T > std::iter::FusedIterator for RawCopyIter< T > {}

pub trait Reader< 'a, C: Context >: Sized {
    fn read_bytes( &mut self, output: &mut [u8] ) -> Result< (), C::Error >;
    fn peek_bytes( &mut self, output: &mut [u8] ) -> Result< (), C::Error >;
    fn context( &self ) -> &C;
    fn context_mut( &mut self ) -> &mut C;

    #[inline(always)]
    unsafe fn read_bytes_into_ptr( &mut self, output: *mut u8, length: usize ) -> Result< (), C::Error > {
        unsafe {
            self.read_bytes( std::slice::from_raw_parts_mut( output, length ) )
        }
    }

    #[inline(always)]
    unsafe fn peek_bytes_into_ptr( &mut self, output: *mut u8, length: usize ) -> Result< (), C::Error > {
        unsafe {
            self.peek_bytes( std::slice::from_raw_parts_mut( output, length ) )
        }
    }

    #[inline(always)]
    fn skip_bytes( &mut self, mut length: usize ) -> Result< (), C::Error > {
        const CHUNK_SIZE: usize = 1024;
        let mut dummy_buffer: [MaybeUninit< u8 >; CHUNK_SIZE] = unsafe { MaybeUninit::uninit().assume_init() };

        while length > 0 {
            let chunk_size = if length < CHUNK_SIZE { length } else { CHUNK_SIZE };
            let dummy_buffer: *mut MaybeUninit< u8 > = dummy_buffer.as_mut_ptr();
            unsafe {
                self.read_bytes_into_ptr( dummy_buffer as *mut u8, chunk_size )?;
            }
            length -= chunk_size;
        }

        Ok(())
    }

    #[inline(always)]
    fn can_read_at_least( &self, _size: usize ) -> Option< bool > {
        None
    }

    #[inline(always)]
    fn read_bytes_borrowed( &mut self, _length: usize ) -> Option< Result< &'a [u8], C::Error > > {
        None
    }

    #[inline(always)]
    fn read_bytes_borrowed_from_reader( &mut self, _length: usize ) -> Option< Result< &[u8], C::Error > > {
        None
    }

    #[inline(always)]
    fn read_bytes_borrowed_until_eof( &mut self ) -> Option< &'a [u8] > {
        None
    }

    #[inline(always)]
    fn read_u8( &mut self ) -> Result< u8, C::Error > {
        if self.can_read_at_least( 1 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< u8 > = MaybeUninit::uninit();
        unsafe {
            self.read_bytes_into_ptr( value.as_mut_ptr(), 1 )?;
            Ok( value.assume_init() )
        }
    }

    #[inline(always)]
    fn peek_u8( &mut self ) -> Result< u8, C::Error > {
        if self.can_read_at_least( 1 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< u8 > = MaybeUninit::uninit();
        unsafe {
            self.peek_bytes_into_ptr( value.as_mut_ptr(), 1 )?;
            Ok( value.assume_init() )
        }
    }

    #[inline(always)]
    fn read_i8( &mut self ) -> Result< i8, C::Error > {
        self.read_u8().map( |byte| byte as i8 )
    }

    #[inline(always)]
    fn peek_i8( &mut self ) -> Result< i8, C::Error > {
        self.peek_u8().map( |byte| byte as i8 )
    }

    #[inline(always)]
    fn read_u16( &mut self ) -> Result< u16, C::Error > {
        if self.can_read_at_least( 2 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< u16 > = MaybeUninit::uninit();
        unsafe {
            self.read_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 2 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn peek_u16( &mut self ) -> Result< u16, C::Error > {
        if self.can_read_at_least( 2 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< u16 > = MaybeUninit::uninit();
        unsafe {
            self.peek_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 2 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn read_i16( &mut self ) -> Result< i16, C::Error > {
        if self.can_read_at_least( 2 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< i16 > = MaybeUninit::uninit();
        unsafe {
            self.read_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 2 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn peek_i16( &mut self ) -> Result< i16, C::Error > {
        if self.can_read_at_least( 2 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< i16 > = MaybeUninit::uninit();
        unsafe {
            self.peek_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 2 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn read_u32( &mut self ) -> Result< u32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< u32 > = MaybeUninit::uninit();
        unsafe {
            self.read_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 4 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn peek_u32( &mut self ) -> Result< u32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< u32 > = MaybeUninit::uninit();
        unsafe {
            self.peek_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 4 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn read_i32( &mut self ) -> Result< i32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< i32 > = MaybeUninit::uninit();
        unsafe {
            self.read_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 4 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn peek_i32( &mut self ) -> Result< i32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< i32 > = MaybeUninit::uninit();
        unsafe {
            self.peek_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 4 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn read_u64( &mut self ) -> Result< u64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< u64 > = MaybeUninit::uninit();
        unsafe {
            self.read_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 8 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn peek_u64( &mut self ) -> Result< u64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< u64 > = MaybeUninit::uninit();
        unsafe {
            self.peek_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 8 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn read_i64( &mut self ) -> Result< i64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< i64 > = MaybeUninit::uninit();
        unsafe {
            self.read_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 8 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn peek_i64( &mut self ) -> Result< i64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< i64 > = MaybeUninit::uninit();
        unsafe {
            self.peek_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 8 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn read_u128( &mut self ) -> Result< u128, C::Error > {
        if self.can_read_at_least( 16 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< u128 > = MaybeUninit::uninit();
        unsafe {
            self.read_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 16 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn peek_u128( &mut self ) -> Result< u128, C::Error > {
        if self.can_read_at_least( 16 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< u128 > = MaybeUninit::uninit();
        unsafe {
            self.peek_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 16 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn read_i128( &mut self ) -> Result< i128, C::Error > {
        if self.can_read_at_least( 16 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< i128 > = MaybeUninit::uninit();
        unsafe {
            self.read_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 16 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn peek_i128( &mut self ) -> Result< i128, C::Error > {
        if self.can_read_at_least( 16 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< i128 > = MaybeUninit::uninit();
        unsafe {
            self.peek_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 16 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn read_f32( &mut self ) -> Result< f32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< f32 > = MaybeUninit::uninit();
        unsafe {
            self.read_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 4 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn peek_f32( &mut self ) -> Result< f32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< f32 > = MaybeUninit::uninit();
        unsafe {
            self.peek_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 4 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn read_f64( &mut self ) -> Result< f64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< f64 > = MaybeUninit::uninit();
        unsafe {
            self.read_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 8 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn peek_f64( &mut self ) -> Result< f64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut value: MaybeUninit< f64 > = MaybeUninit::uninit();
        unsafe {
            self.peek_bytes_into_ptr( value.as_mut_ptr() as *mut u8, 8 )?;
            let value = value.assume_init();
            if self.context().endianness().conversion_necessary() {
                Ok( value.swap_bytes() )
            } else {
                Ok( value )
            }
        }
    }

    #[inline(always)]
    fn read_value< T: Readable< 'a, C > >( &mut self ) -> Result< T, C::Error > {
        T::read_from( self )
    }

    #[inline(always)]
    fn endianness( &self ) -> Endianness {
        self.context().endianness()
    }

    #[inline]
    fn read_vec< T >( &mut self, length: usize ) -> Result< Vec< T >, C::Error >
        where T: Readable< 'a, C >
    {
        let (required, overflow) = T::minimum_bytes_needed().overflowing_mul( length );
        if overflow || self.can_read_at_least( required ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut vec = Vec::with_capacity( length );
        if T::speedy_is_primitive() {
            unsafe {
                self.read_bytes_into_ptr( vec.as_mut_ptr() as *mut u8, vec.capacity() * std::mem::size_of::< T >() )?;
                vec.set_len( length );
            }

            T::speedy_convert_slice_endianness( self.endianness(), &mut vec );
        } else {
            #[inline(never)]
            #[cold]
            fn drop_vec< T >( mut vec: Vec< T >, length: usize ) {
                unsafe {
                    vec.set_len( length );
                }

                std::mem::drop( vec );
            }

            let mut p = vec.as_mut_ptr();
            let e = unsafe { p.add( length ) };

            // We deliberately have a separate counter instead
            // of substracting pointers as this optimizes better.
            let mut count = 0;
            while p < e {
                let value = match self.read_value() {
                    Ok( value ) => value,
                    Err( error ) => {
                        drop_vec( vec, count );
                        return Err( error );
                    }
                };
                unsafe {
                    std::ptr::write( p, value );
                    p = p.add( 1 );
                    count += 1;
                }
            }
            unsafe {
                vec.set_len( length );
            }
        }

        Ok( vec )
    }

    #[inline]
    fn read_vec_until_eof< T >( &mut self ) -> Result< Vec< T >, C::Error >
        where T: Readable< 'a, C >
    {
        // TODO: Optimize this.
        let mut vec: Vec< T > = Vec::new();
        loop {
            let value = match self.read_value() {
                Ok( value ) => value,
                Err( error ) if error.is_eof() => break,
                Err( error ) => return Err( error )
            };
            vec.push( value );
        }

        Ok( vec )
    }

    #[inline]
    fn read_cow< T >( &mut self, length: usize ) -> Result< Cow< 'a, [T] >, C::Error >
        where T: Readable< 'a, C >,
              [T]: ToOwned< Owned = Vec< T > >
    {
        if T::speedy_is_primitive() && (mem::size_of::< T >() == 1 || !self.endianness().conversion_necessary()) {
            if let Some( bytes ) = self.read_bytes_borrowed( length * mem::size_of::< T >() ) {
                let bytes = bytes?;
                assert_eq!( bytes.len(), length * mem::size_of::< T >() );

                if mem::align_of::< T >() == 1 || bytes.as_ptr() as usize % mem::align_of::< T >() == 0 {
                    let slice = unsafe { T::speedy_slice_from_bytes( bytes ) };
                    return Ok( Cow::Borrowed( slice ) );
                } else {
                    let mut vec: Vec< T > = Vec::with_capacity( length );
                    unsafe {
                        std::ptr::copy_nonoverlapping( bytes.as_ptr(), vec.as_mut_ptr() as *mut u8, bytes.len() );
                        vec.set_len( length );
                    }
                    return Ok( Cow::Owned( vec ) );
                }
            }
        }

        Ok( Cow::Owned( self.read_vec( length )? ) )
    }

    #[inline]
    fn read_cow_until_eof< T >( &mut self ) -> Result< Cow< 'a, [T] >, C::Error >
        where T: Readable< 'a, C >,
              [T]: ToOwned< Owned = Vec< T > >
    {
        // TODO: Optimize this.
        Ok( Cow::Owned( self.read_vec_until_eof()? ) )
    }

    #[inline]
    fn read_string( &mut self, length: usize ) -> Result< String, C::Error > {
        let bytes = self.read_vec( length )?;
        crate::private::vec_to_string( bytes )
    }

    #[inline]
    fn read_collection< T, U >( &mut self, length: usize ) -> Result< U, C::Error >
        where U: FromIterator< T >,
              T: Readable< 'a, C >
    {
        if T::speedy_is_primitive() && (mem::size_of::< T >() == 1 || !self.endianness().conversion_necessary()) {
            let bytesize = length.checked_mul( std::mem::size_of::< T >() ).ok_or_else( || {
                crate::error::error_too_big_usize_for_this_architecture() // TODO: Use different error maybe?
            })?;

            if let Some( bytes ) = self.read_bytes_borrowed_from_reader( bytesize ) {
                let bytes = bytes?;
                unsafe {
                    let pointer = bytes.as_ptr().cast::< T >();
                    return Ok( RawCopyIter { pointer: std::ptr::NonNull::new_unchecked( pointer as *mut T ), end: pointer.add( length ) }.collect() );
                }
            }
        }

        (0..length).into_iter().map( |_| self.read_value::< T >() ).collect()
    }

    #[inline]
    fn read_key_value_collection< K, V, U >( &mut self, length: usize ) -> Result< U, C::Error >
        where U: FromIterator< (K, V) >,
              K: Readable< 'a, C >,
              V: Readable< 'a, C >
    {
        #[repr(packed)]
        struct Pair< K, V >( K, V );

        if K::speedy_is_primitive() && V::speedy_is_primitive() && ((mem::size_of::< K >() == 1 && mem::size_of::< V >() == 1) || !self.endianness().conversion_necessary()) {
            let bytesize = length.checked_mul( std::mem::size_of::< Pair< K, V > >() ).ok_or_else( || {
                crate::error::error_too_big_usize_for_this_architecture()
            })?;

            if let Some( bytes ) = self.read_bytes_borrowed_from_reader( bytesize ) {
                let bytes = bytes?;
                unsafe {
                    let pointer = bytes.as_ptr().cast::< Pair< K, V > >();
                    return Ok( RawCopyIter { pointer: std::ptr::NonNull::new_unchecked( pointer as *mut Pair< K, V > ), end: pointer.add( length ) }.map( |pair| (pair.0, pair.1) ).collect() );
                }
            }
        }

        self.read_collection( length )
    }

    #[inline]
    fn read_collection_until_eof< T, U >( &mut self ) -> Result< U, C::Error >
        where U: FromIterator< T >,
              T: Readable< 'a, C >
    {
        std::iter::from_fn( move || {
            match self.read_value::< T >() {
                Ok( value ) => Some( Ok( value ) ),
                Err( error ) if error.is_eof() => None,
                Err( error ) => Some( Err( error ) )
            }
        }).collect()
    }

    #[inline]
    fn read_u64_varint( &mut self ) -> Result< u64, C::Error > {
        let value = VarInt64::read_from( self )?;
        Ok( value.into() )
    }

    #[inline]
    fn peek_u64_varint( &mut self ) -> Result< u64, C::Error > {
        let value = VarInt64::peek_from( self )?;
        Ok( value.into() )
    }
}
