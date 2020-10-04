use std::mem::{self, MaybeUninit};
use std::borrow::Cow;
use std::iter::FromIterator;

use crate::endianness::Endianness;
use crate::readable::Readable;
use crate::context::Context;
use crate::varint::VarInt64;

use crate::error::error_end_of_input;

pub trait Reader< 'a, C: Context >: Sized {
    fn read_bytes( &mut self, output: &mut [u8] ) -> Result< (), C::Error >;
    fn peek_bytes( &mut self, output: &mut [u8] ) -> Result< (), C::Error >;
    fn context( &self ) -> &C;
    fn context_mut( &mut self ) -> &mut C;

    #[inline(always)]
    fn skip_bytes( &mut self, mut length: usize ) -> Result< (), C::Error > {
        while length > 0 {
            const CHUNK_SIZE: usize = 1024;
            let mut dummy_buffer: [u8; CHUNK_SIZE] = unsafe { MaybeUninit::uninit().assume_init() };
            let chunk_size = if length < CHUNK_SIZE { length } else { CHUNK_SIZE };
            self.read_bytes( &mut dummy_buffer[ 0..chunk_size ] )?;
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
    fn read_u8( &mut self ) -> Result< u8, C::Error > {
        if self.can_read_at_least( 1 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 1] = unsafe { MaybeUninit::uninit().assume_init() };
        self.read_bytes( &mut slice )?;
        Ok( slice[0] )
    }

    #[inline(always)]
    fn peek_u8( &mut self ) -> Result< u8, C::Error > {
        if self.can_read_at_least( 1 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 1] = unsafe { MaybeUninit::uninit().assume_init() };
        self.peek_bytes( &mut slice )?;
        Ok( slice[0] )
    }

    #[inline(always)]
    fn read_i8( &mut self ) -> Result< i8, C::Error > {
        self.read_u8().map( |byte| byte as i8 )
    }

    #[inline(always)]
    fn peek_i8( &mut self ) -> Result< i8, C::Error > {
        self.read_u8().map( |byte| byte as i8 )
    }

    #[inline(always)]
    fn read_u16( &mut self ) -> Result< u16, C::Error > {
        if self.can_read_at_least( 2 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 2] = unsafe { MaybeUninit::uninit().assume_init() };
        self.read_bytes( &mut slice )?;
        Ok( self.context().endianness().read_u16( &slice ) )
    }

    #[inline(always)]
    fn peek_u16( &mut self ) -> Result< u16, C::Error > {
        if self.can_read_at_least( 2 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 2] = unsafe { MaybeUninit::uninit().assume_init() };
        self.peek_bytes( &mut slice )?;
        Ok( self.context().endianness().read_u16( &slice ) )
    }

    #[inline(always)]
    fn read_i16( &mut self ) -> Result< i16, C::Error > {
        if self.can_read_at_least( 2 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 2] = unsafe { MaybeUninit::uninit().assume_init() };
        self.read_bytes( &mut slice )?;
        Ok( self.context().endianness().read_i16( &slice ) )
    }

    #[inline(always)]
    fn peek_i16( &mut self ) -> Result< i16, C::Error > {
        if self.can_read_at_least( 2 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 2] = unsafe { MaybeUninit::uninit().assume_init() };
        self.peek_bytes( &mut slice )?;
        Ok( self.context().endianness().read_i16( &slice ) )
    }

    #[inline(always)]
    fn read_u32( &mut self ) -> Result< u32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 4] = unsafe { MaybeUninit::uninit().assume_init() };
        self.read_bytes( &mut slice )?;
        Ok( self.context().endianness().read_u32( &slice ) )
    }

    #[inline(always)]
    fn peek_u32( &mut self ) -> Result< u32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 4] = unsafe { MaybeUninit::uninit().assume_init() };
        self.peek_bytes( &mut slice )?;
        Ok( self.context().endianness().read_u32( &slice ) )
    }

    #[inline(always)]
    fn read_i32( &mut self ) -> Result< i32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 4] = unsafe { MaybeUninit::uninit().assume_init() };
        self.read_bytes( &mut slice )?;
        Ok( self.context().endianness().read_i32( &slice ) )
    }

    #[inline(always)]
    fn peek_i32( &mut self ) -> Result< i32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 4] = unsafe { MaybeUninit::uninit().assume_init() };
        self.peek_bytes( &mut slice )?;
        Ok( self.context().endianness().read_i32( &slice ) )
    }

    #[inline(always)]
    fn read_u64( &mut self ) -> Result< u64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 8] = unsafe { MaybeUninit::uninit().assume_init() };
        self.read_bytes( &mut slice )?;
        Ok( self.context().endianness().read_u64( &slice ) )
    }

    #[inline(always)]
    fn peek_u64( &mut self ) -> Result< u64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 8] = unsafe { MaybeUninit::uninit().assume_init() };
        self.peek_bytes( &mut slice )?;
        Ok( self.context().endianness().read_u64( &slice ) )
    }

    #[inline(always)]
    fn read_i64( &mut self ) -> Result< i64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 8] = unsafe { MaybeUninit::uninit().assume_init() };
        self.read_bytes( &mut slice )?;
        Ok( self.context().endianness().read_i64( &slice ) )
    }

    #[inline(always)]
    fn peek_i64( &mut self ) -> Result< i64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 8] = unsafe { MaybeUninit::uninit().assume_init() };
        self.peek_bytes( &mut slice )?;
        Ok( self.context().endianness().read_i64( &slice ) )
    }

    #[inline(always)]
    fn read_f32( &mut self ) -> Result< f32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 4] = unsafe { MaybeUninit::uninit().assume_init() };
        self.read_bytes( &mut slice )?;
        Ok( self.context().endianness().read_f32( &slice ) )
    }

    #[inline(always)]
    fn peek_f32( &mut self ) -> Result< f32, C::Error > {
        if self.can_read_at_least( 4 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 4] = unsafe { MaybeUninit::uninit().assume_init() };
        self.peek_bytes( &mut slice )?;
        Ok( self.context().endianness().read_f32( &slice ) )
    }

    #[inline(always)]
    fn read_f64( &mut self ) -> Result< f64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 8] = unsafe { MaybeUninit::uninit().assume_init() };
        self.read_bytes( &mut slice )?;
        Ok( self.context().endianness().read_f64( &slice ) )
    }

    #[inline(always)]
    fn peek_f64( &mut self ) -> Result< f64, C::Error > {
        if self.can_read_at_least( 8 ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        let mut slice: [u8; 8] = unsafe { MaybeUninit::uninit().assume_init() };
        self.peek_bytes( &mut slice )?;
        Ok( self.context().endianness().read_f64( &slice ) )
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

        if T::speedy_is_primitive() {
            let mut vec = Vec::with_capacity( length );
            unsafe {
                vec.set_len( length );
                self.read_bytes( T::speedy_slice_as_bytes_mut( &mut vec ) )?;
            }
            T::speedy_convert_slice_endianness( self.endianness(), &mut vec );
            Ok( vec )
        } else {
            let mut vec: Vec< T > = Vec::with_capacity( length );
            for _ in 0..length {
                let value = self.read_value()?;

                // If we don't do this then for some reason LLVM has trouble
                // eliding the Vec's realloc.
                let length = vec.len();
                unsafe {
                    vec.set_len( length + 1 );
                    std::ptr::write( vec.as_mut_ptr().offset( length as isize ), value );
                }
            }

            Ok( vec )
        }
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
                        vec.set_len( length );
                        std::ptr::copy_nonoverlapping( bytes.as_ptr(), vec.as_mut_ptr() as *mut u8, bytes.len() );
                    }
                    return Ok( Cow::Owned( vec ) );
                }
            }
        }

        Ok( Cow::Owned( self.read_vec( length )? ) )
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
        (0..length).into_iter().map( |_| self.read_value::< T >() ).collect()
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
