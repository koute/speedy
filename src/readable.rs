use std::io::{
    self,
    Read
};

use std::fs::File;
use std::path::Path;
use std::marker::PhantomData;

use crate::reader::Reader;
use crate::context::{Context, DefaultContext};
use crate::endianness::Endianness;
use crate::Error;

use crate::error::{
    error_end_of_input,
    error_input_buffer_is_too_small
};

struct BufferReader< 'a, C > where C: Context {
    context: C,
    ptr: *const u8,
    end: *const u8,
    phantom: PhantomData< &'a [u8] >
}

impl< 'a, C: Context > Reader< 'a, C > for BufferReader< 'a, C > {
    #[inline(always)]
    fn read_bytes( &mut self, output: &mut [u8] ) -> Result< (), C::Error > {
        let length = output.len();
        if self.can_read_at_least( length ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        unsafe {
            std::ptr::copy_nonoverlapping( self.ptr, output.as_mut_ptr(), length );
            self.ptr = self.ptr.add( length );
        }

        Ok(())
    }

    #[inline(always)]
    fn skip_bytes( &mut self, length: usize ) -> Result< (), C::Error > {
        if self.can_read_at_least( length ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        unsafe {
            self.ptr = self.ptr.add( length );
        }
        Ok(())
    }

    #[inline(always)]
    fn read_bytes_borrowed( &mut self, length: usize ) -> Option< Result< &'a [u8], C::Error > > {
        if self.can_read_at_least( length ) == Some( false ) {
            return Some( Err( error_end_of_input() ) );
        }

        let slice;
        unsafe {
            slice = std::slice::from_raw_parts( self.ptr, length );
            self.ptr = self.ptr.add( length );
        }

        Some( Ok( slice ) )
    }

    #[inline(always)]
    fn can_read_at_least( &self, size: usize ) -> Option< bool > {
        Some( (self.end as usize - self.ptr as usize) >= size )
    }

    #[inline(always)]
    fn context( &self ) -> &C {
        &self.context
    }

    #[inline(always)]
    fn context_mut( &mut self ) -> &mut C {
        &mut self.context
    }
}

struct CopyingBufferReader< 'a, C > where C: Context {
    context: C,
    ptr: *const u8,
    end: *const u8,
    phantom: PhantomData< &'a [u8] >
}

impl< 'r, 'a, C: Context > Reader< 'r, C > for CopyingBufferReader< 'a, C > {
    #[inline(always)]
    fn read_bytes( &mut self, output: &mut [u8] ) -> Result< (), C::Error > {
        let length = output.len();
        if self.can_read_at_least( length ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        unsafe {
            std::ptr::copy_nonoverlapping( self.ptr, output.as_mut_ptr(), length );
            self.ptr = self.ptr.add( length );
        }

        Ok(())
    }

    #[inline(always)]
    fn skip_bytes( &mut self, length: usize ) -> Result< (), C::Error > {
        if self.can_read_at_least( length ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        unsafe {
            self.ptr = self.ptr.add( length );
        }
        Ok(())
    }

    #[inline(always)]
    fn can_read_at_least( &self, size: usize ) -> Option< bool > {
        Some( (self.end as usize - self.ptr as usize) >= size )
    }

    #[inline(always)]
    fn context( &self ) -> &C {
        &self.context
    }

    #[inline(always)]
    fn context_mut( &mut self ) -> &mut C {
        &mut self.context
    }
}

struct StreamReader< C: Context, S: Read > {
    context: C,
    reader: S
}

impl< 'a, C: Context, S: Read > Reader< 'a, C > for StreamReader< C, S > {
    #[inline(always)]
    fn read_bytes( &mut self, output: &mut [u8] ) -> Result< (), C::Error > {
        self.reader.read_exact( output ).map_err( |error| {
            let error = Error::from_io_error( error );
            <C::Error as From< Error >>::from( error )
        })
    }

    #[inline(always)]
    fn context( &self ) -> &C {
        &self.context
    }

    #[inline(always)]
    fn context_mut( &mut self ) -> &mut C {
        &mut self.context
    }
}

impl< C: Context, S: Read > StreamReader< C, S > {
    #[inline]
    fn deserialize< 'a, T: Readable< 'a, C > >( context: C, reader: S ) -> Result< T, C::Error > {
        let mut reader = StreamReader { context, reader };
        T::read_from( &mut reader )
    }
}

pub trait Readable< 'a, C: Context >: Sized {
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error >;

    #[inline]
    fn minimum_bytes_needed() -> usize {
        0
    }

    #[inline]
    fn read_from_buffer( buffer: &'a [u8] ) -> Result< Self, C::Error > where Self: DefaultContext< Context = C >, C: Default {
        Self::read_from_buffer_with_ctx( Default::default(), buffer )
    }

    #[inline]
    fn read_from_buffer_owned( buffer: &[u8] ) -> Result< Self, C::Error > where Self: DefaultContext< Context = C >, C: Default {
        Self::read_from_buffer_owned_with_ctx( Default::default(), buffer )
    }

    #[inline]
    fn read_from_stream( stream: impl Read ) -> Result< Self, C::Error > where Self: DefaultContext< Context = C >, C: Default {
        Self::read_from_stream_with_ctx( Default::default(), stream )
    }

    #[inline]
    fn read_from_file( path: impl AsRef< Path > ) -> Result< Self, C::Error > where Self: DefaultContext< Context = C >, C: Default {
        Self::read_from_file_with_ctx( Default::default(), path )
    }

    #[inline]
    fn read_from_buffer_with_ctx( context: C, buffer: &'a [u8] ) -> Result< Self, C::Error > {
        let bytes_needed = Self::minimum_bytes_needed();
        let buffer_length = buffer.len();
        if buffer_length < bytes_needed {
            return Err( error_input_buffer_is_too_small( buffer_length, bytes_needed ) );
        }

        let mut reader = BufferReader {
            context,
            ptr: buffer.as_ptr(),
            end: unsafe { buffer.as_ptr().add( buffer_length ) },
            phantom: PhantomData
        };

        Self::read_from( &mut reader )
    }

    #[inline]
    fn read_from_buffer_owned_with_ctx( context: C, buffer: &[u8] ) -> Result< Self, C::Error > {
        let bytes_needed = Self::minimum_bytes_needed();
        let buffer_length = buffer.len();
        if buffer_length < bytes_needed {
            return Err( error_input_buffer_is_too_small( buffer_length, bytes_needed ) );
        }

        let mut reader = CopyingBufferReader {
            context,
            ptr: buffer.as_ptr(),
            end: unsafe { buffer.as_ptr().add( buffer_length ) },
            phantom: PhantomData
        };

        Self::read_from( &mut reader )
    }

    #[inline]
    fn read_from_stream_with_ctx< S: Read >( context: C, stream: S ) -> Result< Self, C::Error > {
        StreamReader::deserialize( context, stream )
    }

    #[inline]
    fn read_from_file_with_ctx( context: C, path: impl AsRef< Path > ) -> Result< Self, C::Error > {
        let stream = File::open( path ).map_err( |error| {
            let error = Error::from_io_error( error );
            <C::Error as From< Error >>::from( error )
        })?;

        let stream = io::BufReader::new( stream );
        Self::read_from_stream_with_ctx( context, stream )
    }

    // Since specialization is not stable yet we do it this way.
    #[doc(hidden)]
    #[inline(always)]
    fn speedy_is_primitive() -> bool {
        false
    }

    #[doc(hidden)]
    #[inline]
    unsafe fn speedy_slice_as_bytes_mut( _: &mut [Self] ) -> &mut [u8] {
        panic!();
    }

    #[doc(hidden)]
    #[inline]
    unsafe fn speedy_slice_from_bytes( _: &[u8] ) -> &[Self] {
        panic!();
    }

    #[doc(hidden)]
    #[inline]
    fn speedy_convert_slice_endianness( _: Endianness, _: &mut [Self] ) {
        panic!()
    }
}
