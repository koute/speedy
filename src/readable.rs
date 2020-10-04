use std::io::{
    Read
};

use std::fs::File;
use std::path::Path;
use std::marker::PhantomData;

use crate::reader::Reader;
use crate::context::{Context, DefaultContext};
use crate::endianness::Endianness;
use crate::Error;
use crate::circular_buffer::CircularBuffer;

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

impl< 'a, C > BufferReader< 'a, C > where C: Context {
    #[inline]
    fn new( context: C, buffer: &'a [u8] ) -> Self {
        BufferReader {
            context,
            ptr: buffer.as_ptr(),
            end: unsafe { buffer.as_ptr().add( buffer.len() ) },
            phantom: PhantomData
        }
    }
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
    fn peek_bytes( &mut self, output: &mut [u8] ) -> Result< (), C::Error > {
        let length = output.len();
        if self.can_read_at_least( length ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        unsafe {
            std::ptr::copy_nonoverlapping( self.ptr, output.as_mut_ptr(), length );
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

impl< 'a, C > CopyingBufferReader< 'a, C > where C: Context {
    #[inline]
    fn new( context: C, buffer: &'a [u8] ) -> Self {
        CopyingBufferReader {
            context,
            ptr: buffer.as_ptr(),
            end: unsafe { buffer.as_ptr().add( buffer.len() ) },
            phantom: PhantomData
        }
    }
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
    fn peek_bytes( &mut self, output: &mut [u8] ) -> Result< (), C::Error > {
        let length = output.len();
        if self.can_read_at_least( length ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        unsafe {
            std::ptr::copy_nonoverlapping( self.ptr, output.as_mut_ptr(), length );
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
    reader: S,
    buffer: CircularBuffer,
    is_buffering: bool
}

impl< 'a, C, S > StreamReader< C, S > where C: Context, S: Read {
    #[inline(never)]
    fn read_bytes_slow( &mut self, mut output: &mut [u8] ) -> Result< (), C::Error > {
        if self.is_buffering && output.len() < self.buffer.capacity() {
            let reader = &mut self.reader;
            while self.buffer.len() < self.buffer.capacity() {
                let bytes_written = self.buffer.try_append_with( self.buffer.capacity() - self.buffer.len(), |chunk| {
                    reader.read( chunk )
                }).map_err( |error| {
                    let error = Error::from_io_error( error );
                    <C::Error as From< Error >>::from( error )
                })?;

                if bytes_written == 0 {
                    if self.buffer.len() < output.len() {
                        return Err( error_end_of_input() );
                    } else {
                        break;
                    }
                }

                if self.buffer.len() >= output.len() {
                    break;
                }
            }
        }

        if self.buffer.len() > 0 {
            let length = std::cmp::min( self.buffer.len(), output.len() );
            self.buffer.consume_into( &mut output[ ..length ] );
            output = &mut output[ length.. ];
        }

        if output.is_empty() {
            return Ok(());
        }

        self.reader.read_exact( output ).map_err( |error| {
            let error = Error::from_io_error( error );
            <C::Error as From< Error >>::from( error )
        })
    }
}

impl< 'a, C: Context, S: Read > Reader< 'a, C > for StreamReader< C, S > {
    #[inline(always)]
    fn read_bytes( &mut self, output: &mut [u8] ) -> Result< (), C::Error > {
        if self.buffer.len() >= output.len() {
            self.buffer.consume_into( output );
            return Ok(());
        }

        self.read_bytes_slow( output )
    }

    fn peek_bytes( &mut self, output: &mut [u8] ) -> Result< (), C::Error > {
        if output.len() > self.buffer.len() {
            let reader = &mut self.reader;
            while self.buffer.len() < output.len() {
                let mut chunk_size = output.len() - self.buffer.len();
                if self.is_buffering {
                    chunk_size = std::cmp::max( chunk_size, self.buffer.capacity() - self.buffer.len() );
                }

                let bytes_written = self.buffer.try_append_with( chunk_size, |chunk| {
                    reader.read( chunk )
                }).map_err( |error| {
                    let error = Error::from_io_error( error );
                    <C::Error as From< Error >>::from( error )
                })?;

                if bytes_written == 0 {
                    return Err( error_end_of_input() );
                }
            }
        }

        let (a, b) = self.buffer.as_slices_of_length( output.len() );
        output[ ..a.len() ].copy_from_slice( a );

        if let Some( b ) = b {
            output[ a.len().. ].copy_from_slice( b );
        }

        Ok(())
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
    fn deserialize< 'a, T: Readable< 'a, C > >( context: C, reader: S, is_buffering: bool ) -> Result< T, C::Error > {
        let capacity = if is_buffering {
            8 * 1024
        } else {
            0
        };

        let mut reader = StreamReader {
            context,
            reader,
            buffer: CircularBuffer::with_capacity( capacity ),
            is_buffering
        };

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
    fn read_with_length_from_buffer( buffer: &'a [u8] ) -> (Result< Self, C::Error >, usize) where Self: DefaultContext< Context = C >, C: Default {
        Self::read_with_length_from_buffer_with_ctx( Default::default(), buffer )
    }

    #[inline]
    fn read_from_buffer_owned( buffer: &[u8] ) -> Result< Self, C::Error > where Self: DefaultContext< Context = C >, C: Default {
        Self::read_from_buffer_owned_with_ctx( Default::default(), buffer )
    }

    /// Reads from a given stream without any buffering.
    ///
    /// This will only read what is necessary from the stream to deserialize
    /// a given type, but is going to be slow.
    ///
    /// Use [`read_from_stream_buffered`](Readable::read_from_stream_buffered) if you need
    /// to read from a stream and you don't care about not overreading.
    #[inline]
    fn read_from_stream_unbuffered( stream: impl Read ) -> Result< Self, C::Error > where Self: DefaultContext< Context = C >, C: Default {
        Self::read_from_stream_unbuffered_with_ctx( Default::default(), stream )
    }

    /// Reads from a given stream with internal buffering.
    ///
    /// This will read more data from the stream than is necessary to deserialize
    /// a given type, however it should be orders of magnitude faster than unbuffered streaming,
    /// especially when reading relatively complex objects.
    ///
    /// Use the slower [`read_from_stream_unbuffered`](Readable::read_from_stream_unbuffered) if you want
    /// to avoid overreading.
    #[inline]
    fn read_from_stream_buffered( stream: impl Read ) -> Result< Self, C::Error > where Self: DefaultContext< Context = C >, C: Default {
        Self::read_from_stream_buffered_with_ctx( Default::default(), stream )
    }

    #[inline]
    fn read_from_file( path: impl AsRef< Path > ) -> Result< Self, C::Error > where Self: DefaultContext< Context = C >, C: Default {
        Self::read_from_file_with_ctx( Default::default(), path )
    }

    #[inline]
    fn read_from_buffer_with_ctx( context: C, buffer: &'a [u8] ) -> Result< Self, C::Error > {
        Self::read_with_length_from_buffer_with_ctx( context, buffer ).0
    }

    #[inline]
    fn read_with_length_from_buffer_with_ctx( context: C, buffer: &'a [u8] ) -> (Result< Self, C::Error >, usize) {
        let bytes_needed = Self::minimum_bytes_needed();
        let buffer_length = buffer.len();
        if buffer_length < bytes_needed {
            return (Err( error_input_buffer_is_too_small( buffer_length, bytes_needed ) ), 0);
        }

        let mut reader = BufferReader::new( context, buffer );
        let value = Self::read_from( &mut reader );
        let bytes_read = reader.ptr as usize - buffer.as_ptr() as usize;
        (value, bytes_read)
    }

    #[inline]
    fn read_from_buffer_owned_with_ctx( context: C, buffer: &[u8] ) -> Result< Self, C::Error > {
        let bytes_needed = Self::minimum_bytes_needed();
        let buffer_length = buffer.len();
        if buffer_length < bytes_needed {
            return Err( error_input_buffer_is_too_small( buffer_length, bytes_needed ) );
        }

        let mut reader = CopyingBufferReader::new( context, buffer );
        Self::read_from( &mut reader )
    }

    #[inline]
    fn read_from_stream_unbuffered_with_ctx< S: Read >( context: C, stream: S ) -> Result< Self, C::Error > {
        StreamReader::deserialize( context, stream, false )
    }

    #[inline]
    fn read_from_stream_buffered_with_ctx< S: Read >( context: C, stream: S ) -> Result< Self, C::Error > {
        StreamReader::deserialize( context, stream, true )
    }

    #[inline]
    fn read_from_file_with_ctx( context: C, path: impl AsRef< Path > ) -> Result< Self, C::Error > {
        let stream = File::open( path ).map_err( |error| {
            let error = Error::from_io_error( error );
            <C::Error as From< Error >>::from( error )
        })?;

        Self::read_from_stream_buffered_with_ctx( context, stream )
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

#[test]
fn test_peek() {
    let value: f64 = 2.0;
    let data = unsafe {
        std::slice::from_raw_parts( (&value as *const f64) as *const u8, 8 )
    };

    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_f64().unwrap(), reader.read_f64().unwrap() );

    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_f32().unwrap(), reader.read_f32().unwrap() );

    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_u64().unwrap(), reader.read_u64().unwrap() );
    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_i64().unwrap(), reader.read_i64().unwrap() );
    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_u32().unwrap(), reader.read_u32().unwrap() );
    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_i32().unwrap(), reader.read_i32().unwrap() );
    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_u16().unwrap(), reader.read_u16().unwrap() );
    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_i16().unwrap(), reader.read_i16().unwrap() );
    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_u8().unwrap(), reader.read_u8().unwrap() );
    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_i8().unwrap(), reader.read_i8().unwrap() );

    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_u64_varint().unwrap(), reader.read_u64_varint().unwrap() );

    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    assert_eq!( reader.peek_u64().unwrap(), reader.peek_u64().unwrap() );

    let mut reader = CopyingBufferReader::new( crate::LittleEndian {}, data );
    reader.peek_u8().unwrap();
    assert_eq!( reader.read_f64().unwrap(), 2.0 );
}
