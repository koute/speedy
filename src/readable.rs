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
    unsafe fn read_bytes_into_ptr( &mut self, output: *mut u8, length: usize ) -> Result< (), C::Error > {
        if self.can_read_at_least( length ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        unsafe { 
            std::ptr::copy_nonoverlapping( self.ptr, output, length );
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
    unsafe fn peek_bytes_into_ptr( &mut self, output: *mut u8, length: usize ) -> Result< (), C::Error > {
        if self.can_read_at_least( length ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        unsafe {
            std::ptr::copy_nonoverlapping( self.ptr, output, length );
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
    fn read_bytes_borrowed_from_reader( &mut self, length: usize ) -> Option< Result< &[u8], C::Error > > {
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
    fn read_bytes_borrowed_until_eof( &mut self ) -> Option< &'a [u8] > {
        let length = self.end as usize - self.ptr as usize;
        let slice;
        unsafe {
            slice = std::slice::from_raw_parts( self.ptr, length );
            self.ptr = self.ptr.add( length );
        }

        Some( slice )
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

struct CopyingBufferReader< 'ctx, 'a, C > where C: Context {
    context: &'ctx mut C,
    ptr: *const u8,
    end: *const u8,
    phantom: PhantomData< &'a [u8] >
}

impl< 'ctx, 'a, C > CopyingBufferReader< 'ctx, 'a, C > where C: Context {
    #[inline]
    fn new( context: &'ctx mut C, buffer: &'a [u8] ) -> Self {
        CopyingBufferReader {
            context,
            ptr: buffer.as_ptr(),
            end: unsafe { buffer.as_ptr().add( buffer.len() ) },
            phantom: PhantomData
        }
    }
}

impl< 'ctx, 'r, 'a, C: Context > Reader< 'r, C > for CopyingBufferReader< 'ctx, 'a, C > {
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
    unsafe fn read_bytes_into_ptr( &mut self, output: *mut u8, length: usize ) -> Result< (), C::Error > {
        if self.can_read_at_least( length ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        unsafe {
            std::ptr::copy_nonoverlapping( self.ptr, output, length );
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
    unsafe fn peek_bytes_into_ptr( &mut self, output: *mut u8, length: usize ) -> Result< (), C::Error > {
        if self.can_read_at_least( length ) == Some( false ) {
            return Err( error_end_of_input() );
        }

        unsafe {
            std::ptr::copy_nonoverlapping( self.ptr, output, length );
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
    fn read_bytes_borrowed_from_reader( &mut self, length: usize ) -> Option< Result< &[u8], C::Error > > {
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
        self.context
    }

    #[inline(always)]
    fn context_mut( &mut self ) -> &mut C {
        self.context
    }
}

struct StreamReader< C: Context, S: Read > {
    context: C,
    reader: S,
    buffer: CircularBuffer,
    is_buffering: bool
}

impl<C, S > StreamReader< C, S > where C: Context, S: Read {
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

        if !self.buffer.len() > 0 {
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

    /// Deserializes from a given buffer.
    ///
    /// This performs zero-copy deserialization when possible.
    #[inline]
    fn read_from_buffer( buffer: &'a [u8] ) -> Result< Self, C::Error > where Self: DefaultContext< Context = C >, C: Default {
        Self::read_from_buffer_with_ctx( Default::default(), buffer )
    }

    /// Deserializes from a given buffer while also returning the amount of bytes consumed.
    ///
    /// This performs zero-copy deserialization when possible.
    #[inline]
    fn read_with_length_from_buffer( buffer: &'a [u8] ) -> (Result< Self, C::Error >, usize) where Self: DefaultContext< Context = C >, C: Default {
        Self::read_with_length_from_buffer_with_ctx( Default::default(), buffer )
    }

    /// Deserializes from a given buffer.
    ///
    /// This never performs zero-copy deserialization.
    #[inline]
    fn read_from_buffer_copying_data( buffer: &[u8] ) -> Result< Self, C::Error > where Self: DefaultContext< Context = C >, C: Default {
        Self::read_from_buffer_copying_data_with_ctx( Default::default(), buffer )
    }

    /// Deserializes from a given buffer while also returning the amount of bytes consumed.
    ///
    /// This never performs zero-copy deserialization.
    #[inline]
    fn read_with_length_from_buffer_copying_data( buffer: &[u8] ) -> (Result< Self, C::Error >, usize) where Self: DefaultContext< Context = C >, C: Default {
        Self::read_with_length_from_buffer_copying_data_with_ctx( Default::default(), buffer )
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
    fn read_from_buffer_copying_data_with_ctx( context: C, buffer: &[u8] ) -> Result< Self, C::Error > {
        Self::read_with_length_from_buffer_copying_data_with_ctx( context, buffer ).0
    }

    #[inline]
    fn read_with_length_from_buffer_copying_data_with_ctx( mut context: C, buffer: &[u8] ) -> (Result< Self, C::Error >, usize) {
        Self::read_with_length_from_buffer_copying_data_with_ctx_mut( &mut context, buffer )
    }

    #[inline]
    fn read_with_length_from_buffer_copying_data_with_ctx_mut( context: &mut C, buffer: &[u8] ) -> (Result< Self, C::Error >, usize) {
        let bytes_needed = Self::minimum_bytes_needed();
        let buffer_length = buffer.len();
        if buffer_length < bytes_needed {
            return (Err( error_input_buffer_is_too_small( buffer_length, bytes_needed ) ), 0);
        }

        let mut reader = CopyingBufferReader::new( context, buffer );
        let value = Self::read_from( &mut reader );
        let bytes_read = reader.ptr as usize - buffer.as_ptr() as usize;
        (value, bytes_read)
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

        #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
        {
            Self::read_from_stream_buffered_with_ctx( context, stream )
        }

        #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
        {
            use std::os::unix::io::AsRawFd;

            // Define our own bindings to avoid extra dependencies.
            extern "C" {
                fn mmap(
                    addr: *mut std::ffi::c_void,
                    len: usize,
                    prot: i32,
                    flags: i32,
                    fd: i32,
                    offset: i64
                ) -> *mut std::ffi::c_void;

                fn madvise(
                    addr: *mut std::ffi::c_void,
                    len: usize,
                    advice: i32
                ) -> i32;

                fn munmap(
                    addr: *mut std::ffi::c_void,
                    len: usize
                ) -> i32;
            }

            const MAP_PRIVATE: i32 = 0x0002;
            const PROT_READ: i32 = 1;
            const MAP_FAILED: *mut std::ffi::c_void = !0 as *mut std::ffi::c_void;
            const MADV_SEQUENTIAL: i32 = 2;
            const MADV_WILLNEED: i32 = 3;
            static EMPTY: &[u8] = &[];

            struct Mmap( *mut std::ffi::c_void, usize );
            impl Mmap {
                fn open( fp: &std::fs::File ) -> Result< Self, Error > {
                    let size = fp.metadata().map_err( Error::from_io_error )?.len();
                    if size > std::usize::MAX as u64 {
                        return Err( crate::error::error_too_big_usize_for_this_architecture() );
                    }

                    if size == 0 {
                        return Ok( Mmap( EMPTY.as_ptr() as _, 0 ) );
                    }

                    let size = size as usize;
                    let pointer = unsafe { mmap( std::ptr::null_mut(), size, PROT_READ, MAP_PRIVATE, fp.as_raw_fd(), 0 ) };
                    if pointer == MAP_FAILED {
                        Err( Error::from_io_error( std::io::Error::last_os_error() ) )
                    } else {
                        Ok( Mmap( pointer, size ) )
                    }
                }

                unsafe fn madvise( &mut self, advice: i32 ) -> Result< (), Error > {
                    if self.1 == 0 {
                        return Ok(());
                    }

                    if unsafe { madvise( self.0, self.1, advice ) } < 0 {
                        Err( Error::from_io_error( std::io::Error::last_os_error() ) )
                    } else {
                        Ok(())
                    }
                }
            }

            impl std::ops::Deref for Mmap {
                type Target = [u8];
                #[inline]
                fn deref( &self ) -> &Self::Target {
                    unsafe {
                        std::slice::from_raw_parts( self.0.cast::< u8 >(), self.1 )
                    }
                }
            }

            impl Drop for Mmap {
                fn drop( &mut self ) {
                    if self.1 != 0 {
                        unsafe {
                            munmap( self.0, self.1 );
                        }
                    }
                }
            }

            let mut mmap = Mmap::open( &stream )?;
            unsafe {
                mmap.madvise( MADV_SEQUENTIAL )?;
                mmap.madvise( MADV_WILLNEED )?;
            }

            Self::read_from_buffer_copying_data_with_ctx( context, &mmap )
        }
    }

    // Since specialization is not stable yet we do it this way.
    #[doc(hidden)]
    #[inline(always)]
    fn speedy_is_primitive() -> bool {
        false
    }

    #[doc(hidden)]
    #[inline]
    unsafe fn speedy_slice_from_bytes( _: &[u8] ) -> &[Self] {
        panic!();
    }

    #[doc(hidden)]
    #[inline]
    unsafe fn speedy_flip_endianness( _: *mut Self ) {
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
    let value: &[f64] = &[2.0, 123.0];
    let data = unsafe {
        std::slice::from_raw_parts( value.as_ptr() as *const u8, 16 )
    };

    let mut ctx = crate::LittleEndian {};

    macro_rules! test {
        ($peek:ident, $read:ident) => {
            let mut reader = CopyingBufferReader::new( &mut ctx, data );
            let value = reader.$peek().unwrap();
            for _ in 0..8 {
                assert_eq!( value, reader.$peek().unwrap() );
            }
            assert_eq!( value, reader.$read().unwrap() );
        }
    }

    test!( peek_f64, read_f64 );
    test!( peek_f32, read_f32 );
    test!( peek_u128, read_u128 );
    test!( peek_u64, read_u64 );
    test!( peek_u32, read_u32 );
    test!( peek_u16, read_u16 );
    test!( peek_u8, read_u8 );
    test!( peek_i128, read_i128 );
    test!( peek_i64, read_i64 );
    test!( peek_i32, read_i32 );
    test!( peek_i16, read_i16 );
    test!( peek_i8, read_i8 );
    test!( peek_u64_varint, read_u64_varint );

    let mut reader = CopyingBufferReader::new( &mut ctx, data );
    reader.peek_u8().unwrap();
    assert_eq!( reader.read_f64().unwrap(), 2.0 );
}
