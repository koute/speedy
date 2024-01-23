use std::io::{
    self,
    Write
};

use std::fs::File;
use std::path::Path;

use crate::writer::Writer;
use crate::context::{Context, DefaultContext};
use crate::endianness::Endianness;
use crate::Error;

use crate::error::{
    error_end_of_output_buffer,
    error_output_buffer_is_too_small
};

struct BufferCollector< 'a, C: Context > {
    context: &'a mut C,
    buffer: &'a mut [u8],
    position: usize
}

impl< 'a, C: Context > Writer< C > for BufferCollector< 'a, C > {
    #[inline]
    fn write_bytes( &mut self, slice: &[u8] ) -> Result< (), C::Error > {
        let buffer = self.buffer.get_mut( self.position..self.position + slice.len() ).ok_or_else( error_end_of_output_buffer )?;
        buffer.copy_from_slice( slice );
        self.position += slice.len();
        Ok(())
    }

    #[inline]
    fn context( &self ) -> &C {
        self.context
    }

    #[inline]
    fn context_mut( &mut self ) -> &mut C {
        self.context
    }

    #[inline(always)]
    fn can_write_at_least( &self, size: usize ) -> Option< bool > {
        Some( self.buffer.get( self.position..self.position + size ).is_some() )
    }
}

struct WritingCollector< C: Context, T: Write > {
    context: C,
    writer: T
}

impl< C: Context, T: Write > Writer< C > for WritingCollector< C, T > {
    #[inline]
    fn write_bytes( &mut self, slice: &[u8] ) -> Result< (), C::Error > {
        self.writer.write_all( slice ).map_err( |error| {
            let error = Error::from_io_error( error );
            <C::Error as From< Error >>::from( error )
        })
    }

    #[inline]
    fn context( &self ) -> &C {
        &self.context
    }

    #[inline]
    fn context_mut( &mut self ) -> &mut C {
        &mut self.context
    }
}

struct SizeCalculatorCollector {
    size: usize
}

impl< C: Context > Writer< C > for SizeCalculatorCollector {
    #[inline]
    fn write_bytes( &mut self, slice: &[u8] ) -> Result< (), C::Error > {
        self.size += slice.len();
        Ok(())
    }

    #[inline]
    fn write_u8( &mut self, _: u8 ) -> Result< (), C::Error > {
        self.size += 1;
        Ok(())
    }

    #[inline]
    fn write_u16( &mut self, _: u16 ) -> Result< (), C::Error > {
        self.size += 2;
        Ok(())
    }

    #[inline]
    fn write_u32( &mut self, _: u32 ) -> Result< (), C::Error > {
        self.size += 4;
        Ok(())
    }

    #[inline]
    fn write_u64( &mut self, _: u64 ) -> Result< (), C::Error > {
        self.size += 8;
        Ok(())
    }

    #[inline]
    fn write_u128( &mut self, _: u128 ) -> Result< (), C::Error > {
        self.size += 16;
        Ok(())
    }

    #[inline]
    fn endianness( &self ) -> Endianness {
        Endianness::NATIVE
    }

    #[inline]
    fn context( &self ) -> &C {
        panic!();
    }

    #[inline]
    fn context_mut( &mut self ) -> &mut C {
        panic!();
    }
}

pub trait Writable< C: Context > {
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error >;

    #[inline]
    fn write_to_buffer( &self, buffer: &mut [u8] ) -> Result< (), C::Error > where Self: DefaultContext< Context = C >, C: Default {
        self.write_to_buffer_with_ctx( Default::default(), buffer )
    }

    fn write_to_vec( &self ) -> Result< Vec< u8 >, C::Error > where Self: DefaultContext< Context = C >, C: Default {
        self.write_to_vec_with_ctx( Default::default() )
    }

    #[inline]
    fn write_to_stream< S: Write >( &self, stream: S ) -> Result< (), C::Error > where Self: DefaultContext< Context = C >, C: Default {
        self.write_to_stream_with_ctx( Default::default(), stream )
    }

    #[inline]
    fn write_to_file( &self, path: impl AsRef< Path > ) -> Result< (), C::Error > where Self: DefaultContext< Context = C >, C: Default {
        self.write_to_file_with_ctx( Default::default(), path )
    }

    #[inline]
    fn write_to_buffer_with_ctx( &self, mut context: C, buffer: &mut [u8] ) -> Result< (), C::Error > {
        self.write_to_buffer_with_ctx_mut( &mut context, buffer )
    }

    #[inline]
    fn write_to_buffer_with_ctx_mut( &self, context: &mut C, buffer: &mut [u8] ) -> Result< (), C::Error > {
        let bytes_needed = self.bytes_needed()?;
        let buffer_length = buffer.len();
        let buffer = buffer.get_mut( 0..bytes_needed ).ok_or_else( || error_output_buffer_is_too_small( buffer_length, bytes_needed ) )?;
        let mut writer = BufferCollector {
            context,
            buffer,
            position: 0
        };

        self.write_to( &mut writer )?;
        Ok(())
    }

    #[inline]
    fn write_to_vec_with_ctx( &self, mut context: C ) -> Result< Vec< u8 >, C::Error > {
        self.write_to_vec_with_ctx_mut( &mut context )
    }

    #[inline]
    fn write_to_vec_with_ctx_mut( &self, context: &mut C ) -> Result< Vec< u8 >, C::Error > {
        let capacity = self.bytes_needed()?;
        let mut vec = Vec::with_capacity( capacity );
        unsafe {
            vec.set_len( capacity );
        }

        let mut writer = BufferCollector {
            context,
            buffer: vec.as_mut_slice(),
            position: 0
        };

        self.write_to( &mut writer )?;

        let position = writer.position;
        unsafe {
            vec.set_len( position );
        }

        debug_assert_eq!( position, capacity );
        Ok( vec )
    }

    #[inline]
    fn write_to_stream_with_ctx< S: Write >( &self, context: C, stream: S ) -> Result< (), C::Error > {
        let mut writer = WritingCollector {
            context,
            writer: stream
        };

        self.write_to( &mut writer )
    }

    #[inline]
    fn write_to_file_with_ctx( &self, context: C, path: impl AsRef< Path > ) -> Result< (), C::Error > {
        let stream = File::create( path ).map_err( |error| {
            let error = Error::from_io_error( error );
            <C::Error as From< Error >>::from( error )
        })?;
        let stream = io::BufWriter::new( stream );
        self.write_to_stream_with_ctx( context, stream )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        let mut writer = SizeCalculatorCollector {
            size: 0
        };

        self.write_to( &mut writer )?;
        Ok( writer.size )
    }

    // Since specialization is not stable yet we do it this way.
    #[doc(hidden)]
    #[inline]
    fn speedy_is_primitive() -> bool {
        false
    }

    #[doc(hidden)]
    #[inline]
    unsafe fn speedy_slice_as_bytes( _: &[Self] ) -> &[u8] where Self: Sized {
        panic!();
    }
}
