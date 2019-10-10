use std::io::{
    self,
    Write,
    Cursor
};

use std::fs::File;
use std::path::Path;

use byteorder::WriteBytesExt;

use crate::writer::Writer;
use crate::context::Context;
use crate::endianness::Endianness;

#[inline(never)]
#[cold]
fn eof() -> io::Error {
    io::Error::new( io::ErrorKind::UnexpectedEof, "unexpected end of output buffer" )
}

struct BufferCollector< 'a, C: Context > {
    context: C,
    buffer: &'a mut [u8],
    position: usize
}

impl< 'a, C: Context > Writer< C > for BufferCollector< 'a, C > {
    #[inline]
    fn write_bytes( &mut self, slice: &[u8] ) -> io::Result< () > {
        let buffer = self.buffer.get_mut( self.position..self.position + slice.len() ).ok_or_else( eof )?;
        buffer.copy_from_slice( slice );
        self.position += slice.len();
        Ok(())
    }

    #[inline]
    fn write_u8( &mut self, value: u8 ) -> io::Result< () > {
        if self.position >= self.buffer.len() {
            return Err( eof() );
        }

        self.buffer[ self.position ] = value;
        self.position += 1;
        Ok(())
    }

    #[inline]
    fn write_u16( &mut self, mut value: u16 ) -> io::Result< () > {
        let buffer = self.buffer.get_mut( self.position..self.position + 2 ).ok_or_else( eof )?;
        self.context.endianness().swap_u16( &mut value );
        let value = unsafe { std::slice::from_raw_parts( &value as *const u16 as *const u8, 2 ) };
        buffer.copy_from_slice( value );
        self.position += 2;
        Ok(())
    }

    #[inline]
    fn write_u32( &mut self, mut value: u32 ) -> io::Result< () > {
        let buffer = self.buffer.get_mut( self.position..self.position + 4 ).ok_or_else( eof )?;
        self.context.endianness().swap_u32( &mut value );
        let value = unsafe { std::slice::from_raw_parts( &value as *const u32 as *const u8, 4 ) };
        buffer.copy_from_slice( value );
        self.position += 4;
        Ok(())
    }

    #[inline]
    fn write_u64( &mut self, mut value: u64 ) -> io::Result< () > {
        let buffer = self.buffer.get_mut( self.position..self.position + 8 ).ok_or_else( eof )?;
        self.context.endianness().swap_u64( &mut value );
        let value = unsafe { std::slice::from_raw_parts( &value as *const u64 as *const u8, 8 ) };
        buffer.copy_from_slice( value );
        self.position += 8;
        Ok(())
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

struct WritingCollector< C: Context, T: Write > {
    context: C,
    writer: T
}

impl< C: Context, T: Write > Writer< C > for WritingCollector< C, T > {
    #[inline]
    fn write_bytes( &mut self, slice: &[u8] ) -> io::Result< () > {
        self.writer.write_all( slice )
    }

    #[inline]
    fn write_u8( &mut self, value: u8 ) -> io::Result< () > {
        self.writer.write_u8( value )
    }

    #[inline]
    fn write_u16( &mut self, value: u16 ) -> io::Result< () > {
        self.context.endianness().write_to_stream_u16( &mut self.writer, value )
    }

    #[inline]
    fn write_u32( &mut self, value: u32 ) -> io::Result< () > {
        self.context.endianness().write_to_stream_u32( &mut self.writer, value )
    }

    #[inline]
    fn write_u64( &mut self, value: u64 ) -> io::Result< () > {
        self.context.endianness().write_to_stream_u64( &mut self.writer, value )
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
    fn write_bytes( &mut self, slice: &[u8] ) -> io::Result< () > {
        self.size += slice.len();
        Ok(())
    }

    #[inline]
    fn write_u8( &mut self, _: u8 ) -> io::Result< () > {
        self.size += 1;
        Ok(())
    }

    #[inline]
    fn write_u16( &mut self, _: u16 ) -> io::Result< () > {
        self.size += 2;
        Ok(())
    }

    #[inline]
    fn write_u32( &mut self, _: u32 ) -> io::Result< () > {
        self.size += 4;
        Ok(())
    }

    #[inline]
    fn write_u64( &mut self, _: u64 ) -> io::Result< () > {
        self.size += 8;
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
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> io::Result< () >;

    #[inline]
    fn write_to_buffer( &self, context: C, buffer: &mut [u8] ) -> io::Result< () > {
        let buffer = buffer.get_mut( 0..self.bytes_needed() ).ok_or_else( eof )?;
        let mut writer = BufferCollector {
            context,
            buffer,
            position: 0
        };

        self.write_to( &mut writer )?;
        Ok(())
    }

    #[inline]
    fn write_to_vec( &self, context: C ) -> io::Result< Vec< u8 > > {
        let mut vec = Vec::with_capacity( self.bytes_needed() );
        self.write_to_stream( context, &mut vec )?;
        Ok( vec )
    }

    #[inline]
    fn write_to_stream< S: Write >( &self, context: C, stream: S ) -> io::Result< () > {
        let mut writer = WritingCollector {
            context,
            writer: stream
        };

        self.write_to( &mut writer )
    }

    #[inline]
    fn write_to_file( &self, context: C, path: impl AsRef< Path > ) -> io::Result< () > {
        let stream = io::BufWriter::new( File::create( path )? );
        self.write_to_stream( context, stream )
    }

    #[inline]
    fn bytes_needed( &self ) -> usize {
        let mut writer = SizeCalculatorCollector {
            size: 0
        };

        if let Err( _ ) = self.write_to( &mut writer ) {
            0
        } else {
            writer.size
        }
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
