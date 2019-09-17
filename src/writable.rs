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

struct WritingCollector< C: Context, T: Write > {
    context: C,
    writer: T
}

impl< 'a, C: Context, T: Write > Writer< 'a, C > for WritingCollector< C, T > {
    #[inline]
    fn write_bytes( &mut self, slice: &'a [u8] ) -> io::Result< () > {
        self.writer.write_all( slice )
    }

    #[inline]
    fn write_owned_bytes( &mut self, vec: Vec< u8 > ) -> io::Result< () > {
        self.writer.write_all( &vec )
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

impl< 'a, C: Context > Writer< 'a, C > for SizeCalculatorCollector {
    #[inline]
    fn write_bytes( &mut self, slice: &'a [u8] ) -> io::Result< () > {
        self.size += slice.len();
        Ok(())
    }

    #[inline]
    fn write_owned_bytes( &mut self, vec: Vec< u8 > ) -> io::Result< () > {
        self.size += vec.len();
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
    fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () >;

    #[inline]
    fn write_to_buffer( &self, context: C, buffer: &mut [u8] ) -> io::Result< () > {
        let mut cursor = Cursor::new( buffer );
        self.write_to_stream( context, &mut cursor )
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

        self.write_to( &mut writer ).unwrap();
        writer.size
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
