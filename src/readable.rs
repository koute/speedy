use std::io::{
    self,
    Read
};

use std::fs::File;
use std::path::Path;

use crate::reader::Reader;
use crate::context::Context;
use crate::endianness::Endianness;

#[inline(never)]
#[cold]
fn eof() -> io::Error {
    io::Error::new( io::ErrorKind::UnexpectedEof, "unexpected end of input buffer" )
}

struct BufferReader< 'a, C > where C: Context {
    context: C,
    position: usize,
    slice: &'a [u8]
}

impl< 'a, C: Context > Reader< 'a, C > for BufferReader< 'a, C > {
    #[inline(always)]
    fn read_bytes( &mut self, output: &mut [u8] ) -> io::Result< () > {
        let length = output.len();
        let bytes = self.read_bytes_borrowed( length ).unwrap()?;
        output.copy_from_slice( bytes );

        Ok(())
    }

    #[inline(always)]
    fn read_bytes_borrowed( &mut self, length: usize ) -> Option< io::Result< &'a [u8] > > {
        let position = self.position;
        self.position += length;

        let result = self.slice.get( position..position + length ).ok_or_else( eof );
        Some( result )
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
    fn read_bytes( &mut self, output: &mut [u8] ) -> io::Result< () > {
        self.reader.read_exact( output )
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
    fn deserialize< 'a, T: Readable< 'a, C > >( context: C, reader: S ) -> io::Result< T > {
        let mut reader = StreamReader { context, reader };
        T::read_from( &mut reader )
    }
}

pub trait Readable< 'a, C: Context >: Sized {
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> io::Result< Self >;

    #[inline]
    fn minimum_bytes_needed() -> usize {
        0
    }

    #[inline]
    fn read_from_buffer( context: C, buffer: &'a [u8] ) -> io::Result< Self > {
        if buffer.len() < Self::minimum_bytes_needed() {
            return Err( eof() );
        }

        let mut reader = BufferReader { context, position: 0, slice: buffer };
        Self::read_from( &mut reader )
    }

    #[inline]
    fn read_from_buffer_owned( context: C, mut buffer: &[u8] ) -> io::Result< Self > {
        StreamReader::deserialize( context, &mut buffer )
    }

    #[inline]
    fn read_from_stream< S: Read >( context: C, stream: S ) -> io::Result< Self > {
        StreamReader::deserialize( context, stream )
    }

    #[inline]
    fn read_from_file( context: C, path: impl AsRef< Path > ) -> io::Result< Self > {
        let stream = io::BufReader::new( File::open( path )? );
        Self::read_from_stream( context, stream )
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
