use std::io::{
    self,
    Read
};

use reader::Reader;
use context::Context;
use endianness::Endianness;

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
    fn read_from_buffer( context: C, mut buffer: &'a [u8] ) -> io::Result< Self > {
        StreamReader::deserialize( context, &mut buffer )
    }

    #[inline]
    fn read_from_buffer_owned( context: C, mut buffer: &[u8] ) -> io::Result< Self > {
        StreamReader::deserialize( context, &mut buffer )
    }

    #[inline]
    fn read_from_stream< S: Read >( context: C, stream: S ) -> io::Result< Self > {
        StreamReader::deserialize( context, stream )
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
    fn speedy_convert_slice_endianness( _: Endianness, _: &mut [Self] ) {
        panic!()
    }
}
