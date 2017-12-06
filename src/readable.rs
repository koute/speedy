use std::io::{
    self,
    Read,
    Cursor
};

use reader::Reader;
use context::Context;

struct DirectSyncReader< C: Context, S: Read > {
    context: C,
    reader: S
}

impl< C: Context, S: Read > Reader< C > for DirectSyncReader< C, S > {
    #[inline]
    fn read_bytes( &mut self, output: &mut [u8] ) -> io::Result< () > {
        self.reader.read_exact( output )
    }

    #[inline]
    fn context( &self ) -> &C {
        &self.context
    }
}

impl< C: Context, S: Read > DirectSyncReader< C, S > {
    #[inline]
    fn deserialize< T: Readable< C > >( context: C, reader: S ) -> io::Result< T > {
        let mut reader = DirectSyncReader { context, reader };
        T::read_from( &mut reader )
    }
}

pub trait Readable< C: Context >: Sized {
    fn read_from< R: Reader< C > >( reader: &mut R ) -> io::Result< Self >;

    #[inline]
    fn minimum_bytes_needed() -> usize {
        0
    }

    #[inline]
    fn read_from_buffer( context: C, buffer: &[u8] ) -> io::Result< Self > {
        DirectSyncReader::deserialize( context, Cursor::new( buffer ) )
    }

    #[inline]
    fn read_from_stream< S: Read >( context: C, stream: S ) -> io::Result< Self > {
        DirectSyncReader::deserialize( context, stream )
    }
}
