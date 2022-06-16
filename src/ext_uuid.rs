use std::convert::TryInto;

use {
    crate::{
        Context,
        Readable,
        Reader,
        Writable,
        Writer
    },
    uuid::Uuid
};


const UUID_SIZE: usize = 16;

impl< 'a, C > Readable< 'a, C > for Uuid
    where C: Context
{
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let buffer: Vec<u8> = reader.read_vec(UUID_SIZE)?;
        Ok(Uuid::from_bytes(
            buffer.try_into()
                .map_err(|_| crate::error::Error::custom( format!( "failed to read a uuid") ).into())?
            ))
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        UUID_SIZE
    }
}


impl< C > Writable< C > for Uuid
    where C: Context
{
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        writer.write_bytes(self.as_bytes())
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok(UUID_SIZE)
    }
}
