use {
    chrono::{
        DateTime,
        TimeZone,
        Utc
    },
    crate::{
        Context,
        Readable,
        Reader,
        Writable,
        Writer
    }
};

impl< 'a, C > Readable< 'a, C > for DateTime< Utc >
    where C: Context
{
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let seconds = reader.read_i64()?;
        let subsec_nanos = reader.read_u32()?;
        Ok( Utc.timestamp_opt( seconds, subsec_nanos ).unwrap() )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        12
    }
}


impl< C > Writable< C > for DateTime< Utc >
    where C: Context
{
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        writer.write_i64( self.timestamp() )?;
        writer.write_u32( self.timestamp_subsec_nanos() )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Ok( 12 )
    }
}
