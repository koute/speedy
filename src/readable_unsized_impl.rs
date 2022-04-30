use {
    crate::{
        Readable,
        Reader,
        Context
    }
};

impl< 'a, C: Context > Readable< 'a, C > for &'a str {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let length = crate::private::read_length( reader )?;
        let bytes = reader.read_bytes_borrowed( length ).ok_or_else( crate::error::error_unsized )??;
        let value = std::str::from_utf8( bytes ).map_err( crate::error::error_invalid_str_utf8 )?;
        Ok( value )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        <String as Readable< 'a, C >>::minimum_bytes_needed()
    }
}

impl< 'a, C: Context > Readable< 'a, C > for &'a [u8] {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let length = crate::private::read_length( reader )?;
        let bytes = reader.read_bytes_borrowed( length ).ok_or_else( crate::error::error_unsized )??;
        Ok( bytes )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}
