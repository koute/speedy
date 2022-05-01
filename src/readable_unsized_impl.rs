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

impl< 'a, C: Context, T > Readable< 'a, C > for &'a [T] where T: crate::utils::ZeroCopyable< T > {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        if std::mem::size_of::< T >() != 1 && reader.endianness().conversion_necessary() {
            return Err( crate::error::error_endianness_mismatch() );
        }

        let length = crate::private::read_length( reader )?;
        let bytelength = length.checked_mul( std::mem::size_of::< T >() ).ok_or_else( crate::error::error_out_of_range_length )?;
        let bytes = reader.read_bytes_borrowed( bytelength ).ok_or_else( crate::error::error_unsized )??;
        let slice = unsafe {
            std::slice::from_raw_parts( bytes.as_ptr() as *const T, length )
        };
        Ok( slice )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}
