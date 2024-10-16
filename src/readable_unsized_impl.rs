use {
    crate::{
        Readable,
        Reader,
        Context
    }
};


#[cfg(feature = "alloc")]
use alloc::string::String;

impl< 'a, C: Context > Readable< 'a, C > for &'a str {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let length = crate::private::read_length( reader )?;
        let bytes = reader.read_bytes_borrowed( length ).ok_or_else( crate::error::error_unsized )??;
        let value = core::str::from_utf8( bytes ).map_err( crate::error::error_invalid_str_utf8 )?;
        Ok( value )
    }

    #[cfg(all(not(feature = "std"), not(feature = "alloc")))]
    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }

    #[cfg(feature = "alloc")]
    #[inline]
    fn minimum_bytes_needed() -> usize {
        <String as Readable< 'a, C >>::minimum_bytes_needed()
    }
}

impl< 'a, C: Context, T > Readable< 'a, C > for &'a [T] where T: crate::utils::ZeroCopyable< C, T > {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        if core::mem::size_of::< T >() != 1 && reader.endianness().conversion_necessary() {
            return Err( crate::error::error_endianness_mismatch() );
        }

        let length = crate::private::read_length( reader )?;
        let bytelength = length.checked_mul( core::mem::size_of::< T >() ).ok_or_else( crate::error::error_out_of_range_length )?;
        let bytes = reader.read_bytes_borrowed( bytelength ).ok_or_else( crate::error::error_unsized )??;
        let slice = unsafe {
            core::slice::from_raw_parts( bytes.as_ptr() as *const T, length )
        };
        Ok( slice )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}

#[inline(always)]
unsafe fn cast_slice< T, const N: usize >( slice: &[u8] ) -> &[T; N] {
    unsafe {
        &*(slice.as_ptr() as *const [T; N])
    }
}

impl< 'a, C: Context, T, const N: usize > Readable< 'a, C > for &'a [T; N] where T: crate::utils::ZeroCopyable< C, T > {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        if core::mem::size_of::< T >() != 1 && reader.endianness().conversion_necessary() {
            return Err( crate::error::error_endianness_mismatch() );
        }

        let bytes = reader.read_bytes_borrowed( core::mem::size_of::< T >() * N ).ok_or_else( crate::error::error_unsized )??;
        let slice = unsafe { cast_slice( bytes ) };
        Ok( slice )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        core::mem::size_of::< T >() * N
    }
}
