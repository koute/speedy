use {
    crate::{
        Context,
        Error,
        Readable,
        Reader,
        Writable,
        Writer,
        error::{
            error_expected_constant,
            error_invalid_string_utf8,
            error_invalid_str_utf8
        }
    },
    std::{
        borrow::{
            Cow
        }
    }
};

pub use crate::varint::VarInt64;
pub use crate::error::{
    ErrorKind,

    error_length_is_not_the_same_as_length_attribute,
    error_out_of_range_length,
    error_invalid_enum_variant,

    get_error_kind,
};

#[inline]
pub fn vec_to_string< E >( bytes: Vec< u8 > ) -> Result< String, E > where E: From< Error > {
    String::from_utf8( bytes ).map_err( error_invalid_string_utf8 )
}

#[inline]
pub fn cow_bytes_to_cow_str< E >( bytes: Cow< [u8] > ) -> Result< Cow< str >, E > where E: From< Error > {
    match bytes {
        Cow::Borrowed( bytes ) => {
            std::str::from_utf8( bytes )
                .map( Cow::Borrowed )
                .map_err( error_invalid_str_utf8 )
        },
        Cow::Owned( bytes ) => {
            String::from_utf8( bytes )
                .map( Cow::Owned )
                .map_err( error_invalid_string_utf8 )
        }
    }
}

#[inline]
pub fn write_length_u64_varint< C, W >( length: usize, writer: &mut W ) -> Result< (), C::Error >
    where C: Context,
          W: ?Sized + Writer< C >
{
    let length = VarInt64::from( length as u64 );
    length.write_to( writer )
}

#[inline]
pub fn write_length_u64< C, W >( length: usize, writer: &mut W ) -> Result< (), C::Error >
    where C: Context,
          W: ?Sized + Writer< C >
{
    writer.write_u64( length as u64 )
}

#[inline]
pub fn write_length_u32< C, W >( length: usize, writer: &mut W ) -> Result< (), C::Error >
    where C: Context,
          W: ?Sized + Writer< C >
{
    if length as u64 > std::u32::MAX as u64 {
         return Err( error_out_of_range_length() );
    }

    writer.write_u32( length as u32 )
}

#[inline]
pub fn write_length_u16< C, W >( length: usize, writer: &mut W ) -> Result< (), C::Error >
    where C: Context,
          W: ?Sized + Writer< C >
{
    if length as u64 > std::u16::MAX as u64 {
         return Err( error_out_of_range_length() );
    }

    writer.write_u16( length as u16 )
}

#[inline]
pub fn write_length_u8< C, W >( length: usize, writer: &mut W ) -> Result< (), C::Error >
    where C: Context,
          W: ?Sized + Writer< C >
{
    if length as u64 > std::u8::MAX as u64 {
         return Err( error_out_of_range_length() );
    }

    writer.write_u8( length as u8 )
}

#[inline]
pub fn write_length_u7< C, W >( length: usize, writer: &mut W ) -> Result< (), C::Error >
    where C: Context,
          W: ?Sized + Writer< C >
{
    if length > 0b01111111 {
         return Err( error_out_of_range_length() );
    }

    writer.write_u8( length as u8 )
}

#[inline]
pub fn write_length< C, W >( length: usize, writer: &mut W ) -> Result< (), C::Error >
    where C: Context,
          W: ?Sized + Writer< C >
{
    write_length_u32( length, writer )
}

#[inline]
pub fn read_length_u64_varint< 'a, C, R >( reader: &mut R ) -> Result< usize, C::Error >
    where C: Context,
          R: Reader< 'a, C >
{
    let length: u64 = VarInt64::read_from( reader )?.into();
    if length > std::usize::MAX as u64 {
        return Err( error_out_of_range_length() );
    }

    Ok( length as usize )
}

#[inline]
pub fn read_length_u64< 'a, C, R >( reader: &mut R ) -> Result< usize, C::Error >
    where C: Context,
          R: Reader< 'a, C >
{
    let length = reader.read_u64()?;
    if length > std::usize::MAX as u64 {
        return Err( error_out_of_range_length() );
    }

    Ok( length as usize )
}

#[inline]
pub fn read_length_u32< 'a, C, R >( reader: &mut R ) -> Result< usize, C::Error >
    where C: Context,
          R: Reader< 'a, C >
{
    reader.read_u32().map( |value| value as usize )
}

#[inline]
pub fn read_length_u16< 'a, C, R >( reader: &mut R ) -> Result< usize, C::Error >
    where C: Context,
          R: Reader< 'a, C >
{
    reader.read_u16().map( |value| value as usize )
}

#[inline]
pub fn read_length_u8< 'a, C, R >( reader: &mut R ) -> Result< usize, C::Error >
    where C: Context,
          R: Reader< 'a, C >
{
    reader.read_u8().map( |value| value as usize )
}

#[inline]
pub fn read_length_u7< 'a, C, R >( reader: &mut R ) -> Result< usize, C::Error >
    where C: Context,
          R: Reader< 'a, C >
{
    let length = reader.read_u8()?;
    if length > 0b01111111 {
        return Err( error_out_of_range_length() );
    }
    Ok( length as usize )
}

#[inline]
pub fn read_length< 'a, C, R >( reader: &mut R ) -> Result< usize, C::Error >
    where C: Context,
          R: Reader< 'a, C >
{
    read_length_u32( reader )
}

pub trait IntoLength {
    fn into_length( self ) -> usize;
}

impl IntoLength for usize {
    fn into_length( self ) -> usize { self }
}

impl IntoLength for u32 {
    fn into_length( self ) -> usize { self as usize }
}

impl IntoLength for u16 {
    fn into_length( self ) -> usize { self as usize }
}

impl IntoLength for u8 {
    fn into_length( self ) -> usize { self as usize }
}

impl< 'a, T > IntoLength for &'a T where T: IntoLength + Copy {
    fn into_length( self ) -> usize { (*self).into_length() }
}

impl< 'a, T > IntoLength for &'a mut T where T: IntoLength + Copy {
    fn into_length( self ) -> usize { (*self).into_length() }
}

#[inline]
pub fn are_lengths_the_same( lhs: usize, rhs: impl IntoLength ) -> bool {
    lhs == rhs.into_length()
}

pub fn read_constant< 'a, C, R >( reader: &mut R, constant: &'static [u8] ) -> Result< (), C::Error >
    where C: Context,
          R: Reader< 'a, C >
{
    let is_ok =
        if let Some( result ) = reader.read_bytes_borrowed( constant.len() ) {
            result? == constant
        } else {
            // TODO: Do this more efficiently for sufficiently small constants.
            let data: Vec< u8 > = reader.read_vec( constant.len() )?;
            data == constant
        };

    if !is_ok {
        let error = error_expected_constant( constant );
        return Err( error );
    }

    Ok(())
}
