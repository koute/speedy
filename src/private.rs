use {
    crate::{
        Context,
        Reader,
        Writer
    },
    std::{
        borrow::{
            Cow
        },
        io
    }
};

#[inline(never)]
#[cold]
fn error_invalid_string_utf8( error: std::string::FromUtf8Error ) -> io::Error {
    io::Error::new( io::ErrorKind::InvalidData, error )
}

#[inline(never)]
#[cold]
fn error_invalid_str_utf8( error: std::str::Utf8Error ) -> io::Error {
    io::Error::new( io::ErrorKind::InvalidData, error )
}

#[inline(never)]
#[cold]
pub fn error_length_is_not_the_same_as_count( field_name: &str ) -> io::Error {
    let error_message = format!( "the length of '{}' is not the same as its 'count' attribute", field_name );
    std::io::Error::new( std::io::ErrorKind::InvalidData, error_message )
}

#[inline]
pub fn vec_to_string( bytes: Vec< u8 > ) -> Result< String, io::Error > {
    String::from_utf8( bytes ).map_err( error_invalid_string_utf8 )
}

#[inline]
pub fn cow_bytes_to_cow_str( bytes: Cow< [u8] > ) -> Result< Cow< str >, io::Error > {
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
pub fn write_length< C, W >( length: usize, writer: &mut W ) -> io::Result< () >
    where C: Context,
          W: ?Sized + Writer< C >
{
    if length as u64 > std::u32::MAX as u64 {
         return Err( io::Error::new( io::ErrorKind::InvalidData, "out of range length" ) );
    }

    writer.write_u32( length as u32 )
}

#[inline]
pub fn read_length< 'a, C, R >( reader: &mut R ) -> io::Result< usize >
    where C: Context,
          R: Reader< 'a, C >
{
    reader.read_u32().map( |value| value as usize )
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
