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

pub use crate::varint::VarInt64;

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

#[inline(never)]
#[cold]
pub fn error_out_of_range_length() -> io::Error {
    io::Error::new( io::ErrorKind::InvalidData, "out of range length" )
}

#[inline(never)]
#[cold]
pub fn error_invalid_enum_variant() -> io::Error {
    io::Error::new( io::ErrorKind::InvalidData, "invalid enum variant" )
}

#[inline(never)]
#[cold]
pub(crate) fn error_out_of_range_char() -> io::Error {
    io::Error::new( io::ErrorKind::InvalidData, "out of range char" )
}

#[inline(never)]
#[cold]
pub(crate) fn error_too_big_usize_for_this_architecture() -> io::Error {
    io::Error::new( io::ErrorKind::InvalidData, "value cannot fit into an usize on this architecture" )
}

#[inline(never)]
#[cold]
pub(crate) fn error_end_of_input() -> io::Error {
    io::Error::new( io::ErrorKind::UnexpectedEof, "unexpected end of input" )
}

#[inline(never)]
#[cold]
pub(crate) fn error_end_of_output_buffer() -> io::Error {
    io::Error::new( io::ErrorKind::UnexpectedEof, "unexpected end of output buffer" )
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
pub fn write_length_u64< C, W >( length: usize, writer: &mut W ) -> io::Result< () >
    where C: Context,
          W: ?Sized + Writer< C >
{
    writer.write_u64( length as u64 )
}

#[inline]
pub fn write_length_u32< C, W >( length: usize, writer: &mut W ) -> io::Result< () >
    where C: Context,
          W: ?Sized + Writer< C >
{
    if length as u64 > std::u32::MAX as u64 {
         return Err( error_out_of_range_length() );
    }

    writer.write_u32( length as u32 )
}

#[inline]
pub fn write_length_u16< C, W >( length: usize, writer: &mut W ) -> io::Result< () >
    where C: Context,
          W: ?Sized + Writer< C >
{
    if length as u64 > std::u16::MAX as u64 {
         return Err( error_out_of_range_length() );
    }

    writer.write_u16( length as u16 )
}

#[inline]
pub fn write_length_u8< C, W >( length: usize, writer: &mut W ) -> io::Result< () >
    where C: Context,
          W: ?Sized + Writer< C >
{
    if length as u64 > std::u8::MAX as u64 {
         return Err( error_out_of_range_length() );
    }

    writer.write_u8( length as u8 )
}

#[inline]
pub fn write_length< C, W >( length: usize, writer: &mut W ) -> io::Result< () >
    where C: Context,
          W: ?Sized + Writer< C >
{
    write_length_u32( length, writer )
}

#[inline]
pub fn read_length_u64< 'a, C, R >( reader: &mut R ) -> io::Result< usize >
    where C: Context,
          R: Reader< 'a, C >
{
    reader.read_u64().map( |value| value as usize )
}

#[inline]
pub fn read_length_u32< 'a, C, R >( reader: &mut R ) -> io::Result< usize >
    where C: Context,
          R: Reader< 'a, C >
{
    reader.read_u32().map( |value| value as usize )
}

#[inline]
pub fn read_length_u16< 'a, C, R >( reader: &mut R ) -> io::Result< usize >
    where C: Context,
          R: Reader< 'a, C >
{
    reader.read_u16().map( |value| value as usize )
}

#[inline]
pub fn read_length_u8< 'a, C, R >( reader: &mut R ) -> io::Result< usize >
    where C: Context,
          R: Reader< 'a, C >
{
    reader.read_u8().map( |value| value as usize )
}

#[inline]
pub fn read_length< 'a, C, R >( reader: &mut R ) -> io::Result< usize >
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
