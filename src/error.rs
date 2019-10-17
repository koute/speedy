use {
    std::{
        fmt,
        io
    }
};

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind
}

#[derive(Debug)]
pub enum ErrorKind {
    IoError( io::Error )
}

impl Error {
    #[inline]
    pub(crate) fn from_io_error( error: io::Error ) -> Self {
        Error {
            kind: ErrorKind::IoError( error )
        }
    }
}

impl fmt::Display for Error {
    fn fmt( &self, fmt: &mut fmt::Formatter ) -> fmt::Result {
        match self.kind {
            ErrorKind::IoError( ref error ) => write!( fmt, "{}", error )
        }
    }
}

impl std::error::Error for Error {
    fn source( &self ) -> Option< &(dyn std::error::Error + 'static) > {
        match self.kind {
            ErrorKind::IoError( ref error ) => Some( error )
        }
    }
}

pub trait IsEof {
    fn is_eof( &self ) -> bool;
}

impl IsEof for Error {
    fn is_eof( &self ) -> bool {
        match self.kind {
            ErrorKind::IoError( ref error ) => error.kind() == std::io::ErrorKind::UnexpectedEof
        }
    }
}

#[inline(never)]
#[cold]
pub fn error_invalid_string_utf8< T >( error: std::string::FromUtf8Error ) -> T where T: From< Error > {
    let error = Error::from_io_error( io::Error::new( io::ErrorKind::InvalidData, error ) );
    T::from( error )
}

#[inline(never)]
#[cold]
pub fn error_invalid_str_utf8< T >( error: std::str::Utf8Error ) -> T where T: From< Error > {
    let error = Error::from_io_error( io::Error::new( io::ErrorKind::InvalidData, error ) );
    T::from( error )
}

#[inline(never)]
#[cold]
pub fn error_length_is_not_the_same_as_count< T >( field_name: &str ) -> T where T: From< Error > {
    let error_message = format!( "the length of '{}' is not the same as its 'count' attribute", field_name );
    let error = Error::from_io_error( std::io::Error::new( std::io::ErrorKind::InvalidData, error_message ) );
    T::from( error )
}

#[inline(never)]
#[cold]
pub fn error_out_of_range_length< T >() -> T where T: From< Error > {
    let error = Error::from_io_error( io::Error::new( io::ErrorKind::InvalidData, "out of range length" ) );
    T::from( error )
}

#[inline(never)]
#[cold]
pub fn error_invalid_enum_variant< T >() -> T where T: From< Error > {
    let error = Error::from_io_error( io::Error::new( io::ErrorKind::InvalidData, "invalid enum variant" ) );
    T::from( error )
}

#[inline(never)]
#[cold]
pub fn error_out_of_range_char< T >() -> T where T: From< Error > {
    let error = Error::from_io_error( io::Error::new( io::ErrorKind::InvalidData, "out of range char" ) );
    T::from( error )
}

#[inline(never)]
#[cold]
pub fn error_too_big_usize_for_this_architecture< T >() -> T where T: From< Error > {
    let error = Error::from_io_error( io::Error::new( io::ErrorKind::InvalidData, "value cannot fit into an usize on this architecture" ) );
    T::from( error )
}

#[inline(never)]
#[cold]
pub fn error_end_of_input< T >() -> T where T: From< Error > {
    let error = Error::from_io_error( io::Error::new( io::ErrorKind::UnexpectedEof, "unexpected end of input" ) );
    T::from( error )
}

#[inline(never)]
#[cold]
pub fn error_end_of_output_buffer< T >() -> T where T: From< Error > {
    let error = Error::from_io_error( io::Error::new( io::ErrorKind::UnexpectedEof, "unexpected end of output buffer" ) );
    T::from( error )
}
