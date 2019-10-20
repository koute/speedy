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
    InvalidChar,
    InvalidEnumVariant,
    InvalidUtf8,
    OutOfRangeLength,
    OutOfRangeUsize,
    UnexpectedEndOfInput,
    UnexpectedEndOfOutputBuffer,

    LengthIsNotTheSameAsCount {
        field_name: &'static str
    },

    IoError( io::Error )
}

impl Error {
    fn new( kind: ErrorKind ) -> Self {
        Error { kind }
    }

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
            ErrorKind::InvalidChar => write!( fmt, "out of range char" ),
            ErrorKind::InvalidEnumVariant => write!( fmt, "invalid enum variant" ),
            ErrorKind::InvalidUtf8 => write!( fmt, "encountered invalid utf-8" ),
            ErrorKind::OutOfRangeLength => write!( fmt, "out of range length" ),
            ErrorKind::OutOfRangeUsize => write!( fmt, "value cannot fit into an usize on this architecture" ),
            ErrorKind::UnexpectedEndOfInput => write!( fmt, "unexpected end of input" ),
            ErrorKind::UnexpectedEndOfOutputBuffer => write!( fmt, "unexpected end of output buffer" ),
            ErrorKind::LengthIsNotTheSameAsCount { field_name } => write!( fmt, "the length of '{}' is not the same as its 'count' attribute", field_name ),
            ErrorKind::IoError( ref error ) => write!( fmt, "{}", error )
        }
    }
}

impl std::error::Error for Error {
    fn source( &self ) -> Option< &(dyn std::error::Error + 'static) > {
        match self.kind {
            ErrorKind::IoError( ref error ) => Some( error ),
            _ => None
        }
    }
}

pub trait IsEof {
    fn is_eof( &self ) -> bool;
}

impl IsEof for Error {
    fn is_eof( &self ) -> bool {
        match self.kind {
            ErrorKind::UnexpectedEndOfInput |
            ErrorKind::UnexpectedEndOfOutputBuffer => true,
            ErrorKind::IoError( ref error ) => error.kind() == std::io::ErrorKind::UnexpectedEof,
            _ => false
        }
    }
}

#[cold]
pub fn error_invalid_string_utf8< T >( _: std::string::FromUtf8Error ) -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::InvalidUtf8 ) )
}

#[cold]
pub fn error_invalid_str_utf8< T >( _: std::str::Utf8Error ) -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::InvalidUtf8 ) )
}

#[cold]
pub fn error_length_is_not_the_same_as_count< T >( field_name: &'static str ) -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::LengthIsNotTheSameAsCount { field_name } ) )
}

#[cold]
pub fn error_out_of_range_length< T >() -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::OutOfRangeLength ) )
}

#[cold]
pub fn error_invalid_enum_variant< T >() -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::InvalidEnumVariant ) )
}

#[cold]
pub fn error_out_of_range_char< T >() -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::InvalidChar ) )
}

#[cold]
pub fn error_too_big_usize_for_this_architecture< T >() -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::OutOfRangeUsize ) )
}

#[cold]
pub fn error_end_of_input< T >() -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::UnexpectedEndOfInput ) )
}

#[cold]
pub fn error_end_of_output_buffer< T >() -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::UnexpectedEndOfOutputBuffer ) )
}
