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
    InvalidSystemTime,
    ZeroNonZero,
    OutOfRangeLength,
    OutOfRangeUsize,
    UnexpectedEndOfInput,
    UnexpectedEndOfOutputBuffer,
    InputBufferIsTooSmall {
        actual_size: usize,
        expected_size: usize
    },
    OutputBufferIsTooSmall {
        actual_size: usize,
        expected_size: usize
    },

    LengthIsNotTheSameAsLengthAttribute {
        field_name: &'static str
    },
    ExpectedConstant {
        constant: &'static [u8]
    },

    IoError( io::Error )
}

impl Error {
    #[inline]
    fn new( kind: ErrorKind ) -> Self {
        Error { kind }
    }

    pub fn custom( message: impl fmt::Display ) -> Self {
        // The LLVM optimizer doesn't like us adding a new variant,
        // so instead we reuse the `IoError` one.
        Error {
            kind: ErrorKind::IoError( io::Error::new( io::ErrorKind::Other, message.to_string() ) )
        }
    }

    #[inline]
    pub(crate) fn from_io_error( error: io::Error ) -> Self {
        Error {
            kind: ErrorKind::IoError( error )
        }
    }
}

impl From< Error > for io::Error {
    fn from( error: Error ) -> Self {
        if let ErrorKind::IoError( error ) = error.kind {
            return error;
        }

        let is_eof = error.is_eof();
        let kind = if is_eof {
            io::ErrorKind::UnexpectedEof
        } else {
            io::ErrorKind::InvalidData
        };

        io::Error::new( kind, format!( "{}", error ) )
    }
}

#[inline]
pub fn get_error_kind( error: &Error ) -> &ErrorKind {
    &error.kind
}

impl fmt::Display for Error {
    fn fmt( &self, fmt: &mut fmt::Formatter ) -> fmt::Result {
        match self.kind {
            ErrorKind::InvalidChar => write!( fmt, "out of range char" ),
            ErrorKind::InvalidEnumVariant => write!( fmt, "invalid enum variant" ),
            ErrorKind::InvalidUtf8 => write!( fmt, "encountered invalid utf-8" ),
            ErrorKind::InvalidSystemTime => write!( fmt, "encountered invalid system time object" ),
            ErrorKind::ZeroNonZero => write!( fmt, "a field which is supposed to be non-zero is zero" ),
            ErrorKind::OutOfRangeLength => write!( fmt, "out of range length" ),
            ErrorKind::OutOfRangeUsize => write!( fmt, "value cannot fit into an usize on this architecture" ),
            ErrorKind::UnexpectedEndOfInput => write!( fmt, "unexpected end of input" ),
            ErrorKind::UnexpectedEndOfOutputBuffer => write!( fmt, "unexpected end of output buffer" ),
            ErrorKind::InputBufferIsTooSmall { actual_size, expected_size } => write!( fmt, "input buffer is too small; expected at least {} bytes, got {}", expected_size, actual_size ),
            ErrorKind::OutputBufferIsTooSmall { actual_size, expected_size } => write!( fmt, "output buffer is too small; expected at least {} bytes, got {}", expected_size, actual_size ),
            ErrorKind::LengthIsNotTheSameAsLengthAttribute { field_name } => write!( fmt, "the length of '{}' is not the same as its 'length' attribute", field_name ),
            ErrorKind::ExpectedConstant { constant } => write!( fmt, "expected a predefined {} bytes(s) long constant", constant.len() ),
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
pub fn error_length_is_not_the_same_as_length_attribute< T >( field_name: &'static str ) -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::LengthIsNotTheSameAsLengthAttribute { field_name } ) )
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

#[cold]
pub fn error_input_buffer_is_too_small< T >( actual_size: usize, expected_size: usize ) -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::InputBufferIsTooSmall { actual_size, expected_size } ) )
}

#[cold]
pub fn error_output_buffer_is_too_small< T >( actual_size: usize, expected_size: usize ) -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::OutputBufferIsTooSmall { actual_size, expected_size } ) )
}

#[cold]
pub fn error_zero_non_zero< T >() -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::ZeroNonZero ) )
}

#[cold]
pub fn error_invalid_system_time< T >() -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::InvalidSystemTime ) )
}

#[cold]
pub fn error_expected_constant< T >( constant: &'static [u8] ) -> T where T: From< Error > {
    T::from( Error::new( ErrorKind::ExpectedConstant { constant } ) )
}
