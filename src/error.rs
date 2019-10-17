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
