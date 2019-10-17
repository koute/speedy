use byteorder::{
    LittleEndian,
    BigEndian
};

use crate::endianness::Endianness;

pub trait Context {
    type Error: From< crate::Error > + crate::IsEof;
    fn endianness( &self ) -> Endianness;
}

impl Context for Endianness {
    type Error = crate::Error;

    #[inline(always)]
    fn endianness( &self ) -> Endianness {
        *self
    }
}

impl Context for LittleEndian {
    type Error = crate::Error;

    #[inline(always)]
    fn endianness( &self ) -> Endianness {
        Endianness::LittleEndian
    }
}

impl Context for BigEndian {
    type Error = crate::Error;

    #[inline(always)]
    fn endianness( &self ) -> Endianness {
        Endianness::BigEndian
    }
}
