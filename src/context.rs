use byteorder::{
    LittleEndian,
    BigEndian
};

use crate::endianness::Endianness;

pub trait Context {
    fn endianness( &self ) -> Endianness;
}

impl Context for Endianness {
    #[inline(always)]
    fn endianness( &self ) -> Endianness {
        *self
    }
}

impl Context for LittleEndian {
    #[inline(always)]
    fn endianness( &self ) -> Endianness {
        Endianness::LittleEndian
    }
}

impl Context for BigEndian {
    #[inline(always)]
    fn endianness( &self ) -> Endianness {
        Endianness::BigEndian
    }
}
