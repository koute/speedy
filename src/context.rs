use byteorder::{
    LittleEndian,
    BigEndian
};

use endianness::Endianness;

pub trait Context {
    fn endianness( &self ) -> Endianness;
}

impl Context for Endianness {
    #[inline]
    fn endianness( &self ) -> Endianness {
        *self
    }
}

impl Context for LittleEndian {
    #[inline]
    fn endianness( &self ) -> Endianness {
        Endianness::LittleEndian
    }
}

impl Context for BigEndian {
    #[inline]
    fn endianness( &self ) -> Endianness {
        Endianness::BigEndian
    }
}
