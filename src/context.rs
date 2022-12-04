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

#[derive(Default)]
pub struct LittleEndian {}

#[derive(Default)]
pub struct BigEndian {}

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

#[cfg(target_endian = "little")]
pub use LittleEndian as NativeContext;

#[cfg(target_endian = "big")]
pub use BigEndian as NativeContext;

pub trait DefaultContext {
    type Context;
}

impl< T > DefaultContext for T where T: ?Sized {
    type Context = LittleEndian;
}
