use std::slice;
use std::mem;

pub unsafe trait Primitive {}
unsafe impl Primitive for u64 {}

pub fn as_bytes< T: Primitive >( slice: &[T] ) -> &[u8] {
    unsafe {
        slice::from_raw_parts( slice.as_ptr() as *const u8, slice.len() * mem::size_of::< T >() )
    }
}
