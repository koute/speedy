use std::slice;
use std::mem;

pub unsafe trait Primitive {}
unsafe impl Primitive for i8 {}
unsafe impl Primitive for u8 {}
unsafe impl Primitive for i16 {}
unsafe impl Primitive for u16 {}
unsafe impl Primitive for i32 {}
unsafe impl Primitive for u32 {}
unsafe impl Primitive for i64 {}
unsafe impl Primitive for u64 {}
unsafe impl Primitive for f32 {}
unsafe impl Primitive for f64 {}

#[inline]
pub fn as_bytes< T: Primitive >( slice: &[T] ) -> &[u8] {
    unsafe {
        slice::from_raw_parts( slice.as_ptr() as *const u8, slice.len() * mem::size_of::< T >() )
    }
}

#[inline]
pub fn as_bytes_mut< T: Primitive >( slice: &mut [T] ) -> &mut [u8] {
    unsafe {
        slice::from_raw_parts_mut( slice.as_mut_ptr() as *mut u8, slice.len() * mem::size_of::< T >() )
    }
}
