use std::slice;
use std::mem;

macro_rules! unsafe_is_length {
    ($expr:expr) => {
        if $expr as u64 >= 0x7FFFFFFF_FFFFFFFF {
            // It's not physically possible to have a valid slice
            // which is bigger than 8 exabytes. No machine exists
            // which could have this much RAM, and you can't even
            // have this much virtual address space on any modern CPU.
            //
            // So in practice this should be totally harmless while
            // it will allow the LLVM optimizer to better do its job.
            //
            // It actually *does* affect optimization in practice
            // allowing LLVM to assume the length won't overflow
            // in certain cases.
            unsafe { std::hint::unreachable_unchecked() }
        }
    }
}

pub unsafe trait Primitive {}
unsafe impl Primitive for i8 {}
unsafe impl Primitive for u8 {}
unsafe impl Primitive for i16 {}
unsafe impl Primitive for u16 {}
unsafe impl Primitive for i32 {}
unsafe impl Primitive for u32 {}
unsafe impl Primitive for i64 {}
unsafe impl Primitive for u64 {}
unsafe impl Primitive for i128 {}
unsafe impl Primitive for u128 {}
unsafe impl Primitive for f32 {}
unsafe impl Primitive for f64 {}

#[inline(always)]
pub fn as_bytes< T: Primitive >( slice: &[T] ) -> &[u8] {
    unsafe {
        slice::from_raw_parts( slice.as_ptr() as *const u8, slice.len() * mem::size_of::< T >() )
    }
}

#[inline(always)]
pub fn as_bytes_mut< T: Primitive >( slice: &mut [T] ) -> &mut [u8] {
    unsafe {
        slice::from_raw_parts_mut( slice.as_mut_ptr() as *mut u8, slice.len() * mem::size_of::< T >() )
    }
}
