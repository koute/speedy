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
            unsafe { core::hint::unreachable_unchecked() }
        }
    }
}

// TODO: Remove the T parameter once #![feature(trivial_bounds)] is stable.
pub unsafe trait ZeroCopyable< C, T > where T: ?Sized {}
unsafe impl< C, T > ZeroCopyable< C, T > for i8 {}
unsafe impl< C, T > ZeroCopyable< C, T > for u8 {}
unsafe impl< T > ZeroCopyable< crate::context::NativeContext, T > for i16 {}
unsafe impl< T > ZeroCopyable< crate::context::NativeContext, T > for u16 {}
unsafe impl< T > ZeroCopyable< crate::context::NativeContext, T > for i32 {}
unsafe impl< T > ZeroCopyable< crate::context::NativeContext, T > for u32 {}
unsafe impl< T > ZeroCopyable< crate::context::NativeContext, T > for i64 {}
unsafe impl< T > ZeroCopyable< crate::context::NativeContext, T > for u64 {}
unsafe impl< T > ZeroCopyable< crate::context::NativeContext, T > for i128 {}
unsafe impl< T > ZeroCopyable< crate::context::NativeContext, T > for u128 {}
unsafe impl< T > ZeroCopyable< crate::context::NativeContext, T > for f32 {}
unsafe impl< T > ZeroCopyable< crate::context::NativeContext, T > for f64 {}

pub trait SwapBytes {
    fn swap_bytes( self ) -> Self;
}

impl SwapBytes for f32 {
    #[inline(always)]
    fn swap_bytes( self ) -> Self {
        union Union {
            float: f32,
            int: u32
        }

        let mut u = Union { float: self };
        unsafe {
            u.int = u.int.swap_bytes();
            u.float
        }
    }
}

impl SwapBytes for f64 {
    #[inline(always)]
    fn swap_bytes( self ) -> Self {
        union Union {
            float: f64,
            int: u64
        }

        let mut u = Union { float: self };
        unsafe {
            u.int = u.int.swap_bytes();
            u.float
        }
    }
}
