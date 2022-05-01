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

// TODO: Remove the T parameter once #![feature(trivial_bounds)] is stable.
pub unsafe trait Primitive< T > where T: ?Sized {}
unsafe impl< T > Primitive< T > for i8 {}
unsafe impl< T > Primitive< T > for u8 {}

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

        unsafe {
            let mut u = Union { float: self };
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

        unsafe {
            let mut u = Union { float: self };
            u.int = u.int.swap_bytes();
            u.float
        }
    }
}
