use {
    crate::{Context, Readable, Reader, Writable, Writer},
    glam::{IVec2, IVec3, IVec4, Quat, UVec2, UVec3, UVec4, Vec2, Vec3, Vec4},
};

macro_rules! read_write_vec {
    ($T:ty, $ctor:ident, $comp_read_fn:ident, $comp_write_fn:ident, $($comp:ident),+) => {
        impl<'a, C> Readable<'a, C> for $T
        where
            C: Context,
        {
            #[inline]
            fn read_from<R: Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
                $(
                    let $comp = reader.$comp_read_fn()?;
                )+

                Ok(<$T>::$ctor($($comp),+))
            }

            #[inline]
            fn minimum_bytes_needed() -> usize {
                std::mem::size_of::<$T>()
            }
        }

        impl<C> Writable<C> for $T
        where
            C: Context,
        {
            #[inline]
            fn write_to<T: ?Sized + Writer<C>>(&self, writer: &mut T) -> Result<(), C::Error> {
                $(
                    writer.$comp_write_fn(self.$comp)?;
                )+

                Ok(())
            }

            #[inline]
            fn bytes_needed(&self) -> Result<usize, C::Error> {
                Ok(std::mem::size_of::<$T>())
            }
        }
    };
}

read_write_vec! {Vec2, new, read_f32, write_f32, x, y}
read_write_vec! {Vec3, new, read_f32, write_f32, x, y, z}
read_write_vec! {Vec4, new, read_f32, write_f32, x, y, z, w}

read_write_vec! {IVec2, new, read_i32, write_i32, x, y}
read_write_vec! {IVec3, new, read_i32, write_i32, x, y, z}
read_write_vec! {IVec4, new, read_i32, write_i32, x, y, z, w}

read_write_vec! {UVec2, new, read_u32, write_u32, x, y}
read_write_vec! {UVec3, new, read_u32, write_u32, x, y, z}
read_write_vec! {UVec4, new, read_u32, write_u32, x, y, z, w}

read_write_vec! {Quat, from_xyzw, read_f32, write_f32, x, y, z, w}
