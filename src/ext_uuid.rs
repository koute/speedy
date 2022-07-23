use uuid::Uuid;

use crate::{Context, Readable, Reader, Writable, Writer};

const UUID_SIZE: usize = 16;

impl<'a, C> Readable<'a, C> for Uuid
where
    C: Context,
{
    #[inline]
    fn read_from<R: Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let mut buffer = [0; UUID_SIZE];
        reader.read_bytes(&mut buffer)?;
        Ok(Uuid::from_bytes(buffer))
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        UUID_SIZE
    }
}

impl<C> Writable<C> for Uuid
where
    C: Context,
{
    #[inline]
    fn write_to<T: ?Sized + Writer<C>>(&self, writer: &mut T) -> Result<(), C::Error> {
        writer.write_bytes(self.as_bytes())
    }

    #[inline]
    fn bytes_needed(&self) -> Result<usize, C::Error> {
        Ok(UUID_SIZE)
    }
}
