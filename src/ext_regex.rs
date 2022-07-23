use regex::Regex;

use crate::{Context, Readable, Reader, Writable, Writer};

impl<'a, C> Readable<'a, C> for Regex
where
    C: Context,
{
    #[inline]
    fn read_from<R: Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let regex: std::borrow::Cow<str> = reader.read_value()?;
        Regex::new(&regex).map_err(|error| {
            crate::error::Error::custom(format!("failed to read a regex: {}", error)).into()
        })
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}

impl<C> Writable<C> for Regex
where
    C: Context,
{
    #[inline]
    fn write_to<T: ?Sized + Writer<C>>(&self, writer: &mut T) -> Result<(), C::Error> {
        self.as_str().write_to(writer)
    }

    #[inline]
    fn bytes_needed(&self) -> Result<usize, C::Error> {
        Writable::<C>::bytes_needed(self.as_str())
    }
}
