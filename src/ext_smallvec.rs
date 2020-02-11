use {
    crate::{
        Context,
        Readable,
        Reader,
        Writable,
        Writer
    },
    smallvec::{
        Array,
        SmallVec
    }
};

impl< 'a, C, A > Readable< 'a, C > for SmallVec< A >
    where C: Context,
          A: Array,
          <A as Array>::Item: Readable< 'a, C >
{
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> Result< Self, C::Error > {
        let length = crate::private::read_length( reader )?;
        // TODO: This can be more efficient if we directly read into the `SmallVec`.
        let value = reader.read_vec( length )?;
        Ok( value.into() )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        4
    }
}


impl< C, A > Writable< C > for SmallVec< A >
    where C: Context,
          A: Array,
          <A as Array>::Item: Writable< C >
{
    #[inline]
    fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> Result< (), C::Error > {
        self.as_slice().write_to( writer )
    }

    #[inline]
    fn bytes_needed( &self ) -> Result< usize, C::Error > {
        Writable::< C >::bytes_needed( self.as_slice() )
    }
}
