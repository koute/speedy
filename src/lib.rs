#![cfg_attr(feature = "external_doc", feature(external_doc))]
#![cfg_attr(feature = "external_doc", doc(include = "../README.md"))]

mod utils;
mod readable;
mod readable_impl;
mod reader;
mod writable;
mod writable_impl;
mod writer;
mod context;
mod endianness;

#[cfg(feature = "speedy-derive")]
pub use speedy_derive::{Readable, Writable};

pub use crate::readable::Readable;
pub use crate::reader::Reader;

pub use crate::writable::Writable;
pub use crate::writer::Writer;

pub use crate::endianness::Endianness;
pub use crate::context::Context;

#[doc(hidden)]
pub mod private {
    pub use crate::readable_impl::read_vec;
    pub use crate::writable_impl::write_slice;
}

#[cfg(test)]
mod tests {
    use std::io;
    use super::{
        Reader,
        Readable,
        Writer,
        Writable,
        Context,
        Endianness
    };

    #[derive(PartialEq, Debug)]
    struct SimpleStruct {
        a: u8,
        b: u8,
        c: u8
    }

    impl< C: Context > Writable< C > for SimpleStruct {
        fn write_to< T: ?Sized + Writer< C > >( &self, writer: &mut T ) -> io::Result< () > {
            writer.write_value( &self.a )?;
            writer.write_value( &self.b )?;
            writer.write_value( &self.c )?;

            Ok(())
        }
    }

    impl< 'a, C: Context > Readable< 'a, C > for SimpleStruct {
        fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> io::Result< Self > {
            let a = reader.read_u8()?;
            let b = reader.read_u8()?;
            let c = reader.read_u8()?;
            Ok( SimpleStruct { a, b, c } )
        }
    }

    #[test]
    fn simple_write_to_vec() {
        let value = SimpleStruct { a: 1, b: 2, c: 3 };
        let data = value.write_to_vec( Endianness::NATIVE ).unwrap();
        assert_eq!( data, vec![ 1, 2, 3 ] );
    }

    #[test]
    fn simple_read_from_stream() {
        let data = vec![ 1, 2, 3 ];
        let cursor = io::Cursor::new( data );
        let value = SimpleStruct::read_from_stream( Endianness::NATIVE, cursor ).unwrap();
        assert_eq!( value, SimpleStruct { a: 1, b: 2, c: 3 } );
    }

    #[test]
    fn simple_read_from_buffer() {
        let data = vec![ 1, 2, 3 ];
        let value = SimpleStruct::read_from_buffer( Endianness::NATIVE, &data ).unwrap();
        assert_eq!( value, SimpleStruct { a: 1, b: 2, c: 3 } );
    }

    #[test]
    fn read_write_u8_vec() {
        let original: Vec< u8 > = vec![ 1, 2, 3 ];
        let serialized = original.write_to_vec( Endianness::NATIVE ).unwrap();
        let deserialized: Vec< u8 > = Vec::< u8 >::read_from_buffer( Endianness::NATIVE, &serialized ).unwrap();
        assert_eq!( original, deserialized );
    }

    #[test]
    fn read_write_u64_vec() {
        let original: Vec< u64 > = vec![ 1, 2, 3 ];
        let serialized = original.write_to_vec( Endianness::NATIVE ).unwrap();
        let deserialized: Vec< u64 > = Vec::< u64 >::read_from_buffer( Endianness::NATIVE, &serialized ).unwrap();
        assert_eq!( original, deserialized );
    }

    #[test]
    fn read_write_string() {
        let original: String = "Hello world!".to_owned();
        let serialized = original.write_to_vec( Endianness::NATIVE ).unwrap();
        let deserialized: String = String::read_from_buffer( Endianness::NATIVE, &serialized ).unwrap();
        assert_eq!( original, deserialized );
    }
}
