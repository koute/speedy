extern crate byteorder;

mod utils;
mod readable;
mod readable_impl;
mod reader;
mod writable;
mod writable_impl;
mod writer;
mod context;
mod endianness;

pub use readable::Readable;
pub use reader::Reader;

pub use writable::Writable;
pub use writer::Writer;

pub use endianness::Endianness;
pub use context::Context;

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
        fn write_to< 'a, T: ?Sized + Writer< 'a, C > >( &'a self, writer: &mut T ) -> io::Result< () > {
            writer.write_value( &self.a )?;
            writer.write_value( &self.b )?;
            writer.write_value( &self.c )?;

            Ok(())
        }
    }

    impl< C: Context > Readable< C > for SimpleStruct {
        fn read_from< R: Reader< C > >( reader: &mut R ) -> io::Result< Self > {
            let a = reader.read_u8()?;
            let b = reader.read_u8()?;
            let c = reader.read_u8()?;
            Ok( SimpleStruct { a, b, c } )
        }
    }

    #[test]
    fn simple_write_to_vec() {
        let value = SimpleStruct { a: 1, b: 2, c: 3 };
        let data = value.write_to_vec( Endianness::default() ).unwrap();
        assert_eq!( data, vec![ 1, 2, 3 ] );
    }

    #[test]
    fn simple_read_from_stream() {
        let data = vec![ 1, 2, 3 ];
        let cursor = io::Cursor::new( data );
        let value = SimpleStruct::read_from_stream( Endianness::default(), cursor ).unwrap();
        assert_eq!( value, SimpleStruct { a: 1, b: 2, c: 3 } );
    }

    #[test]
    fn simple_read_from_buffer() {
        let data = vec![ 1, 2, 3 ];
        let value = SimpleStruct::read_from_buffer( Endianness::default(), &data ).unwrap();
        assert_eq!( value, SimpleStruct { a: 1, b: 2, c: 3 } );
    }

    #[test]
    fn read_write_u8_vec() {
        let original: Vec< u8 > = vec![ 1, 2, 3 ];
        let serialized = original.write_to_vec( Endianness::default() ).unwrap();
        let deserialized: Vec< u8 > = Vec::< u8 >::read_from_buffer( Endianness::default(), &serialized ).unwrap();
        assert_eq!( original, deserialized );
    }

    #[test]
    fn read_write_u64_vec() {
        let original: Vec< u64 > = vec![ 1, 2, 3 ];
        let serialized = original.write_to_vec( Endianness::default() ).unwrap();
        println!( "{:?}", serialized );
        let deserialized: Vec< u64 > = Vec::< u64 >::read_from_buffer( Endianness::default(), &serialized ).unwrap();
        assert_eq!( original, deserialized );
    }

    #[test]
    fn read_write_string() {
        let original: String = "Hello world!".to_owned();
        let serialized = original.write_to_vec( Endianness::default() ).unwrap();
        let deserialized: String = String::read_from_buffer( Endianness::default(), &serialized ).unwrap();
        assert_eq!( original, deserialized );
    }
}
