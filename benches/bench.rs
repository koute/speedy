#![feature(test)]

extern crate test;
extern crate byteorder;
extern crate speedy;

use std::io::{self, Read, Write};
use std::borrow::Cow;
use test::{Bencher, black_box};
use byteorder::{ReadBytesExt, NativeEndian};
use speedy::{Context, Readable, Reader, Writable, Endianness};

#[bench]
fn serialization_manual_megabyte_buffer( b: &mut Bencher ) {
    let mut buffer: Vec< u8 > = Vec::new();
    buffer.resize( 1024 * 1024, 1 );
    buffer = black_box( buffer );
    b.iter( || {
        let mut output = Vec::new();
        Write::write_all( &mut output, &buffer ).unwrap();
        output
    })
}

// These two benchmarks should have exactly the same speeds.
#[bench]
fn serialization_speedy_megabyte_buffer_le( b: &mut Bencher ) {
    let mut buffer: Vec< u8 > = Vec::new();
    buffer.resize( 1024 * 1024, 1 );
    buffer = black_box( buffer );
    b.iter( || {
        let mut output = Vec::new();
        buffer.write_to_stream( Endianness::LittleEndian, &mut output ).unwrap();
        output
    })
}

#[bench]
fn serialization_speedy_megabyte_buffer_be( b: &mut Bencher ) {
    let mut buffer: Vec< u8 > = Vec::new();
    buffer.resize( 1024 * 1024, 1 );
    buffer = black_box( buffer );
    b.iter( || {
        let mut output = Vec::new();
        buffer.write_to_stream( Endianness::BigEndian, &mut output ).unwrap();
        output
    })
}

#[bench]
fn deserialization_speedy_megabyte_buffer_cow_borrowed( b: &mut Bencher ) {
    let mut buffer: Vec< u8 > = Vec::new();
    buffer.resize( 1024 * 1024, 1 );
    let mut buffer = buffer.write_to_vec( Endianness::NATIVE ).unwrap();

    buffer = black_box( buffer );
    b.iter( || {
        let deserialized: Cow< [u8] > = Readable::read_from_buffer( Endianness::NATIVE, &buffer ).unwrap();
        deserialized
    })
}

#[bench]
fn deserialization_speedy_megabyte_buffer_cow_owned( b: &mut Bencher ) {
    let mut buffer: Vec< u8 > = Vec::new();
    buffer.resize( 1024 * 1024, 1 );
    let mut buffer = buffer.write_to_vec( Endianness::NATIVE ).unwrap();

    buffer = black_box( buffer );
    b.iter( || {
        let deserialized: Cow< [u8] > = Readable::read_from_buffer_owned( Endianness::NATIVE, &buffer ).unwrap();
        deserialized
    })
}

#[repr(transparent)]
#[derive(Copy, Clone, Writable)]
struct Byte( u8 );

impl< 'a, C: Context > Readable< 'a, C > for Byte {
    #[inline]
    fn read_from< R: Reader< 'a, C > >( reader: &mut R ) -> io::Result< Self > {
        Ok( Byte( reader.read_u8()? ) )
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        1
    }
}

#[bench]
fn deserialization_speedy_megabyte_buffer_vec_non_primitive( b: &mut Bencher ) {
    let mut buffer: Vec< Byte > = Vec::new();
    buffer.resize( 1024 * 1024, Byte( 1 ) );
    let mut buffer = buffer.write_to_vec( Endianness::NATIVE ).unwrap();

    buffer = black_box( buffer );
    b.iter( || {
        let deserialized: Vec< Byte > = Readable::read_from_buffer_owned( Endianness::NATIVE, &buffer ).unwrap();
        deserialized
    })
}

#[derive(Clone, Readable, Writable)]
struct Dummy {
    a: u64,
    b: u32,
    c: u16,
    d: u8,
    e: f32,
    f: f64,
    g: bool
}

#[bench]
fn deserialization_speedy_many_small_structs( b: &mut Bencher ) {
    let mut buffer: Vec< Dummy > = Vec::new();
    let dummy = Dummy {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
        e: 5.0,
        f: 6.0,
        g: true
    };
    buffer.resize( 1024 * 1024, dummy );
    let mut buffer = buffer.write_to_vec( Endianness::NATIVE ).unwrap();

    buffer = black_box( buffer );
    b.iter( || {
        let deserialized: Vec< Dummy > = Readable::read_from_buffer_owned( Endianness::NATIVE, &buffer ).unwrap();
        deserialized
    })
}
