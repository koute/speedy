#![feature(test)]

extern crate test;
extern crate byteorder;
extern crate speedy;

use std::io::{Read, Write};
use std::borrow::Cow;
use test::{Bencher, black_box};
use byteorder::{ReadBytesExt, NativeEndian};
use speedy::{Readable, Writable, Endianness};

#[bench]
fn deserialization_manual_bytes( b: &mut Bencher ) {
    let original: &[u8] = black_box( &[1, 2, 3, 4] );
    let data = original.write_to_vec( Endianness::NATIVE ).unwrap();
    b.iter( || {
        let mut buffer = &data[..];

        let len = buffer.read_u32::< NativeEndian >().unwrap() as usize;
        let mut vec = Vec::with_capacity( len );
        unsafe { vec.set_len( len ); }
        buffer.read_exact( &mut vec[..] ).unwrap();

        vec
    })
}

#[bench]
fn deserialization_speedy_bytes( b: &mut Bencher ) {
    let original: &[u8] = black_box( &[1, 2, 3, 4] );
    let data = original.write_to_vec( Endianness::NATIVE ).unwrap();
    b.iter( || {
        let deserialized: Vec< u8 > = Readable::read_from_buffer( Endianness::NATIVE, &data ).unwrap();
        deserialized
    })
}

#[bench]
fn deserialization_manual_string( b: &mut Bencher ) {
    let original: &str = black_box( "Hello world!" );
    let data = original.write_to_vec( Endianness::NATIVE ).unwrap();
    b.iter( || {
        let mut buffer = &data[..];

        let len = buffer.read_u32::< NativeEndian >().unwrap() as usize;
        let mut vec = Vec::with_capacity( len );
        unsafe { vec.set_len( len ); }
        buffer.read_exact( &mut vec[..] ).unwrap();
        String::from_utf8( vec )
    })
}

#[bench]
fn deserialization_speedy_string( b: &mut Bencher ) {
    let original: &str = black_box( "Hello world!" );
    let data = original.write_to_vec( Endianness::NATIVE ).unwrap();
    b.iter( || {
        let deserialized: String = Readable::read_from_buffer( Endianness::NATIVE, &data ).unwrap();
        deserialized
    })
}

#[bench]
fn deserialization_manual_u8( b: &mut Bencher ) {
    let original: u8 = black_box( 12 );
    let data = original.write_to_vec( Endianness::NATIVE ).unwrap();
    b.iter( || {
        let mut buffer = &data[..];
        buffer.read_u8().unwrap()
    })
}

#[bench]
fn deserialization_speedy_u8( b: &mut Bencher ) {
    let original: u8 = black_box( 12 );
    let data = original.write_to_vec( Endianness::NATIVE ).unwrap();
    b.iter( || {
        let deserialized: u8 = Readable::read_from_buffer( Endianness::NATIVE, &data ).unwrap();
        deserialized
    })
}

#[bench]
fn deserialization_manual_u64( b: &mut Bencher ) {
    let original: u64 = black_box( 1234 );
    let data = original.write_to_vec( Endianness::NATIVE ).unwrap();
    b.iter( || {
        let mut buffer = &data[..];
        buffer.read_u64::< NativeEndian >().unwrap()
    })
}

#[bench]
fn deserialization_speedy_u64( b: &mut Bencher ) {
    let original: u64 = black_box( 1234 );
    let data = original.write_to_vec( Endianness::NATIVE ).unwrap();
    b.iter( || {
        let deserialized: u64 = Readable::read_from_buffer( Endianness::NATIVE, &data ).unwrap();
        deserialized
    })
}

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
