#![feature(test)]

extern crate test;
extern crate byteorder;
extern crate speedy;

use std::io::Read;
use test::{Bencher, black_box};
use byteorder::{ReadBytesExt, NativeEndian};
use speedy::{Readable, Writable};

#[bench]
fn deserialization_manual_bytes( b: &mut Bencher ) {
    let original: &[u8] = black_box( &[1, 2, 3, 4] );
    let data = original.write_to_vec(()).unwrap();
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
    let data = original.write_to_vec(()).unwrap();
    b.iter( || {
        let deserialized: Vec< u8 > = Readable::read_from_buffer( (), &data ).unwrap();
        deserialized
    })
}

#[bench]
fn deserialization_manual_string( b: &mut Bencher ) {
    let original: &str = black_box( "Hello world!" );
    let data = original.write_to_vec(()).unwrap();
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
    let data = original.write_to_vec(()).unwrap();
    b.iter( || {
        let deserialized: String = Readable::read_from_buffer( (), &data ).unwrap();
        deserialized
    })
}

#[bench]
fn deserialization_manual_u8( b: &mut Bencher ) {
    let original: u8 = black_box( 12 );
    let data = original.write_to_vec(()).unwrap();
    b.iter( || {
        let mut buffer = &data[..];
        buffer.read_u8().unwrap()
    })
}

#[bench]
fn deserialization_speedy_u8( b: &mut Bencher ) {
    let original: u8 = black_box( 12 );
    let data = original.write_to_vec(()).unwrap();
    b.iter( || {
        let deserialized: u8 = Readable::read_from_buffer( (), &data ).unwrap();
        deserialized
    })
}

#[bench]
fn deserialization_manual_u64( b: &mut Bencher ) {
    let original: u64 = black_box( 1234 );
    let data = original.write_to_vec(()).unwrap();
    b.iter( || {
        let mut buffer = &data[..];
        buffer.read_u64::< NativeEndian >().unwrap()
    })
}

#[bench]
fn deserialization_speedy_u64( b: &mut Bencher ) {
    let original: u64 = black_box( 1234 );
    let data = original.write_to_vec(()).unwrap();
    b.iter( || {
        let deserialized: u64 = Readable::read_from_buffer( (), &data ).unwrap();
        deserialized
    })
}
