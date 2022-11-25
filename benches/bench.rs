#![feature(test)]

extern crate test;

use speedy::{Context, Endianness, Readable, Reader, Writable};
use std::borrow::Cow;
use std::io::Write;
use test::{black_box, Bencher};

#[bench]
fn write_manual_megabyte_buffer(b: &mut Bencher) {
    let mut buffer: Vec<u8> = Vec::new();
    buffer.resize(1024 * 1024, 1);
    buffer = black_box(buffer);
    b.iter(|| {
        let mut output = Vec::new();
        Write::write_all(&mut output, &buffer).unwrap();
        output
    })
}

// These two benchmarks should have exactly the same speeds.
#[bench]
fn write_speedy_megabyte_buffer_le(b: &mut Bencher) {
    let mut buffer: Vec<u8> = Vec::new();
    buffer.resize(1024 * 1024, 1);
    buffer = black_box(buffer);
    b.iter(|| {
        let mut output = Vec::new();
        buffer
            .write_to_stream_with_ctx(Endianness::LittleEndian, &mut output)
            .unwrap();
        output
    })
}

#[bench]
fn write_speedy_megabyte_buffer_be(b: &mut Bencher) {
    let mut buffer: Vec<u8> = Vec::new();
    buffer.resize(1024 * 1024, 1);
    buffer = black_box(buffer);
    b.iter(|| {
        let mut output = Vec::new();
        buffer
            .write_to_stream_with_ctx(Endianness::BigEndian, &mut output)
            .unwrap();
        output
    })
}

#[bench]
fn read_speedy_megabyte_buffer_cow_borrowed(b: &mut Bencher) {
    let mut buffer: Vec<u8> = Vec::new();
    buffer.resize(1024 * 1024, 1);
    let mut buffer = buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap();

    buffer = black_box(buffer);
    b.iter(|| {
        let deserialized: Cow<[u8]> =
            Readable::read_from_buffer_with_ctx(Endianness::NATIVE, &buffer).unwrap();
        deserialized
    })
}

#[bench]
fn read_speedy_megabyte_buffer_cow_owned(b: &mut Bencher) {
    let mut buffer: Vec<u8> = Vec::new();
    buffer.resize(1024 * 1024, 1);
    let mut buffer = buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap();

    buffer = black_box(buffer);
    b.iter(|| {
        let deserialized: Cow<[u8]> =
            Readable::read_from_buffer_copying_data_with_ctx(Endianness::NATIVE, &buffer).unwrap();
        deserialized
    })
}

#[repr(transparent)]
#[derive(Copy, Clone, Writable)]
struct Byte(u8);

impl<'a, C: Context> Readable<'a, C> for Byte {
    #[inline]
    fn read_from<R: Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        Ok(Byte(reader.read_u8()?))
    }

    #[inline]
    fn minimum_bytes_needed() -> usize {
        1
    }
}

#[bench]
fn read_speedy_megabyte_buffer_vec_non_primitive(b: &mut Bencher) {
    let mut buffer: Vec<Byte> = Vec::new();
    buffer.resize(1024 * 1024, Byte(1));
    let mut buffer = buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap();

    buffer = black_box(buffer);
    b.iter(|| {
        let deserialized: Vec<Byte> =
            Readable::read_from_buffer_copying_data_with_ctx(Endianness::NATIVE, &buffer).unwrap();
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
    g: bool,
}

#[bench]
fn read_speedy_many_small_structs(b: &mut Bencher) {
    let mut buffer: Vec<Dummy> = Vec::new();
    let dummy = Dummy {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
        e: 5.0,
        f: 6.0,
        g: true,
    };
    buffer.resize(1024 * 1024, dummy);
    let mut buffer = buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap();

    buffer = black_box(buffer);
    b.iter(|| {
        let deserialized: Vec<Dummy> =
            Readable::read_from_buffer_copying_data_with_ctx(Endianness::NATIVE, &buffer).unwrap();
        deserialized
    })
}

#[bench]
fn write_speedy_many_small_structs(b: &mut Bencher) {
    let mut buffer: Vec<Dummy> = Vec::new();
    let dummy = Dummy {
        a: 1,
        b: 2,
        c: 3,
        d: 4,
        e: 5.0,
        f: 6.0,
        g: true,
    };
    buffer.resize(1024 * 1024, dummy);

    buffer = black_box(buffer);
    b.iter(|| buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap())
}

pub struct XorShift64 {
    a: u64,
}

impl XorShift64 {
    pub fn new(seed: u64) -> XorShift64 {
        XorShift64 { a: seed }
    }

    pub fn next(&mut self) -> u64 {
        let mut x = self.a;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.a = x;
        x
    }
}

#[bench]
fn read_varint_random(b: &mut Bencher) {
    use speedy::private::VarInt64;
    let mut rng = XorShift64 { a: 1234 };

    let buffer: Vec<VarInt64> = (0..1024 * 1024)
        .into_iter()
        .map(|_| (1_u64 << (rng.next() % 63)).into())
        .collect();
    let mut buffer = buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap();

    buffer = black_box(buffer);
    b.iter(|| {
        let deserialized: Vec<VarInt64> =
            Readable::read_from_buffer_copying_data_with_ctx(Endianness::NATIVE, &buffer).unwrap();
        deserialized
    })
}

#[bench]
fn read_varint_always_one_byte(b: &mut Bencher) {
    use speedy::private::VarInt64;
    let mut rng = XorShift64 { a: 1234 };

    let buffer: Vec<VarInt64> = (0..1024 * 1024)
        .into_iter()
        .map(|_| (rng.next() % 100).into())
        .collect();
    let mut buffer = buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap();

    buffer = black_box(buffer);
    b.iter(|| {
        let deserialized: Vec<VarInt64> =
            Readable::read_from_buffer_copying_data_with_ctx(Endianness::NATIVE, &buffer).unwrap();
        deserialized
    })
}

#[bench]
fn read_varint_always_eight_bytes(b: &mut Bencher) {
    use speedy::private::VarInt64;
    let mut rng = XorShift64 { a: 1234 };

    let buffer: Vec<VarInt64> = (0..1024 * 1024)
        .into_iter()
        .map(|_| ((rng.next() % 100) | (1 << 63)).into())
        .collect();
    let mut buffer = buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap();

    buffer = black_box(buffer);
    b.iter(|| {
        let deserialized: Vec<VarInt64> =
            Readable::read_from_buffer_copying_data_with_ctx(Endianness::NATIVE, &buffer).unwrap();
        deserialized
    })
}

#[bench]
fn write_varint_random(b: &mut Bencher) {
    use speedy::private::VarInt64;
    let mut rng = XorShift64 { a: 1234 };

    let buffer: Vec<VarInt64> = (0..1024 * 1024)
        .into_iter()
        .map(|_| (1_u64 << (rng.next() % 63)).into())
        .collect();
    let mut buffer = buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap();

    buffer = black_box(buffer);
    b.iter(|| buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap())
}

#[bench]
fn write_varint_always_one_byte(b: &mut Bencher) {
    use speedy::private::VarInt64;
    let mut rng = XorShift64 { a: 1234 };

    let buffer: Vec<VarInt64> = (0..1024 * 1024)
        .into_iter()
        .map(|_| (rng.next() % 100).into())
        .collect();
    let mut buffer = buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap();

    buffer = black_box(buffer);
    b.iter(|| buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap())
}

#[bench]
fn write_varint_always_eight_bytes(b: &mut Bencher) {
    use speedy::private::VarInt64;
    let mut rng = XorShift64 { a: 1234 };

    let buffer: Vec<VarInt64> = (0..1024 * 1024)
        .into_iter()
        .map(|_| ((rng.next() % 100) | (1 << 63)).into())
        .collect();
    let mut buffer = buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap();

    buffer = black_box(buffer);
    b.iter(|| buffer.write_to_vec_with_ctx(Endianness::NATIVE).unwrap())
}
