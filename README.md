[![Build Status](https://api.travis-ci.org/koute/speedy.svg)](https://travis-ci.org/koute/speedy)

# A fast binary serialization framework

[![Documentation](https://docs.rs/speedy/badge.svg)](https://docs.rs/speedy/*/speedy/)

The goal of this crate is to provide fast, simple and easy binary serialization.

## Benchmarks

See [serde-bench](https://github.com/koute/serde-bench) for benchmarks.

## Example

```rust
use std::borrow::Cow;
use speedy::{Readable, Writable, Endianness};

#[derive(PartialEq, Debug, Readable, Writable)]
enum Enum {
    A,
    B,
    C,
}

#[derive(PartialEq, Debug, Readable, Writable)]
struct Struct< 'a > {
    number: u64,
    string: String,
    vector: Vec< u8 >,
    cow: Cow< 'a, [i64] >,
    float: f32,
    enumeration: Enum
}

fn main() {
    let original = Struct {
        number: 0x12345678ABCDEF00,
        string: "A totally pointless string".to_owned(),
        vector: vec![ 1, 2, 3 ],
        cow: Cow::Borrowed( &[ 4, 5, 6 ] ),
        float: 3.1415,
        enumeration: Enum::C
    };

    let endian = Endianness::LittleEndian;
    let bytes = original.write_to_vec_with_ctx( endian ).unwrap();
    let deserialized: Struct =
        Struct::read_from_buffer_with_ctx( endian, &bytes ).unwrap();

    assert_eq!( original, deserialized );
}
```

## Supported types

Out-of-box the following types are supported:

|           Type |                Serialized as |
| -------------- | ---------------------------- |
|           `u8` |                        as-is |
|          `u16` |                        as-is |
|          `u32` |                        as-is |
|          `u64` |                        as-is |
|        `usize` |                        `u64` |
|           `i8` |                        as-is |
|          `i16` |                        as-is |
|          `i32` |                        as-is |
|          `i64` |                        as-is |
|          `f32` |                        as-is |
|          `f64` |                        as-is |
|         `bool` |      `u8`, either `0` or `1` |
|       `String` | `{length: u32, bytes: [u8]}` |
| `Cow<'a, str>` | `{length: u32, bytes: [u8]}` |
|       `Vec<T>` | `{length: u32, values: [T]}` |
| `Cow<'a, [T]>` | `{length: u32, values: [T]}` |
|     `Range<T>` |                     `(T, T)` |
|    `Option<T>` |        `(1_u8, T)` or `0_u8` |
|           `()` |                      nothing |
|          `(T)` |                        as-is |
|       `(T, T)` |                        as-is |
|   `(T, .., T)` |                        as-is |
|        `enum`s |     `{tag: u32, variant: T}` |

These are stable and will not change in the future.

## Attributes

### `#[speedy(count = ...)]`

Can be used on a `Vec<T>` or on a `Cow<'a, [T]>` to specify
the field's length. Can refer to any of the previous fields.

For example:

```rust
use speedy::{Readable, Writable};

#[derive(Readable, Writable)]
struct Struct {
    byte_count: u8,
    #[speedy(count = byte_count / 4)]
    data: Vec< u32 >
}
```

Before serializing you need to make sure that whatever is set as `count`
is equal to the `.len()` of the field; if it's not then you will get
an error when trying to serialize it.

Setting this attribute changes the serialization format as follows:


|           Type |                Serialized as |
| -------------- | ---------------------------- |
|       `Vec<T>` |                        `[T]` |
| `Cow<'a, [T]>` |                        `[T]` |

## License

Licensed under either of

  * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
  * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
