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

|                    Type |                            Serialized as |
| ----------------------- | ---------------------------------------- |
|                    `u8` |                                    as-is |
|                   `u16` |                                    as-is |
|                   `u32` |                                    as-is |
|                   `u64` |                                    as-is |
|                 `usize` |                                    `u64` |
|                    `i8` |                                    as-is |
|                   `i16` |                                    as-is |
|                   `i32` |                                    as-is |
|                   `i64` |                                    as-is |
|                   `f32` |                                    as-is |
|                   `f64` |                                    as-is |
|                  `bool` |                  `u8`, either `0` or `1` |
|                  `char` |                                    `u32` |
|                `String` |             `{length: u32, bytes: [u8]}` |
|          `Cow<'a, str>` |             `{length: u32, bytes: [u8]}` |
|                `Vec<T>` |             `{length: u32, values: [T]}` |
|          `Cow<'a, [T]>` |             `{length: u32, values: [T]}` |
|         `HashMap<K, V>` |          `{length: u32, values: [K, V]}` |
|        `BTreeMap<K, V>` |          `{length: u32, values: [K, V]}` |
|            `HashSet<T>` |             `{length: u32, values: [T]}` |
|           `BTreeSet<T>` |             `{length: u32, values: [T]}` |
|              `Range<T>` |                                 `(T, T)` |
|             `Option<T>` |                    `(1_u8, T)` or `0_u8` |
|                    `()` |                                  nothing |
|                   `(T)` |                                    as-is |
|                `(T, T)` |                                    as-is |
|            `(T, .., T)` |                                    as-is |
|                 `enum`s |                 `{tag: u32, variant: T}` |
|              `AtomicU8` |                                     `u8` |
|              `AtomicI8` |                                     `i8` |
|             `AtomicU16` |                                    `u16` |
|             `AtomicI16` |                                    `i16` |
|             `AtomicU32` |                                    `u32` |
|             `AtomicI32` |                                    `i32` |
|             `AtomicU64` |                                    `u64` |
|             `AtomicI64` |                                    `i64` |
|            `NonZeroU32` |                                    `u32` |
|    `std::net::Ipv4Addr` |                                    `u32` |
|    `std::net::Ipv6Addr` |                                   `u128` |
|      `std::net::IpAddr` |    `{is_ipv4: u8, value: {u32 or u128}}` |
|   `std::time::Duration` |         `{secs: u64, subsec_nanos: u32}` |
| `std::time::SystemTime` | `std::time::Duration` since `UNIX_EPOCH` |

These are stable and will not change in the future.

## Field attributes

### `#[speedy(length = ...)]`

Can be used on most standard containers to specify the field's length.
Can refer to any of the previous fields.

For example:

```rust
use speedy::{Readable, Writable};

#[derive(Readable, Writable)]
struct Struct {
    byte_count: u8,
    #[speedy(length = byte_count / 4)]
    data: Vec< u32 >
}
```

Before serializing you need to make sure that whatever is set as `length`
is equal to the `.len()` of the field; if it's not then you will get
an error when trying to serialize it.

Setting this attribute changes the serialization format as follows:


|             Type |                Serialized as |
| ---------------- | ---------------------------- |
|         `Vec<T>` |                        `[T]` |
|   `Cow<'a, [T]>` |                        `[T]` |
|         `String` |                       `[u8]` |
|   `Cow<'a, str>` |                       `[u8]` |
|  `HashMap<K, V>` |                     `[K, V]` |
| `BTreeMap<K, V>` |                     `[K, V]` |
|     `HashSet<T>` |                        `[T]` |
|    `BTreeSet<T>` |                        `[T]` |

### `#[speedy(length_type = ...)]`

Can be used to specify the exact size of the implicit length field of a container
as it is read or written.

Possible values:
  - `u7` (same as u8, but restricted to 7 bits for `u64_varint` compatibility)
  - `u8`
  - `u16`
  - `u32` (default)
  - `u64_varint`

### `#[speedy(skip)]`

Skips a given field when reading and writing.

### `#[speedy(default_on_eof)]`

If an EOF is encountered when reading this field its value will be set
to the default value for its type and the EOF will be ignored.

### `#[speedy(constant_prefix = ...)]`

Specifies a static string of bytes which will be written or has to be present
when reading before a given field.

## Enum attributes

### `#[speedy(tag_type = ...)]`

Can be used to specify the exact size of the enum's tag as it is read or written.

Possible values:
  - `u7` (same as u8, but restricted to 7 bits for `u64_varint` compatibility)
  - `u8`
  - `u16`
  - `u32` (default)
  - `u64_varint`

### `#[speedy(peek_tag)]`

An enum marked with this attribute will not consume its tag value when reading
from a stream, nor will it write its own tag when writing.

## Enum variant attributes

### `#[speedy(tag = ...)]`

Specifies a preset tag value to be used for a given enum variant.

## License

Licensed under either of

  * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
  * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
