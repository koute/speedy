use speedy::{Endianness, Readable};
use std::io;

#[derive(Debug, Readable, PartialEq, Eq)]
struct Test {
    #[speedy(default_on_eof)]
    list: Vec<u8>,
    #[speedy(default_on_eof)]
    value_kind: ValueKind,
}

#[derive(Debug, Default, Readable, PartialEq, Eq)]
#[speedy(tag_type = u8)]
enum ValueKind {
    #[default]
    Default = 0,
    Other = 1,
}

#[test]
fn read_from_consistent_results() {
    let test_buffer: Vec<u8> = vec![
        0, 32, 0, 0, // list length (8192 or 8 * 1024)
        // list data (missing)
        2, // value_kind
    ];

    let test_stream = io::Cursor::new(&test_buffer);

    let mut ctx = Endianness::LittleEndian;

    // BufferReader
    let (result_buffer, _) = Test::read_with_length_from_buffer_with_ctx(ctx, &test_buffer);

    dbg!(&result_buffer);
    assert!(
        result_buffer.is_err(),
        "expected `result_buffer` to be an error"
    );

    // CopyingBufferReader
    let (result_buffer_copying, _) =
        Test::read_with_length_from_buffer_copying_data_with_ctx_mut(&mut ctx, &test_buffer);

    dbg!(&result_buffer_copying);
    assert!(
        result_buffer_copying.is_err(),
        "expected `result_buffer_copying` to be an error"
    );

    // StreamReader(is_buffering = false)
    let result_stream_unbuffered =
        Test::read_from_stream_unbuffered_with_ctx(ctx, test_stream.clone());

    dbg!(&result_stream_unbuffered);
    assert!(
        result_stream_unbuffered.is_err(),
        "expected `result_stream_unbuffered` to be an error"
    );

    // StreamReader(is_buffering = true)
    let result_stream_buffered = Test::read_from_stream_buffered_with_ctx(ctx, test_stream);

    dbg!(&result_stream_buffered);
    assert!(
        result_stream_buffered.is_err(),
        "expected `result_stream_buffered` to be an error"
    );
}
