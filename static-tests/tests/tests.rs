use {
    static_test::{
        static_test
    },
    speedy::{
        Endianness,
        Readable,
        Writable,

        private::{
            ErrorKind,
            get_error_kind
        }
    }
};

#[static_test]
fn read_u16_from_buffer_when_buffer_length_is_known_and_is_big_enough( slice: &[u8] ) {
    assume!( slice.len() == 2 );
    static_assert!( u16::read_from_buffer_with_ctx( Endianness::NATIVE, slice ).is_ok() );
}

#[static_test]
fn read_u16_from_buffer_when_buffer_length_is_known_and_is_not_big_enough_1( slice: &[u8] ) {
    assume!( slice.len() == 1 );
    static_assert!( u16::read_from_buffer_with_ctx( Endianness::NATIVE, slice ).is_err() );
}

#[static_test]
fn read_u16_from_buffer_when_buffer_length_is_known_and_is_not_big_enough_2( slice: &[u8] ) {
    assume!( slice.len() == 1 );
    match u16::read_from_buffer_with_ctx( Endianness::NATIVE, slice ) {
        Ok( _ ) => static_unreachable!(),
        Err( error ) => {
            match get_error_kind( &error ) {
                ErrorKind::InputBufferIsTooSmall { actual_size, expected_size } => {
                    static_assert!( *actual_size == 1 );
                    static_assert!( *expected_size == 2 );
                },
                _ => static_unreachable!()
            }
        }
    }
}

#[static_test]
fn read_vec_u8_from_buffer_when_buffer_length_is_known_and_is_not_big_enough( slice: &[u8] ) {
    assume!( slice.len() == 3 );
    match Vec::< u8 >::read_from_buffer_with_ctx( Endianness::NATIVE, slice ) {
        Ok( _ ) => {},
        Err( error ) => {
            match get_error_kind( &error ) {
                ErrorKind::InputBufferIsTooSmall { actual_size, expected_size } => {
                    static_assert!( *actual_size == 3 );
                    static_assert!( *expected_size == 4 );
                },
                _ => static_unreachable!()
            }
        }
    }
}

#[static_test]
fn read_vec_u8_from_buffer_when_buffer_length_is_not_known( slice: &[u8] ) {
    match Vec::< u8 >::read_from_buffer_with_ctx( Endianness::NATIVE, slice ) {
        Ok( _ ) => {},
        Err( error ) => {
            match get_error_kind( &error ) {
                ErrorKind::InputBufferIsTooSmall { expected_size, .. } => {
                    static_assert!( *expected_size == 4 );
                },
                ErrorKind::UnexpectedEndOfInput => {},
                _ => static_unreachable!()
            }
        }
    }
}

#[static_test]
fn write_u16_to_buffer_when_buffer_length_is_known_and_there_is_enough_space( slice: &mut [u8], value: u16 ) {
    assume!( slice.len() == 2 );
    static_assert!( value.write_to_buffer_with_ctx( Endianness::NATIVE, slice ).is_ok() );
}

#[static_test]
fn write_u16_to_buffer_when_buffer_length_is_known_and_there_is_not_enough_space_1( slice: &mut [u8], value: u16 ) {
    assume!( slice.len() == 1 );
    static_assert!( value.write_to_buffer_with_ctx( Endianness::NATIVE, slice ).is_err() );
}

#[static_test]
fn write_u16_to_buffer_when_buffer_length_is_known_and_there_is_not_enough_space_2( slice: &mut [u8], value: u16 ) {
    assume!( slice.len() == 1 );
    match value.write_to_buffer_with_ctx( Endianness::NATIVE, slice ) {
        Ok( _ ) => static_unreachable!(),
        Err( error ) => {
            match get_error_kind( &error ) {
                ErrorKind::OutputBufferIsTooSmall { actual_size, expected_size } => {
                    static_assert!( *actual_size == 1 );
                    static_assert!( *expected_size == 2 );
                },
                _ => static_unreachable!()
            }
        }
    }
}

#[static_test]
fn write_vec_u8_to_buffer_when_both_lengths_are_known_and_there_is_enough_space( slice: &mut [u8], value: Vec< u8 > ) {
    assume!( slice.len() == 5 );
    assume!( value.len() == 1 );
    static_assert!( value.write_to_buffer_with_ctx( Endianness::NATIVE, slice ).is_ok() );
}

#[static_test]
fn write_vec_u8_to_buffer_when_both_lengths_are_known_and_there_is_not_enough_space_1( slice: &mut [u8], value: Vec< u8 > ) {
    assume!( slice.len() == 5 );
    assume!( value.len() == 2 );
    static_assert!( value.write_to_buffer_with_ctx( Endianness::NATIVE, slice ).is_err() );
}

#[static_test]
fn write_vec_u8_to_buffer_when_both_lengths_are_known_and_there_is_not_enough_space_2( slice: &mut [u8], value: Vec< u8 > ) {
    assume!( slice.len() == 5 );
    assume!( value.len() == 2 );
    match value.write_to_buffer_with_ctx( Endianness::NATIVE, slice ) {
        Ok( _ ) => static_unreachable!(),
        Err( error ) => {
            match get_error_kind( &error ) {
                ErrorKind::OutputBufferIsTooSmall { actual_size, expected_size } => {
                    static_assert!( *actual_size == 5 );
                    static_assert!( *expected_size == 6 );
                },
                _ => static_unreachable!()
            }
        }
    }
}

#[static_test]
fn write_vec_u8_to_buffer_when_buffer_length_is_known_and_there_is_not_enough_space_1( slice: &mut [u8], value: Vec< u8 > ) {
    assume!( slice.len() == 3 );
    static_assert!( value.write_to_buffer_with_ctx( Endianness::NATIVE, slice ).is_err() );
}

#[static_test]
fn write_vec_u8_to_buffer_when_buffer_length_is_known_and_there_is_not_enough_space_2( slice: &mut [u8], value: Vec< u8 > ) {
    assume!( slice.len() == 3 );
    match value.write_to_buffer_with_ctx( Endianness::NATIVE, slice ) {
        Ok( _ ) => static_unreachable!(),
        Err( error ) => {
            match get_error_kind( &error ) {
                ErrorKind::OutputBufferIsTooSmall { actual_size, .. } => {
                    static_assert!( *actual_size == 3 );
                }
                _ => static_unreachable!()
            }
        }
    }
}

#[static_test]
fn write_vec_u8_to_buffer_when_vec_length_is_known( slice: &mut [u8], value: Vec< u8 > ) {
    assume!( value.len() == 2 );
    match value.write_to_buffer_with_ctx( Endianness::NATIVE, slice ) {
        Ok( _ ) => {},
        Err( error ) => {
            match get_error_kind( &error ) {
                ErrorKind::OutputBufferIsTooSmall { expected_size, .. } => {
                    static_assert!( *expected_size == 6 );
                }
                _ => static_unreachable!()
            }
        }
    }
}

#[static_test]
fn write_vec_u8_to_buffer_when_no_lengths_are_known( slice: &mut [u8], value: Vec< u8 > ) {
    match value.write_to_buffer_with_ctx( Endianness::NATIVE, slice ) {
        Ok( _ ) => {},
        Err( error ) => {
            match get_error_kind( &error ) {
                ErrorKind::OutOfRangeLength => {},
                ErrorKind::OutputBufferIsTooSmall { .. } => {},
                _ => static_unreachable!()
            }
        }
    }
}
