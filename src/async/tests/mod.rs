use core::str::from_utf8;

use alloc::{vec::Vec, string::ToString};
use futures::{AsyncReadExt, AsyncWriteExt};

use crate::{AhoCorasick, r#async::tests::test_utils::{BytesAsyncReader, BytesAsyncWriter}};

mod test_utils;

#[test]
fn test_async() {
    futures::executor::block_on(async {
        for (_test_index, (source_string, replacement_tuples, expected_output)) in [
            (
                // Simple test
                "abc".repeat(100),
                Vec::from(
                    [
                        ("ab","AB"),
                    ]
                ),
                "ABc".repeat(100),
            ),
            (
                // Testing that last match, being a partial replacement in the potential buffer, is properly discarded as the stream ends
                "aaa".repeat(101),
                Vec::from(
                    [
                        ("aa","AA"),
                    ]
                ),
                "AAA".repeat(100) + "AAa"
            ),
            (
                // Testing replacing with empty string
                "pppabcsss".repeat(2),
                Vec::from(
                    [
                        ("abc",""),
                    ]
                ),
                "pppsss".repeat(2),
            ),
            (
                // Testing the discarding of partial  match
                "abcdef".repeat(100),
                Vec::from(
                    [
                        ("ab","ABAB"),
                        ("efg","EFG"),
                    ]
                ),
                "ABABcdef".repeat(100),
            ),
            (
                // Additional overlapping patterns
                "Now he is here, so is she. his hair is blond, her bag is big".to_string(),
                Vec::from(
                    [
                        ("he","she"),
                        ("she","he"),
                        ("his","her"),
                        ("her","his"), // This replacement should never occur, as 'he' will have priority
                        ("big","huge"),
                    ]
                ),
                "Now she is shere, so is he. her hair is blond, sher bag is huge".to_string(),
            ),
        ].iter().enumerate() {
            // Testing different buffer sizes to confirm the proper behavior of matches in consecutive chunks
            for test_buffer_size in [1,2,3,5,7,10,100,10000] {
                let (patterns, replacements): (Vec<_>, Vec<_>) = replacement_tuples.iter().map(|tuple| {
                    (tuple.0.as_bytes(), tuple.1.as_bytes())
                }).unzip();
                let ac = AhoCorasick::new(patterns).expect("Error building AhoCorasick");

                let mut buf: Vec<u8> = Vec::with_capacity(test_buffer_size);
                buf.resize(test_buffer_size, 0u8);
                {
                    // Testing the Reader : with and without forced_pending
                    for forced_pending in [0usize, 2] {
                        let reader = BytesAsyncReader::new(source_string.as_bytes().to_vec(), forced_pending);
                        let mut ac_reader = ac.async_reader(reader, &replacements)
                            .expect("Error get_reader");
    
                        let mut output: Vec<u8> = Vec::new();
                        loop {
                            match ac_reader.read(&mut buf).await {
                                Ok(size) => {
                                    if size == 0 {
                                        break;
                                    } else {
                                        output.extend(&buf[..size]);
                                    }
                                },
                                Err(err) => {
                                    panic!("BytesAsyncReader error : {}", err)
                                },
                            }
                        }
                        assert_eq!(from_utf8(&output).unwrap_or("<utf8 error>"), expected_output);
                    }
                }
                {
                    // Testing the Writer : with and without forced_pending
                    for forced_pending in [0usize, 2] {
                        let mut reader = BytesAsyncReader::new(source_string.as_bytes().to_vec(), 0);
                        let writer = BytesAsyncWriter::new(forced_pending);
                        let mut ac_writer = ac.async_writer(writer.clone(), &replacements)
                            .expect("Error get_writer");
    
                        loop {
                            match reader.read(&mut buf).await {
                                Ok(size) => {
                                    if size == 0 {
                                        ac_writer.close().await.unwrap();
                                        break;
                                    } else {
                                        ac_writer.write(&buf[..size]).await.unwrap();
                                    }
                                },
                                Err(err) => {
                                    panic!("BytesAsyncReader error : {}", err)
                                },
                            }
                        }
                        assert_eq!(from_utf8(&writer.sink.borrow()).unwrap_or("<utf8 error>"), expected_output);
                    }
                }
                {
                    for forced_pending in [0usize, 2] {
                        let mut reader = BytesAsyncReader::new(source_string.as_bytes().to_vec(), forced_pending);
                        let mut writer = BytesAsyncWriter::new(forced_pending);
                        
                        let result = ac.try_async_stream_replace_all(&mut reader, &mut writer, &replacements, test_buffer_size).await;
                        assert!(result.is_ok());
                        assert_eq!(from_utf8(&writer.sink.borrow()).unwrap_or("<utf8 error>"), expected_output);
                    }
                }
            }
        }
    });
}
