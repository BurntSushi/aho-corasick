use core::{task::Poll, cell::RefCell};

use alloc::{vec::Vec, rc::Rc};
use futures::{AsyncRead, AsyncWrite};

/// Simple AsyncReader from a Vec<u8>
pub struct BytesAsyncReader {
    source: Vec<u8>,
    cursor: usize,
    forced_pending: usize, // forced_pending represents the number of times this Reader will answer Pending for each poll_read (for testing repeated calls)
    forced_pending_counter: usize,
}

/// Simple AsyncWriter from a Vec<u8>
pub struct BytesAsyncWriter {
    pub sink: Rc<RefCell<Vec<u8>>>,
    forced_pending: usize, // forced_pending represents the number of times this Writer will answer Pending for each poll_write (for testing repeated calls)
    forced_pending_counter: usize,
}

impl BytesAsyncReader {
    pub fn new(source: Vec<u8>, forced_pending: usize) -> Self {
        Self {
            source,
            cursor: 0,
            forced_pending,
            forced_pending_counter: 0
        }
    }
}

impl AsyncRead for BytesAsyncReader {
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut [u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        if self.forced_pending_counter < self.forced_pending {
            self.forced_pending_counter += 1;
            cx.waker().wake_by_ref();
            return Poll::Pending;
        }
        self.forced_pending_counter = 0; // Reset the counter, to simulate Pending responses for every poll
        if self.cursor >= self.source.len() {
            return Poll::Ready(Ok(0));
        }
        let source_from_cursor = &self.source[self.cursor..];
        let remaining_len = source_from_cursor.len();
        let buf_len = buf.len();
        if buf_len >= remaining_len {
            buf[..remaining_len].copy_from_slice(source_from_cursor);
            self.cursor += remaining_len;
            Poll::Ready(Ok(remaining_len))
        } else {
            buf[..].copy_from_slice(&source_from_cursor[..buf_len]);
            self.cursor += buf_len;
            Poll::Ready(Ok(buf.len()))
        }
    }
}

impl BytesAsyncWriter {
    pub fn new(forced_pending: usize) -> Self {
        Self {
            sink: Rc::new(RefCell::new(Vec::new())),
            forced_pending,
            forced_pending_counter: 0
        }
    }
}

impl Clone for BytesAsyncWriter {
    fn clone(&self) -> Self {
        Self {
            sink: Rc::clone(&self.sink),
            forced_pending: self.forced_pending,
            forced_pending_counter: 0
        }
    }
}

impl AsyncWrite for BytesAsyncWriter {
    fn poll_write(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        if self.forced_pending_counter < self.forced_pending {
            self.forced_pending_counter += 1;
            cx.waker().wake_by_ref();
            return Poll::Pending;
        }
        self.forced_pending_counter = 0; // Reset the counter, to simulate Pending responses for every poll
        self.sink.borrow_mut().extend(buf);
        Poll::Ready(Ok(buf.len()))
    }

    fn poll_flush(self: std::pin::Pin<&mut Self>, _cx: &mut std::task::Context<'_>) -> Poll<std::io::Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_close(self: std::pin::Pin<&mut Self>, _cx: &mut std::task::Context<'_>) -> Poll<std::io::Result<()>> {
        Poll::Ready(Ok(()))
    }
}