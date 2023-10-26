use core::task::Poll;
use alloc::{sync::Arc, vec::Vec, collections::VecDeque};
use futures::AsyncRead;
use pin_project_lite::pin_project;
use crate::{ahocorasick::AcAutomaton, Anchored, MatchError, automaton::StateID};

// Wrapper over an AsyncRead. Reading from AhoCorasickAsyncReader polls replaced results
pin_project! {
    pub struct AhoCorasickAsyncReader<'a, R, B> {
        #[pin]
        source: R,
        aut: Arc<dyn AcAutomaton>,
        sid: StateID,
        replace_with: &'a [B],
        buffer: Vec<u8>, // Used to buffer initially read bytes (before replacements)
        potential_buffer: VecDeque<u8>, // Buffer holding the start of a potential match
        pending_write_buffer: VecDeque<u8>, // Buffer holding the data ready to be written. Might need to wait until next chunk
    }
}

impl<'a, R, B> AhoCorasickAsyncReader<'a, R, B>
where
    R: AsyncRead,
    B: AsRef<[u8]> + 'a,
{
    pub(crate) fn new(aut: Arc<dyn AcAutomaton>, source: R, replace_with: &'a [B]) -> Result<Self, MatchError> {
        let sid = aut.start_state(Anchored::No)?;
        Ok(AhoCorasickAsyncReader {
            source,
            aut,
            sid,
            replace_with,
            buffer: Vec::new(),
            potential_buffer: VecDeque::new(),
            pending_write_buffer: VecDeque::new(),
        })
    }

    // Helper uniformizing method : writing to buffer with index. Does not check index boundary and may panic
    #[inline(always)]
    fn write_to_buffer(buf: &mut [u8], idx: &mut usize, char: u8) {
        buf[*idx] = char;
        *idx += 1;
    }
    // Helper uniformizing method : writes to the buffer at index, or pushes the char to the deque in case of buffer overflow
    #[inline(always)]
    fn write_to_buffer_overflow_deque(buf: &mut [u8], deque: &mut VecDeque<u8>, idx: &mut usize, char: u8) {
        if *idx < buf.len() {
            Self::write_to_buffer(buf, idx, char);
        } else {
            deque.push_back(char);
        }
    }
}

impl<'a, R, B> AsyncRead for AhoCorasickAsyncReader<'a, R, B>
where
    R: AsyncRead,
    B: AsRef<[u8]> + 'a,
{
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut [u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        let this = self.as_mut().project();
        if this.buffer.len() < buf.len() {
            this.buffer.resize(buf.len(), b'\0');
        }
        let mut write_idx: usize = 0;
        while this.pending_write_buffer.len() > 0 {
            // First, write pending buffer if any
            if write_idx < buf.len() {
                Self::write_to_buffer(buf, &mut write_idx, this.pending_write_buffer.pop_front().unwrap());
            } else {
                break;
            }
        }
        if write_idx >= buf.len() {
            // Pending buffer had enough data to fully fill buf => no need to poll from source, wait for next read
            return Poll::Ready(Ok(write_idx));
        }
        match this.source.poll_read(cx, this.buffer) {
            Poll::Ready(result) => {
                match result {
                    Ok(size) => {
                        if size == 0 {
                            // End reached - discard potential buffer
                            while this.potential_buffer.len() > 0 {
                                Self::write_to_buffer_overflow_deque(buf, this.pending_write_buffer, &mut write_idx, this.potential_buffer.pop_front().unwrap());
                            }
                        }
                        for byte in &this.buffer[..size] {
                            *this.sid = this.aut.next_state(Anchored::No, *this.sid, *byte);
                            if this.aut.is_start(*this.sid) {
                                // No potential replacements
                                while this.potential_buffer.len() > 0 {
                                    // At this point potential buffer is discareded (written)
                                    Self::write_to_buffer_overflow_deque(buf, this.pending_write_buffer, &mut write_idx, this.potential_buffer.pop_front().unwrap());
                                }
                                Self::write_to_buffer_overflow_deque(buf, this.pending_write_buffer, &mut write_idx, *byte);
                            } else {
                                this.potential_buffer.push_back(*byte);
                                if this.aut.is_match(*this.sid) {
                                    let pattern_id = this.aut.match_pattern(*this.sid, 0);
                                    let pattern_len = this.aut.pattern_len(pattern_id);
                                    // Either we followed a potential word all the way down, or we jumped to a different branch following the suffix link
                                    // In the second case, we need to discard (write away) first part of the potential buffer, as it will be bigger than the max match,
                                    // keeping as new potential the last part containing the amount of bytes equal to the new state node depth (equal to the pattern_len)
                                    while this.potential_buffer.len() > pattern_len {
                                        Self::write_to_buffer_overflow_deque(buf, this.pending_write_buffer, &mut write_idx, this.potential_buffer.pop_front().unwrap());
                                    }

                                    let replacement = this.replace_with[pattern_id].as_ref();
                                    // Replacement is given by the automaton node, so we only need to clear the potential buffer
                                    this.potential_buffer.clear();
                                    for replaced_byte in replacement.iter() {
                                        Self::write_to_buffer_overflow_deque(buf, this.pending_write_buffer, &mut write_idx, *replaced_byte);
                                    }
                                    // Reset the state after a replacement
                                    *this.sid = this.aut.start_state(Anchored::No)
                                        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
                                }
                            }
                        }
                        if write_idx > 0 {
                            // Something has been written
                            Poll::Ready(Ok(write_idx))
                        } else if size > 0 {
                            // Special cases handling : a non-empty chunk has been read from the source, however nothing has been written
                            // Identified cases where this might happen :
                            // 1. When the pattern exceeds the chunk size, and is fully buffered in potential_buffer waiting to be replaced or discarded
                            // 2. When the chunk fully matches a pattern, and the replacement is an empty string (very specific)
                            //
                            // We cannot respond with Ok(0), which would mean end of read, so we simply request a new poll immediately,
                            // and proceed reading more chunks from the source
                            cx.waker().wake_by_ref();
                            Poll::Pending
                        } else {
                            // Nothing left to write
                            Poll::Ready(Ok(0))
                        }
                    },
                    Err(err) => {
                        Poll::Ready(Err(err))
                    }
                }
            },
            Poll::Pending => {
                if write_idx > 0 {
                    // While waiting for the source, if some bytes have already been written from pending buffer, we can return them immediately to speed things up
                    Poll::Ready(Ok(write_idx))
                } else {
                    Poll::Pending
                }
            }
        }
    }
}