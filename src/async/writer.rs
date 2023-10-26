use crate::{
    ahocorasick::AcAutomaton, automaton::StateID, Anchored, MatchError,
};
use alloc::{collections::VecDeque, sync::Arc, vec::Vec};
use core::task::Poll;
use futures::AsyncWrite;
use pin_project_lite::pin_project;

// Wrapper over an AsyncWrite. Writing to AhoCorasickAsyncWriter will write replaced results to the underlying writer
pin_project! {
    pub struct AhoCorasickAsyncWriter<'a, W, B> {
        #[pin]
        sink: W,
        aut: Arc<dyn AcAutomaton>,
        sid: StateID,
        replace_with: &'a [B],
        buffer: Vec<u8>, // Buffer holding the data that will be sent to the sink
        potential_buffer: VecDeque<u8>, // Buffer holding the start of a potential match
        pending_state: Option<PendingState> // If the underlying sink responded with Pending, we save the state
    }
}

struct PendingState {
    bytes_to_write: usize, // How much bytes to send from the buffer to the sink
    bytes_read: usize, // How much input bytes have been processed (previous buf.len basically)
}

impl<'a, W, B> AhoCorasickAsyncWriter<'a, W, B>
where
    W: AsyncWrite,
    B: AsRef<[u8]> + 'a,
{
    pub(crate) fn new(
        aut: Arc<dyn AcAutomaton>,
        sink: W,
        replace_with: &'a [B],
    ) -> Result<Self, MatchError>
    where
        W: AsyncWrite,
        B: AsRef<[u8]> + 'a,
    {
        let sid = aut.start_state(Anchored::No)?;
        Ok(AhoCorasickAsyncWriter {
            sink,
            aut,
            sid,
            replace_with,
            buffer: Vec::new(),
            potential_buffer: VecDeque::new(),
            pending_state: None,
        })
    }

    /// Writing to the buffer while making rare incremental resizes
    #[inline(always)]
    fn write_to_buffer(buf: &mut Vec<u8>, idx: &mut usize, char: u8) {
        if *idx >= buf.len() {
            // Since this function is called with incremental idx, we simply double current buffer length every time
            buf.resize(buf.len() * 2, b'\0');
        }
        buf[*idx] = char;
        *idx += 1;
    }
}

impl<'a, W, B> AsyncWrite for AhoCorasickAsyncWriter<'a, W, B>
where
    W: AsyncWrite,
    B: AsRef<[u8]> + 'a,
{
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        let this = self.project();
        if let Some(pending_state) = this.pending_state.take() {
            return match this
                .sink
                .poll_write(cx, &this.buffer[..pending_state.bytes_to_write])
            {
                Poll::Ready(_) => Poll::Ready(Ok(pending_state.bytes_read)),
                Poll::Pending => {
                    // Still not ready : put back PendingState, as it has been taken
                    *this.pending_state = Some(pending_state);
                    Poll::Pending
                }
            };
        }
        if this.buffer.len() < buf.len() + this.potential_buffer.len() {
            // Default buffer length to buf once to avoid incremental size increases & capacity reallocations during the buffer writing process
            this.buffer.resize(buf.len() + this.potential_buffer.len(), b'\0');
        }
        let mut write_idx = 0usize;
        for byte in buf {
            *this.sid = this.aut.next_state(Anchored::No, *this.sid, *byte);
            if this.aut.is_start(*this.sid) {
                // No potential replacements
                while this.potential_buffer.len() > 0 {
                    // At this point potential buffer is discareded (written)
                    Self::write_to_buffer(
                        this.buffer,
                        &mut write_idx,
                        this.potential_buffer.pop_front().unwrap(),
                    );
                }
                Self::write_to_buffer(this.buffer, &mut write_idx, *byte);
            } else {
                this.potential_buffer.push_back(*byte);
                if this.aut.is_match(*this.sid) {
                    let pattern_id = this.aut.match_pattern(*this.sid, 0);
                    let pattern_len = this.aut.pattern_len(pattern_id);
                    // Either we followed a potential word all the way down, or we jumped to a different branch following the suffix link
                    // In the second case, we need to discard (write away) first part of the potential buffer, as it will be bigger than the max match,
                    // keeping as new potential the last part containing the amount of bytes equal to the new state node depth (equal to the pattern_len)
                    while this.potential_buffer.len() > pattern_len {
                        Self::write_to_buffer(
                            this.buffer,
                            &mut write_idx,
                            this.potential_buffer.pop_front().unwrap(),
                        );
                    }

                    let replacement = this.replace_with[pattern_id].as_ref();
                    // Replacement is given by the automaton node, so we only need to clear the potential buffer
                    this.potential_buffer.clear();
                    for replaced_byte in replacement.iter() {
                        Self::write_to_buffer(
                            this.buffer,
                            &mut write_idx,
                            *replaced_byte,
                        );
                    }
                    // Reset the state after a replacement
                    *this.sid =
                        this.aut.start_state(Anchored::No).map_err(|e| {
                            std::io::Error::new(std::io::ErrorKind::Other, e)
                        })?;
                }
            }
        }
        // Now (unless buf was empty), either the bytes are in the buffer ready to be written, or they are in the potential buffer awaiting for the next chunk before being written
        // In both cases, all of them are considered "written" from the standpoint of AhoCorasickAsyncWriter, and we need to return not how many we have actually written to the sink with replacements,
        // but how many we have "consumed" - which should always match the length of input buf. So the return count is independent from write_idx
        if write_idx > 0 {
            match this.sink.poll_write(cx, &this.buffer[..write_idx]) {
                Poll::Ready(_) => Poll::Ready(Ok(buf.len())),
                Poll::Pending => {
                    // Tricky state : the sink is not yet ready to accept the buffer, but we have processed the chunk, including moving automaton state around
                    // So because don't want to redo the processing, we simply save the Pending state with current buffer & write idx,
                    // and on the next call at the beginning of this poll_write, this Pending state is handled
                    *this.pending_state = Some(PendingState {
                        bytes_to_write: write_idx,
                        bytes_read: buf.len(),
                    });
                    Poll::Pending
                }
            }
        } else if this.potential_buffer.len() > 0 {
            // Nothing written, but potential buffer is not empty - request immediate poll again with new buffer by saying we have accepted the buffer fully
            // This case happens when the potential buffer (replacement word length) exceeds the current chunk size while matching the entire chunk :
            // nothing can be written yet, but next chunk(s) are needed to determine the outcome (discard as-is, or replace)
            // Different to the Reader, here we cannot reply with Pending, as same bytes will be sent again - we have to acknowledge that we have consumed them
            Poll::Ready(Ok(buf.len()))
        } else {
            // This case can happen in 2 scenarios :
            // 1. Input buf is empty (most likely a bug on the consumer side)
            // 2. The contents of buf match entirely a word which has the empty string replacement. We still inform the consumer that we have "written" the bytes we received,
            //    even though we has nothing to write to the sink
            Poll::Ready(Ok(buf.len()))
        }
    }

    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        // Nothing special to do here
        self.project().sink.poll_flush(cx)
    }

    fn poll_close(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        let this = self.project();
        if this.potential_buffer.len() > 0 {
            // We have to ensure that potential buffer bytes are written, in case there was a beginning of a match at the end of the stream
            this.potential_buffer.make_contiguous();
            match this.sink.poll_write(cx, this.potential_buffer.as_slices().0)
            {
                Poll::Ready(_) => {
                    // Bytes have been written : empty potential_buffer, and ask for the next call to poll_close
                    this.potential_buffer.clear();
                    cx.waker().wake_by_ref();
                    Poll::Pending
                }
                Poll::Pending => Poll::Pending, // The last bytes can't be written yet, so poll_close will be called again when sink.poll_write is ready to make progress
            }
        } else {
            this.sink.poll_close(cx)
        }
    }
}
