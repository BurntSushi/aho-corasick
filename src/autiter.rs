use std::io::{self, BufRead, Read};

use super::{ROOT_STATE, PatIdx, StateIdx};

/// An abstraction over automatons and their corresponding iterators.
pub trait Automaton: Sized {
    /// Return the next state given the current state and next character.
    fn next_state(&self, si: StateIdx, b: u8) -> StateIdx;

    /// Return true if and only if the given state and current pattern index
    /// indicate a match.
    fn has_match(&self, si: StateIdx, outi: PatIdx) -> bool;

    /// Build a match given the current state, pattern index and input index.
    fn get_match(&self, si: StateIdx, outi: PatIdx, texti: usize) -> Match;

    /// Attempt to skip through the input.
    ///
    /// This returns the index into `text` at which the next match attempt
    /// should start. (If no skipping occurred, then the return value should
    /// be equal to `at`.)
    ///
    /// Finally, if no match is possible, then return `text.len()`.
    fn skip_to(&self, si: StateIdx, text: &[u8], at: usize) -> usize;

    /// Returns true if and only if this automaton can skip through the input.
    fn is_skippable(&self) -> bool;

    /// Returns all of the patterns matched by this automaton.
    ///
    /// The order of the patterns is the order in which they were added.
    fn patterns(&self) -> &[String];

    /// Returns the pattern indexed at `i`.
    ///
    /// The index corresponds to the position at which the pattern was added
    /// to the automaton, starting at `0`.
    fn pattern(&self, i: usize) -> &str;

    /// Return the number of patterns in the automaton.
    #[inline]
    fn len(&self) -> usize {
        self.patterns().len()
    }

    /// Returns true if the automaton has no patterns.
    #[inline]
    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns an iterator of non-overlapping matches in `s`.
    fn find<'a, 's>(
        &'a self,
        s: &'s str,
    ) -> Matches<'a, 's, Self> {
        Matches {
            aut: self,
            text: s.as_bytes(),
            texti: 0,
            si: ROOT_STATE,
        }
    }

    /// Returns an iterator of overlapping matches in `s`.
    fn find_overlapping<'a, 's>(
        &'a self,
        s: &'s str,
    ) -> MatchesOverlapping<'a, 's, Self> {
        MatchesOverlapping {
            aut: self,
            text: s.as_bytes(),
            texti: 0,
            si: ROOT_STATE,
            outi: 0,
        }
    }

    /// Returns an iterator of non-overlapping matches in the given reader.
    fn stream_find<'a, R: io::Read>(
        &'a self,
        rdr: R,
    ) -> StreamMatches<'a, R, Self> {
        StreamMatches {
            aut: self,
            buf: io::BufReader::new(rdr),
            texti: 0,
            si: ROOT_STATE,
        }
    }

    /// Returns an iterator of overlapping matches in the given reader.
    fn stream_find_overlapping<'a, R: io::Read>(
        &'a self,
        rdr: R,
    ) -> StreamMatchesOverlapping<'a, R, Self> {
        StreamMatchesOverlapping {
            aut: self,
            buf: io::BufReader::new(rdr),
            texti: 0,
            si: ROOT_STATE,
            outi: 0,
        }
    }
}

/// Records a match in the search text.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Match {
    /// The pattern index.
    ///
    /// This corresponds to the ordering in which the matched pattern was
    /// added to the automaton, starting at `0`.
    pub pati: usize,
    /// The starting byte offset of the match in the search text.
    pub start: usize,
    /// The ending byte offset of the match in the search text.
    ///
    /// (This can be re-captiulated with `pati` and adding the pattern's
    /// length to `start`, but it is convenient to have it here.)
    pub end: usize,
}

/// An iterator of non-overlapping matches for in-memory text.
///
/// This iterator yields `Match` values.
///
/// `'a` is the lifetime of the automaton and `'s` is the lifetime of the
/// search text.
#[derive(Debug)]
pub struct Matches<'a, 's, A: 'a + Automaton> {
    aut: &'a A,
    text: &'s [u8],
    texti: usize,
    si: StateIdx,
}

impl<'a, 's, A: Automaton> Iterator for Matches<'a, 's, A> {
    type Item = Match;

    fn next(&mut self) -> Option<Match> {
        // When there's an initial lone start byte, it is usually worth it
        // to use `memchr` to skip along the input. The problem is that
        // the skipping function is called in the inner match loop, which
        // can be quite costly if the skipping condition is never met.
        // Therefore, we lift the case analysis outside of the main loop at
        // the cost of repeating code.
        if self.aut.is_skippable() {
            self.texti = self.aut.skip_to(self.si, self.text, self.texti);
            while self.texti < self.text.len() {
                self.si = self.aut.next_state(self.si, self.text[self.texti]);
                if self.aut.has_match(self.si, 0) {
                    let m = self.aut.get_match(self.si, 0, self.texti);
                    self.si = ROOT_STATE;
                    self.texti += 1;
                    return Some(m);
                }
                self.texti = self.aut.skip_to(
                    self.si, self.text, self.texti + 1);
            }
        } else {
            while self.texti < self.text.len() {
                self.si = self.aut.next_state(self.si, self.text[self.texti]);
                if self.aut.has_match(self.si, 0) {
                    let m = self.aut.get_match(self.si, 0, self.texti);
                    self.si = ROOT_STATE;
                    self.texti += 1;
                    return Some(m);
                }
                self.texti += 1;
            }
        }
        None
    }
}

/// An iterator of non-overlapping matches for streaming text.
///
/// This iterator yields `io::Result<Match>` values.
///
/// `'a` is the lifetime of the automaton and `R` is the type of the underlying
/// `io::Read`er.
#[derive(Debug)]
pub struct StreamMatches<'a, R, A: 'a + Automaton> {
    aut: &'a A,
    buf: io::BufReader<R>,
    texti: usize,
    si: StateIdx,
}

impl<'a, R: io::Read, A: Automaton> Iterator for StreamMatches<'a, R, A> {
    type Item = io::Result<Match>;

    fn next(&mut self) -> Option<io::Result<Match>> {
        let mut m = None;
        let mut consumed = 0;
'LOOP:  loop {
            self.buf.consume(consumed);
            let bs = match self.buf.fill_buf() {
                Err(err) => return Some(Err(err)),
                Ok(bs) if bs.len() == 0 => break,
                Ok(bs) => bs,
            };
            consumed = bs.len(); // is shortened if we find a match
            for (i, &b) in bs.iter().enumerate() {
                self.si = self.aut.next_state(self.si, b);
                if self.aut.has_match(self.si, 0) {
                    m = Some(Ok(self.aut.get_match(self.si, 0, self.texti)));
                    consumed = i + 1;
                    self.texti += 1;
                    self.si = ROOT_STATE;
                    break 'LOOP;
                }
                self.texti += 1;
            }
        }
        self.buf.consume(consumed);
        m
    }
}

/// An iterator of overlapping matches for in-memory text.
///
/// This iterator yields `Match` values.
///
/// `'a` is the lifetime of the automaton and `'s` is the lifetime of the
/// search text.
#[derive(Debug)]
pub struct MatchesOverlapping<'a, 's, A: 'a + Automaton> {
    aut: &'a A,
    text: &'s [u8],
    texti: usize,
    si: StateIdx,
    outi: usize,
}

impl<'a, 's, A: Automaton> Iterator for MatchesOverlapping<'a, 's, A> {
    type Item = Match;

    fn next(&mut self) -> Option<Match> {
        if self.aut.has_match(self.si, self.outi) {
            let m = self.aut.get_match(self.si, self.outi, self.texti);
            self.outi += 1;
            if !self.aut.has_match(self.si, self.outi) {
                self.texti += 1;
            }
            return Some(m);
        }

        self.outi = 0;
        self.texti = self.aut.skip_to(self.si, self.text, self.texti);
        while self.texti < self.text.len() {
            let b = self.text[self.texti];
            self.si = self.aut.next_state(self.si, b);
            if self.aut.has_match(self.si, self.outi) {
                return self.next();
            }
            self.texti = self.aut.skip_to(self.si, self.text, self.texti + 1);
        }
        None
    }
}

/// An iterator of overlapping matches for streaming text.
///
/// This iterator yields `io::Result<Match>` values.
///
/// `'a` is the lifetime of the automaton and `R` is the type of the underlying
/// `io::Read`er.
#[derive(Debug)]
pub struct StreamMatchesOverlapping<'a, R, A: 'a + Automaton> {
    aut: &'a A,
    buf: io::BufReader<R>,
    texti: usize,
    si: StateIdx,
    outi: usize,
}

impl<
    'a,
    R: io::Read,
    A: Automaton,
> Iterator for StreamMatchesOverlapping<'a, R, A> {
    type Item = io::Result<Match>;

    fn next(&mut self) -> Option<io::Result<Match>> {
        if self.aut.has_match(self.si, self.outi) {
            let m = self.aut.get_match(self.si, self.outi, self.texti);
            self.outi += 1;
            if !self.aut.has_match(self.si, self.outi) {
                self.texti += 1;
            }
            return Some(Ok(m));
        }
        let mut m = None;
        let mut consumed = 0;
        self.outi = 0;
'LOOP:  loop {
            self.buf.consume(consumed);
            let bs = match self.buf.fill_buf() {
                Err(err) => return Some(Err(err)),
                Ok(bs) if bs.len() == 0 => break,
                Ok(bs) => bs,
            };
            consumed = bs.len(); // is shortened if we find a match
            for (i, &b) in bs.iter().enumerate() {
                self.si = self.aut.next_state(self.si, b);
                if self.aut.has_match(self.si, self.outi) {
                    m = Some(Ok(self.aut.get_match(self.si, self.outi,
                                                   self.texti)));
                    consumed = i + 1;
                    self.outi += 1;
                    if !self.aut.has_match(self.si, self.outi) {
                        self.texti += 1;
                    }
                    break 'LOOP;
                }
                self.texti += 1;
            }
        }
        self.buf.consume(consumed);
        m
    }
}

