use std::io::{self, BufRead};
use std::marker::PhantomData;

use super::{ROOT_STATE, StateIdx};

/// An abstraction over automatons and their corresponding iterators.
/// The type parameter `P` is the type of the pattern that was used to
/// construct this Automaton.
pub trait Automaton<P> {
    /// Return the next state given the current state and next character.
    fn next_state(&self, si: StateIdx, b: u8) -> StateIdx;

    /// Return true if and only if the given state and current pattern index
    /// indicate a match.
    fn has_match(&self, si: StateIdx, outi: usize) -> bool;

    /// Build a match given the current state, pattern index and input index.
    fn get_match(&self, si: StateIdx, outi: usize, texti: usize) -> Match;

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
    fn patterns(&self) -> &[P];

    /// Returns the pattern indexed at `i`.
    ///
    /// The index corresponds to the position at which the pattern was added
    /// to the automaton, starting at `0`.
    fn pattern(&self, i: usize) -> &P;

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
    fn find<'a, 's, Q: ?Sized + AsRef<[u8]>>(
        &'a self,
        s: &'s Q,
    ) -> Matches<'a, 's, P, Self>
    where Self: Sized {
        Matches {
            aut: self,
            text: s.as_ref(),
            texti: 0,
            si: ROOT_STATE,
            _m: PhantomData,
        }
    }

    /// Returns an iterator of overlapping matches in `s`.
    fn find_overlapping<'a, 's, Q: ?Sized + AsRef<[u8]>>(
        &'a self,
        s: &'s Q,
    ) -> MatchesOverlapping<'a, 's, P, Self>
    where Self: Sized {
        MatchesOverlapping {
            aut: self,
            text: s.as_ref(),
            texti: 0,
            si: ROOT_STATE,
            outi: 0,
            _m: PhantomData,
        }
    }

    /// Returns an iterator of non-overlapping matches in the given reader.
    fn stream_find<'a, R: io::Read>(
        &'a self,
        rdr: R,
    ) -> StreamMatches<'a, R, P, Self>
    where Self: Sized {
        StreamMatches {
            aut: self,
            buf: io::BufReader::new(rdr),
            texti: 0,
            si: ROOT_STATE,
            _m: PhantomData,
        }
    }

    /// Returns an iterator of overlapping matches in the given reader.
    fn stream_find_overlapping<'a, R: io::Read>(
        &'a self,
        rdr: R,
    ) -> StreamMatchesOverlapping<'a, R, P, Self>
    where Self: Sized {
        StreamMatchesOverlapping {
            aut: self,
            buf: io::BufReader::new(rdr),
            texti: 0,
            si: ROOT_STATE,
            outi: 0,
            _m: PhantomData,
        }
    }
}

impl<'a, P: AsRef<[u8]>, A: 'a + Automaton<P> + ?Sized>
        Automaton<P> for &'a A {
    fn next_state(&self, si: StateIdx, b: u8) -> StateIdx {
        (**self).next_state(si, b)
    }

    fn has_match(&self, si: StateIdx, outi: usize) -> bool {
        (**self).has_match(si, outi)
    }

    fn skip_to(&self, si: StateIdx, text: &[u8], at: usize) -> usize {
        (**self).skip_to(si, text, at)
    }

    fn is_skippable(&self) -> bool {
        (**self).is_skippable()
    }

    fn patterns(&self) -> &[P] {
        (**self).patterns()
    }

    fn pattern(&self, i: usize) -> &P {
        (**self).pattern(i)
    }

    fn get_match(&self, si: StateIdx, outi: usize, texti: usize) -> Match {
        (**self).get_match(si, outi, texti)
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
/// `'a` is the lifetime of the automaton, `'s` is the lifetime of the
/// search text, and `P` is the type of the Automaton's pattern.
#[derive(Debug)]
pub struct Matches<'a, 's, P, A: 'a + Automaton<P> + ?Sized> {
    aut: &'a A,
    text: &'s [u8],
    texti: usize,
    si: StateIdx,
    _m: PhantomData<P>,
}

// When there's an initial lone start byte, it is usually worth it
// to use `memchr` to skip along the input. The problem is that
// the skipping function is called in the inner match loop, which
// can be quite costly if the skipping condition is never met.
// Therefore, we lift the case analysis outside of the inner loop at
// the cost of repeating code.
//
// `step_to_match` is the version of the inner loop without skipping,
// and `skip_to_match` is the version with skipping.
fn step_to_match<P, A: Automaton<P> + ?Sized>(
    aut: &A,
    text: &[u8],
    mut texti: usize,
    mut si: StateIdx
) -> Option<(usize, StateIdx)> {
    while texti < text.len() {
        si = aut.next_state(si, text[texti]);
        if aut.has_match(si, 0) {
            return Some((texti, si));
        }
        texti += 1;
    }
    None
}

fn skip_to_match<P, A: Automaton<P> + ?Sized>(
    aut: &A,
    text: &[u8],
    mut texti: usize,
    mut si: StateIdx
) -> Option<(usize, StateIdx)> {
    texti = aut.skip_to(si, text, texti);
    while texti < text.len() {
        si = aut.next_state(si, text[texti]);
        if aut.has_match(si, 0) {
            return Some((texti, si));
        }
        texti = aut.skip_to(si, text, texti + 1);
    }
    None
}

impl<'a, 's, P, A: Automaton<P> + ?Sized> Iterator for Matches<'a, 's, P, A> {
    type Item = Match;

    fn next(&mut self) -> Option<Match> {
        if self.aut.is_skippable() {
            let skip = skip_to_match(self.aut, self.text, self.texti, self.si);
            if let Some((texti, si)) = skip {
                self.texti = texti + 1;
                self.si = ROOT_STATE;
                return Some(self.aut.get_match(si, 0, texti));
            }
        } else {
            let step = step_to_match(self.aut, self.text, self.texti, self.si);
            if let Some((texti, si)) = step {
                self.texti = texti + 1;
                self.si = ROOT_STATE;
                return Some(self.aut.get_match(si, 0, texti));
            }
        }
        None
    }
}

/// An iterator of non-overlapping matches for streaming text.
///
/// This iterator yields `io::Result<Match>` values.
///
/// `'a` is the lifetime of the automaton, `R` is the type of the underlying
/// `io::Read`er, and P is the type of the Automaton's pattern.
#[derive(Debug)]
pub struct StreamMatches<'a, R, P, A: 'a + Automaton<P> + ?Sized> {
    aut: &'a A,
    buf: io::BufReader<R>,
    texti: usize,
    si: StateIdx,
    _m: PhantomData<P>,
}

impl<'a, R: io::Read, P, A: Automaton<P>>
        Iterator for StreamMatches<'a, R, P, A> {
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
/// `'a` is the lifetime of the automaton, `'s` is the lifetime of the
/// search text, and `P` is the type of the Automaton's pattern.
#[derive(Debug)]
pub struct MatchesOverlapping<'a, 's, P, A: 'a + Automaton<P> + ?Sized> {
    aut: &'a A,
    text: &'s [u8],
    texti: usize,
    si: StateIdx,
    outi: usize,
    _m: PhantomData<P>,
}

impl<'a, 's, P, A: Automaton<P> + ?Sized>
        Iterator for MatchesOverlapping<'a, 's, P, A> {
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
        if self.aut.is_skippable() {
            let skip = skip_to_match(self.aut, self.text, self.texti, self.si);
            if let Some((texti, si)) = skip {
                self.texti = texti;
                self.si = si;
                return self.next();
            }
        } else {
            let step = step_to_match(self.aut, self.text, self.texti, self.si);
            if let Some((texti, si)) = step {
                self.texti = texti;
                self.si = si;
                return self.next();
            }
        }
        None
    }
}

/// An iterator of overlapping matches for streaming text.
///
/// This iterator yields `io::Result<Match>` values.
///
/// `'a` is the lifetime of the automaton, `R` is the type of the underlying
/// `io::Read`er, and P is the type of the Automaton's pattern.
#[derive(Debug)]
pub struct StreamMatchesOverlapping<'a, R, P, A: 'a + Automaton<P> + ?Sized> {
    aut: &'a A,
    buf: io::BufReader<R>,
    texti: usize,
    si: StateIdx,
    outi: usize,
    _m: PhantomData<P>,
}

impl<'a, R: io::Read, P, A: Automaton<P> + ?Sized>
        Iterator for StreamMatchesOverlapping<'a, R, P, A> {
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
                    m = Some(Ok(self.aut.get_match(
                        self.si, self.outi, self.texti)));
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
