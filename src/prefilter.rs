use std::fmt;
use std::panic::{RefUnwindSafe, UnwindSafe};

use memchr::{memchr, memchr2, memchr3};

/// A prefilter describes the behavior of fast literal scanners for quickly
/// skipping past bytes in the haystack that we know cannot possibly
/// participate in a match.
pub trait Prefilter:
    Send + Sync + RefUnwindSafe + UnwindSafe + fmt::Debug
{
    /// Returns the next possible match candidate. This may yield false
    /// positives, so callers must confirm a match starting at the position
    /// returned. This, however, must never produce false negatives. That is,
    /// this must, at minimum, return the starting position of the next match
    /// in the given haystack after or at the given position.
    fn next_candidate(&self, haystack: &[u8], at: usize) -> Option<usize>;

    /// A method for cloning a prefilter, to work-around the fact that Clone
    /// is not object-safe.
    fn clone_prefilter(&self) -> Box<Prefilter>;
}

impl<'a, P: Prefilter + ?Sized> Prefilter for &'a P {
    #[inline]
    fn next_candidate(&self, haystack: &[u8], at: usize) -> Option<usize> {
        (**self).next_candidate(haystack, at)
    }

    fn clone_prefilter(&self) -> Box<Prefilter> {
        (**self).clone_prefilter()
    }
}

/// A convenience object for representing any type that implements Prefilter
/// and is cloneable.
#[derive(Debug)]
pub struct PrefilterObj(Box<Prefilter>);

impl Clone for PrefilterObj {
    fn clone(&self) -> Self {
        PrefilterObj(self.0.clone_prefilter())
    }
}

impl PrefilterObj {
    /// Create a new prefilter object.
    pub fn new<T: Prefilter + 'static>(t: T) -> PrefilterObj {
        PrefilterObj(Box::new(t))
    }

    /// Return the underlying prefilter trait object.
    pub fn as_ref(&self) -> &Prefilter {
        &*self.0
    }
}

/// PrefilterState tracks state associated with the effectiveness of a
/// prefilter. It is used to track how many bytes, on average, are skipped by
/// the prefilter. If this average dips below a certain threshold over time,
/// then the state renders the prefilter inert and stops using it.
///
/// A prefilter state should be created for each search. (Where creating an
/// iterator via, e.g., `find_iter`, is treated as a single search.)
#[derive(Clone, Debug)]
pub struct PrefilterState {
    /// The number of skips that has been executed.
    skips: usize,
    /// The total number of bytes that have been skipped.
    skipped: usize,
    /// The maximum length of a match. This is used to help determine how many
    /// bytes on average should be skipped in order for a prefilter to be
    /// effective.
    max_match_len: usize,
    /// Once this heuristic has been deemed ineffective, it will be inert
    /// throughout the rest of its lifetime. This serves as a cheap way to
    /// check inertness.
    inert: bool,
}

impl PrefilterState {
    /// The minimum number of skip attempts to try before considering whether
    /// a prefilter is effective or not.
    const MIN_SKIPS: usize = 40;

    /// The minimum amount of bytes that skipping must average, expressed as a
    /// factor of the multiple of the length of a possible match.
    ///
    /// That is, after MIN_SKIPS have occurred, if the average number of bytes
    /// skipped ever falls below MIN_AVG_FACTOR * max-match-length, then the
    /// prefilter outed to be rendered inert.
    const MIN_AVG_FACTOR: usize = 2;

    /// Create a fresh prefilter state.
    pub fn new(max_match_len: usize) -> PrefilterState {
        PrefilterState { skips: 0, skipped: 0, max_match_len, inert: false }
    }

    /// Update this state with the number of bytes skipped on the last
    /// invocation of the prefilter.
    #[inline]
    fn update(&mut self, skipped: usize) {
        self.skips += 1;
        self.skipped += skipped;
    }

    /// Return true if and only if this state indicates that a prefilter is
    /// still effective.
    #[inline]
    pub fn is_effective(&mut self) -> bool {
        if self.inert {
            return false;
        }
        if self.skips < PrefilterState::MIN_SKIPS {
            return true;
        }

        let min_avg = PrefilterState::MIN_AVG_FACTOR * self.max_match_len;
        if self.skipped >= min_avg * self.skips {
            return true;
        }

        // We're inert.
        self.inert = true;
        false
    }
}

/// A builder for constructing the best possible prefilter. When constructed,
/// this builder will heuristically select the best prefilter it can build,
/// if any, and discard the rest.
#[derive(Debug)]
pub struct Builder {
    count: usize,
    ascii_case_insensitive: bool,
    start_bytes: StartBytesBuilder,
}

impl Builder {
    /// Create a new builder for constructing the best possible prefilter.
    pub fn new() -> Builder {
        Builder {
            count: 0,
            ascii_case_insensitive: false,
            start_bytes: StartBytesBuilder::new(),
        }
    }

    /// Enable ASCII case insensitivity. When set, byte strings added to this
    /// builder will be interpreted without respect to ASCII case.
    pub fn ascii_case_insensitive(mut self, yes: bool) -> Builder {
        self.ascii_case_insensitive = yes;
        self.start_bytes = self.start_bytes.ascii_case_insensitive(yes);
        self
    }

    /// Return a prefilter suitable for quickly finding potential matches.
    ///
    /// All patterns added to an Aho-Corasick automaton should be added to this
    /// builder before attempting to construct the prefilter.
    pub fn build(&self) -> Option<PrefilterObj> {
        if self.count == 0 {
            return None;
        }
        self.start_bytes.build()
    }

    /// Add a literal string to this prefilter builder.
    pub fn add(&mut self, bytes: &[u8]) {
        self.count += 1;
        self.start_bytes.add(bytes);
    }
}

/// A builder for constructing a starting byte prefilter.
///
/// A starting byte prefilter is a simplistic prefilter that looks for possible
/// matches by reporting all positions corresponding to a particular byte. This
/// generally only takes affect when there are at most 3 distinct possible
/// starting bytes. e.g., the patterns `foo`, `bar`, and `baz` have two
/// distinct starting bytes (`f` and `b`), and this prefiler returns all
/// occurrences of either `f` or `b`.
///
/// In some cases, a heuristic frequency analysis may determine that it would
/// be better not to use this prefilter even when there are 3 or fewer distinct
/// starting bytes.
#[derive(Clone, Debug)]
struct StartBytesBuilder {
    ascii_case_insensitive: bool,
    byteset: Vec<bool>,
}

impl StartBytesBuilder {
    /// Create a new builder for constructing a start byte prefilter.
    fn new() -> StartBytesBuilder {
        StartBytesBuilder {
            ascii_case_insensitive: false,
            byteset: vec![false; 256],
        }
    }

    /// Enable ASCII case insensitivity. When set, byte strings added to this
    /// builder will be interpreted without respect to ASCII case.
    fn ascii_case_insensitive(mut self, yes: bool) -> StartBytesBuilder {
        self.ascii_case_insensitive = yes;
        self
    }

    /// Build the starting bytes prefilter.
    ///
    /// If there are more than 3 distinct starting bytes, or if heuristics
    /// otherwise determine that this prefilter should not be used, then `None`
    /// is returned.
    fn build(&self) -> Option<PrefilterObj> {
        let (mut bytes, mut len) = ([0; 3], 0);
        for b in 0..256 {
            if !self.byteset[b] {
                continue;
            }
            // We've exceeded our limit, so bail.
            if len == 3 {
                return None;
            }
            // We don't handle non-ASCII bytes for now. Getting non-ASCII
            // bytes right is trickier, since we generally don't want to put
            // a leading UTF-8 code unit into a prefilter that isn't ASCII,
            // since they can frequently. Instead, it would be better to use a
            // continuation byte, but this requires more sophisticated analysis
            // of the automaton and a richer prefilter API.
            if b > 0x7F {
                return None;
            }
            bytes[len] = b as u8;
            len += 1;
        }
        match len {
            0 => None,
            1 => Some(PrefilterObj::new(StartBytesOne { byte1: bytes[0] })),
            2 => Some(PrefilterObj::new(StartBytesTwo {
                byte1: bytes[0],
                byte2: bytes[1],
            })),
            3 => Some(PrefilterObj::new(StartBytesThree {
                byte1: bytes[0],
                byte2: bytes[1],
                byte3: bytes[2],
            })),
            _ => unreachable!(),
        }
    }

    /// Add a byte string to this builder.
    ///
    /// All patterns added to an Aho-Corasick automaton should be added to this
    /// builder before attempting to construct the prefilter.
    fn add(&mut self, bytes: &[u8]) {
        if let Some(&byte) = bytes.get(0) {
            self.byteset[byte as usize] = true;
            if self.ascii_case_insensitive {
                self.byteset[opposite_ascii_case(byte) as usize] = true;
            }
        }
    }
}

/// A prefilter for scanning for a single starting byte.
#[derive(Clone, Debug)]
struct StartBytesOne {
    byte1: u8,
}

impl Prefilter for StartBytesOne {
    fn next_candidate(&self, haystack: &[u8], at: usize) -> Option<usize> {
        memchr(self.byte1, &haystack[at..]).map(|i| at + i)
    }

    fn clone_prefilter(&self) -> Box<Prefilter> {
        Box::new(self.clone())
    }
}

/// A prefilter for scanning for two starting bytes.
#[derive(Clone, Debug)]
struct StartBytesTwo {
    byte1: u8,
    byte2: u8,
}

impl Prefilter for StartBytesTwo {
    fn next_candidate(&self, haystack: &[u8], at: usize) -> Option<usize> {
        memchr2(self.byte1, self.byte2, &haystack[at..]).map(|i| at + i)
    }

    fn clone_prefilter(&self) -> Box<Prefilter> {
        Box::new(self.clone())
    }
}

/// A prefilter for scanning for three starting bytes.
#[derive(Clone, Debug)]
struct StartBytesThree {
    byte1: u8,
    byte2: u8,
    byte3: u8,
}

impl Prefilter for StartBytesThree {
    fn next_candidate(&self, haystack: &[u8], at: usize) -> Option<usize> {
        memchr3(self.byte1, self.byte2, self.byte3, &haystack[at..])
            .map(|i| at + i)
    }

    fn clone_prefilter(&self) -> Box<Prefilter> {
        Box::new(self.clone())
    }
}

/// Return the next candidate reported by the given prefilter while
/// simultaneously updating the given prestate.
///
/// The caller is responsible for checking the prestate before deciding whether
/// to initiate a search.
#[inline]
pub fn next<P: Prefilter>(
    prestate: &mut PrefilterState,
    prefilter: P,
    haystack: &[u8],
    at: usize,
) -> Option<usize> {
    match prefilter.next_candidate(haystack, at) {
        None => {
            prestate.update(haystack.len() - at);
            None
        }
        Some(i) => {
            prestate.update(i - at);
            Some(i)
        }
    }
}

/// If the given byte is an ASCII letter, then return it in the opposite case.
/// e.g., Given `b'A'`, this returns `b'a'`, and given `b'a'`, this returns
/// `b'A'`. If a non-ASCII letter is given, then the given byte is returned.
pub fn opposite_ascii_case(b: u8) -> u8 {
    if b'A' <= b && b <= b'Z' {
        b.to_ascii_lowercase()
    } else if b'a' <= b && b <= b'z' {
        b.to_ascii_uppercase()
    } else {
        b
    }
}
