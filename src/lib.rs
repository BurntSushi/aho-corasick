/*!
An implementation of the
[Aho-Corasick string search algorithm](https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_string_matching_algorithm).

The Aho-Corasick algorithm is principally useful when you need to search many
large texts for a fixed (possibly large) set of keywords. In particular, the
Aho-Corasick algorithm preprocesses the set of keywords by constructing a
finite state machine. The search phase is then a quick linear scan through the
text. Each character in the search text causes a state transition in the
automaton. Matches are reported when the automaton enters a match state.

# Examples

The main type exposed by this crate is `AcAutomaton`, which can be constructed
from an iterator of pattern strings:

```rust
use aho_corasick::AcAutomaton;

let aut = AcAutomaton::new(vec!["apple", "maple"]);

// AcAutomaton also implements `FromIterator`:
let aut: AcAutomaton = ["apple", "maple"].iter().cloned().collect();
```

Finding matches can be done with `find`:

```rust
use aho_corasick::{AcAutomaton, Match};

let aut = AcAutomaton::new(vec!["apple", "maple"]);
let mut it = aut.find("I like maple apples.");
assert_eq!(it.next(), Some(Match {
    pati: 1,
    start: 7,
    end: 12,
}));
assert_eq!(it.next(), Some(Match {
    pati: 0,
    start: 13,
    end: 18,
}));
assert_eq!(it.next(), None);
```

Use `find_overlapping` if you want to report all matches, even if they
overlap with each other.

```rust
use aho_corasick::{AcAutomaton, Match};

let aut = AcAutomaton::new(vec!["abc", "a"]);
let matches: Vec<_> = aut.find_overlapping("abc").collect();
assert_eq!(matches, vec![
    Match { pati: 1, start: 0, end: 1}, Match { pati: 0, start: 0, end: 3 },
]);

// Regular `find` will report only one match:
let matches: Vec<_> = aut.find("abc").collect();
assert_eq!(matches, vec![Match { pati: 1, start: 0, end: 1}]);
```

Finally, there are also methods for finding matches on *streams*. Namely, the
search text does not have to live in memory. It's useful to run this on files
that can't fit into memory:

```no_run
use std::fs::File;

use aho_corasick::AcAutomaton;

let aut = AcAutomaton::new(vec!["foo", "bar", "baz"]);
let rdr = File::open("search.txt").unwrap();
for m in aut.stream_find(rdr) {
    let m = m.unwrap(); // could be an IO error
    println!("Pattern '{}' matched at: ({}, {})",
             aut.pattern(m.pati), m.start, m.end);
}
```

There is also `stream_find_overlapping`, which is just like `find_overlapping`,
but it operates on streams.

Please see `dict-search.rs` in this crate's `examples` directory for a more
complete example. It creates a large automaton from a dictionary and can do a
streaming match over arbitrarily large data.

# Memory usage

A key aspect of an Aho-Corasick implementation is how the state transitions
are represented. The easiest way to make the automaton fast is to store a
dense 256-slot map in each state. It maps an input byte to a state index.
This makes the matching loop extremely fast, since it translates to a simple
pointer read.

The problem is that as the automaton accumulates more states, you end up paying
a `256 * 4` (`4` is for the `u32` state index) byte penalty for every state
regardless of how many transitions it has.

To solve this, only states near the root of the automaton have this sparse
map representation. States near the leaves of the automaton use a dense mapping
that requires a linear scan.

(The specific limit currently set is `3`, so that states with a depth less than
or equal to `3` are less memory efficient. The result is that the memory usage
of the automaton stops growing rapidly past ~60MB, even for automatons with
thousands of patterns.)
*/

#[deny(missing_docs)]

extern crate memchr;
#[cfg(test)] extern crate quickcheck;
#[cfg(test)] extern crate rand;

use std::collections::VecDeque;
use std::fmt;
use std::io::{self, BufRead, Read};
use std::iter::FromIterator;

use memchr::memchr;

/// The integer type used for the state index.
///
/// Limiting this to 32 bit integers can have a big impact on memory usage
/// when using the `Sparse` transition representation.
pub type StateIdx = u32;

type PatIdx = usize;

// Constants for special state indexes.
const FAIL_STATE: u32 = 0;
const ROOT_STATE: u32 = 1;

// Limit the depth at which we use a dense alphabet map. Once the limit is
// reached, a sparse set is used (and lookup becomes O(n)).
//
// This does have a performance hit, but the (straight forward) alternative
// is to have a `256 * 4` byte overhead for every state.
// Given that Aho-Corasick is typically used for dictionary searching, this
// can lead to dramatic memory bloat.
//
// This limit should only be increased at your peril. Namely, in the worst
// case, `256^DENSE_DEPTH_THRESHOLD * 4` corresponds to the memory usage in
// bytes. A value of `3` gives us a solid cap at around 64MB, which works
// well in practice even for dictionary sized automatons.
//
// Why not make this user configurable? Well, it doesn't make much sense
// because we pay for it with case analysis in the matching loop. Increasing it
// doesn't have much impact on performance (outside of pathological cases?).
//
// There is probably room for adding a new automaton type that doesn't have
// this restriction.
//
// N.B. Someone else seems to have discovered an alternative, but I haven't
// grokked it yet: https://github.com/mischasan/aho-corasick
const DENSE_DEPTH_THRESHOLD: u32 = 3;

/// An Aho-Corasick finite automaton.
#[derive(Clone)]
pub struct AcAutomaton<T=Dense> {
    pats: Vec<String>,
    states: Vec<State<T>>,
    start_bytes: Vec<u8>,
}

#[derive(Clone)]
struct State<T> {
    out: Vec<PatIdx>,
    fail: StateIdx,
    goto: T,
    depth: u32,
}

impl AcAutomaton {
    /// Create a new automaton from an iterator of patterns.
    ///
    /// The patterns must be convertible to Unicode `String` values via the
    /// `Into` trait.
    pub fn new<S, I>(pats: I) -> AcAutomaton<Dense>
            where S: Into<String>, I: IntoIterator<Item=S> {
        AcAutomaton::with_transitions(pats)
    }
}

impl<T: Transitions> AcAutomaton<T> {
    /// Create a new automaton from an iterator of patterns.
    ///
    /// This constructor allows one to choose the transition representation.
    ///
    /// The patterns must be convertible to Unicode `String` values via the
    /// `Into` trait.
    pub fn with_transitions<S, I>(pats: I) -> AcAutomaton<T>
            where S: Into<String>, I: IntoIterator<Item=S> {
        AcAutomaton {
            pats: vec![], // filled in later, avoid wrath of borrow checker
            states: vec![State::new(0), State::new(0)], // empty and root
            start_bytes: vec![], // also filled in later
        }.build(pats.into_iter().map(Into::into).collect())
    }

    /// Returns an iterator of non-overlapping matches in `s`.
    pub fn find<'a, 's>(&'a self, s: &'s str) -> Matches<'a, 's, T> {
        Matches {
            aut: self,
            text: s.as_bytes(),
            texti: 0,
            si: ROOT_STATE,
        }
    }

    /// Returns an iterator of overlapping matches in `s`.
    pub fn find_overlapping<'a, 's>(
        &'a self,
        s: &'s str,
    ) -> MatchesOverlapping<'a, 's, T> {
        MatchesOverlapping {
            aut: self,
            text: s.as_bytes(),
            texti: 0,
            si: ROOT_STATE,
            outi: 0,
        }
    }

    /// Returns an iterator of non-overlapping matches in the given reader.
    pub fn stream_find<'a, R: io::Read>(
        &'a self,
        rdr: R,
    ) -> StreamMatches<'a, R, T> {
        StreamMatches {
            aut: self,
            buf: io::BufReader::new(rdr),
            texti: 0,
            si: ROOT_STATE,
        }
    }

    /// Returns an iterator of overlapping matches in the given reader.
    pub fn stream_find_overlapping<'a, R: io::Read>(
        &'a self,
        rdr: R,
    ) -> StreamMatchesOverlapping<'a, R, T> {
        StreamMatchesOverlapping {
            aut: self,
            buf: io::BufReader::new(rdr),
            texti: 0,
            si: ROOT_STATE,
            outi: 0,
        }
    }

    /// Returns the pattern indexed at `i`.
    ///
    /// The index corresponds to the position at which the pattern was added
    /// to the automaton, starting at `0`.
    pub fn pattern(&self, i: usize) -> &str {
        &self.pats[i]
    }

    /// Return the number of patterns in the automaton.
    pub fn len(&self) -> usize {
        self.pats.len()
    }

    /// Returns true if the automaton has no patterns.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn next_state(&self, mut si: StateIdx, b: u8) -> StateIdx {
        loop {
            let maybe_si = self.states[si as usize].goto(b);
            if maybe_si != FAIL_STATE {
                si = maybe_si;
                break;
            } else {
                si = self.states[si as usize].fail;
            }
        }
        si
    }

    fn get_match(&self, si: StateIdx, outi: usize, texti: usize) -> Match {
        let pati = self.states[si as usize].out[outi];
        let patlen = self.pats[pati].len();
        let start = texti + 1 - patlen;
        Match {
            pati: pati,
            start: start,
            end: start + patlen,
        }
    }

    fn has_match(&self, si: StateIdx, outi: usize) -> bool {
        outi < self.states[si as usize].out.len()
    }

    fn skip_to(&self, si: StateIdx, text: &[u8], at: usize) -> usize {
        if si != ROOT_STATE || self.start_bytes.len() != 1 {
            return at;
        }
        let b = self.start_bytes[0];
        match memchr(b, &text[at..]) {
            None => text.len(),
            Some(i) => at + i,
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
pub struct Matches<'a, 's, T: 'a + Transitions> {
    aut: &'a AcAutomaton<T>,
    text: &'s [u8],
    texti: usize,
    si: StateIdx,
}

impl<'a, 's, T: Transitions> Iterator for Matches<'a, 's, T> {
    type Item = Match;

    fn next(&mut self) -> Option<Match> {
        self.texti = self.aut.skip_to(self.si, self.text, self.texti);
        while self.texti < self.text.len() {
            let b = self.text[self.texti];
            self.si = self.aut.next_state(self.si, b);
            if self.aut.has_match(self.si, 0) {
                let m = self.aut.get_match(self.si, 0, self.texti);
                self.texti += 1;
                self.si = ROOT_STATE;
                return Some(m);
            }
            self.texti = self.aut.skip_to(self.si, self.text, self.texti + 1);
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
pub struct StreamMatches<'a, R, T: 'a + Transitions> {
    aut: &'a AcAutomaton<T>,
    buf: io::BufReader<R>,
    texti: usize,
    si: StateIdx,
}

impl<'a, R: io::Read, T: Transitions> Iterator for StreamMatches<'a, R, T> {
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
pub struct MatchesOverlapping<'a, 's, T: 'a + Transitions> {
    aut: &'a AcAutomaton<T>,
    text: &'s [u8],
    texti: usize,
    si: StateIdx,
    outi: usize,
}

impl<'a, 's, T: Transitions> Iterator for MatchesOverlapping<'a, 's, T> {
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
pub struct StreamMatchesOverlapping<'a, R, T: 'a + Transitions> {
    aut: &'a AcAutomaton<T>,
    buf: io::BufReader<R>,
    texti: usize,
    si: StateIdx,
    outi: usize,
}

impl<
    'a,
    R: io::Read,
    T: Transitions,
> Iterator for StreamMatchesOverlapping<'a, R, T> {
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

// Below contains code for *building* the automaton. It's a reasonably faithful
// translation of the description/psuedo-code from:
// http://www.cs.uku.fi/~kilpelai/BSA05/lectures/slides04.pdf

impl<T: Transitions> AcAutomaton<T> {
    // This is the first phase and builds the initial keyword tree.
    fn build(mut self, pats: Vec<String>) -> AcAutomaton<T> {
        for (pati, pat) in pats.iter().enumerate() {
            if pat.is_empty() {
                continue;
            }
            let mut previ = ROOT_STATE;
            for &b in pat.as_bytes() {
                if self.states[previ as usize].goto(b) != FAIL_STATE {
                    previ = self.states[previ as usize].goto(b);
                } else {
                    let depth = self.states[previ as usize].depth + 1;
                    let nexti = self.add_state(State::new(depth));
                    self.states[previ as usize].set_goto(b, nexti);
                    previ = nexti;
                }
            }
            self.states[previ as usize].out.push(pati);
        }
        for c in (0..256).into_iter().map(|c| c as u8) {
            if self.states[ROOT_STATE as usize].goto(c) == FAIL_STATE {
                self.states[ROOT_STATE as usize].set_goto(c, ROOT_STATE);
            } else {
                self.start_bytes.push(c);
            }
        }
        self.pats = pats;
        self.fill()
    }

    // The second phase that fills in the back links.
    fn fill(mut self) -> AcAutomaton<T> {
        // Fill up the queue with all non-root transitions out of the root
        // node. Then proceed by breadth first traversal.
        let mut q = VecDeque::new();
        for c in (0..256).into_iter().map(|c| c as u8) {
            let si = self.states[ROOT_STATE as usize].goto(c);
            if si != ROOT_STATE {
                q.push_front(si);
            }
        }
        while let Some(si) = q.pop_back() {
            for c in (0..256).into_iter().map(|c| c as u8) {
                let u = self.states[si as usize].goto(c);
                if u != FAIL_STATE {
                    q.push_front(u);
                    let mut v = self.states[si as usize].fail;
                    while self.states[v as usize].goto(c) == FAIL_STATE {
                        v = self.states[v as usize].fail;
                    }
                    let ufail = self.states[v as usize].goto(c);
                    self.states[u as usize].fail = ufail;
                    let ufail_out = self.states[ufail as usize].out.clone();
                    self.states[u as usize].out.extend(ufail_out);
                }
            }
        }
        self
    }

    fn add_state(&mut self, state: State<T>) -> StateIdx {
        let i = self.states.len();
        self.states.push(state);
        i as StateIdx
    }
}

impl<T: Transitions> State<T> {
    fn new(depth: u32) -> State<T> {
        State {
            out: vec![],
            fail: 1,
            goto: Transitions::new(depth),
            depth: depth,
        }
    }

    fn goto(&self, b: u8) -> StateIdx { self.goto.goto(b) }
    fn set_goto(&mut self, b: u8, si: StateIdx) { self.goto.set_goto(b, si); }
}

/// An abstraction over state transition strategies.
///
/// This is an attempt to let the caller choose the space/time trade offs
/// used for state transitions.
///
/// (It's possible that this interface is merely good enough for just the two
/// implementations in this crate.)
pub trait Transitions {
    fn new(depth: u32) -> Self;
    fn goto(&self, alpha: u8) -> StateIdx;
    fn set_goto(&mut self, alpha: u8, si: StateIdx);
}

/// State transitions that can be stored either sparsely or densely.
///
/// This uses less space but at the expense of slower matching.
#[derive(Clone, Debug)]
pub struct Dense(DenseChoice);

#[derive(Clone, Debug)]
enum DenseChoice {
    Sparse(Vec<StateIdx>), // indexed by alphabet
    Dense(Vec<(u8, StateIdx)>),
}

impl Transitions for Dense {
    fn new(depth: u32) -> Dense {
        if depth <= DENSE_DEPTH_THRESHOLD {
            Dense(DenseChoice::Sparse(vec![0; 256]))
        } else {
            Dense(DenseChoice::Dense(vec![]))
        }
    }

    fn goto(&self, b1: u8) -> StateIdx {
        match self.0 {
            DenseChoice::Sparse(ref m) => m[b1 as usize],
            DenseChoice::Dense(ref m) => {
                for &(b2, si) in m {
                    if b1 == b2 {
                        return si;
                    }
                }
                FAIL_STATE
            }
        }
    }

    fn set_goto(&mut self, b: u8, si: StateIdx) {
        match self.0 {
            DenseChoice::Sparse(ref mut m) => m[b as usize] = si,
            DenseChoice::Dense(ref mut m) => m.push((b, si)),
        }
    }
}

/// State transitions that are always sparse.
///
/// This can use enormous amounts of memory when there are many patterns,
/// but matching is very fast.
#[derive(Clone, Debug)]
pub struct Sparse(Vec<StateIdx>);

impl Transitions for Sparse {
    fn new(_: u32) -> Sparse {
        Sparse(vec![0; 256])
    }

    #[inline]
    fn goto(&self, b: u8) -> StateIdx {
        self.0[b as usize]
    }

    fn set_goto(&mut self, b: u8, si: StateIdx) {
        self.0[b as usize] = si;
    }
}

impl<S: Into<String>> FromIterator<S> for AcAutomaton {
    /// Create an automaton from an iterator of strings.
    fn from_iter<T>(it: T) -> AcAutomaton where T: IntoIterator<Item=S> {
        AcAutomaton::new(it)
    }
}

// Provide some question debug impls for viewing automatons.
// The custom impls mostly exist for special showing of sparse maps.

impl<T: Transitions> fmt::Debug for AcAutomaton<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::iter::repeat;

        try!(writeln!(f, "{}", repeat('-').take(79).collect::<String>()));
        try!(writeln!(f, "Patterns: {:?}", self.pats));
        for (i, state) in self.states.iter().enumerate().skip(1) {
            try!(writeln!(f, "{:3}: {}", i, state.debug(i == 1)));
        }
        write!(f, "{}", repeat('-').take(79).collect::<String>())
    }
}

impl<T: Transitions> State<T> {
    fn debug(&self, root: bool) -> String {
        format!("State {{ depth: {:?}, out: {:?}, fail: {:?}, goto: {{{}}} }}",
                self.depth, self.out, self.fail, self.goto_string(root))
    }

    fn goto_string(&self, root: bool) -> String {
        use std::char::from_u32;

        let mut goto = vec![];
        for b in (0..256).map(|b| b as u8) {
            let si = self.goto(b);
            if (!root && si == FAIL_STATE) || (root && si == ROOT_STATE) {
                continue;
            }
            goto.push(format!("{} => {}", from_u32(b as u32).unwrap(), si));
        }
        goto.connect(", ")
    }
}

impl<T: Transitions> fmt::Debug for State<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.debug(false))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::io;

    use quickcheck::{Arbitrary, Gen, quickcheck};
    use rand::Rng;

    use super::{AcAutomaton, Match};

    fn aut_find<S>(xs: &[S], haystack: &str) -> Vec<Match>
            where S: Clone + Into<String> {
        AcAutomaton::new(xs.to_vec()).find(&haystack).collect()
    }

    fn aut_finds<S>(xs: &[S], haystack: &str) -> Vec<Match>
            where S: Clone + Into<String> {
        let cur = io::Cursor::new(haystack.as_bytes());
        AcAutomaton::new(xs.to_vec())
            .stream_find(cur).map(|r| r.unwrap()).collect()
    }

    fn aut_findo<S>(xs: &[S], haystack: &str) -> Vec<Match>
            where S: Clone + Into<String> {
        AcAutomaton::new(xs.to_vec()).find_overlapping(haystack).collect()
    }

    fn aut_findos<S>(xs: &[S], haystack: &str) -> Vec<Match>
            where S: Clone + Into<String> {
        let cur = io::Cursor::new(haystack.as_bytes());
        AcAutomaton::new(xs.to_vec())
            .stream_find_overlapping(cur).map(|r| r.unwrap()).collect()
    }

    #[test]
    fn one_pattern_one_match() {
        let ns = vec!["a"];
        let hay = "za";
        let matches = vec![
            Match { pati: 0, start: 1, end: 2 },
        ];
        assert_eq!(&aut_find(&ns, hay), &matches);
        assert_eq!(&aut_finds(&ns, hay), &matches);
    }

    #[test]
    fn one_pattern_many_match() {
        let ns = vec!["a"];
        let hay = "zazazzzza";
        let matches = vec![
            Match { pati: 0, start: 1, end: 2 },
            Match { pati: 0, start: 3, end: 4 },
            Match { pati: 0, start: 8, end: 9 },
        ];
        assert_eq!(&aut_find(&ns, hay), &matches);
        assert_eq!(&aut_finds(&ns, hay), &matches);
    }

    #[test]
    fn one_longer_pattern_one_match() {
        let ns = vec!["abc"];
        let hay = "zazabcz";
        let matches = vec![ Match { pati: 0, start: 3, end: 6 } ];
        assert_eq!(&aut_find(&ns, hay), &matches);
        assert_eq!(&aut_finds(&ns, hay), &matches);
    }

    #[test]
    fn one_longer_pattern_many_match() {
        let ns = vec!["abc"];
        let hay = "zazabczzzzazzzabc";
        let matches = vec![
            Match { pati: 0, start: 3, end: 6 },
            Match { pati: 0, start: 14, end: 17 },
        ];
        assert_eq!(&aut_find(&ns, hay), &matches);
        assert_eq!(&aut_finds(&ns, hay), &matches);
    }

    #[test]
    fn many_pattern_one_match() {
        let ns = vec!["a", "b"];
        let hay = "zb";
        let matches = vec![ Match { pati: 1, start: 1, end: 2 } ];
        assert_eq!(&aut_find(&ns, hay), &matches);
        assert_eq!(&aut_finds(&ns, hay), &matches);
    }

    #[test]
    fn many_pattern_many_match() {
        let ns = vec!["a", "b"];
        let hay = "zbzazzzzb";
        let matches = vec![
            Match { pati: 1, start: 1, end: 2 },
            Match { pati: 0, start: 3, end: 4 },
            Match { pati: 1, start: 8, end: 9 },
        ];
        assert_eq!(&aut_find(&ns, hay), &matches);
        assert_eq!(&aut_finds(&ns, hay), &matches);
    }

    #[test]
    fn many_longer_pattern_one_match() {
        let ns = vec!["abc", "xyz"];
        let hay = "zazxyzz";
        let matches = vec![ Match { pati: 1, start: 3, end: 6 } ];
        assert_eq!(&aut_find(&ns, hay), &matches);
        assert_eq!(&aut_finds(&ns, hay), &matches);
    }

    #[test]
    fn many_longer_pattern_many_match() {
        let ns = vec!["abc", "xyz"];
        let hay = "zazxyzzzzzazzzabcxyz";
        let matches = vec![
            Match { pati: 1, start: 3, end: 6 },
            Match { pati: 0, start: 14, end: 17 },
            Match { pati: 1, start: 17, end: 20 },
        ];
        assert_eq!(&aut_find(&ns, hay), &matches);
        assert_eq!(&aut_finds(&ns, hay), &matches);
    }

    #[test]
    fn many_longer_pattern_overlap_one_match() {
        let ns = vec!["abc", "bc"];
        let hay = "zazabcz";
        let matches = vec![
            Match { pati: 0, start: 3, end: 6 },
            Match { pati: 1, start: 4, end: 6 },
        ];
        assert_eq!(&aut_findo(&ns, hay), &matches);
        assert_eq!(&aut_findos(&ns, hay), &matches);
    }

    #[test]
    fn many_longer_pattern_overlap_one_match_reverse() {
        let ns = vec!["abc", "bc"];
        let hay = "xbc";
        let matches = vec![ Match { pati: 1, start: 1, end: 3 } ];
        assert_eq!(&aut_findo(&ns, hay), &matches);
        assert_eq!(&aut_findos(&ns, hay), &matches);
    }

    #[test]
    fn many_longer_pattern_overlap_many_match() {
        let ns = vec!["abc", "bc", "c"];
        let hay = "zzzabczzzbczzzc";
        let matches = vec![
            Match { pati: 0, start: 3, end: 6 },
            Match { pati: 1, start: 4, end: 6 },
            Match { pati: 2, start: 5, end: 6 },
            Match { pati: 1, start: 9, end: 11 },
            Match { pati: 2, start: 10, end: 11 },
            Match { pati: 2, start: 14, end: 15 },
        ];
        assert_eq!(&aut_findo(&ns, hay), &matches);
        assert_eq!(&aut_findos(&ns, hay), &matches);
    }

    #[test]
    fn many_longer_pattern_overlap_many_match_reverse() {
        let ns = vec!["abc", "bc", "c"];
        let hay = "zzzczzzbczzzabc";
        let matches = vec![
            Match { pati: 2, start: 3, end: 4 },
            Match { pati: 1, start: 7, end: 9 },
            Match { pati: 2, start: 8, end: 9 },
            Match { pati: 0, start: 12, end: 15 },
            Match { pati: 1, start: 13, end: 15 },
            Match { pati: 2, start: 14, end: 15 },
        ];
        assert_eq!(&aut_findo(&ns, hay), &matches);
        assert_eq!(&aut_findos(&ns, hay), &matches);
    }

    // Quickcheck time.

    // This generates very small ascii strings, which makes them more likely
    // to interact in interesting ways with larger haystack strings.
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub struct SmallAscii(String);

    impl Arbitrary for SmallAscii {
        fn arbitrary<G: Gen>(g: &mut G) -> SmallAscii {
            use std::char::from_u32;
            SmallAscii((0..2)
                       .map(|_| from_u32(g.gen_range(97, 123)).unwrap())
                       .collect())
        }

        fn shrink(&self) -> Box<Iterator<Item=SmallAscii>> {
            Box::new(self.0.shrink().map(SmallAscii))
        }
    }

    impl From<SmallAscii> for String {
        fn from(s: SmallAscii) -> String { s.0 }
    }

    // This is the same arbitrary impl as `String`, except it has a bias toward
    // ASCII characters.
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    pub struct BiasAscii(String);

    impl Arbitrary for BiasAscii {
        fn arbitrary<G: Gen>(g: &mut G) -> BiasAscii {
            use std::char::from_u32;
            let size = { let s = g.size(); g.gen_range(0, s) };
            let mut s = String::with_capacity(size);
            for _ in 0..size {
                if g.gen_weighted_bool(3) {
                    s.push(char::arbitrary(g));
                } else {
                    for _ in 0..5 {
                        s.push(from_u32(g.gen_range(97, 123)).unwrap());
                    }
                }
            }
            BiasAscii(s)
        }

        fn shrink(&self) -> Box<Iterator<Item=BiasAscii>> {
            Box::new(self.0.shrink().map(BiasAscii))
        }
    }

    fn naive_find<S>(xs: &[S], haystack: &str) -> Vec<Match>
            where S: Clone + Into<String> {
        let needles: Vec<String> =
            xs.to_vec().into_iter().map(Into::into).collect();
        let mut matches = vec![];
        for hi in 0..haystack.len() {
            for (pati, needle) in needles.iter().enumerate() {
                let needle = needle.as_bytes();
                if needle.len() == 0 || needle.len() > haystack.len() - hi {
                    continue;
                }
                if needle == &haystack.as_bytes()[hi..hi+needle.len()] {
                    matches.push(Match {
                        pati: pati,
                        start: hi,
                        end: hi + needle.len(),
                    });
                }
            }
        }
        matches
    }

    #[test]
    fn qc_ac_equals_naive() {
        fn prop(needles: Vec<SmallAscii>, haystack: BiasAscii) -> bool {
            let aut_matches = aut_findo(&needles, &haystack.0);
            let naive_matches = naive_find(&needles, &haystack.0);
            // Ordering isn't always the same. I don't think we care, so do
            // an unordered comparison.
            let aset: HashSet<Match> = aut_matches.iter().cloned().collect();
            let nset: HashSet<Match> = naive_matches.iter().cloned().collect();
            aset == nset
        }
        quickcheck(prop as fn(Vec<SmallAscii>, BiasAscii) -> bool);
    }
}
