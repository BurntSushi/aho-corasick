/*!
A fast implementation of the Aho-Corasick string search algorithm.
*/

#![allow(dead_code)]

extern crate memchr;

use std::collections::VecDeque;
use std::fmt;
use std::iter::FromIterator;

use memchr::memchr;

type PatIdx = usize;
type StateIdx = u32;

const FAIL_STATE: u32 = 0;
const ROOT_STATE: u32 = 1;

#[derive(Clone)]
pub struct AcAutomaton {
    pats: Vec<String>,
    states: Vec<State>,
    start_bytes: Vec<u8>,
}

#[derive(Clone)]
struct State {
    out: Vec<PatIdx>,
    fail: StateIdx,
    goto: Vec<StateIdx>, // indexed by alphabet
}

impl AcAutomaton {
    pub fn new<S, I>(pats: I) -> AcAutomaton
            where S: Into<String>, I: IntoIterator<Item=S> {
        AcAutomaton {
            pats: vec![], // filled in later, avoid wrath of borrow checker
            states: vec![State::new(), State::new()], // empty and root
            start_bytes: vec![], // also filled in later
        }.build(pats.into_iter().map(Into::into).collect())
    }

    pub fn len(&self) -> usize {
        self.pats.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn find<'a, 's>(&'a self, s: &'s str) -> Matches<'a, 's> {
        Matches {
            aut: self,
            text: s.as_bytes(),
            texti: 0,
            si: ROOT_STATE,
            outi: 0,
            overlapping: false,
        }
    }

    pub fn find_overlapping<'a, 's>(&'a self, s: &'s str) -> Matches<'a, 's> {
        Matches {
            aut: self,
            text: s.as_bytes(),
            texti: 0,
            si: ROOT_STATE,
            outi: 0,
            overlapping: true,
        }
    }

    pub fn pattern(&self, i: usize) -> &str {
        &self.pats[i]
    }

    fn next_state(&self, mut si: StateIdx, b: u8) -> StateIdx {
        // Eliding bounds checking here increases throughput by about 18%
        // on the `*_every_match` benchmarks.
        // For the `*_random` benchmarks, it's more like 5%.
        // And for the `*_no_match` benchmarks, it's more like 0.5%.
        //
        // 5% on the average ("random") case seems worth it. Improving the
        // absolute worst case ("every match") by 18% is nice too, I guess.
        loop {
            let maybe_si = unsafe {
                *self.states.get_unchecked(si as usize)
                     .goto.get_unchecked(b as usize)
            };
            if maybe_si != FAIL_STATE {
                si = maybe_si;
                break;
            } else {
                si = unsafe { self.states.get_unchecked(si as usize).fail };
            }
        }
        si
    }
}

#[derive(Debug)]
pub struct Matches<'a, 's> {
    aut: &'a AcAutomaton,
    text: &'s [u8],
    texti: usize,
    si: StateIdx,
    outi: usize,
    overlapping: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Match {
    pub pati: usize,
    pub start: usize,
    pub end: usize,
}

impl<'a, 's> Iterator for Matches<'a, 's> {
    type Item = Match;

    fn next(&mut self) -> Option<Match> {
        if self.has_match() {
            let m = self.get_match();
            self.outi += 1;
            if !self.has_match() {
                self.texti += 1;
            }
            return Some(m);
        }
        self.outi = 0;

        // If there is a common single byte prefix, then it's worth it to try
        // to skip along the input with `memchr`.
        //
        // If not, then the `skip`s hurt performance pretty bad (possibly
        // harming the branch predictor), so move the case analysis to every
        // match.
        if self.aut.start_bytes.len() == 1 {
            if self.si == ROOT_STATE {
                self.skip();
            }
            while self.texti < self.text.len() {
                let b = self.text[self.texti];
                self.si = self.aut.next_state(self.si, b);
                if self.has_match() {
                    return self.next();
                }
                self.texti += 1;
                if self.si == ROOT_STATE {
                    self.skip();
                }
            }
        } else {
            while self.texti < self.text.len() {
                let b = self.text[self.texti];
                self.si = self.aut.next_state(self.si, b);
                if self.has_match() {
                    return self.next();
                }
                self.texti += 1;
            }
        }
        None
    }
}

impl<'a, 's> Matches<'a, 's> {
    fn skip(&mut self) {
        let b = self.aut.start_bytes[0];
        self.texti = match memchr(b, &self.text[self.texti..]) {
            Some(i) => self.texti + i,
            None => self.text.len(),
        };
    }

    fn has_match(&self) -> bool {
        // Eliding the bounds checking here seems to have very similar perf
        // characteristics as eliding bounds checking in `next_state`.
        // (See comments in `next_state`.)
        let outs = unsafe {
            self.aut.states.get_unchecked(self.si as usize).out.len()
        };
        self.outi < outs
        // (self.overlapping && self.outi < outs)
        // || (!self.overlapping && self.outi == 0 && outs > 0)
    }

    fn get_match(&self) -> Match {
        let pati = self.aut.states[self.si as usize].out[self.outi];
        let patlen = self.aut.pats[pati].len();
        let start = self.texti + 1 - patlen;
        Match {
            pati: pati,
            start: start,
            end: start + patlen,
        }
    }
}

impl AcAutomaton {
    fn build(mut self, pats: Vec<String>) -> AcAutomaton {
        for (pati, pat) in pats.iter().enumerate() {
            let mut previ = ROOT_STATE;
            for &b in pat.as_bytes() {
                if self.states[previ as usize].goto[b as usize] != FAIL_STATE {
                    previ = self.states[previ as usize].goto[b as usize];
                } else {
                    let nexti = self.add_state(State::new());
                    self.states[previ as usize].goto[b as usize] = nexti;
                    previ = nexti;
                }
            }
            self.states[previ as usize].out.push(pati);
        }
        for c in 0..256 {
            if self.states[ROOT_STATE as usize].goto[c] == FAIL_STATE {
                self.states[ROOT_STATE as usize].goto[c] = ROOT_STATE;
            } else {
                self.start_bytes.push(c as u8);
            }
        }
        self.pats = pats;
        self.fill()
    }

    fn fill(mut self) -> AcAutomaton {
        let mut q = VecDeque::new();
        for &si in self.states[ROOT_STATE as usize].goto.iter() {
            if si != ROOT_STATE {
                q.push_front(si);
            }
        }
        while let Some(si) = q.pop_back() {
            for c in 0..256 {
                let u = self.states[si as usize].goto[c];
                if u != FAIL_STATE {
                    q.push_front(u);
                    let mut v = self.states[si as usize].fail;
                    while self.states[v as usize].goto[c] == FAIL_STATE {
                        v = self.states[v as usize].fail;
                    }
                    let ufail = self.states[v as usize].goto[c];
                    self.states[u as usize].fail = ufail;
                    let ufail_out = self.states[ufail as usize].out.clone();
                    self.states[u as usize].out.extend(ufail_out);
                }
            }
        }
        self
    }

    fn add_state(&mut self, state: State) -> StateIdx {
        let i = self.states.len();
        self.states.push(state);
        i as StateIdx
    }
}

impl State {
    fn new() -> State {
        State {
            out: vec![],
            fail: 1,
            goto: vec![0; 256],
        }
    }
}

impl<S: Into<String>> FromIterator<S> for AcAutomaton {
    fn from_iter<T>(it: T) -> AcAutomaton where T: IntoIterator<Item=S> {
        AcAutomaton::new(it)
    }
}

impl fmt::Debug for AcAutomaton {
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

impl State {
    fn debug(&self, root: bool) -> String {
        format!("State {{ out: {:?}, fail: {:?}, goto: {{{}}} }}",
                self.out, self.fail, self.dense_goto_string(root))
    }

    fn dense_goto_string(&self, root: bool) -> String {
        use std::char::from_u32;

        let mut goto = vec![];
        for (i, &state) in self.goto.iter().enumerate() {
            if (!root && state == 0) || (root && state == 1) { continue; }
            goto.push(format!("{} => {}", from_u32(i as u32).unwrap(), state));
        }
        goto.connect(", ")
    }
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.debug(false))
    }
}

#[cfg(test)]
mod tests {
    use super::{AcAutomaton, Match};

    fn naive_find<S: Into<String>>(xs: Vec<S>, haystack: S) -> Vec<Match> {
        let needles: Vec<String> = xs.into_iter().map(Into::into).collect();
        let haystack: String = haystack.into();
        let mut matches = vec![];
        for hi in 0..haystack.len() {
            let rest = &haystack.as_bytes()[hi..];
            for (pati, needle) in needles.iter().enumerate() {
                let needle = needle.as_bytes();
                if needle.len() > rest.len() {
                    continue;
                }
                if needle == rest {
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

    fn aut_find<S: Into<String>>(xs: Vec<S>, haystack: S) -> Vec<Match> {
        AcAutomaton::new(xs).find(&haystack.into()).collect()
    }

    fn aut_findo<S: Into<String>>(xs: Vec<S>, haystack: S) -> Vec<Match> {
        AcAutomaton::new(xs).find_overlapping(&haystack.into()).collect()
    }

    #[test]
    fn one_pattern_one_match() {
        let ns = vec!["a"];
        let hay = "za";
        assert_eq!(aut_find(ns, hay), vec![
            Match { pati: 0, start: 1, end: 2 },
        ]);
    }

    #[test]
    fn one_pattern_many_match() {
        let ns = vec!["a"];
        let hay = "zazazzzza";
        assert_eq!(aut_find(ns, hay), vec![
            Match { pati: 0, start: 1, end: 2 },
            Match { pati: 0, start: 3, end: 4 },
            Match { pati: 0, start: 8, end: 9 },
        ]);
    }

    #[test]
    fn one_longer_pattern_one_match() {
        let ns = vec!["abc"];
        let hay = "zazabcz";
        assert_eq!(aut_find(ns, hay), vec![
            Match { pati: 0, start: 3, end: 6 },
        ]);
    }

    #[test]
    fn one_longer_pattern_many_match() {
        let ns = vec!["abc"];
        let hay = "zazabczzzzazzzabc";
        assert_eq!(aut_find(ns, hay), vec![
            Match { pati: 0, start: 3, end: 6 },
            Match { pati: 0, start: 14, end: 17 },
        ]);
    }

    #[test]
    fn many_pattern_one_match() {
        let ns = vec!["a", "b"];
        let hay = "zb";
        assert_eq!(aut_find(ns, hay), vec![
            Match { pati: 1, start: 1, end: 2 },
        ]);
    }

    #[test]
    fn many_pattern_many_match() {
        let ns = vec!["a", "b"];
        let hay = "zbzazzzzb";
        assert_eq!(aut_find(ns, hay), vec![
            Match { pati: 1, start: 1, end: 2 },
            Match { pati: 0, start: 3, end: 4 },
            Match { pati: 1, start: 8, end: 9 },
        ]);
    }

    #[test]
    fn many_longer_pattern_one_match() {
        let ns = vec!["abc", "xyz"];
        let hay = "zazxyzz";
        assert_eq!(aut_find(ns, hay), vec![
            Match { pati: 1, start: 3, end: 6 },
        ]);
    }

    #[test]
    fn many_longer_pattern_many_match() {
        let ns = vec!["abc", "xyz"];
        let hay = "zazxyzzzzzazzzabcxyz";
        assert_eq!(aut_find(ns, hay), vec![
            Match { pati: 1, start: 3, end: 6 },
            Match { pati: 0, start: 14, end: 17 },
            Match { pati: 1, start: 17, end: 20 },
        ]);
    }

    #[test]
    fn many_longer_pattern_overlap_one_match() {
        let ns = vec!["abc", "bc"];
        let hay = "zazabcz";
        assert_eq!(aut_findo(ns, hay), vec![
            Match { pati: 0, start: 3, end: 6 },
            Match { pati: 1, start: 4, end: 6 },
        ]);
    }

    #[test]
    fn many_longer_pattern_overlap_one_match_reverse() {
        let ns = vec!["abc", "bc"];
        println!("{:?}", AcAutomaton::new(ns.clone()));
        let hay = "xbc";
        assert_eq!(aut_findo(ns, hay), vec![
            Match { pati: 1, start: 1, end: 3 },
        ]);
    }


    #[test]
    fn many_longer_pattern_overlap_many_match() {
        let ns = vec!["abc", "bc", "c"];
        let hay = "zzzabczzzbczzzc";
        assert_eq!(aut_findo(ns, hay), vec![
            Match { pati: 0, start: 3, end: 6 },
            Match { pati: 1, start: 4, end: 6 },
            Match { pati: 2, start: 5, end: 6 },
            Match { pati: 1, start: 9, end: 11 },
            Match { pati: 2, start: 10, end: 11 },
            Match { pati: 2, start: 14, end: 15 },
        ]);
    }

    #[test]
    fn many_longer_pattern_overlap_many_match_reverse() {
        let ns = vec!["abc", "bc", "c"];
        let hay = "zzzczzzbczzzabc";
        assert_eq!(aut_findo(ns, hay), vec![
            Match { pati: 2, start: 3, end: 4 },
            Match { pati: 1, start: 7, end: 9 },
            Match { pati: 2, start: 8, end: 9 },
            Match { pati: 0, start: 12, end: 15 },
            Match { pati: 1, start: 13, end: 15 },
            Match { pati: 2, start: 14, end: 15 },
        ]);
    }

    #[test]
    fn weird_dna() {
        let ns = vec!["tttacccc"];
        println!("{:?}", AcAutomaton::new(ns.clone()));
        let hay = "ttttacccc";
        assert_eq!(aut_find(ns, hay), vec![
            Match { pati: 0, start: 1, end: 9 },
        ]);
    }

    #[test]
    fn scratch() {
        let aut = AcAutomaton::new(vec!["he", "she", "his", "hers"]);
        println!("{:?}", aut);
        println!("{:?}", aut.find("but she said").next());
    }
}
