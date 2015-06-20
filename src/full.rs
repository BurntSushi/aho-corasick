use memchr::memchr;

use super::{
    FAIL_STATE, ROOT_STATE,
    StateIdx, PatIdx,
    AcAutomaton, Transitions, Match,
};
use super::autiter::Automaton;

/// A complete Aho-Corasick automaton.
///
/// This uses a single transition matrix that permits each input character
/// to move to the next state with a single lookup in the matrix.
///
/// This is as fast as it gets, but it is guaranteed to use a lot of memory.
/// Namely, it will use at least `4 * 256 * #states`, where the number of
/// states is capped at length of all patterns concatenated.
#[derive(Clone, Debug)]
pub struct FullAcAutomaton {
    pats: Vec<String>,
    // i * #states + si
    trans: Vec<StateIdx>,  // row-major, where states are rows
    out: Vec<Vec<PatIdx>>, // indexed by StateIdx
    start_bytes: Vec<u8>,
}

impl FullAcAutomaton {
    /// Build a new expanded Aho-Corasick automaton from an existing
    /// Aho-Corasick automaton.
    pub fn new<T: Transitions>(ac: AcAutomaton<T>) -> FullAcAutomaton {
        let mut fac = FullAcAutomaton {
            pats: vec![],
            trans: vec![FAIL_STATE; 256 * ac.states.len()],
            out: vec![vec![]; ac.states.len()],
            start_bytes: vec![],
        };
        fac.build_matrix(&ac);
        fac.pats = ac.pats;
        fac.start_bytes = ac.start_bytes;
        fac
    }

    fn set(&mut self, si: StateIdx, i: u8, goto: StateIdx) {
        let ns = self.num_states();
        self.trans[i as usize * ns + si as usize] = goto;
    }

    #[inline]
    fn num_states(&self) -> usize {
        self.out.len()
    }
}

impl Automaton for FullAcAutomaton {
    #[inline]
    fn next_state(&self, si: StateIdx, i: u8) -> StateIdx {
        self.trans[i as usize * self.num_states() + si as usize]
    }

    #[inline]
    fn get_match(&self, si: StateIdx, outi: usize, texti: usize) -> Match {
        let pati = self.out[si as usize][outi];
        let patlen = self.pats[pati].len();
        let start = texti + 1 - patlen;
        Match {
            pati: pati,
            start: start,
            end: start + patlen,
        }
    }

    #[inline]
    fn has_match(&self, si: StateIdx, outi: usize) -> bool {
        outi < self.out[si as usize].len()
    }

    #[inline]
    fn skip_to(&self, si: StateIdx, text: &[u8], at: usize) -> usize {
        if si != ROOT_STATE || !self.is_skippable() {
            return at;
        }
        let b = self.start_bytes[0];
        match memchr(b, &text[at..]) {
            None => text.len(),
            Some(i) => at + i,
        }
    }

    #[inline]
    fn is_skippable(&self) -> bool {
        self.start_bytes.len() == 1
    }

    #[inline]
    fn patterns(&self) -> &[String] {
        &self.pats
    }

    #[inline]
    fn pattern(&self, i: usize) -> &str {
        &self.pats[i]
    }
}

impl FullAcAutomaton {
    fn build_matrix<T: Transitions>(&mut self, ac: &AcAutomaton<T>) {
        for (si, s) in ac.states.iter().enumerate().skip(1) {
            for b in (0..256).map(|b| b as u8) {
                self.set(si as StateIdx, b, ac.next_state(si as StateIdx, b));
            }
            for &pati in &s.out {
                self.out[si].push(pati);
            }
        }
    }
}
