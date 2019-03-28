use ahocorasick::MatchKind;
use prefilter::{Prefilter, PrefilterState};
use state_id::{StateID, dead_id, fail_id};
use Match;

// NOTE: This trait was essentially copied from regex-automata, with some
// wording changed since we use this trait for NFAs in addition to DFAs in this
// crate. Additionally, we do not export this trait. It's only used internally
// to reduce code duplication. The regex-automata crate needs to expose it
// because its Regex type is generic over implementations of this trait. In
// this crate, we can encapsulate everything behind the AhoCorasick type.

/// A trait describing the interface of an Aho-Corasick finite state machine.
///
/// Every automaton has exactly one fail state, one dead state and exactly one
/// start state. Generally, these correspond to the first, second and third
/// states, respectively. The failure state is always treated as a sentinel.
/// That is, no correct Aho-Corasick automaton will ever transition into the
/// fail state. The dead state, however, can be transitioned into, but only
/// when leftmost-first or leftmost-longest match semantics are enabled and
/// only when at least one match has been observed.
///
/// Every automaton also has one or more match states, such that
/// `Automaton::is_match_state_unchecked(id)` returns `true` if and only if
/// `id` corresponds to a match state.
pub trait Automaton {
    /// The representation used for state identifiers in this automaton.
    ///
    /// Typically, this is one of `u8`, `u16`, `u32`, `u64` or `usize`.
    type ID: StateID;

    /// The type of matching that should be done.
    fn match_kind(&self) -> &MatchKind;

    /// An optional prefilter for quickly skipping to the next candidate match.
    /// A prefilter must report at least every match, although it may report
    /// positions that do not correspond to a match. That is, it must not allow
    /// false negatives, but can allow false positives.
    ///
    /// Currently, a prefilter only runs when the automaton is in the start
    /// state. That is, the position reported by a prefilter should always
    /// correspond to the start of a potential match.
    fn prefilter(&self) -> Option<&Prefilter>;

    /// Return the identifier of this automaton's start state.
    fn start_state(&self) -> Self::ID;

    /// Returns true if and only if the given state identifier refers to a
    /// valid state.
    fn is_valid(&self, id: Self::ID) -> bool;

    /// Returns true if and only if the given identifier corresponds to a match
    /// state.
    ///
    /// The state ID given must be valid, or else implementors may panic.
    fn is_match_state(&self, id: Self::ID) -> bool;

    /// Returns true if and only if the given identifier corresponds to a state
    /// that is either the dead state or a match state.
    ///
    /// Depending on the implementation of the automaton, this routine can
    /// be used to save a branch in the core matching loop. Nevertheless,
    /// `is_match_state(id) || id == dead_id()` is always a valid
    /// implementation. Indeed, this is the default implementation.
    ///
    /// The state ID given must be valid, or else implementors may panic.
    fn is_match_or_dead_state(&self, id: Self::ID) -> bool {
        id == dead_id() || self.is_match_state(id)
    }

    /// If the given state is a match state, return the match corresponding
    /// to the given match index. `end` must be the ending position of the
    /// detected match. If no match exists or if `match_index` exceeds the
    /// number of matches in this state, then `None` is returned.
    ///
    /// The state ID given must be valid, or else implementors may panic.
    ///
    /// If the given state ID is correct and if the `match_index` is less than
    /// the number of matches for that state, then this is guaranteed to return
    /// a match.
    fn get_match(
        &self,
        id: Self::ID,
        match_index: usize,
        end: usize,
    ) -> Option<Match>;

    /// Returns the number of matches for the given state. If the given state
    /// is not a match state, then this returns 0.
    ///
    /// The state ID given must be valid, or else implementors must panic.
    fn match_count(&self, id: Self::ID) -> usize;

    /// Given the current state that this automaton is in and the next input
    /// byte, this method returns the identifier of the next state. The
    /// identifier returned must always be valid and may never correspond to
    /// the fail state. The returned identifier may, however, point to the
    /// dead state.
    ///
    /// This is not safe so that implementors may look up the next state
    /// without memory safety checks such as bounds checks. As such, callers
    /// must ensure that the given identifier corresponds to a valid automaton
    /// state. Implementors must, in turn, ensure that this routine is safe for
    /// all valid state identifiers and for all possible `u8` values.
    unsafe fn next_state_unchecked(
        &self,
        current: Self::ID,
        input: u8,
    ) -> Self::ID;

    /// Like next_state_unchecked, but debug_asserts that the underlying
    /// implementation never returns a `fail_id()` for the next state.
    unsafe fn next_state_unchecked_no_fail(
        &self,
        current: Self::ID,
        input: u8,
    ) -> Self::ID {
        let next = self.next_state_unchecked(current, input);
        // We should never see a transition to the failure state.
        debug_assert!(
            next != fail_id(),
            "automaton should never return fail_id for next state"
        );
        next
    }

    /// Execute a search using standard match semantics.
    ///
    /// This can be used even when the automaton was constructed with leftmost
    /// match semantics when you want to find the earliest possible match. This
    /// can also be used as part of an overlapping search implementation.
    ///
    /// N.B. This does not report a match if `state_id` is given as a matching
    /// state. As such, this should not be used directly.
    #[inline(always)]
    fn standard_find_at(
        &self,
        prestate: &mut PrefilterState,
        haystack: &[u8],
        at: usize,
        state_id: &mut Self::ID,
    ) -> Option<Match> {
        if let Some(pre) = self.prefilter() {
            self.standard_find_at_imp(
                prestate, Some(pre), haystack, at, state_id,
            )
        } else {
            self.standard_find_at_imp(
                prestate, None, haystack, at, state_id,
            )
        }
    }

    // It's important for this to always be inlined. Namely, it's only caller
    // is standard_find_at, and the inlining should remove the case analysis
    // for prefilter scanning when there is no prefilter available.
    #[inline(always)]
    fn standard_find_at_imp(
        &self,
        prestate: &mut PrefilterState,
        prefilter: Option<&Prefilter>,
        haystack: &[u8],
        at: usize,
        state_id: &mut Self::ID,
    ) -> Option<Match> {
        // This is necessary for guaranteeing a safe API, since we use the
        // state ID below in a function that exhibits UB if called with an
        // invalid state ID.
        assert!(
            self.is_valid(*state_id),
            "{} is not a valid state ID",
            state_id.to_usize()
        );
        unsafe {
            let start = haystack.as_ptr();
            let end = haystack[haystack.len()..].as_ptr();
            let mut ptr = haystack[at..].as_ptr();
            while ptr < end {
                if let Some(pre) = prefilter {
                    if prestate.is_effective()
                        && *state_id == self.start_state()
                    {
                        let at = ptr as usize - start as usize;
                        match pre.next_candidate(haystack, at) {
                            None => return None,
                            Some(i) => {
                                prestate.update(i - at);
                                ptr = start.offset(i as isize);
                            }
                        }
                    }
                }
                // SAFETY: next_state is safe for all possible u8 values,
                // so the only thing we're concerned about is the validity
                // of `state_id`. `state_id` either comes from the caller
                // (in which case, we assert above that it is valid), or it
                // comes from the return value of next_state, which is also
                // guaranteed to be valid.
                *state_id = self.next_state_unchecked_no_fail(*state_id, *ptr);
                ptr = ptr.offset(1);
                // This routine always quits immediately after seeing a
                // match, and since dead states can only come after seeing
                // a match, seeing a dead state here is impossible.
                debug_assert!(
                    *state_id != dead_id(),
                    "standard find should never see a dead state"
                );

                let end = ptr as usize - start as usize;
                if let Some(m) = self.get_match(*state_id, 0, end) {
                    return Some(m);
                }
            }
            None
        }
    }

    /// Execute a search using leftmost (either first or longest) match
    /// semantics.
    ///
    /// The principle difference between searching with standard semantics and
    /// searching with leftmost semantics is that leftmost searching will
    /// continue searching even after a match has been found. Once a match
    /// is found, the search does not stop until either the haystack has been
    /// exhausted or a dead state is observed in the automaton. (Dead states
    /// only exist in automatons constructed with leftmost semantics.) That is,
    /// we rely on the construction of the automaton to tell us when to quit.
    #[inline(never)]
    fn leftmost_find_at(
        &self,
        prestate: &mut PrefilterState,
        haystack: &[u8],
        at: usize,
        state_id: &mut Self::ID,
    ) -> Option<Match> {
        if let Some(pre) = self.prefilter() {
            self.leftmost_find_at_imp(
                prestate, Some(pre), haystack, at, state_id,
            )
        } else {
            self.leftmost_find_at_imp(
                prestate, None, haystack, at, state_id,
            )
        }
    }

    // It's important for this to always be inlined. Namely, it's only caller
    // is leftmost_find_at, and the inlining should remove the case analysis
    // for prefilter scanning when there is no prefilter available.
    #[inline(always)]
    fn leftmost_find_at_imp(
        &self,
        prestate: &mut PrefilterState,
        prefilter: Option<&Prefilter>,
        haystack: &[u8],
        at: usize,
        state_id: &mut Self::ID,
    ) -> Option<Match> {
        debug_assert!(self.match_kind().is_leftmost());
        // This is necessary for guaranteeing a safe API, since we use the
        // state ID below in a function that exhibits UB if called with an
        // invalid state ID.
        assert!(
            self.is_valid(*state_id),
            "{} is not a valid state ID",
            state_id.to_usize()
        );
        unsafe {
            let start = haystack.as_ptr();
            let end = haystack[haystack.len()..].as_ptr();
            let mut ptr = haystack[at..].as_ptr();

            let mut last_match = self.get_match(*state_id, 0, at);
            while ptr < end {
                if let Some(pre) = prefilter {
                    if prestate.is_effective()
                        && *state_id == self.start_state()
                    {
                        let at = ptr as usize - start as usize;
                        match pre.next_candidate(haystack, at) {
                            None => return None,
                            Some(i) => {
                                prestate.update(i - at);
                                ptr = start.offset(i as isize);
                            }
                        }
                    }
                }
                // SAFETY: next_state is safe for all possible u8 values,
                // so the only thing we're concerned about is the validity
                // of `state_id`. `state_id` either comes from the caller
                // (in which case, we assert above that it is valid), or it
                // comes from the return value of next_state, which is also
                // guaranteed to be valid.
                *state_id = self.next_state_unchecked_no_fail(*state_id, *ptr);
                ptr = ptr.offset(1);
                if self.is_match_or_dead_state(*state_id) {
                    if *state_id == dead_id() {
                        // The only way to enter into a dead state is if a
                        // match has been found, so we assert as much. This
                        // is different from normal automata, where you might
                        // enter a dead state if you know a subsequent match
                        // will never be found (regardless of whether a match
                        // has already been found). For Aho-Corasick, it is
                        // built so that we can match at any position, so the
                        // possibility of a match always exists.
                        debug_assert!(
                            last_match.is_some(),
                            "failure state should only be seen after match"
                        );
                        return last_match;
                    }
                    let end = ptr as usize - start as usize;
                    last_match = self.get_match(*state_id, 0, end);
                }
            }
            last_match
        }
    }

    /// Execute an overlapping search.
    ///
    /// When executing an overlapping match, the previous state ID in addition
    /// to the previous match index should be given. If there are more matches
    /// at the given state, then the match is reported and the given index is
    /// incremented.
    #[inline(always)]
    fn overlapping_find_at(
        &self,
        prestate: &mut PrefilterState,
        haystack: &[u8],
        at: usize,
        state_id: &mut Self::ID,
        match_index: &mut usize,
    ) -> Option<Match> {
        let match_count = self.match_count(*state_id);
        if *match_index < match_count {
            // This is guaranteed to return a match since
            // match_index < match_count.
            let result = self.get_match(
                *state_id,
                *match_index,
                at,
            );
            debug_assert!(result.is_some(), "must be a match");
            *match_index += 1;
            return result;
        }

        *match_index = 0;
        match self.standard_find_at(prestate, haystack, at, state_id) {
            None => None,
            Some(m) => {
                *match_index = 1;
                Some(m)
            }
        }
    }

    /// Return the earliest match found. This returns as soon as we know that
    /// we have a match. As such, this does not necessarily correspond to the
    /// leftmost starting match, but rather, the leftmost position at which a
    /// match ends.
    #[inline(always)]
    fn earliest_find_at(
        &self,
        prestate: &mut PrefilterState,
        haystack: &[u8],
        at: usize,
        state_id: &mut Self::ID,
    ) -> Option<Match> {
        if *state_id == self.start_state() {
            if let Some(m) = self.get_match(*state_id, 0, at) {
                return Some(m);
            }
        }
        self.standard_find_at(prestate, haystack, at, state_id)
    }

    /// A convenience function for finding the next match according to the
    /// match semantics of this automaton. For standard match semantics, this
    /// finds the earliest match. Otherwise, the leftmost match is found.
    #[inline(always)]
    fn find_at(
        &self,
        prestate: &mut PrefilterState,
        haystack: &[u8],
        at: usize,
        state_id: &mut Self::ID,
    ) -> Option<Match> {
        match *self.match_kind() {
            MatchKind::Standard => {
                self.earliest_find_at(prestate, haystack, at, state_id)
            }
            MatchKind::LeftmostFirst | MatchKind::LeftmostLongest => {
                self.leftmost_find_at(prestate, haystack, at, state_id)
            }
            MatchKind::__Nonexhaustive => unreachable!(),
        }
    }
}
