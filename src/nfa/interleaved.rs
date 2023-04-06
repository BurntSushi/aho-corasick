/*!
Provides an interleaved NFA implementation of Aho-Corasick.

This is a low-level API that generally only needs to be used in niche
circumstances. When possible, prefer using [`AhoCorasick`](crate::AhoCorasick)
instead of an interleaved NFA directly. Using an `NFA` directly is typically
only necessary when one needs access to the [`Automaton`] trait implementation.
*/

use alloc::{collections::VecDeque, vec, vec::Vec};

use crate::{
    automaton::Automaton,
    nfa::noncontiguous,
    util::{
        alphabet::ByteClasses,
        debug::DebugByte,
        error::{BuildError, MatchError},
        prefilter::Prefilter,
        primitives::{PatternID, SmallIndex, StateID},
        search::{Anchored, MatchKind},
        special::Special,
    },
};

use super::noncontiguous::State;

/// An interleaved NFA implementation of Aho-Corasick.
///
/// When possible, prefer using [`AhoCorasick`](crate::AhoCorasick) instead of
/// this type directly. Using an `NFA` directly is typically only necessary
/// when one needs access to the [`Automaton`] trait implementation.
///
/// This NFA can only be built by first constructing a [`noncontiguous::NFA`].
/// Both [`NFA::new`] and [`Builder::build`] do this for you automatically,
/// but [`Builder::build_from_noncontiguous`] permits doing it explicitly.
///
/// This implementation mostly comes from http://goo.gl/lE6zG, with however
/// several adaptations to the design of this crate.
///
/// This NFA implementation optimizes the noncontiguous NFA with several
/// optimizations to reduce the size of the automaton. A single allocation
/// is used to store states and transitions, and transitions of each state
/// can be interleaved together.
///
/// For example, lets say the Aho-Corasick contains those two states:
///
/// state A [ class 0x00 => state C, class 0x04 => state D, fail => state S ]
/// state B [ class 0x01 => state E, class 0x02 => state F, fail => state V ]
///
/// And the alphabet has size 8.
///
/// The DFA would store those states like this:
///
/// ```text
///   A                               B                               C
/// | C | . | . | . | D | . | . | . | . | E | F | . | . | . | . | . | ..
/// ```
///
/// For the interleaved NFA however, slots that correspond to failed
/// transitions are considered empty, and transitions can be inserted if
/// there is place for them:
///
/// ```text
///   A       B               C
/// | S | C | V | E | F | D | ..
/// ```
///
/// The first slot is reserved for the fail transition, and the other ones
/// are translated by one. There is however an issue in making sure that a
/// transition is the right one. For example, to make sure that on state A
/// and with class 0x03, the right transition is to S (fail state) instead
/// of to state E (which belongs the B's transition table). This is done by
/// storing not only the target state of the transition, but the offset to
/// to the owner's state:
///
/// ```text
///     A                  B                                 C
/// | (S, 0) | (C, 1) | (V, 0) | (E, 1) | (F, 2) | (D, 5) | ..
/// ```
///
/// This means that in each element of the array, we need to store a state
/// id, and an offset, which can go from 0 to 256 included. This means 9 bits
/// for the offset, leaving 23 bits for the state id.
///
/// This limits the number of states that can be stored in this NFA version.
/// If there are too many states, the build of the automaton will fail.
/// Building it is however quite cheap, so trying it before building another
/// automaton kind can be worth it.
///
/// # Example
///
/// This example shows how to build an `NFA` directly and use it to execute
/// [`Automaton::try_find`]:
///
/// ```
/// use aho_corasick::{
///     automaton::Automaton,
///     nfa::interleaved::NFA,
///     Input, Match,
/// };
///
/// let patterns = &["b", "abc", "abcd"];
/// let haystack = "abcd";
///
/// let nfa = NFA::new(patterns).unwrap();
/// assert_eq!(
///     Some(Match::must(0, 1..2)),
///     nfa.try_find(&Input::new(haystack))?,
/// );
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
///
/// It is also possible to implement your own version of `try_find`. See the
/// [`Automaton`] documentation for an example.
#[derive(Clone)]
pub struct NFA {
    /// The raw NFA representation. State IDs are indexes into this array,
    /// and each elements packs a state id (the target of the transition)
    /// and an offset to the state ID that is owning the transition.
    repr: Vec<Element>,
    /// Matches per states. This is needed because the state ids are no
    /// longer ordered to easily detect match states. Non matching states
    /// are not stored in the map.
    matches: Vec<Vec<PatternID>>,
    /// The amount of heap memory used, in bytes, by the inner Vecs of
    /// 'matches'.
    matches_memory_usage: usize,
    /// The length of each pattern. This is used to compute the start offset
    /// of a match.
    pattern_lens: Vec<SmallIndex>,
    /// The total number of states in this NFA.
    state_len: usize,
    /// A prefilter for accelerating searches, if one exists.
    prefilter: Option<Prefilter>,
    /// The match semantics built into this NFA.
    match_kind: MatchKind,
    /// The equivalence classes for this NFA. All transitions are defined
    /// on equivalence classes and not on the 256 distinct byte values.
    byte_classes: ByteClasses,
    /// The length of the shortest pattern in this automaton.
    min_pattern_len: usize,
    /// The length of the longest pattern in this automaton.
    max_pattern_len: usize,
    /// The information required to deduce which states are "special" in this
    /// NFA.
    special: Special,
    fail_state: StateID,
}

impl NFA {
    /// Create a new Aho-Corasick interleaved NFA using the default
    /// configuration.
    ///
    /// Use a [`Builder`] if you want to change the configuration.
    pub fn new<I, P>(patterns: I) -> Result<NFA, BuildError>
    where
        I: IntoIterator<Item = P>,
        P: AsRef<[u8]>,
    {
        NFA::builder().build(patterns)
    }

    /// A convenience method for returning a new Aho-Corasick interleaved
    /// NFA builder.
    ///
    /// This usually permits one to just import the `NFA` type.
    pub fn builder() -> Builder {
        Builder::new()
    }

    /// Build the automaton table using the interleaving details.
    fn build_table(
        &mut self,
        nnfa: &noncontiguous::NFA,
        index_to_state_id: &[StateID],
        max_state_id: StateID,
    ) -> Result<(), BuildError> {
        // Grow the table to max_state_id + 1 + alphabet_len, to ensure the next_state_id
        // implementation can always index into this array. Unfilled elements have
        // offset == 0, which will always be invalid outside of state indexes.
        self.repr =
            vec![
                Element::EMPTY;
                max_state_id.as_usize() + 1 + self.byte_classes.alphabet_len()
            ];

        for (old_sid, state) in nnfa.states().iter().enumerate() {
            let new_sid = index_to_state_id[old_sid];

            // Add fail transition.
            let new_fail = index_to_state_id[state.fail];
            self.repr[new_sid] = Element::new(new_fail, 0)?;

            // Add the byte transitions.
            for (byte, old_target_sid) in &state.trans {
                let new_target_sid = index_to_state_id[*old_target_sid];
                let class = self.byte_classes.get(*byte) as u32;

                let idx = new_sid.as_usize() + (class as usize) + 1;
                self.repr[idx] = Element::new(new_target_sid, class + 1)?;
            }
        }

        Ok(())
    }
}

impl NFA {
    /// A sentinel state ID indicating that a search should stop once it has
    /// entered this state. When a search stops, it returns a match if one
    /// has been found, otherwise no match. An interleaved NFA always has an
    /// actual dead state at this ID.
    const DEAD: StateID = StateID::new_unchecked(0);
}

// SAFETY: 'start_state' always returns a valid state ID, 'next_state' always
// returns a valid state ID given a valid state ID. We otherwise claim that
// all other methods are correct as well.
unsafe impl Automaton for NFA {
    #[inline(always)]
    fn start_state(&self, anchored: Anchored) -> Result<StateID, MatchError> {
        match anchored {
            Anchored::No => Ok(self.special.start_unanchored_id),
            Anchored::Yes => Ok(self.special.start_anchored_id),
        }
    }

    #[inline(always)]
    fn next_state(
        &self,
        anchored: Anchored,
        mut sid: StateID,
        byte: u8,
    ) -> StateID {
        let table = &self.repr;
        let class = self.byte_classes.get(byte) as u32;

        loop {
            let o = sid.as_usize();
            let trans = &table[o + (class as usize) + 1];

            if trans.offset() == class + 1 {
                // Transition is valid
                let dest = trans.target_state();
                if dest != self.fail_state {
                    return dest;
                }
            }

            // For an anchored search, we never follow failure transitions
            // because failure transitions lead us down a path to matching
            // a *proper* suffix of the path we were on. Thus, it can only
            // produce matches that appear after the beginning of the search.
            if anchored.is_anchored() {
                return NFA::DEAD;
            }

            // Invalid transition, use the fail transition.
            sid = table[o].target_state();
        }
    }

    #[inline(always)]
    fn is_special(&self, sid: StateID) -> bool {
        sid <= self.special.max_special_id
    }

    #[inline(always)]
    fn is_dead(&self, sid: StateID) -> bool {
        sid == NFA::DEAD
    }

    #[inline(always)]
    fn is_match(&self, sid: StateID) -> bool {
        !self.is_dead(sid) && sid <= self.special.max_match_id
    }

    #[inline(always)]
    fn is_start(&self, sid: StateID) -> bool {
        sid == self.special.start_unanchored_id
            || sid == self.special.start_anchored_id
    }

    #[inline(always)]
    fn match_kind(&self) -> MatchKind {
        self.match_kind
    }

    #[inline(always)]
    fn patterns_len(&self) -> usize {
        self.pattern_lens.len()
    }

    #[inline(always)]
    fn pattern_len(&self, pid: PatternID) -> usize {
        self.pattern_lens[pid].as_usize()
    }

    #[inline(always)]
    fn min_pattern_len(&self) -> usize {
        self.min_pattern_len
    }

    #[inline(always)]
    fn max_pattern_len(&self) -> usize {
        self.max_pattern_len
    }

    #[inline(always)]
    fn match_len(&self, sid: StateID) -> usize {
        debug_assert!(self.is_match(sid));
        self.matches[sid].len()
    }

    #[inline(always)]
    fn match_pattern(&self, sid: StateID, index: usize) -> PatternID {
        debug_assert!(self.is_match(sid));
        self.matches[sid][index]
    }

    #[inline(always)]
    fn memory_usage(&self) -> usize {
        use core::mem::size_of;

        (self.repr.len() * size_of::<u32>())
            + (self.matches.len() * size_of::<Vec<PatternID>>())
            + self.matches_memory_usage
            + (self.pattern_lens.len() * size_of::<SmallIndex>())
            + self.prefilter.as_ref().map_or(0, |p| p.memory_usage())
    }

    #[inline(always)]
    fn prefilter(&self) -> Option<&Prefilter> {
        self.prefilter.as_ref()
    }
}

impl core::fmt::Debug for NFA {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        use crate::automaton::fmt_state_indicator;

        writeln!(f, "interleaved::NFA(")?;
        for (index, trans) in self.repr.iter().enumerate() {
            // The state "owning" this transition is the index minus the byteclass value.
            let offset = trans.offset();
            let source_sid = StateID::from_u32_unchecked(
                (index - (trans.offset() as usize)) as u32,
            );
            let target_sid = trans.target_state();

            fmt_state_indicator(f, self, source_sid)?;
            if offset == 0 {
                writeln!(
                    f,
                    "{:06}: {:06}({:06})",
                    index,
                    source_sid.as_usize(),
                    target_sid.as_usize()
                )?;

                if self.is_match(source_sid) {
                    let patterns = &self.matches[source_sid];
                    write!(f, "         matches: ")?;
                    for (i, pid) in patterns.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", pid.as_usize())?;
                    }
                    write!(f, "\n")?;
                }
            } else {
                writeln!(
                    f,
                    "{:06}: {:06} + {:?} => {:06}",
                    index,
                    source_sid.as_usize(),
                    DebugByte((offset - 1) as u8),
                    target_sid.as_usize()
                )?;
            }
        }
        writeln!(f, "match kind: {:?}", self.match_kind)?;
        writeln!(f, "prefilter: {:?}", self.prefilter.is_some())?;
        writeln!(f, "state length: {:?}", self.state_len)?;
        writeln!(f, "pattern length: {:?}", self.patterns_len())?;
        writeln!(f, "shortest pattern length: {:?}", self.min_pattern_len)?;
        writeln!(f, "longest pattern length: {:?}", self.max_pattern_len)?;
        writeln!(
            f,
            "alphabet length: {:?}",
            self.byte_classes.alphabet_len()
        )?;
        writeln!(f, "byte classes: {:?}", self.byte_classes)?;
        writeln!(f, "memory usage: {:?}", self.memory_usage())?;
        writeln!(f, ")")?;

        Ok(())
    }
}

#[derive(Clone)]
struct Element(u32);

impl Element {
    /// An empty element. Holes in the table will have this value, which
    /// always represents an invalid transition (except on state IDs indexes,
    /// but those are filled with other values).
    const EMPTY: Self = Self(0);

    /// The stride to retrieve the state ID. 9 bits are reserved for the
    /// offset.
    const SID_STRIDE: u32 = 9;

    /// Maximum sid storable.
    const MAX_SID: u32 = u32::MAX >> Self::SID_STRIDE;

    /// Build a new element.
    ///
    /// Fails if the state id overflows the maximum value storable.
    fn new(sid: StateID, offset: u32) -> Result<Self, BuildError> {
        let sid = sid.as_u32();

        if sid > Self::MAX_SID {
            Err(BuildError::state_id_overflow(
                Self::MAX_SID as u64,
                sid as u64,
            ))
        } else {
            Ok(Self((sid << Self::SID_STRIDE) | offset))
        }
    }

    /// Returns the offset of this transition
    #[inline(always)]
    fn offset(&self) -> u32 {
        self.0 & ((1 << Self::SID_STRIDE) - 1)
    }

    /// Returns the target state ID for this transition
    #[inline(always)]
    fn target_state(&self) -> StateID {
        StateID::from_u32_unchecked(self.0 >> Self::SID_STRIDE)
    }
}

impl core::fmt::Debug for Element {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(
            f,
            "{:?} => {:?}",
            self.offset(),
            self.target_state().as_usize()
        )
    }
}

/// A builder for configuring an Aho-Corasick interleaved NFA.
///
/// This builder has a subset of the options available to a
/// [`AhoCorasickBuilder`](crate::AhoCorasickBuilder). Of the shared options,
/// their behavior is identical.
#[derive(Clone, Debug)]
pub struct Builder {
    noncontiguous: noncontiguous::Builder,
    byte_classes: bool,
}

impl Default for Builder {
    fn default() -> Builder {
        Builder {
            noncontiguous: noncontiguous::Builder::new(),
            byte_classes: true,
        }
    }
}

impl Builder {
    /// Create a new builder for configuring an Aho-Corasick interleaved NFA.
    pub fn new() -> Builder {
        Builder::default()
    }

    /// Build an Aho-Corasick interleaved NFA from the given iterator of
    /// patterns.
    ///
    /// A builder may be reused to create more NFAs.
    pub fn build<I, P>(&self, patterns: I) -> Result<NFA, BuildError>
    where
        I: IntoIterator<Item = P>,
        P: AsRef<[u8]>,
    {
        let nnfa = self.noncontiguous.build(patterns)?;
        self.build_from_noncontiguous(&nnfa)
    }

    /// Build an Aho-Corasick interleaved NFA from the given noncontiguous NFA.
    ///
    /// Note that when this method is used, only the `byte_classes` settings
    /// on this builder are respected. The other settings only apply to the
    /// initial construction of the Aho-Corasick automaton. Since using this
    /// method requires that initial construction has already completed, all
    /// settings impacting only initial construction are no longer relevant.
    pub fn build_from_noncontiguous(
        &self,
        nnfa: &noncontiguous::NFA,
    ) -> Result<NFA, BuildError> {
        debug!("building contiguous NFA");
        let byte_classes = if self.byte_classes {
            nnfa.byte_classes().clone()
        } else {
            ByteClasses::singletons()
        };
        let mut nfa = NFA {
            repr: vec![],
            matches: Vec::new(),
            matches_memory_usage: 0,
            pattern_lens: nnfa.pattern_lens_raw().to_vec(),
            state_len: nnfa.states().len(),
            prefilter: nnfa.prefilter().map(|p| p.clone()),
            match_kind: nnfa.match_kind(),
            byte_classes,
            min_pattern_len: nnfa.min_pattern_len(),
            max_pattern_len: nnfa.max_pattern_len(),
            // The special state IDs are set later.
            special: Special::zero(),
            fail_state: StateID::ZERO,
        };
        // Meat of the implementation: remap the state ids to interleave them in a much shorter
        // vec.
        let interleave_state = interleave(nnfa, &nfa.byte_classes)?;
        // Create the table with these interleaved state ids.
        nfa.build_table(
            nnfa,
            &interleave_state.index_to_state_id,
            interleave_state.max_state_id,
        )?;

        // Save matches details with the new state ids.
        let remap = &interleave_state.index_to_state_id;
        nfa.matches = vec![
            Vec::new();
            interleave_state.max_matching_state_id.as_usize()
                + 1
        ];
        for (old_sid, state) in nnfa.states().iter().enumerate() {
            if !state.matches.is_empty() {
                nfa.matches[remap[old_sid]] = state.matches.clone();
                nfa.matches_memory_usage +=
                    std::mem::size_of::<PatternID>() * state.matches.len();
            }
        }

        // Now that we've remapped all the IDs in our states, all that's left
        // is remapping the special state IDs.
        // Since this NFA version does not use ordering on states, we only really
        // need the start ids, so just save thoses.
        let old = nnfa.special();
        let mut new = &mut nfa.special;
        new.start_unanchored_id = remap[old.start_unanchored_id];
        new.start_anchored_id = remap[old.start_anchored_id];
        nfa.fail_state = remap[1];
        new.max_match_id = interleave_state.max_matching_state_id;
        new.max_special_id = if old.max_special_id == old.max_match_id {
            interleave_state.max_matching_state_id
        } else {
            interleave_state.max_starting_state_id
        };

        debug!(
            "interleaved NFA built, <states: {:?}, size: {:?}, \
             alphabet len: {:?}>",
            nfa.state_len,
            nfa.memory_usage(),
            nfa.byte_classes.alphabet_len(),
        );
        Ok(nfa)
    }

    /// Set the desired match semantics.
    ///
    /// This only applies when using [`Builder::build`] and not
    /// [`Builder::build_from_noncontiguous`].
    ///
    /// See
    /// [`AhoCorasickBuilder::match_kind`](crate::AhoCorasickBuilder::match_kind)
    /// for more documentation and examples.
    pub fn match_kind(&mut self, kind: MatchKind) -> &mut Builder {
        self.noncontiguous.match_kind(kind);
        self
    }

    /// Enable ASCII-aware case insensitive matching.
    ///
    /// This only applies when using [`Builder::build`] and not
    /// [`Builder::build_from_noncontiguous`].
    ///
    /// See
    /// [`AhoCorasickBuilder::ascii_case_insensitive`](crate::AhoCorasickBuilder::ascii_case_insensitive)
    /// for more documentation and examples.
    pub fn ascii_case_insensitive(&mut self, yes: bool) -> &mut Builder {
        self.noncontiguous.ascii_case_insensitive(yes);
        self
    }

    /// Enable heuristic prefilter optimizations.
    ///
    /// This only applies when using [`Builder::build`] and not
    /// [`Builder::build_from_noncontiguous`].
    ///
    /// See
    /// [`AhoCorasickBuilder::prefilter`](crate::AhoCorasickBuilder::prefilter)
    /// for more documentation and examples.
    pub fn prefilter(&mut self, yes: bool) -> &mut Builder {
        self.noncontiguous.prefilter(yes);
        self
    }

    /// Whether to attempt to shrink the size of the automaton's alphabet or
    /// not.
    ///
    /// This should never be enabled unless you're debugging an automaton.
    /// Namely, disabling byte classes makes transitions easier to reason
    /// about, since they use the actual bytes instead of equivalence classes.
    /// Disabling this confers no performance benefit at search time.
    ///
    /// See
    /// [`AhoCorasickBuilder::byte_classes`](crate::AhoCorasickBuilder::byte_classes)
    /// for more documentation and examples.
    pub fn byte_classes(&mut self, yes: bool) -> &mut Builder {
        self.byte_classes = yes;
        self
    }
}

/// Result of interleaving states
struct Interleave {
    /// Mapping from the old state ids to the new ones.
    index_to_state_id: Vec<StateID>,

    /// Largest new state id. Useful to allocate the right size
    /// for the final table.
    max_state_id: StateID,

    /// Indicates which indexes in the table are free.
    available_indexes: AvailableIndexes,

    /// First index to search at for a given offset.
    /// That is, if needing to add in the table a transition with a given offset, the
    /// search should start at first_available[offset] or later.
    /// This is only used to speed up the algorithm, which could otherwise take
    /// quite long when the number of states grows.
    offset_to_search_index: [usize; 257],

    /// Max state id for matching state.
    max_matching_state_id: StateID,

    /// Max state id for starting state.
    max_starting_state_id: StateID,
}

/// Interleave states to reduce the size of the mapping and improve
/// cache locality.
///
/// See documentation on [`NFA`] for more details.
fn interleave(
    nnfa: &noncontiguous::NFA,
    byte_classes: &ByteClasses,
) -> Result<Interleave, BuildError> {
    let mut interleave = Interleave {
        index_to_state_id: vec![NFA::DEAD; nnfa.states().len()],
        max_state_id: NFA::DEAD,
        available_indexes: AvailableIndexes::default(),
        offset_to_search_index: [0_usize; 257],
        max_matching_state_id: StateID::ZERO,
        max_starting_state_id: StateID::ZERO,
    };

    let states = nnfa.states();
    let mut states_to_visit = VecDeque::new();
    let mut visited = vec![false; nnfa.states().len()];

    // Add dead & fail state first
    visited[0] = true;
    interleave.add_state(
        StateID::from_u32_unchecked(0),
        &states[0],
        byte_classes,
        &mut visited,
        &mut states_to_visit,
    )?;
    visited[1] = true;
    interleave.add_state(
        StateID::from_u32_unchecked(1),
        &states[1],
        byte_classes,
        &mut visited,
        &mut states_to_visit,
    )?;

    // Then, add all matching states
    for i in 2..=nnfa.special().max_match_id.as_u32() {
        visited[i as usize] = true;
        interleave.add_state(
            StateID::from_u32_unchecked(i),
            &states[i as usize],
            byte_classes,
            &mut visited,
            &mut states_to_visit,
        )?;
    }

    // Modify the offset_to_search_index to force all future states to be greater than
    // the matching states. This allows making sure that the max_matching_id can still be used.
    interleave.max_matching_state_id = interleave.max_state_id;
    for offset in &mut interleave.offset_to_search_index {
        *offset = interleave.max_state_id.as_usize();
    }

    // Then, add start states.
    let sid = nnfa.special().start_unanchored_id;
    // Check if the starting state has been visited before. This can happen if it is a matching
    // state.
    if !std::mem::replace(&mut visited[sid.as_usize()], true) {
        interleave.add_state(
            sid,
            &states[sid],
            byte_classes,
            &mut visited,
            &mut states_to_visit,
        )?;
    }
    let sid = nnfa.special().start_anchored_id;
    if !std::mem::replace(&mut visited[sid.as_usize()], true) {
        interleave.add_state(
            sid,
            &states[sid],
            byte_classes,
            &mut visited,
            &mut states_to_visit,
        )?;
    }

    // Modify the offset_to_search_index to force all future states to be greater than
    // the starting states. This allows making sure that the max_special_id can be used,
    // if it depends on the starting states.
    interleave.max_starting_state_id = interleave.max_state_id;
    for offset in &mut interleave.offset_to_search_index {
        *offset = interleave.max_state_id.as_usize();
    }

    // Finally, go through the rest of the states in breadth-first fashion
    while let Some(sid) = states_to_visit.pop_front() {
        interleave.add_state(
            sid,
            &states[sid],
            byte_classes,
            &mut visited,
            &mut states_to_visit,
        )?;
    }

    Ok(interleave)
}

impl Interleave {
    fn add_state(
        &mut self,
        sid: StateID,
        state: &State,
        byte_classes: &ByteClasses,
        visited: &mut [bool],
        states_to_visit: &mut VecDeque<StateID>,
    ) -> Result<(), BuildError> {
        // If a matching state has a child that is also a matching state, we will visit the state
        // when building the max_matching_state_id value, but also add the state in the queue.
        // This check is thus needed to avoid visiting the state twice.
        if self.index_to_state_id[sid] != NFA::DEAD {
            return Ok(());
        }

        // Start the search at the max of all search indexes for the offsets to push in.
        let mut max_offset = 0;
        let mut search_index = self.offset_to_search_index[0];
        for (byte, _) in &state.trans {
            let offset = *byte as usize + 1;
            let new_search_index = self.offset_to_search_index[offset];
            if new_search_index > search_index {
                search_index = new_search_index;
                max_offset = offset;
            }
        }

        // Search for a place in the table for the state + its transitions.
        'OUTER: loop {
            while self.available_indexes.used(search_index) {
                search_index += 1;
                continue;
            }

            for (byte, _) in &state.trans {
                let offset = (byte_classes.get(*byte) as usize) + 1;
                if self.available_indexes.used(search_index + offset) {
                    search_index += 1;
                    continue 'OUTER;
                }
            }
            break;
        }

        // An index to place this state has been found, save the new state id.
        let new_sid = StateID::new(search_index).map_err(|e| {
            BuildError::state_id_overflow(StateID::MAX.as_u64(), e.attempted())
        })?;
        self.index_to_state_id[sid] = new_sid;
        self.max_state_id = std::cmp::max(self.max_state_id, new_sid);

        // Mark indexes that are now used.
        self.available_indexes.mark_used(search_index);
        for (byte, target_sid) in &state.trans {
            let class = byte_classes.get(*byte);
            let idx = search_index + (class as usize) + 1;
            self.available_indexes.mark_used(idx);

            if !std::mem::replace(&mut visited[target_sid.as_usize()], true) {
                states_to_visit.push_back(*target_sid);
            }
        }

        // Advance the search index for the offset that was the max.
        // This is a completely arbitrary heuristic to advance it,
        // and does not represent the "first available" index for
        // this offset. This strives to strike a balance between
        // speeding up the algorithm (so avoiding starting searches
        // too early in the table) and keeping a good compression
        // ratio thanks to interleaving.
        let v = &mut self.offset_to_search_index;
        v[max_offset] +=
            (search_index - v[max_offset]) / (state.trans.len() + 1);

        Ok(())
    }
}

/// Helper to memorize which indexes in the final table are used.
#[derive(Debug, Default)]
struct AvailableIndexes {
    used: Vec<bool>,
}

impl AvailableIndexes {
    /// Returns whether an index in the table is already used.
    pub fn used(&mut self, idx: usize) -> bool {
        self.used.get(idx).copied().unwrap_or(false)
    }

    pub fn mark_used(&mut self, idx: usize) {
        if idx >= self.used.len() {
            self.used.resize(idx + 1, false);
        }
        self.used[idx] = true;
    }
}
