use core::{
    fmt::Debug,
    panic::{RefUnwindSafe, UnwindSafe},
};

use alloc::sync::Arc;

use crate::{
    packed::{
        ext::Pointer,
        pattern::Patterns,
        teddy::generic::{self, Match},
        vector::Vector,
    },
    util::int::U16,
};

/// A builder for constructing a Teddy matcher.
///
/// The builder primarily permits fine grained configuration of the Teddy
/// matcher. Most options are made only available for testing/benchmarking
/// purposes. In reality, options are automatically determined by the nature
/// and number of patterns given to the builder.
#[derive(Clone, Debug)]
pub(crate) struct Builder {
    /// When none, this is automatically determined. Otherwise, `false` means
    /// slim Teddy is used (8 buckets) and `true` means fat Teddy is used
    /// (16 buckets). Fat Teddy requires AVX2, so if that CPU feature isn't
    /// available and Fat Teddy was requested, no matcher will be built.
    only_fat: Option<bool>,
    /// When none, this is automatically determined. Otherwise, `false` means
    /// that 128-bit vectors will be used (up to SSSE3 instructions) where as
    /// `true` means that 256-bit vectors will be used. As with `fat`, if
    /// 256-bit vectors are requested and they aren't available, then a
    /// searcher will not be built.
    only_256bit: Option<bool>,
}

impl Default for Builder {
    fn default() -> Builder {
        Builder::new()
    }
}

impl Builder {
    /// Create a new builder for configuring a Teddy matcher.
    pub(crate) fn new() -> Builder {
        Builder { only_fat: None, only_256bit: None }
    }

    /// Build a matcher for the set of patterns given. If a matcher could not
    /// be built, then `None` is returned.
    ///
    /// Generally, a matcher isn't built if the necessary CPU features aren't
    /// available, an unsupported target or if the searcher is believed to be
    /// slower than standard techniques (i.e., if there are too many literals).
    pub(crate) fn build(&self, patterns: Arc<Patterns>) -> Option<Searcher> {
        self.build_imp(patterns)
    }

    /// Require the use of Fat (true) or Slim (false) Teddy. Fat Teddy uses
    /// 16 buckets where as Slim Teddy uses 8 buckets. More buckets are useful
    /// for a larger set of literals.
    ///
    /// `None` is the default, which results in an automatic selection based
    /// on the number of literals and available CPU features.
    pub(crate) fn only_fat(&mut self, yes: Option<bool>) -> &mut Builder {
        self.only_fat = yes;
        self
    }

    /// Request the use of 256-bit vectors (true) or 128-bit vectors (false).
    /// Generally, a larger vector size is better since it either permits
    /// matching more patterns or matching more bytes in the haystack at once.
    ///
    /// `None` is the default, which results in an automatic selection based on
    /// the number of literals and available CPU features.
    pub(crate) fn only_256bit(&mut self, yes: Option<bool>) -> &mut Builder {
        self.only_256bit = yes;
        self
    }

    fn build_imp(&self, patterns: Arc<Patterns>) -> Option<Searcher> {
        if patterns.len() > 64 {
            debug!("skipping Teddy because of too many patterns");
            return None;
        }
        let mask_len = core::cmp::min(4, patterns.minimum_len());
        let beefy = patterns.len() > 32;

        #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
        {
            use self::x86_64::{FatAVX2, SlimAVX2, SlimSSSE3};

            let has_avx2 = self::x86_64::is_available_avx2();
            let has_ssse3 = has_avx2 || self::x86_64::is_available_ssse3();
            let use_avx2 = if self.only_256bit == Some(true) {
                if !has_avx2 {
                    debug!(
                    "skipping Teddy because avx2 was demanded but unavailable"
                );
                    return None;
                }
                true
            } else if self.only_256bit == Some(false) {
                if !has_ssse3 {
                    debug!(
                    "skipping Teddy because ssse3 was demanded but unavailable"
                );
                    return None;
                }
                false
            } else if !has_ssse3 && !has_avx2 {
                debug!(
                    "skipping Teddy because ssse3 and avx2 are unavailable"
                );
                return None;
            } else {
                has_avx2
            };
            let fat = match self.only_fat {
                None => use_avx2 && beefy,
                Some(false) => false,
                Some(true) if !use_avx2 => {
                    debug!(
                        "skipping Teddy because fat was demanded, but fat \
                         Teddy requires avx2 which is unavailable"
                    );
                    return None;
                }
                Some(true) => true,
            };
            match (mask_len, use_avx2, fat) {
                (1, false, _) => {
                    debug!("Teddy choice: 128-bit slim, 1 byte");
                    SlimSSSE3::<1>::new(&patterns)
                }
                (1, true, false) => {
                    debug!("Teddy choice: 256-bit slim, 1 byte");
                    SlimAVX2::<1>::new(&patterns)
                }
                (1, true, true) => {
                    debug!("Teddy choice: 256-bit fat, 1 byte");
                    FatAVX2::<1>::new(&patterns)
                }
                (2, false, _) => {
                    debug!("Teddy choice: 128-bit slim, 2 bytes");
                    SlimSSSE3::<2>::new(&patterns)
                }
                (2, true, false) => {
                    debug!("Teddy choice: 256-bit slim, 2 bytes");
                    SlimAVX2::<2>::new(&patterns)
                }
                (2, true, true) => {
                    debug!("Teddy choice: 256-bit fat, 2 bytes");
                    FatAVX2::<2>::new(&patterns)
                }
                (3, false, _) => {
                    debug!("Teddy choice: 128-bit slim, 3 bytes");
                    SlimSSSE3::<3>::new(&patterns)
                }
                (3, true, false) => {
                    debug!("Teddy choice: 256-bit slim, 3 bytes");
                    SlimAVX2::<3>::new(&patterns)
                }
                (3, true, true) => {
                    debug!("Teddy choice: 256-bit fat, 3 bytes");
                    FatAVX2::<3>::new(&patterns)
                }
                (4, false, _) => {
                    debug!("Teddy choice: 128-bit slim, 4 bytes");
                    SlimSSSE3::<4>::new(&patterns)
                }
                (4, true, false) => {
                    debug!("Teddy choice: 256-bit slim, 4 bytes");
                    SlimAVX2::<4>::new(&patterns)
                }
                (4, true, true) => {
                    debug!("Teddy choice: 256-bit fat, 4 bytes");
                    FatAVX2::<4>::new(&patterns)
                }
                _ => {
                    debug!("no supported Teddy configuration found");
                    None
                }
            }
        }
        #[cfg(target_arch = "aarch64")]
        {
            use self::aarch64::SlimNeon;

            if self.only_256bit == Some(true) {
                debug!(
                    "skipping Teddy because 256-bits were demanded \
                     but unavailable"
                );
                return None;
            }
            if self.only_fat == Some(true) {
                debug!(
                    "skipping Teddy because fat was demanded but unavailable"
                );
            }
            match mask_len {
                1 => {
                    debug!("Teddy choice: 128-bit slim, 1 byte");
                    SlimNeon::<1>::new(&patterns)
                }
                2 => {
                    debug!("Teddy choice: 128-bit slim, 2 bytes");
                    SlimNeon::<2>::new(&patterns)
                }
                3 => {
                    debug!("Teddy choice: 128-bit slim, 3 bytes");
                    SlimNeon::<3>::new(&patterns)
                }
                4 => {
                    debug!("Teddy choice: 128-bit slim, 4 bytes");
                    SlimNeon::<4>::new(&patterns)
                }
                _ => {
                    debug!("no supported Teddy configuration found");
                    None
                }
            }
        }
        #[cfg(not(any(
            all(target_arch = "x86_64", target_feature = "sse2"),
            target_arch = "aarch64"
        )))]
        {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Searcher {
    imp: Arc<dyn SearcherT>,
    memory_usage: usize,
    minimum_len: usize,
}

impl Searcher {
    #[inline(always)]
    pub(crate) fn find(
        &self,
        haystack: &[u8],
        at: usize,
    ) -> Option<crate::Match> {
        let hayptr = haystack.as_ptr();
        let teddym = unsafe {
            self.imp.find(hayptr.add(at), hayptr.add(haystack.len()))?
        };
        let start = teddym.start().as_usize().wrapping_sub(hayptr.as_usize());
        let end = teddym.end().as_usize().wrapping_sub(hayptr.as_usize());
        let span = crate::Span { start, end };
        // OK because we won't permit the construction of a searcher that
        // could report a pattern ID bigger than what can fit in the crate-wide
        // PatternID type.
        let pid = crate::PatternID::new_unchecked(teddym.pattern().as_usize());
        let m = crate::Match::new(pid, span);
        Some(m)
    }

    /// Returns the approximate total amount of heap used by this type, in
    /// units of bytes.
    #[inline(always)]
    pub(crate) fn memory_usage(&self) -> usize {
        self.memory_usage
    }

    /// Returns the minimum length, in bytes, that a haystack must be in order
    /// to use it with this searcher.
    #[inline(always)]
    pub(crate) fn minimum_len(&self) -> usize {
        self.minimum_len
    }
}

/// A trait that provides dynamic dispatch over the different possible Teddy
/// variants on the same algorithm.
///
/// On `x86_64` for example, it isn't known until runtime which of 12 possible
/// variants will be used. One might use one of the four slim 128-bit vector
/// variants, or one of the four 256-bit vector variants or even one of the
/// four fat 256-bit vector variants.
///
/// Since this choice is generally made when the Teddy searcher is constructed
/// and this choice is based on the patterns given and what the current CPU
/// supports, it follows that there must be some kind of indirection at search
/// time that "selects" the variant chosen at build time.
///
/// There are a few different ways to go about this. One approach is to use an
/// enum. It works fine, but in my experiments, this generally results in worse
/// codegen. Another approach, which is what we use here, is dynamic dispatch
/// via a trait object. We basically implement this trait for each possible
/// variant, select the variant we want at build time and convert it to a
/// trait object for use at search time.
///
/// Another approach is to use function pointers and stick each of the possible
/// variants into a union. This is essentially isomorphic to the dynamic
/// dispatch approach, but doesn't require any allocations. Since this crate
/// requires `alloc`, there's no real reason (AFAIK) to go down this path. (The
/// `memchr` crate does this.)
trait SearcherT:
    Debug + Send + Sync + UnwindSafe + RefUnwindSafe + 'static
{
    /// Execute a search on the given haystack (identified by `start` and `end`
    /// raw pointers).
    ///
    /// # Safety
    ///
    /// Essentially, the `start` and `end` pointers must be valid and point
    /// to a haystack one can read. As long as you derive them from, for
    /// example, a `&[u8]`, they should automatically satisfy all of the safety
    /// obligations:
    ///
    /// * Both `start` and `end` must be valid for reads.
    /// * Both `start` and `end` must point to an initialized value.
    /// * Both `start` and `end` must point to the same allocated object and
    /// must either be in bounds or at most one byte past the end of the
    /// allocated object.
    /// * Both `start` and `end` must be _derived from_ a pointer to the same
    /// object.
    /// * The distance between `start` and `end` must not overflow `isize`.
    /// * The distance being in bounds must not rely on "wrapping around" the
    /// address space.
    /// * It must be the case that `start <= end`.
    unsafe fn find(&self, start: *const u8, end: *const u8) -> Option<Match>;
}

#[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
mod x86_64 {
    use core::arch::x86_64::{__m128i, __m256i};

    use alloc::sync::Arc;

    use crate::packed::{
        ext::Pointer,
        pattern::Patterns,
        teddy::generic::{self, Match},
        vector::Vector,
    };

    use super::{Searcher, SearcherT};

    #[derive(Clone, Debug)]
    pub(super) struct SlimSSSE3<const N: usize> {
        slim128: generic::Slim<__m128i, N>,
    }

    // Defines SlimSSSE3 wrapper functions for 1, 2, 3 and 4 bytes.
    macro_rules! slim_ssse3 {
        ($len:expr) => {
            impl SlimSSSE3<$len> {
                /// Creates a new searcher using "slim" Teddy with 128-bit
                /// vectors. If SSSE3 is not available in the current
                /// environment, then this returns `None`.
                pub(super) fn new(
                    patterns: &Arc<Patterns>,
                ) -> Option<Searcher> {
                    if !is_available_ssse3() {
                        return None;
                    }
                    Some(unsafe { SlimSSSE3::<$len>::new_unchecked(patterns) })
                }

                /// Creates a new searcher using "slim" Teddy with 256-bit
                /// vectors without checking whether SSSE3 is available or not.
                ///
                /// # Safety
                ///
                /// Callers must ensure that SSSE3 is available in the current
                /// environment.
                #[target_feature(enable = "ssse3")]
                unsafe fn new_unchecked(patterns: &Arc<Patterns>) -> Searcher {
                    let slim128 = generic::Slim::<__m128i, $len>::new(
                        Arc::clone(patterns),
                    );
                    let memory_usage = slim128.memory_usage();
                    let minimum_len = slim128.minimum_len();
                    let imp = Arc::new(SlimSSSE3 { slim128 });
                    Searcher { imp, memory_usage, minimum_len }
                }
            }

            impl SearcherT for SlimSSSE3<$len> {
                #[target_feature(enable = "ssse3")]
                #[inline]
                unsafe fn find(
                    &self,
                    start: *const u8,
                    end: *const u8,
                ) -> Option<Match> {
                    // SAFETY: All obligations except for `target_feature` are
                    // passed to the caller. Our use of `target_feature` is
                    // safe because construction of this type requires that the
                    // requisite target features are available.
                    self.slim128.find(start, end)
                }
            }
        };
    }

    slim_ssse3!(1);
    slim_ssse3!(2);
    slim_ssse3!(3);
    slim_ssse3!(4);

    #[derive(Clone, Debug)]
    pub(super) struct SlimAVX2<const N: usize> {
        slim128: generic::Slim<__m128i, N>,
        slim256: generic::Slim<__m256i, N>,
    }

    // Defines SlimAVX2 wrapper functions for 1, 2, 3 and 4 bytes.
    macro_rules! slim_avx2 {
        ($len:expr) => {
            impl SlimAVX2<$len> {
                /// Creates a new searcher using "slim" Teddy with 256-bit
                /// vectors. If AVX2 is not available in the current
                /// environment, then this returns `None`.
                pub(super) fn new(
                    patterns: &Arc<Patterns>,
                ) -> Option<Searcher> {
                    if !is_available_avx2() {
                        return None;
                    }
                    Some(unsafe { SlimAVX2::<$len>::new_unchecked(patterns) })
                }

                /// Creates a new searcher using "slim" Teddy with 256-bit
                /// vectors without checking whether AVX2 is available or not.
                ///
                /// # Safety
                ///
                /// Callers must ensure that AVX2 is available in the current
                /// environment.
                #[target_feature(enable = "avx2")]
                unsafe fn new_unchecked(patterns: &Arc<Patterns>) -> Searcher {
                    let slim128 = generic::Slim::<__m128i, $len>::new(
                        Arc::clone(&patterns),
                    );
                    let slim256 = generic::Slim::<__m256i, $len>::new(
                        Arc::clone(&patterns),
                    );
                    let memory_usage =
                        slim128.memory_usage() + slim256.memory_usage();
                    let minimum_len = slim128.minimum_len();
                    let imp = Arc::new(SlimAVX2 { slim128, slim256 });
                    Searcher { imp, memory_usage, minimum_len }
                }
            }

            impl SearcherT for SlimAVX2<$len> {
                #[target_feature(enable = "avx2")]
                #[inline]
                unsafe fn find(
                    &self,
                    start: *const u8,
                    end: *const u8,
                ) -> Option<Match> {
                    // SAFETY: All obligations except for `target_feature` are
                    // passed to the caller. Our use of `target_feature` is
                    // safe because construction of this type requires that the
                    // requisite target features are available.
                    let len = end.distance(start);
                    if len < self.slim256.minimum_len() {
                        self.slim128.find(start, end)
                    } else {
                        self.slim256.find(start, end)
                    }
                }
            }
        };
    }

    slim_avx2!(1);
    slim_avx2!(2);
    slim_avx2!(3);
    slim_avx2!(4);

    #[derive(Clone, Debug)]
    pub(super) struct FatAVX2<const N: usize> {
        fat256: generic::Fat<__m256i, N>,
    }

    // Defines SlimAVX2 wrapper functions for 1, 2, 3 and 4 bytes.
    macro_rules! fat_avx2 {
        ($len:expr) => {
            impl FatAVX2<$len> {
                /// Creates a new searcher using "slim" Teddy with 256-bit
                /// vectors. If AVX2 is not available in the current
                /// environment, then this returns `None`.
                pub(super) fn new(
                    patterns: &Arc<Patterns>,
                ) -> Option<Searcher> {
                    if !is_available_avx2() {
                        return None;
                    }
                    Some(unsafe { FatAVX2::<$len>::new_unchecked(patterns) })
                }

                /// Creates a new searcher using "slim" Teddy with 256-bit
                /// vectors without checking whether AVX2 is available or not.
                ///
                /// # Safety
                ///
                /// Callers must ensure that AVX2 is available in the current
                /// environment.
                #[target_feature(enable = "avx2")]
                unsafe fn new_unchecked(patterns: &Arc<Patterns>) -> Searcher {
                    let fat256 = generic::Fat::<__m256i, $len>::new(
                        Arc::clone(&patterns),
                    );
                    let memory_usage = fat256.memory_usage();
                    let minimum_len = fat256.minimum_len();
                    let imp = Arc::new(FatAVX2 { fat256 });
                    Searcher { imp, memory_usage, minimum_len }
                }
            }

            impl SearcherT for FatAVX2<$len> {
                #[target_feature(enable = "avx2")]
                #[inline]
                unsafe fn find(
                    &self,
                    start: *const u8,
                    end: *const u8,
                ) -> Option<Match> {
                    // SAFETY: All obligations except for `target_feature` are
                    // passed to the caller. Our use of `target_feature` is
                    // safe because construction of this type requires that the
                    // requisite target features are available.
                    self.fat256.find(start, end)
                }
            }
        };
    }

    fat_avx2!(1);
    fat_avx2!(2);
    fat_avx2!(3);
    fat_avx2!(4);

    #[inline]
    pub(super) fn is_available_ssse3() -> bool {
        #[cfg(not(target_feature = "sse2"))]
        {
            false
        }
        #[cfg(target_feature = "sse2")]
        {
            #[cfg(target_feature = "ssse3")]
            {
                true
            }
            #[cfg(not(target_feature = "ssse3"))]
            {
                #[cfg(feature = "std")]
                {
                    std::is_x86_feature_detected!("ssse3")
                }
                #[cfg(not(feature = "std"))]
                {
                    false
                }
            }
        }
    }

    #[inline]
    pub(super) fn is_available_avx2() -> bool {
        #[cfg(not(target_feature = "sse2"))]
        {
            false
        }
        #[cfg(target_feature = "sse2")]
        {
            #[cfg(target_feature = "avx2")]
            {
                true
            }
            #[cfg(not(target_feature = "avx2"))]
            {
                #[cfg(feature = "std")]
                {
                    std::is_x86_feature_detected!("avx2")
                }
                #[cfg(not(feature = "std"))]
                {
                    false
                }
            }
        }
    }
}

#[cfg(target_arch = "aarch64")]
mod aarch64 {
    use core::arch::aarch64::uint8x16_t;

    use alloc::sync::Arc;

    use crate::packed::{
        ext::Pointer,
        pattern::Patterns,
        teddy::generic::{self, Match},
        vector::Vector,
    };

    use super::{Searcher, SearcherT};

    #[derive(Clone, Debug)]
    pub(super) struct SlimNeon<const N: usize> {
        slim128: generic::Slim<uint8x16_t, N>,
    }

    // Defines SlimSSSE3 wrapper functions for 1, 2, 3 and 4 bytes.
    macro_rules! slim_neon {
        ($len:expr) => {
            impl SlimNeon<$len> {
                /// Creates a new searcher using "slim" Teddy with 128-bit
                /// vectors. If SSSE3 is not available in the current
                /// environment, then this returns `None`.
                pub(super) fn new(
                    patterns: &Arc<Patterns>,
                ) -> Option<Searcher> {
                    Some(unsafe { SlimNeon::<$len>::new_unchecked(patterns) })
                }

                /// Creates a new searcher using "slim" Teddy with 256-bit
                /// vectors without checking whether SSSE3 is available or not.
                ///
                /// # Safety
                ///
                /// Callers must ensure that SSSE3 is available in the current
                /// environment.
                #[target_feature(enable = "neon")]
                unsafe fn new_unchecked(patterns: &Arc<Patterns>) -> Searcher {
                    let slim128 = generic::Slim::<uint8x16_t, $len>::new(
                        Arc::clone(patterns),
                    );
                    let memory_usage = slim128.memory_usage();
                    let minimum_len = slim128.minimum_len();
                    let imp = Arc::new(SlimNeon { slim128 });
                    Searcher { imp, memory_usage, minimum_len }
                }
            }

            impl SearcherT for SlimNeon<$len> {
                #[target_feature(enable = "neon")]
                #[inline]
                unsafe fn find(
                    &self,
                    start: *const u8,
                    end: *const u8,
                ) -> Option<Match> {
                    // SAFETY: All obligations except for `target_feature` are
                    // passed to the caller. Our use of `target_feature` is
                    // safe because construction of this type requires that the
                    // requisite target features are available.
                    self.slim128.find(start, end)
                }
            }
        };
    }

    slim_neon!(1);
    slim_neon!(2);
    slim_neon!(3);
    slim_neon!(4);
}
