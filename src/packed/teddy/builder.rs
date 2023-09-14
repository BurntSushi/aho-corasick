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
    fat: Option<bool>,
    /// When none, this is automatically determined. Otherwise, `false` means
    /// that 128-bit vectors will be used (up to SSSE3 instructions) where as
    /// `true` means that 256-bit vectors will be used. As with `fat`, if
    /// 256-bit vectors are requested and they aren't available, then a
    /// searcher will not be built.
    avx: Option<bool>,
}

impl Default for Builder {
    fn default() -> Builder {
        Builder::new()
    }
}

impl Builder {
    /// Create a new builder for configuring a Teddy matcher.
    pub(crate) fn new() -> Builder {
        Builder { fat: None, avx: None }
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
    pub(crate) fn fat(&mut self, yes: Option<bool>) -> &mut Builder {
        self.fat = yes;
        self
    }

    /// Request the use of 256-bit vectors (true) or 128-bit vectors (false).
    /// Generally, a larger vector size is better since it either permits
    /// matching more patterns or matching more bytes in the haystack at once.
    ///
    /// `None` is the default, which results in an automatic selection based on
    /// the number of literals and available CPU features.
    pub(crate) fn avx(&mut self, yes: Option<bool>) -> &mut Builder {
        self.avx = yes;
        self
    }

    fn build_imp(&self, patterns: Arc<Patterns>) -> Option<Searcher> {
        if patterns.len() > 64 {
            debug!("skipping Teddy because of too many patterns");
            return None;
        }
        let mask_len = core::cmp::min(4, patterns.minimum_len());

        #[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
        {
            use core::arch::x86_64::{__m128i, __m256i};

            let has_avx2 = self::x86_64::is_available_avx2();
            let has_ssse3 = has_avx2 || self::x86_64::is_available_ssse3();
            let use_avx2 = if self.avx == Some(true) {
                if !has_avx2 {
                    debug!(
                    "skipping Teddy because avx2 was demanded but unavailable"
                );
                    return None;
                }
                true
            } else if self.avx == Some(false) {
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
            let fat = match self.fat {
                None => use_avx2 && patterns.len() > 32,
                Some(false) => false,
                Some(true) if !use_avx2 => {
                    debug!(
                        "skipping Teddy because it needs to be fat, but fat \
                         Teddy requires avx2 which is unavailable"
                    );
                    return None;
                }
                Some(true) => true,
            };
            match (mask_len, use_avx2, fat) {
                (1, false, _) => {
                    debug!("Teddy choice: 128-bit slim, 1 byte");
                    let slim128 =
                        unsafe { generic::Slim::<__m128i, 1>::new(patterns) };
                    let memory_usage = slim128.memory_usage();
                    let minimum_len = slim128.minimum_len();
                    let imp = Arc::new(self::x86_64::SlimSSSE3 { slim128 });
                    return Some(Searcher { imp, memory_usage, minimum_len });
                }
                (1, true, false) => {
                    debug!("Teddy choice: 256-bit slim, 1 byte");
                    let slim128 = unsafe {
                        generic::Slim::<__m128i, 1>::new(Arc::clone(&patterns))
                    };
                    let slim256 =
                        unsafe { generic::Slim::<__m256i, 1>::new(patterns) };
                    let memory_usage =
                        slim128.memory_usage() + slim256.memory_usage();
                    let minimum_len = slim128.minimum_len();
                    let imp =
                        Arc::new(self::x86_64::SlimAVX2 { slim128, slim256 });
                    return Some(Searcher { imp, memory_usage, minimum_len });
                }
                _ => {
                    debug!("no supported Teddy configuration found");
                    return None;
                }
            }
        }

        None
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

trait SearcherT:
    Debug + Send + Sync + UnwindSafe + RefUnwindSafe + 'static
{
    unsafe fn find(&self, start: *const u8, end: *const u8) -> Option<Match>;
}

#[cfg(all(target_arch = "x86_64", target_feature = "sse2"))]
mod x86_64 {
    use core::arch::x86_64::{__m128i, __m256i};

    use crate::packed::{
        ext::Pointer,
        teddy::generic::{self, Match},
        vector::Vector,
    };

    use super::SearcherT;

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

    #[derive(Clone, Debug)]
    pub(super) struct SlimSSSE3<const N: usize> {
        pub(super) slim128: generic::Slim<__m128i, N>,
    }

    impl SearcherT for SlimSSSE3<1> {
        #[target_feature(enable = "ssse3")]
        #[inline]
        unsafe fn find(
            &self,
            start: *const u8,
            end: *const u8,
        ) -> Option<Match> {
            self.slim128.find(start, end)
        }
    }

    #[derive(Clone, Debug)]
    pub(super) struct SlimAVX2<const N: usize> {
        pub(super) slim128: generic::Slim<__m128i, N>,
        pub(super) slim256: generic::Slim<__m256i, N>,
    }

    impl SearcherT for SlimAVX2<1> {
        #[target_feature(enable = "avx2")]
        #[inline]
        unsafe fn find(
            &self,
            start: *const u8,
            end: *const u8,
        ) -> Option<Match> {
            let len = end.distance(start);
            if len < self.slim256.minimum_len() {
                self.slim128.find(start, end)
            } else {
                self.slim256.find(start, end)
            }
        }
    }
}
