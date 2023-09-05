/// A trait for describing vector operations used by vectorized searchers.
///
/// The trait is highly constrained to low level vector operations needed for
/// the specific algorithms used in this crate. In general, it was invented
/// mostly to be generic over x86's __m128i and __m256i types. At time of
/// writing, it also supports wasm and aarch64 128-bit vector types as well.
///
/// # Safety
///
/// All methods are not safe since they are intended to be implemented using
/// vendor intrinsics, which are also not safe. Callers must ensure that
/// the appropriate target features are enabled in the calling function,
/// and that the current CPU supports them. All implementations should
/// avoid marking the routines with `#[target_feature]` and instead mark
/// them as `#[inline(always)]` to ensure they get appropriately inlined.
/// (`inline(always)` cannot be used with target_feature.)
pub(crate) trait Vector: Copy + core::fmt::Debug {
    /// The number of bits in the vector.
    const BITS: usize;
    /// The number of bytes in the vector. That is, this is the size of the
    /// vector in memory.
    const BYTES: usize;
    /// The bits that must be zero in order for a `*const u8` pointer to be
    /// correctly aligned to read vector values.
    const ALIGN: usize;

    /// Create a vector with 8-bit lanes with the given byte repeated into each
    /// lane.
    ///
    /// # Safety
    ///
    /// Callers must ensure that this is okay to call in the current target for
    /// the current CPU.
    unsafe fn splat(byte: u8) -> Self;

    /// Read a vector-size number of bytes from the given pointer. The pointer
    /// does not need to be aligned.
    ///
    /// # Safety
    ///
    /// Callers must ensure that this is okay to call in the current target for
    /// the current CPU.
    ///
    /// Callers must guarantee that at least `BYTES` bytes are readable from
    /// `data`.
    unsafe fn load_unaligned(data: *const u8) -> Self;

    /// Do an 8-bit pairwise equality check. If lane `i` is equal in this
    /// vector and the one given, then lane `i` in the resulting vector is set
    /// to `0xFF`. Otherwise, it is set to `0x00`.
    ///
    /// # Safety
    ///
    /// Callers must ensure that this is okay to call in the current target for
    /// the current CPU.
    unsafe fn cmpeq(self, vector2: Self) -> Self;

    /// Perform a bitwise 'and' of this vector and the one given and return
    /// the result.
    ///
    /// # Safety
    ///
    /// Callers must ensure that this is okay to call in the current target for
    /// the current CPU.
    unsafe fn and(self, vector2: Self) -> Self;

    /// Perform a bitwise 'or' of this vector and the one given and return
    /// the result.
    ///
    /// # Safety
    ///
    /// Callers must ensure that this is okay to call in the current target for
    /// the current CPU.
    unsafe fn or(self, vector2: Self) -> Self;

    /// Returns true if and only if this vector has zero in all of its lanes.
    ///
    /// # Safety
    ///
    /// Callers must ensure that this is okay to call in the current target for
    /// the current CPU.
    unsafe fn is_zero(self) -> bool;
}

#[cfg(target_arch = "x86_64")]
mod x86sse2 {
    use core::arch::x86_64::*;

    use super::Vector;

    impl Vector for __m128i {
        const BITS: usize = 128;
        const BYTES: usize = 16;
        const ALIGN: usize = Self::BYTES - 1;

        #[inline(always)]
        unsafe fn splat(byte: u8) -> __m128i {
            _mm_set1_epi8(byte as i8)
        }

        #[inline(always)]
        unsafe fn load_unaligned(data: *const u8) -> __m128i {
            _mm_loadu_si128(data as *const __m128i)
        }

        #[inline(always)]
        unsafe fn cmpeq(self, vector2: Self) -> __m128i {
            _mm_cmpeq_epi8(self, vector2)
        }

        #[inline(always)]
        unsafe fn and(self, vector2: Self) -> __m128i {
            _mm_and_si128(self, vector2)
        }

        #[inline(always)]
        unsafe fn or(self, vector2: Self) -> __m128i {
            _mm_or_si128(self, vector2)
        }

        #[inline(always)]
        unsafe fn is_zero(self) -> bool {
            let cmp = self.cmpeq(Self::splat(0));
            _mm_movemask_epi8(cmp) as u32 == 0xFFFF
        }
    }
}

#[cfg(target_arch = "x86_64")]
mod x86avx2 {
    use core::arch::x86_64::*;

    use super::Vector;

    impl Vector for __m256i {
        const BITS: usize = 256;
        const BYTES: usize = 32;
        const ALIGN: usize = Self::BYTES - 1;

        #[inline(always)]
        unsafe fn splat(byte: u8) -> __m256i {
            _mm256_set1_epi8(byte as i8)
        }

        #[inline(always)]
        unsafe fn load_unaligned(data: *const u8) -> __m256i {
            _mm256_loadu_si256(data as *const __m256i)
        }

        #[inline(always)]
        unsafe fn cmpeq(self, vector2: Self) -> __m256i {
            _mm256_cmpeq_epi8(self, vector2)
        }

        #[inline(always)]
        unsafe fn and(self, vector2: Self) -> __m256i {
            _mm256_and_si256(self, vector2)
        }

        #[inline(always)]
        unsafe fn or(self, vector2: Self) -> __m256i {
            _mm256_or_si256(self, vector2)
        }

        #[inline(always)]
        unsafe fn is_zero(self) -> bool {
            let cmp = self.cmpeq(Self::splat(0));
            _mm256_movemask_epi8(cmp) as u32 == 0xFFFFFFFF
        }
    }
}

#[cfg(target_arch = "aarch64")]
mod aarch64neon {
    use core::arch::aarch64::*;

    use super::Vector;

    impl Vector for uint8x16_t {
        const BITS: usize = 128;
        const BYTES: usize = 16;
        const ALIGN: usize = Self::BYTES - 1;

        #[inline(always)]
        unsafe fn splat(byte: u8) -> uint8x16_t {
            vdupq_n_u8(byte)
        }

        #[inline(always)]
        unsafe fn load_unaligned(data: *const u8) -> uint8x16_t {
            vld1q_u8(data)
        }

        #[inline(always)]
        unsafe fn cmpeq(self, vector2: Self) -> uint8x16_t {
            vceqq_u8(self, vector2)
        }

        #[inline(always)]
        unsafe fn and(self, vector2: Self) -> uint8x16_t {
            vandq_u8(self, vector2)
        }

        #[inline(always)]
        unsafe fn or(self, vector2: Self) -> uint8x16_t {
            vorrq_u8(self, vector2)
        }

        #[inline(always)]
        unsafe fn is_zero(self) -> bool {
            // could also use vmaxvq_u8
            let maxes = vreinterpretq_u64_u8(vpmaxq_u8(self, self));
            vgetq_lane_u64(maxes, 0) == 0
        }
    }
}

// BREADCRUMBS: Write example tests using intrinsics directly. Do it by
// writing a convenience function for loading a vector from an array using
// load_unaligned. This should make documenting my understanding easier since
// Teddy requires a much bigger vocabulary of vector functions than the memchr
// crate does.

#[cfg(all(test, target_arch = "aarch64", target_feature = "neon"))]
mod tests {
    use core::arch::aarch64::*;

    use super::*;

    #[target_feature(enable = "neon")]
    unsafe fn load(lanes: [u8; 16]) -> uint8x16_t {
        uint8x16_t::load_unaligned(&lanes as *const u8)
    }

    #[target_feature(enable = "neon")]
    unsafe fn unload(v: uint8x16_t) -> [u8; 16] {
        [
            vgetq_lane_u8(v, 0),
            vgetq_lane_u8(v, 1),
            vgetq_lane_u8(v, 2),
            vgetq_lane_u8(v, 3),
            vgetq_lane_u8(v, 4),
            vgetq_lane_u8(v, 5),
            vgetq_lane_u8(v, 6),
            vgetq_lane_u8(v, 7),
            vgetq_lane_u8(v, 8),
            vgetq_lane_u8(v, 9),
            vgetq_lane_u8(v, 10),
            vgetq_lane_u8(v, 11),
            vgetq_lane_u8(v, 12),
            vgetq_lane_u8(v, 13),
            vgetq_lane_u8(v, 14),
            vgetq_lane_u8(v, 15),
        ]
    }

    #[test]
    fn vector_splat() {
        #[target_feature(enable = "neon")]
        unsafe fn test() {
            let v = uint8x16_t::splat(0xAF);
            assert_eq!(
                unload(v),
                [
                    0xAF, 0xAF, 0xAF, 0xAF, 0xAF, 0xAF, 0xAF, 0xAF, 0xAF,
                    0xAF, 0xAF, 0xAF, 0xAF, 0xAF, 0xAF, 0xAF
                ]
            );
        }
        unsafe { test() }
    }

    #[test]
    fn vector_cmpeq() {
        #[target_feature(enable = "neon")]
        unsafe fn test() {
            let v1 =
                load([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1]);
            let v2 =
                load([16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]);
            assert_eq!(
                unload(v1.cmpeq(v2)),
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0xFF]
            );
        }
        unsafe { test() }
    }

    #[test]
    fn vector_and() {
        #[target_feature(enable = "neon")]
        unsafe fn test() {
            let v1 =
                load([0, 0, 0, 0, 0, 0b1001, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
            let v2 =
                load([0, 0, 0, 0, 0, 0b1010, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
            assert_eq!(
                unload(v1.and(v2)),
                [0, 0, 0, 0, 0, 0b1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            );
        }
        unsafe { test() }
    }

    #[test]
    fn vector_or() {
        #[target_feature(enable = "neon")]
        unsafe fn test() {
            let v1 =
                load([0, 0, 0, 0, 0, 0b1001, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
            let v2 =
                load([0, 0, 0, 0, 0, 0b1010, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
            assert_eq!(
                unload(v1.or(v2)),
                [0, 0, 0, 0, 0, 0b1011, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
            );
        }
        unsafe { test() }
    }

    #[test]
    fn vector_is_zero() {
        #[target_feature(enable = "neon")]
        unsafe fn test() {
            let v = load([0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
            assert!(!v.is_zero());
            let v = load([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
            assert!(v.is_zero());
        }
        unsafe { test() }
    }

    #[test]
    fn example_vmaxvq_u8_non_zero() {
        #[target_feature(enable = "neon")]
        unsafe fn example() {
            let v = load([0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
            assert_eq!(vmaxvq_u8(v), 1);
        }
        unsafe { example() }
    }

    #[test]
    fn example_vmaxvq_u8_zero() {
        #[target_feature(enable = "neon")]
        unsafe fn example() {
            let v = load([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
            assert_eq!(vmaxvq_u8(v), 0);
        }
        unsafe { example() }
    }

    #[test]
    fn example_vpmaxq_u8_non_zero() {
        #[target_feature(enable = "neon")]
        unsafe fn example() {
            let v = load([0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);
            let r = vpmaxq_u8(v, v);
            assert_eq!(
                unload(r),
                [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0]
            );
        }
        unsafe { example() }
    }

    #[test]
    fn example_vpmaxq_u8_self() {
        #[target_feature(enable = "neon")]
        unsafe fn example() {
            let v =
                load([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]);
            let r = vpmaxq_u8(v, v);
            assert_eq!(
                unload(r),
                [2, 4, 6, 8, 10, 12, 14, 16, 2, 4, 6, 8, 10, 12, 14, 16]
            );
        }
        unsafe { example() }
    }

    #[test]
    fn example_vpmaxq_u8_other() {
        #[target_feature(enable = "neon")]
        unsafe fn example() {
            let v1 =
                load([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]);
            let v2 = load([
                17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
            ]);
            let r = vpmaxq_u8(v1, v2);
            assert_eq!(
                unload(r),
                [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32]
            );
        }
        unsafe { example() }
    }
}
