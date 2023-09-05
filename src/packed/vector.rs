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

    /// Returns a scalar such that bit `i` is set if and only if the most
    /// significant bit of the corresponding 8-bit lane `i` is set.
    ///
    /// # Safety
    ///
    /// Callers must ensure that this is okay to call in the current target for
    /// the current CPU.
    unsafe fn movemask(self) -> u32;

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
        unsafe fn movemask(self) -> u32 {
            _mm_movemask_epi8(self) as u32
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
        unsafe fn movemask(self) -> u32 {
            _mm256_movemask_epi8(self) as u32
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
        unsafe fn movemask(self) -> u32 {
            todo!()
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
    }
}

// BREADCRUMBS: Write example tests using intrinsics directly. Do it by
// writing a convenience function for loading a vector from an array using
// load_unaligned. This should make documenting my understanding easier since
// Teddy requires a much bigger vocabulary of vector functions than the memchr
// crate does.
