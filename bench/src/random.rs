use criterion::Criterion;

use crate::define_aho_corasick;
use crate::input::*;

/// These benchmarks test various words on random text.
pub fn all(c: &mut Criterion) {
    memchr_optimizations(c);
    misc(c);
    many_patterns(c);
}

/// These benchmarks test the prefix byte optimization, and the impact that
/// match-vs-non-match has.
///
/// More specifically, Aho-Corasick will use highly optimized vectorized
/// routines (on x86) if it determines that all matches start with 1, 2 or 3
/// distinct bytes. Beyond that, no prefix optimizations are used.
///
/// For match-vs-non-match, we keep the match counts fixed across the different
/// prefix optimizations as a way to control what we measure.
fn memchr_optimizations(c: &mut Criterion) {
    define_random(c, "onebyte/match", 352, vec!["a"]);
    define_random(c, "onebyte/nomatch", 0, vec!["\x00"]);
    define_random(c, "twobytes/match", 352, vec!["a", "\x00"]);
    define_random(c, "twobytes/nomatch", 0, vec!["\x00", "\x01"]);
    define_random(c, "threebytes/match", 352, vec!["a", "\x00", "\x01"]);
    define_random(c, "threebytes/nomatch", 0, vec!["\x00", "\x01", "\x02"]);
    define_random(
        c,
        "fourbytes/match",
        352,
        vec!["a", "\x00", "\x01", "\x02"],
    );
    define_random(
        c,
        "fourbytes/nomatch",
        0,
        vec!["\x00", "\x01", "\x02", "\x03"],
    );
    define_random(
        c,
        "fivebytes/match",
        352,
        vec!["a", "\x00", "\x01", "\x02", "\x03"],
    );
    define_random(
        c,
        "fivebytes/nomatch",
        0,
        vec!["\x00", "\x01", "\x02", "\x03", "\x04"],
    );
}

/// Some miscellaneous benchmarks on random data.
fn misc(c: &mut Criterion) {
    define_random(
        c,
        "ten-one-prefix",
        0,
        vec![
            "zacdef", "zbcdef", "zccdef", "zdcdef", "zecdef", "zfcdef",
            "zgcdef", "zhcdef", "zicdef", "zjcdef",
        ],
    );
    define_random(
        c,
        "ten-diff-prefix",
        0,
        vec![
            "abcdef", "bcdefg", "cdefgh", "defghi", "efghij", "fghijk",
            "ghijkl", "hijklm", "ijklmn", "jklmno",
        ],
    );

    // Test on patterns that fills up the alphabet.
    // - The root state is dense
    // - The children of the root states are dense
    // - The grandchildren all have only one valid transition.

    // Build [
    //      "\x00\x00\x00\x00", "\x00\x01\x01\x01", ..., "\x00\xFF\xFF\xFF",
    //      "\x01\x00\x00\x00", "\x01\x01\x01\x01", ..., "\x01\xFF\xFF\xFF",
    //      ..,
    //      "\xFF\x00\x00\x00", "\xFF\x01\x01\x01", ..., "\xFF\xFF\xFF\xFF",
    // ]
    let pats: Vec<Vec<u8>> = (0_u8..255)
        .flat_map(|b| {
            (0_u8..255).map(move |c| {
                std::iter::once(c)
                    .chain(std::iter::repeat(b).take(3))
                    .collect::<Vec<_>>()
            })
        })
        .collect();
    define_random(c, "dense-root", 16, pats);
}

/// Various benchmarks using a large pattern set.
fn many_patterns(c: &mut Criterion) {
    let name = "5000words";

    let group = "random10x/leftmost-first";
    define_aho_corasick(c, group, name, RANDOM10X, 0, words_5000());

    let group = "random10x/leftmost-first";
    define_aho_corasick(c, group, name, RANDOM10X, 0, words_100());
}

fn define_random<B: AsRef<[u8]>>(
    c: &mut Criterion,
    bench_name: &str,
    count: usize,
    patterns: Vec<B>,
) {
    define_aho_corasick(c, "random", bench_name, RANDOM, count, patterns);
}
