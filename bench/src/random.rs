use criterion::Criterion;

use input::*;
use {define_aho_corasick, define_aho_corasick_dfa};

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
}

/// Various benchmarks using a large pattern set.
fn many_patterns(c: &mut Criterion) {
    use aho_corasick::MatchKind::*;

    let name = "5000words";

    let group = "random10x/standard";
    define_aho_corasick_dfa(
        c,
        group,
        name,
        RANDOM10X,
        Standard,
        0,
        words_5000(),
        |ac, haystack| ac.find_iter(haystack).count(),
    );
    let group = "random10x/leftmost-first";
    define_aho_corasick_dfa(
        c,
        group,
        name,
        RANDOM10X,
        LeftmostFirst,
        0,
        words_5000(),
        |ac, haystack| ac.find_iter(haystack).count(),
    );
    let group = "random10x/leftmost-longest";
    define_aho_corasick_dfa(
        c,
        group,
        name,
        RANDOM10X,
        LeftmostLongest,
        0,
        words_5000(),
        |ac, haystack| ac.find_iter(haystack).count(),
    );

    let group = "random10x/overlapping";
    define_aho_corasick_dfa(
        c,
        group,
        name,
        RANDOM10X,
        Standard,
        0,
        words_5000(),
        |ac, haystack| ac.find_overlapping_iter(haystack).count(),
    );

    let name = "100words";

    let group = "random10x/standard";
    define_aho_corasick_dfa(
        c,
        group,
        name,
        RANDOM10X,
        Standard,
        0,
        words_100(),
        |ac, haystack| ac.find_iter(haystack).count(),
    );
    let group = "random10x/leftmost-first";
    define_aho_corasick_dfa(
        c,
        group,
        name,
        RANDOM10X,
        LeftmostFirst,
        0,
        words_100(),
        |ac, haystack| ac.find_iter(haystack).count(),
    );
    let group = "random10x/leftmost-longest";
    define_aho_corasick_dfa(
        c,
        group,
        name,
        RANDOM10X,
        LeftmostLongest,
        0,
        words_100(),
        |ac, haystack| ac.find_iter(haystack).count(),
    );

    let group = "random10x/overlapping";
    define_aho_corasick_dfa(
        c,
        group,
        name,
        RANDOM10X,
        Standard,
        0,
        words_100(),
        |ac, haystack| ac.find_overlapping_iter(haystack).count(),
    );
}

fn define_random<B: AsRef<[u8]>>(
    c: &mut Criterion,
    bench_name: &str,
    count: usize,
    patterns: Vec<B>,
) {
    define_aho_corasick(c, "random", bench_name, RANDOM, count, patterns);
}
