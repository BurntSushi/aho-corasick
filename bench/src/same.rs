use criterion::Criterion;

use define_aho_corasick;

/// These benchmarks test various workloads on a corpus that corresponds to the
/// repetition of a single byte. For the most part, we compare the impact of
/// prefix optimizations on this case. Namely, if the repeated byte is `z` and
/// a prefix byte is `z`, then this represents the worst case for the prefix
/// optimizations.
pub fn all(c: &mut Criterion) {
    define_same(c, "onebyte/match", 10_000, vec!["z"]);
    define_same(c, "onebyte/nomatch", 0, vec!["a"]);

    define_same(c, "twobytes/match", 10_000, vec!["z", "a"]);
    define_same(c, "twobytes/nomatch", 0, vec!["a", "b"]);

    define_same(c, "threebytes/match", 10_000, vec!["z", "a", "b"]);
    define_same(c, "threebytes/nomatch", 0, vec!["a", "b", "c"]);

    define_same(c, "fourbytes/match", 10_000, vec!["z", "a", "b", "c"]);
    define_same(c, "fourbytes/nomatch", 0, vec!["a", "b", "c", "d"]);

    define_same(c, "fivebytes/match", 10_000, vec!["z", "a", "b", "c", "d"]);
    define_same(c, "fivebytes/nomatch", 0, vec!["a", "b", "c", "d", "e"]);

    define_same(c, "samebyte/match", 1_000, vec!["zzzzzzzzzz"]);
}

fn define_same<B: AsRef<[u8]>>(
    c: &mut Criterion,
    bench_name: &str,
    count: usize,
    patterns: Vec<B>,
) {
    let corpus = "z".repeat(10_000);
    define_aho_corasick(
        c,
        "same",
        bench_name,
        corpus.as_bytes(),
        count,
        patterns,
    );
}
