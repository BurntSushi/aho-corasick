use std::time::Duration;

use aho_corasick::{packed, AhoCorasick, AhoCorasickKind, MatchKind};
use criterion::{
    criterion_group, criterion_main, Bencher, Criterion, Throughput,
};

mod build;
mod input;
mod random;
mod same;
mod sherlock;

fn all(c: &mut Criterion) {
    build::all(c);
    sherlock::all(c);
    random::all(c);
    same::all(c);
}

/// Define a benchmark that tests the standard non-overlapping Aho-Corasick
/// algorithm, using both its NFA and DFA variants.
fn define_aho_corasick<B: AsRef<[u8]>>(
    c: &mut Criterion,
    group_name: &str,
    bench_name: &str,
    corpus: &[u8],
    count: usize,
    patterns: Vec<B>,
) {
    let patterns: Vec<Vec<u8>> =
        patterns.into_iter().map(|b| b.as_ref().to_vec()).collect();

    let haystack = corpus.to_vec();
    let name = format!("nfa/noncontiguous/{}", bench_name);
    let aut = AhoCorasick::builder()
        .match_kind(MatchKind::LeftmostFirst)
        .kind(AhoCorasickKind::NoncontiguousNFA)
        .build(patterns.clone())
        .unwrap();
    define(c, group_name, &name, corpus, move |b| {
        b.iter(|| assert_eq!(count, aut.find_iter(&haystack).count()));
    });

    let haystack = corpus.to_vec();
    let name = format!("nfa/contiguous/{}", bench_name);
    let aut = AhoCorasick::builder()
        .match_kind(MatchKind::LeftmostFirst)
        .kind(AhoCorasickKind::ContiguousNFA)
        .build(patterns.clone())
        .unwrap();
    define(c, group_name, &name, corpus, move |b| {
        b.iter(|| assert_eq!(count, aut.find_iter(&haystack).count()));
    });

    let haystack = corpus.to_vec();
    let name = format!("dfa/{}", bench_name);
    let aut = AhoCorasick::builder()
        .match_kind(MatchKind::LeftmostFirst)
        .kind(AhoCorasickKind::DFA)
        .build(patterns.clone())
        .unwrap();
    define(c, group_name, &name, corpus, move |b| {
        b.iter(|| assert_eq!(count, aut.find_iter(&haystack).count()));
    });

    let name = format!("packed/teddy/{}", bench_name);
    let haystack = corpus.to_vec();
    let mut builder = packed::Config::new().force_teddy(true).builder();
    builder.extend(patterns.clone());
    if let Some(searcher) = builder.build() {
        define(c, group_name, &name, corpus, move |b| {
            b.iter(|| {
                assert_eq!(count, searcher.find_iter(&haystack).count())
            });
        });
    }

    let name = format!("packed/rabinkarp/{}", bench_name);
    let haystack = corpus.to_vec();
    let mut builder = packed::Config::new().force_rabin_karp(true).builder();
    builder.extend(patterns.clone());
    if let Some(searcher) = builder.build() {
        define(c, group_name, &name, corpus, move |b| {
            b.iter(|| {
                assert_eq!(count, searcher.find_iter(&haystack).count())
            });
        });
    }
}

/// A convenience wrapper for defining a benchmark tied to a particular corpus.
/// The corpus is used to provide throughput statistics. This also tweaks the
/// standard Criterion configuration so that benchmarks run a bit more quickly.
fn define(
    c: &mut Criterion,
    group_name: &str,
    bench_name: &str,
    corpus: &[u8],
    bench: impl FnMut(&mut Bencher<'_>) + 'static,
) {
    c.benchmark_group(group_name)
        .throughput(Throughput::Bytes(corpus.len() as u64))
        .sample_size(10)
        .warm_up_time(Duration::from_millis(500))
        .measurement_time(Duration::from_secs(2))
        .bench_function(bench_name, bench);
}

/// Like define, but specifically useful for defining benchmarks that measure a
/// slower routine (i.e., in the low milliseconds per iteration).
fn define_long(
    c: &mut Criterion,
    group_name: &str,
    bench_name: &str,
    corpus: &[u8],
    bench: impl FnMut(&mut Bencher<'_>) + 'static,
) {
    c.benchmark_group(group_name)
        .throughput(Throughput::Bytes(corpus.len() as u64))
        .sample_size(20)
        .warm_up_time(Duration::from_millis(500))
        .measurement_time(Duration::from_secs(2))
        .bench_function(bench_name, bench);
}

criterion_group!(g1, all);
criterion_main!(g1);
