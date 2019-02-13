extern crate aho_corasick;
#[macro_use]
extern crate criterion;

use std::time::Duration;

use aho_corasick::{Automaton, AcAutomaton};
use criterion::{Bencher, Benchmark, Criterion, Throughput};

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
    let patterns: Vec<Vec<u8>> = patterns
        .into_iter()
        .map(|b| b.as_ref().to_vec())
        .collect();

    let haystack = corpus.to_vec();
    let name = format!("nfa/{}", bench_name);
    let aut = AcAutomaton::new(patterns.clone());
    define(c, group_name, &name, corpus, move |b| {
        b.iter(|| assert_eq!(count, aut.find(&haystack).count()));
    });

    let haystack = corpus.to_vec();
    let name = format!("dfa/{}", bench_name);
    let aut = AcAutomaton::new(patterns).into_full();
    define(c, group_name, &name, corpus, move |b| {
        b.iter(|| assert_eq!(count, aut.find(&haystack).count()));
    });
}

/// A convenience wrapper for defining a benchmark tied to a particular corpus.
/// The corpus is used to provide throughput statistics. This also tweaks the
/// standard Criterion configuration so that benchmarks run a bit more quickly.
fn define(
    c: &mut Criterion,
    group_name: &str,
    bench_name: &str,
    corpus: &[u8],
    bench: impl FnMut(&mut Bencher) + 'static,
) {

    let tput = Throughput::Bytes(corpus.len() as u32);
    let benchmark = Benchmark::new(bench_name, bench)
        .throughput(tput)
        .warm_up_time(Duration::from_millis(500))
        .measurement_time(Duration::from_secs(2));
    c.bench(group_name, benchmark);
}

/// Like define, but specifically useful for defining benchmarks that measure a
/// slower routine (i.e., in the low milliseconds per iteration).
fn define_long(
    c: &mut Criterion,
    group_name: &str,
    bench_name: &str,
    corpus: &[u8],
    bench: impl FnMut(&mut Bencher) + 'static,
) {

    let tput = Throughput::Bytes(corpus.len() as u32);
    let benchmark = Benchmark::new(bench_name, bench)
        .throughput(tput)
        .sample_size(20)
        .warm_up_time(Duration::from_millis(500))
        .measurement_time(Duration::from_secs(2));
    c.bench(group_name, benchmark);
}

criterion_group!(g1, all);
criterion_main!(g1);
