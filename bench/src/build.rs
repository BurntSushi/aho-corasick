use aho_corasick::{AhoCorasick, AhoCorasickBuilder};
use criterion::{black_box, Criterion};

use input::{words_15000, words_5000};
use {define, define_long};

/// Benchmarks that measure the performance of constructing an Aho-Corasick
/// automaton.
pub fn all(c: &mut Criterion) {
    define_build::<String>(c, false, "empty", vec![]);
    define_build(c, false, "onebyte", vec!["a"]);
    define_build(c, false, "twobytes", vec!["a", "b"]);
    define_build(
        c,
        false,
        "many-short",
        vec![
            "ADL", "ADl", "AdL", "Adl", "BAK", "BAk", "BAK", "BaK", "Bak",
            "BaK", "HOL", "HOl", "HoL", "Hol", "IRE", "IRe", "IrE", "Ire",
            "JOH", "JOh", "JoH", "Joh", "SHE", "SHe", "ShE", "She", "WAT",
            "WAt", "WaT", "Wat", "aDL", "aDl", "adL", "adl", "bAK", "bAk",
            "bAK", "baK", "bak", "baK", "hOL", "hOl", "hoL", "hol", "iRE",
            "iRe", "irE", "ire", "jOH", "jOh", "joH", "joh", "sHE", "sHe",
            "shE", "she", "wAT", "wAt", "waT", "wat", "ſHE", "ſHe", "ſhE",
            "ſhe",
        ],
    );
    define_build(c, true, "5000words", words_5000());
    define_build(c, true, "15000words", words_15000());
}

fn define_build<B: AsRef<[u8]>>(
    c: &mut Criterion,
    long: bool,
    bench_name: &str,
    patterns: Vec<B>,
) {
    let patterns: Vec<Vec<u8>> =
        patterns.into_iter().map(|b| b.as_ref().to_vec()).collect();

    let pats = patterns.clone();
    let name = format!("nfa/{}", bench_name);
    if long {
        define_long(c, "build", &name, &[], move |b| {
            b.iter(|| black_box(AhoCorasick::new(&pats)))
        });
    } else {
        define(c, "build", &name, &[], move |b| {
            b.iter(|| black_box(AhoCorasick::new(&pats)))
        });
    }

    let pats = patterns.clone();
    let name = format!("dfa/{}", bench_name);
    if long {
        define_long(c, "build", &name, &[], move |b| {
            b.iter(|| {
                black_box(AhoCorasickBuilder::new().dfa(true).build(&pats))
            })
        });
    } else {
        define(c, "build", &name, &[], move |b| {
            b.iter(|| {
                black_box(AhoCorasickBuilder::new().dfa(true).build(&pats))
            })
        });
    }
}
