#![no_main]
use libfuzzer_sys::fuzz_target;
use libfuzzer_sys::arbitrary;

use aho_corasick::{AhoCorasickBuilder, MatchKind};

#[derive(arbitrary::Arbitrary,Debug,Clone)]
enum Operation {
    Find(String),
    ReplaceAll(String,Vec<String>)
}

#[derive(arbitrary::Arbitrary,Debug,Clone)]
struct Inputs {
    patterns: Vec<String>,
    auto_configure: bool,
    match_kind: u8,
    anchored: bool,
    ascii_case_insensitive: bool,
    dfa: bool,
    dense_depth: Option<usize>,
    prefilter: bool,
    operation: Operation,
}

fuzz_target!(|input: Inputs| {
    let mut acb = AhoCorasickBuilder::new();
    let acb = if input.auto_configure {
        acb.auto_configure(&input.patterns)
    } else {
        &mut acb
    };
    let match_kind = input.match_kind % 4;
    let acb = match match_kind {
        0 => acb,
        1 => acb.match_kind(MatchKind::Standard),
        2 => acb.match_kind(MatchKind::LeftmostFirst),
        3 => acb.match_kind(MatchKind::LeftmostLongest),
        _ => unreachable!(),
    };
    let acb = if input.anchored {
        acb.anchored(true)
    } else {
        acb
    };
    let acb = if input.ascii_case_insensitive {
        acb.ascii_case_insensitive(true)
    } else {
        acb
    };
    let acb = if input.dfa {
        acb.dfa(true)
    } else {
        acb
    };
    let acb = if let Some(dense_depth) = input.dense_depth {
        acb.dense_depth(dense_depth)
    } else {
        acb
    };
    let acb = if input.prefilter {
        acb.prefilter(true)
    } else {
        acb
    };
    let num_patterns = input.patterns.len();
    let ac = acb.build(input.patterns);
    match input.operation {
        Operation::Find(haystack) => { ac.find(&haystack); },
        Operation::ReplaceAll(haystack, substitutions) => {
            if substitutions.len() == num_patterns {
                ac.replace_all(&haystack, &substitutions);
            }
        },
    }
});
