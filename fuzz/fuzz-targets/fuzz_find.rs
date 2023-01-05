#![no_main]

use libfuzzer_sys::{arbitrary, fuzz_target};

use aho_corasick::{AhoCorasick, AhoCorasickKind, MatchKind};

#[derive(arbitrary::Arbitrary, Debug, Clone)]
enum Operation {
    Find(String),
    ReplaceAll(String, Vec<String>),
}

#[derive(arbitrary::Arbitrary, Debug, Clone)]
struct Inputs {
    patterns: Vec<String>,
    kind: u8,
    match_kind: u8,
    ascii_case_insensitive: bool,
    dense_depth: Option<usize>,
    prefilter: bool,
    operation: Operation,
    byte_classes: bool,
}

fuzz_target!(|input: Inputs| {
    let mut acb = AhoCorasick::builder();
    acb.ascii_case_insensitive(input.ascii_case_insensitive)
        .prefilter(input.prefilter)
        .byte_classes(input.byte_classes);
    match input.kind % 5 {
        0 => &mut acb,
        1 => acb.kind(AhoCorasickKind::Auto),
        2 => acb.kind(AhoCorasickKind::NoncontiguousNFA),
        3 => acb.kind(AhoCorasickKind::ContiguousNFA),
        4 => acb.kind(AhoCorasickKind::DFA),
        _ => unreachable!(),
    };
    match input.match_kind % 4 {
        0 => &mut acb,
        1 => acb.match_kind(MatchKind::Standard),
        2 => acb.match_kind(MatchKind::LeftmostFirst),
        3 => acb.match_kind(MatchKind::LeftmostLongest),
        _ => unreachable!(),
    };
    if let Some(dense_depth) = input.dense_depth {
        acb.dense_depth(dense_depth);
    }

    let num_patterns = input.patterns.len();
    let ac = acb.build(input.patterns).unwrap();
    match input.operation {
        Operation::Find(haystack) => {
            ac.find(&haystack);
        }
        Operation::ReplaceAll(haystack, substitutions) => {
            if substitutions.len() == num_patterns {
                ac.replace_all(&haystack, &substitutions);
            }
        }
    }
});
