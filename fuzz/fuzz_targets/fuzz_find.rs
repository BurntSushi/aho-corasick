#![no_main]
use libfuzzer_sys::fuzz_target;
use libfuzzer_sys::arbitrary;

use aho_corasick::AhoCorasick;

#[derive(arbitrary::Arbitrary,Debug,Clone)]
struct Inputs {
    patterns: Vec<String>,
    haystack: String,
}

fuzz_target!(|input: Inputs| {
    let ac = AhoCorasick::new(input.patterns);
    let _ = ac.find(&input.haystack);
});
