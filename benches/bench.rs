#![feature(test)]

extern crate aho_corasick;
extern crate test;

use std::iter;

use aho_corasick::AcAutomaton;
use test::Bencher;

const HAYSTACK_RANDOM: &'static str = include_str!("random.txt");

fn bench_aut_no_match(b: &mut Bencher, aut: AcAutomaton, haystack: &str) {
    b.bytes = haystack.len() as u64;
    b.iter(|| assert!(aut.find(haystack).next().is_none()));
}

fn bench_naive_no_match<S>(b: &mut Bencher, needles: Vec<S>, haystack: &str)
        where S: Into<String> {
    b.bytes = haystack.len() as u64;
    let needles: Vec<String> = needles.into_iter().map(Into::into).collect();
    b.iter(|| assert!(!naive_find(&needles, haystack)));
}

fn haystack_same(letter: char) -> String {
    iter::repeat(letter).take(10000).collect()
}

#[bench]
fn ac_one_byte(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["a"]);
    bench_aut_no_match(b, aut, &haystack_same('z'));
}

#[bench]
fn ac_one_prefix_byte_no_match(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["zbc"]);
    bench_aut_no_match(b, aut, &haystack_same('y'));
}

#[bench]
fn ac_one_prefix_byte_every_match(b: &mut Bencher) {
    // We lose the benefit of `memchr` because the first byte matches
    // in every position in the haystack.
    let aut = AcAutomaton::new(vec!["zbc"]);
    bench_aut_no_match(b, aut, &haystack_same('z'));
}

#[bench]
fn ac_one_prefix_byte_random(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["zbc\x00"]);
    bench_aut_no_match(b, aut, HAYSTACK_RANDOM);
}

#[bench]
fn ac_two_bytes(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["a", "b"]);
    bench_aut_no_match(b, aut, &haystack_same('z'));
}

#[bench]
fn ac_two_diff_prefix(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["abcdef", "bmnopq"]);
    bench_aut_no_match(b, aut, &haystack_same('z'));
}

#[bench]
fn ac_two_one_prefix_byte_every_match(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["zbcdef", "zmnopq"]);
    bench_aut_no_match(b, aut, &haystack_same('z'));
}

#[bench]
fn ac_two_one_prefix_byte_no_match(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["zbcdef", "zmnopq"]);
    bench_aut_no_match(b, aut, &haystack_same('y'));
}

#[bench]
fn ac_two_one_prefix_byte_random(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["zbcdef\x00", "zmnopq\x00"]);
    bench_aut_no_match(b, aut, HAYSTACK_RANDOM);
}

#[bench]
fn ac_ten_bytes(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["a", "b", "c", "d", "e",
                                    "f", "g", "h", "i", "j"]);
    bench_aut_no_match(b, aut, &haystack_same('z'));
}

#[bench]
fn ac_ten_diff_prefix(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["abcdef", "bbcdef", "cbcdef", "dbcdef",
                                    "ebcdef", "fbcdef", "gbcdef", "hbcdef",
                                    "ibcdef", "jbcdef"]);
    bench_aut_no_match(b, aut, &haystack_same('z'));
}

#[bench]
fn ac_ten_one_prefix_byte_every_match(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["zacdef", "zbcdef", "zccdef", "zdcdef",
                                    "zecdef", "zfcdef", "zgcdef", "zhcdef",
                                    "zicdef", "zjcdef"]);
    bench_aut_no_match(b, aut, &haystack_same('z'));
}

#[bench]
fn ac_ten_one_prefix_byte_no_match(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["zacdef", "zbcdef", "zccdef", "zdcdef",
                                    "zecdef", "zfcdef", "zgcdef", "zhcdef",
                                    "zicdef", "zjcdef"]);
    bench_aut_no_match(b, aut, &haystack_same('y'));
}

#[bench]
fn ac_ten_one_prefix_byte_random(b: &mut Bencher) {
    let aut = AcAutomaton::new(vec!["zacdef\x00", "zbcdef\x00", "zccdef\x00",
                                    "zdcdef\x00", "zecdef\x00", "zfcdef\x00",
                                    "zgcdef\x00", "zhcdef\x00", "zicdef\x00",
                                    "zjcdef\x00"]);
    bench_aut_no_match(b, aut, HAYSTACK_RANDOM);
}

// A naive multi-pattern search.
// We use this to benchmark *throughput*, so it should never match anything.
fn naive_find(needles: &[String], haystack: &str) -> bool {
    for hi in 0..haystack.len() {
        let rest = &haystack.as_bytes()[hi..];
        for needle in needles {
            let needle = needle.as_bytes();
            if needle.len() > rest.len() {
                continue;
            }
            if needle == &rest[..needle.len()] {
                // should never happen in throughput benchmarks.
                return true;
            }
        }
    }
    false
}

#[bench]
fn naive_one_byte(b: &mut Bencher) {
    bench_naive_no_match(b, vec!["a"], &haystack_same('z'));
}

#[bench]
fn naive_one_prefix_byte_no_match(b: &mut Bencher) {
    bench_naive_no_match(b, vec!["zbc"], &haystack_same('y'));
}

#[bench]
fn naive_one_prefix_byte_every_match(b: &mut Bencher) {
    bench_naive_no_match(b, vec!["zbc"], &haystack_same('z'));
}

#[bench]
fn naive_one_prefix_byte_random(b: &mut Bencher) {
    bench_naive_no_match(b, vec!["zbc\x00"], HAYSTACK_RANDOM);
}

#[bench]
fn naive_two_bytes(b: &mut Bencher) {
    bench_naive_no_match(b, vec!["a", "b"], &haystack_same('z'));
}

#[bench]
fn naive_two_diff_prefix(b: &mut Bencher) {
    bench_naive_no_match(b, vec!["abcdef", "bmnopq"], &haystack_same('z'));
}

#[bench]
fn naive_two_one_prefix_byte_every_match(b: &mut Bencher) {
    bench_naive_no_match(b, vec!["zbcdef", "zmnopq"], &haystack_same('z'));
}

#[bench]
fn naive_two_one_prefix_byte_no_match(b: &mut Bencher) {
    bench_naive_no_match(b, vec!["zbcdef", "zmnopq"], &haystack_same('y'));
}

#[bench]
fn naive_two_one_prefix_byte_random(b: &mut Bencher) {
    bench_naive_no_match(b, vec!["zbcdef\x00", "zmnopq\x00"], HAYSTACK_RANDOM);
}

#[bench]
fn naive_ten_bytes(b: &mut Bencher) {
    let needles = vec!["a", "b", "c", "d", "e",
                       "f", "g", "h", "i", "j"];
    bench_naive_no_match(b, needles, &haystack_same('z'));
}

#[bench]
fn naive_ten_diff_prefix(b: &mut Bencher) {
    let needles = vec!["abcdef", "bbcdef", "cbcdef", "dbcdef",
                       "ebcdef", "fbcdef", "gbcdef", "hbcdef",
                       "ibcdef", "jbcdef"];
    bench_naive_no_match(b, needles, &haystack_same('z'));
}

#[bench]
fn naive_ten_one_prefix_byte_every_match(b: &mut Bencher) {
    let needles = vec!["zacdef", "zbcdef", "zccdef", "zdcdef",
                       "zecdef", "zfcdef", "zgcdef", "zhcdef",
                       "zicdef", "zjcdef"];
    bench_naive_no_match(b, needles, &haystack_same('z'));
}

#[bench]
fn naive_ten_one_prefix_byte_no_match(b: &mut Bencher) {
    let needles = vec!["zacdef", "zbcdef", "zccdef", "zdcdef",
                       "zecdef", "zfcdef", "zgcdef", "zhcdef",
                       "zicdef", "zjcdef"];
    bench_naive_no_match(b, needles, &haystack_same('y'));
}

#[bench]
fn naive_ten_one_prefix_byte_random(b: &mut Bencher) {
    let needles = vec!["zacdef\x00", "zbcdef\x00", "zccdef\x00",
                       "zdcdef\x00", "zecdef\x00", "zfcdef\x00",
                       "zgcdef\x00", "zhcdef\x00", "zicdef\x00",
                       "zjcdef\x00"];
    bench_naive_no_match(b, needles, HAYSTACK_RANDOM);
}
