#![feature(test)]

extern crate aho_corasick;
extern crate test;

use std::iter;

use aho_corasick::{Builder};
use test::Bencher;

#[bench]
fn one_byte(b: &mut Bencher) {
    let haystack: String = iter::repeat("z").take(10000).collect();
    let aut = Builder::new().add("a").build();
    b.iter(|| {
        assert!(aut.find(&haystack).is_empty());
    });
}

/*
#[bench]
fn one(b: &mut Bencher) {
    let haystack: String = iter::repeat("z").take(10000).collect();
    let haystack = haystack.as_bytes();
    let needle = b"abc";
    b.iter(|| {
        assert!(prefix::find_one(needle, haystack).is_none());
    });
}

#[bench]
fn one_tricky(b: &mut Bencher) {
    // We lose the benefit of `memchr` because the first byte matches
    // in every position in the haystack.
    let haystack: String = iter::repeat("z").take(10000).collect();
    let haystack = haystack.as_bytes();
    let needle = b"zbc";
    b.iter(|| {
        assert!(prefix::find_one(needle, haystack).is_none());
    });
}

#[bench]
fn two_bytes(b: &mut Bencher) {
    let haystack: String = iter::repeat("z").take(10000).collect();
    let haystack = haystack.as_bytes();
    let needles = &["a", "b"];
    let needles: Vec<String> = needles.iter().map(|&s| s.to_owned()).collect();
    b.iter(|| {
        assert!(prefix::find_any(&needles, haystack).is_none());
    });
}

#[bench]
fn two(b: &mut Bencher) {
    let haystack: String = iter::repeat("z").take(10000).collect();
    let haystack = haystack.as_bytes();
    let needles = &["abcdef", "bmnopq"];
    let needles: Vec<String> = needles.iter().map(|&s| s.to_owned()).collect();
    b.iter(|| {
        assert!(prefix::find_any(&needles, haystack).is_none());
    });
}

#[bench]
fn two_tricky(b: &mut Bencher) {
    let haystack: String = iter::repeat("z").take(10000).collect();
    let haystack = haystack.as_bytes();
    let needles = &["zbcdef", "zmnopq"];
    let needles: Vec<String> = needles.iter().map(|&s| s.to_owned()).collect();
    b.iter(|| {
        assert!(prefix::find_any(&needles, haystack).is_none());
    });
}

#[bench]
fn ten_bytes(b: &mut Bencher) {
    let haystack: String = iter::repeat("z").take(10000).collect();
    let haystack = haystack.as_bytes();
    let needles = &["a", "b", "c", "d", "e",
                    "f", "g", "h", "i", "j"];
    let needles: Vec<String> = needles.iter().map(|&s| s.to_owned()).collect();
    b.iter(|| {
        assert!(prefix::find_any(&needles, haystack).is_none());
    });
}

#[bench]
fn ten(b: &mut Bencher) {
    let haystack: String = iter::repeat("z").take(10000).collect();
    let haystack = haystack.as_bytes();
    let needles = &["abcdef", "bbcdef", "cbcdef", "dbcdef",
                    "ebcdef", "fbcdef", "gbcdef", "hbcdef",
                    "ibcdef", "jbcdef"];
    let needles: Vec<String> = needles.iter().map(|&s| s.to_owned()).collect();
    b.iter(|| {
        assert!(prefix::find_any(&needles, haystack).is_none());
    });
}
*/

#[bench]
fn ten_tricky(b: &mut Bencher) {
    let haystack: String = iter::repeat("z").take(10000).collect();
    let needles = &["zacdef", "zbcdef", "zccdef", "zdcdef",
                    "zecdef", "zfcdef", "zgcdef", "zhcdef",
                    "zicdef", "zjcdef"];
    let mut baut = Builder::new();
    for &needle in needles { baut = baut.add(needle); }
    let aut = baut.build();
    b.iter(|| {
        assert!(aut.find(&haystack).is_empty());
    });
}
