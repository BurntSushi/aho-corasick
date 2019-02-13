pub const SHERLOCK: &'static [u8] = include_bytes!("../data/sherlock.txt");

pub const RANDOM: &'static [u8] = include_bytes!("../data/random.txt");

pub const WORDS_5000_RAW: &'static str = include_str!("../data/words-5000");

pub fn words_5000() -> Vec<&'static str> {
    WORDS_5000_RAW.lines().collect()
}
