pub const SHERLOCK: &'static [u8] = include_bytes!("../data/sherlock.txt");

pub const RANDOM: &'static [u8] = include_bytes!("../data/random.txt");
pub const RANDOM10X: &'static [u8] = include_bytes!("../data/random10x.txt");

pub const WORDS_100_RAW: &'static str = include_str!("../data/words-100");
pub const WORDS_5000_RAW: &'static str = include_str!("../data/words-5000");
pub const WORDS_15000_RAW: &'static str = include_str!("../data/words-15000");

pub fn words_100() -> Vec<&'static str> {
    WORDS_100_RAW.lines().collect()
}

pub fn words_5000() -> Vec<&'static str> {
    WORDS_5000_RAW.lines().collect()
}

pub fn words_15000() -> Vec<&'static str> {
    WORDS_15000_RAW.lines().collect()
}
