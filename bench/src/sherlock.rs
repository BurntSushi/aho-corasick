use criterion::Criterion;

use define_aho_corasick;
use input::*;

/// These benchmarks test various words on natural language text.
///
/// These benchmarks were mostly taken from the regex crate's benchmarks. For
/// example, the case insensitive benchmarks below are the result of generating
/// all possible case variants for a fixed prefix of each string.
pub fn all(c: &mut Criterion) {
    define_sherlock(c, "name/alt1", 158, vec!["Sherlock", "Street"]);
    define_sherlock(c, "name/alt2", 558, vec!["Sherlock", "Holmes"]);
    define_sherlock(
        c,
        "name/alt3",
        740,
        vec![
            "Sherlock", "Holmes", "Watson", "Irene", "Adler", "John", "Baker",
        ],
    );
    define_sherlock(c, "name/alt4", 582, vec!["Sher", "Hol"]);
    define_sherlock(c, "name/alt5", 639, vec!["Sherlock", "Holmes", "Watson"]);
    define_sherlock(
        c,
        "name/alt6",
        0,
        vec!["SherlockZ", "HolmesZ", "WatsonZ", "IreneZ", "MoriartyZ"],
    );
    define_sherlock(c, "name/alt7", 0, vec!["Шерлок Холмс", "Джон Уотсон"]);

    define_sherlock(
        c,
        "name/nocase1",
        1764,
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
    define_sherlock(
        c,
        "name/nocase2",
        1307,
        vec![
            "HOL", "HOl", "HoL", "Hol", "SHE", "SHe", "ShE", "She", "hOL",
            "hOl", "hoL", "hol", "sHE", "sHe", "shE", "she", "ſHE", "ſHe",
            "ſhE", "ſhe",
        ],
    );
    define_sherlock(
        c,
        "name/nocase3",
        1442,
        vec![
            "HOL", "HOl", "HoL", "Hol", "SHE", "SHe", "ShE", "She", "WAT",
            "WAt", "WaT", "Wat", "hOL", "hOl", "hoL", "hol", "sHE", "sHe",
            "shE", "she", "wAT", "wAt", "waT", "wat", "ſHE", "ſHe", "ſhE",
            "ſhe",
        ],
    );

    define_sherlock(c, "5000words", 567, words_5000());
}

fn define_sherlock<B: AsRef<[u8]>>(
    c: &mut Criterion,
    bench_name: &str,
    count: usize,
    patterns: Vec<B>,
) {
    define_aho_corasick(c, "sherlock", bench_name, SHERLOCK, count, patterns);
}
