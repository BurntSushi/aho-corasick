use std::{fs, path::PathBuf, time::Instant};

use aho_corasick::{AhoCorasick, AhoCorasickKind, MatchKind, StartKind};
use memmap2::Mmap;

fn main() -> anyhow::Result<()> {
    env_logger::try_init()?;

    let args = Args::parse()?;
    let ac = args.aho_corasick()?;
    let haystack = args.haystack()?;

    eprintln!("automaton heap usage: {} bytes", ac.memory_usage());
    if args.no_search || args.debug {
        if args.debug {
            eprintln!("{:?}", ac);
        }
        return Ok(());
    }

    let start = Instant::now();
    let count = ac.find_iter(&haystack).count();
    println!("match count: {}", count);

    let count_time = Instant::now().duration_since(start);
    eprintln!("count time: {:?}", count_time);
    Ok(())
}

#[derive(Debug)]
struct Args {
    dictionary: PathBuf,
    haystack: PathBuf,
    match_kind: MatchKind,
    start_kind: StartKind,
    kind: AhoCorasickKind,
    ascii_casei: bool,
    dense_depth: usize,
    no_prefilter: bool,
    no_classes: bool,
    no_search: bool,
    debug: bool,
}

impl Args {
    fn parse() -> anyhow::Result<Args> {
        use clap::{crate_authors, crate_version, App, Arg};

        let parsed = App::new("Search using aho-corasick")
            .author(crate_authors!())
            .version(crate_version!())
            .max_term_width(100)
            .arg(Arg::with_name("dictionary").required(true))
            .arg(Arg::with_name("haystack").required(true))
            .arg(
                Arg::with_name("kind")
                    .long("kind")
                    .possible_values(&[
                        "auto",
                        "noncontiguous",
                        "contiguous",
                        "dfa",
                    ])
                    .default_value("auto"),
            )
            .arg(
                Arg::with_name("match-kind")
                    .long("match-kind")
                    .possible_values(&[
                        "standard",
                        "leftmost-first",
                        "leftmost-longest",
                    ])
                    .default_value("standard"),
            )
            .arg(
                Arg::with_name("start-kind")
                    .long("start-kind")
                    .possible_values(&["both", "unanchored", "anchored"])
                    .default_value("unanchored"),
            )
            .arg(
                Arg::with_name("ascii-case-insensitive")
                    .long("ascii-case-insensitive")
                    .short("i"),
            )
            .arg(
                Arg::with_name("dense-depth")
                    .long("dense-depth")
                    .default_value("2"),
            )
            .arg(
                Arg::with_name("no-prefilter").long("no-prefilter").short("f"),
            )
            .arg(Arg::with_name("no-classes").long("no-classes").short("C"))
            .arg(Arg::with_name("no-search").long("no-search"))
            .arg(Arg::with_name("debug").long("debug"))
            .get_matches();

        let dictionary =
            PathBuf::from(parsed.value_of_os("dictionary").unwrap());
        let haystack = PathBuf::from(parsed.value_of_os("haystack").unwrap());
        let match_kind = match parsed.value_of("match-kind").unwrap() {
            "standard" => MatchKind::Standard,
            "leftmost-first" => MatchKind::LeftmostFirst,
            "leftmost-longest" => MatchKind::LeftmostLongest,
            _ => unreachable!(),
        };
        let start_kind = match parsed.value_of("start-kind").unwrap() {
            "both" => StartKind::Both,
            "unanchored" => StartKind::Unanchored,
            "anchored" => StartKind::Anchored,
            _ => unreachable!(),
        };
        let kind = match parsed.value_of("kind").unwrap() {
            "auto" => AhoCorasickKind::Auto,
            "noncontiguous" => AhoCorasickKind::NoncontiguousNFA,
            "contiguous" => AhoCorasickKind::ContiguousNFA,
            "dfa" => AhoCorasickKind::DFA,
            _ => unreachable!(),
        };
        let dense_depth = parsed.value_of("dense-depth").unwrap().parse()?;

        Ok(Args {
            dictionary,
            haystack,
            match_kind,
            start_kind,
            kind,
            dense_depth,
            ascii_casei: parsed.is_present("ascii-case-insensitive"),
            no_prefilter: parsed.is_present("no-prefilter"),
            no_classes: parsed.is_present("no-classes"),
            no_search: parsed.is_present("no-search"),
            debug: parsed.is_present("debug"),
        })
    }

    fn aho_corasick(&self) -> anyhow::Result<AhoCorasick> {
        let start = Instant::now();
        let patterns = fs::read_to_string(&self.dictionary)?;
        let read_time = Instant::now().duration_since(start);
        eprintln!("pattern read time: {:?}", read_time);

        let start = Instant::now();
        let ac = AhoCorasick::builder()
            .match_kind(self.match_kind)
            .start_kind(self.start_kind)
            .kind(self.kind)
            .ascii_case_insensitive(self.ascii_casei)
            .dense_depth(self.dense_depth)
            .prefilter(!self.no_prefilter)
            .byte_classes(!self.no_classes)
            .build(patterns.lines())?;
        let build_time = Instant::now().duration_since(start);
        eprintln!("automaton build time: {:?}", build_time);
        Ok(ac)
    }

    fn haystack(&self) -> anyhow::Result<Mmap> {
        // SAFETY: We only read from this content and generally assume the file
        // is not mutated while it is searched.
        Ok(unsafe { Mmap::map(&fs::File::open(&self.haystack)?)? })
    }
}
