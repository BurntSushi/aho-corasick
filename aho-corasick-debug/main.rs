use std::error::Error;
use std::fs;
use std::path::PathBuf;
use std::process;
use std::result;
use std::time::Instant;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, MatchKind};
use memmap::Mmap;

type Result<T> = result::Result<T, Box<dyn Error>>;

// Change this to tweak the size of state IDs used in the automaton.
type Size = u32;

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        process::exit(1);
    }
}

fn try_main() -> Result<()> {
    let args = Args::parse()?;
    let ac = args.automaton()?;
    let haystack = args.haystack()?;

    eprintln!("automaton heap usage: {} bytes", ac.heap_bytes());
    if args.no_search {
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
    ascii_casei: bool,
    dense_depth: usize,
    dfa: bool,
    prefilter: bool,
    classes: bool,
    premultiply: bool,
    no_search: bool,
}

impl Args {
    fn parse() -> Result<Args> {
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
                        "standard",
                        "leftmost-first",
                        "leftmost-longest",
                    ])
                    .default_value("standard"),
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
            .arg(Arg::with_name("dfa").long("dfa").short("d"))
            .arg(Arg::with_name("prefilter").long("prefilter").short("f"))
            .arg(Arg::with_name("classes").long("classes").short("c"))
            .arg(Arg::with_name("premultiply").long("premultiply").short("p"))
            .arg(Arg::with_name("no-search").long("no-search"))
            .get_matches();

        let dictionary =
            PathBuf::from(parsed.value_of_os("dictionary").unwrap());
        let haystack = PathBuf::from(parsed.value_of_os("haystack").unwrap());
        let match_kind = match parsed.value_of("kind").unwrap() {
            "standard" => MatchKind::Standard,
            "leftmost-first" => MatchKind::LeftmostFirst,
            "leftmost-longest" => MatchKind::LeftmostLongest,
            _ => unreachable!(),
        };
        let dense_depth = parsed.value_of("dense-depth").unwrap().parse()?;

        Ok(Args {
            dictionary,
            haystack,
            match_kind,
            dense_depth,
            ascii_casei: parsed.is_present("ascii-case-insensitive"),
            dfa: parsed.is_present("dfa"),
            prefilter: parsed.is_present("prefilter"),
            classes: parsed.is_present("classes"),
            premultiply: parsed.is_present("premultiply"),
            no_search: parsed.is_present("no-search"),
        })
    }

    fn automaton(&self) -> Result<AhoCorasick<Size>> {
        let start = Instant::now();
        let patterns = fs::read_to_string(&self.dictionary)?;
        let read_time = Instant::now().duration_since(start);
        eprintln!("pattern read time: {:?}", read_time);

        let start = Instant::now();
        let ac = AhoCorasickBuilder::new()
            .match_kind(self.match_kind)
            .ascii_case_insensitive(self.ascii_casei)
            .dense_depth(self.dense_depth)
            .dfa(self.dfa)
            .prefilter(self.prefilter)
            .byte_classes(self.classes)
            .premultiply(self.premultiply)
            .build_with_size::<Size, _, _>(patterns.lines())?;
        let build_time = Instant::now().duration_since(start);
        eprintln!("automaton build time: {:?}", build_time);

        Ok(ac)
    }

    fn haystack(&self) -> Result<Mmap> {
        Ok(unsafe { Mmap::map(&fs::File::open(&self.haystack)?)? })
    }
}
