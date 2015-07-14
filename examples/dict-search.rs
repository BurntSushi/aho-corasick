// This example demonstrates how to use the Aho-Corasick algorithm to rapidly
// scan text for matches in a large dictionary of keywords. This example by
// default reads your system's dictionary (~120,000 words).
extern crate aho_corasick;
extern crate csv;
extern crate docopt;
extern crate rustc_serialize;

use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead, Write};
use std::process;

use aho_corasick::{Automaton, AcAutomaton, Match};
use docopt::Docopt;

static USAGE: &'static str = "
Usage: dict-search [options] <input>
       dict-search --help

Options:
    -d <path>, --dict <path>   Path to dictionary of keywords to search.
                               [default: /usr/share/dict/words]
    -m <len>, --min-len <len>  The minimum length for a keyword in UTF-8
                               encoded bytes. [default: 5]
    --overlapping              Report overlapping matches.
    -h, --help                 Show this usage message.
";

#[derive(Clone, Debug, RustcDecodable)]
struct Args {
    arg_input: String,
    flag_dict: String,
    flag_min_len: usize,
    flag_overlapping: bool,
}

fn main() {
    let args: Args = Docopt::new(USAGE)
                            .and_then(|d| d.decode())
                            .unwrap_or_else(|e| e.exit());
    match run(&args) {
        Ok(()) => {}
        Err(err) => {
            writeln!(&mut io::stderr(), "{}", err).unwrap();
            process::exit(1);
        }
    }
}

fn run(args: &Args) -> Result<(), Box<Error>> {
    let aut = try!(build_automaton(&args.flag_dict, args.flag_min_len));
    let rdr = try!(File::open(&args.arg_input));
    if args.flag_overlapping {
        try!(write_matches(&aut, aut.stream_find_overlapping(rdr)));
    } else {
        try!(write_matches(&aut, aut.stream_find(rdr)));
    }
    Ok(())
}

fn write_matches<A, I>(aut: &A, it: I) -> Result<(), Box<Error>>
        where A: Automaton<String>, I: Iterator<Item=io::Result<Match>> {
    let mut wtr = csv::Writer::from_writer(io::stdout());
    try!(wtr.encode(("pattern", "start", "end")));
    for m in it {
        let m = try!(m);
        try!(wtr.encode((aut.pattern(m.pati), m.start, m.end)));
    }
    try!(wtr.flush());
    Ok(())
}

fn build_automaton(
    dict_path: &str,
    min_len: usize,
) -> Result<AcAutomaton<String>, Box<Error>> {
    let buf = io::BufReader::new(try!(File::open(dict_path)));
    let mut lines = Vec::with_capacity(1 << 10);
    for line in buf.lines() {
        let line = try!(line);
        if line.len() >= min_len {
            lines.push(line);
        }
    }
    Ok(AcAutomaton::new(lines))
}
