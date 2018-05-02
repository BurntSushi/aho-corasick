extern crate aho_corasick;
use std::env;

use aho_corasick::AcAutomaton;

fn main() {
    let aut = AcAutomaton::new(env::args().skip(1));
    println!("{}", aut.dot().trim());
}
