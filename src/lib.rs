/*!
A fast implementation of the Aho-Corasick string search algorithm.
*/

use std::fmt;

#[derive(Clone, Debug)]
pub struct Builder {
    pats: Vec<String>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder { pats: vec![] }
    }

    pub fn add<S: Into<String>>(mut self, s: S) -> Builder {
        self.pats.push(s.into());
        self
    }

    pub fn build(self) -> Automaton {
        Automaton::new(self.pats)
    }
}

type PatIdx = usize;
type StateIdx = usize;

#[derive(Clone)]
pub struct Automaton {
    pats: Vec<String>,
    states: Vec<State>,
}

#[derive(Clone)]
struct State {
    out: Vec<PatIdx>,
    fail: StateIdx,
    goto: Vec<StateIdx>, // indexed by alphabet
}

impl Automaton {
    fn new(pats: Vec<String>) -> Automaton {
        Automaton {
            pats: vec![], // filled in later, avoid wrath of borrow checker
            states: vec![State::new()],
        }.build(pats)
    }

    fn build(mut self, pats: Vec<String>) -> Automaton {
        let rooti = self.add_state(State::new());
        for (pati, pat) in pats.iter().enumerate() {
            let mut previ = rooti;
            for &b in pat.as_bytes() {
                if let Some(si) = self.states[previ].goto(b) {
                    previ = si;
                } else {
                    let nexti = self.add_state(State::new());
                    self.states[previ].goto[b as usize] = nexti;
                    previ = nexti;
                }
            }
            self.states[previ].out.push(pati);
        }
        for v in &mut self.states[rooti].goto {
            if *v == 0 {
                *v = 1;
            }
        }
        self.pats = pats;
        self
    }

    fn add_state(&mut self, state: State) -> StateIdx {
        let i = self.states.len();
        self.states.push(state);
        i
    }
}

impl State {
    fn new() -> State {
        State {
            out: vec![],
            fail: 1,
            goto: vec![0; 256],
        }
    }

    fn goto(&self, b: u8) -> Option<StateIdx> {
        let i = self.goto[b as usize];
        if i == 0 { None } else { Some(i) }
    }

}

impl fmt::Debug for Automaton {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::iter::repeat;

        try!(writeln!(f, "{}", repeat('-').take(79).collect::<String>()));
        try!(writeln!(f, "Patterns: {:?}", self.pats));
        for (i, state) in self.states.iter().enumerate().skip(1) {
            try!(writeln!(f, "{:3}: {}", i, state.debug(i == 1)));
        }
        write!(f, "{}", repeat('-').take(79).collect::<String>())
    }
}

impl State {
    fn debug(&self, root: bool) -> String {
        format!("State {{ out: {:?}, fail: {:?}, goto: {{{}}} }}",
                self.out, self.fail, self.dense_goto_string(root))
    }

    fn dense_goto_string(&self, root: bool) -> String {
        use std::char::from_u32;

        let mut goto = vec![];
        for (i, &state) in self.goto.iter().enumerate() {
            if (!root && state == 0) || (root && state == 1) { continue; }
            goto.push(format!("{} => {}", from_u32(i as u32).unwrap(), state));
        }
        goto.connect(", ")
    }
}

impl fmt::Debug for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.debug(false))
    }
}

#[cfg(test)]
mod tests {
    use super::Builder;

    #[test]
    fn scratch() {
        let aut = Builder::new().add("he").add("she").build();
        println!("{:?}", aut);
    }
}
