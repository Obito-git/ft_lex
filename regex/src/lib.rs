use std::usize;

mod ast;
mod token;

pub struct Regex {
    pub pattern: String,
}

struct NfaTransition {
    pub start: usize,
    pub end: usize,
    pub symbol: Option<char>, // None == epsilon
}

struct Nfa {
    pub states_count: usize,
    pub transitions: Vec<NfaTransition>,
    pub start_state: usize,
    pub accept_state: usize,
}

impl Nfa {
    fn add_transition(&mut self, start: usize, end: usize, symbol: Option<char>) {
        self.transitions.push(NfaTransition { start, end, symbol });
    }

    pub fn from_char(c: char) -> Self {
        let mut nfa = Nfa {
            transitions: vec![],
            start_state: 0,
            accept_state: 1,
            states_count: 2,
        };
        nfa.add_transition(0, 1, Some(c));
        nfa
    }

    pub fn concatenate(&mut self, other: &Self) {
        let offset = self.states_count;
        let old_accept_state = self.accept_state;

        // merge states count
        self.states_count += other.states_count;

        // merge transitions but update including offsets
        for t in &other.transitions {
            self.add_transition(t.start + offset, t.end + offset, t.symbol);
        }

        // connect two machines using the epsilon
        self.add_transition(old_accept_state, other.start_state + offset, None);

        // new accept state is the other's accept state but need to respect the offset
        self.accept_state = other.accept_state + offset;
    }

    pub fn alternate(&mut self, other: &Self) {
        let old_start_state = self.start_state;
        let old_accept_state = self.accept_state;
        let offset = self.states_count;

        self.states_count += other.states_count;

        // merge transitions but update including offsets
        for t in &other.transitions {
            self.add_transition(t.start + offset, t.end + offset, t.symbol);
        }

        // add new state and make it new start state
        self.states_count += 1;
        let new_start_state = self.states_count;
        self.start_state = new_start_state;

        // add 2 epsilon transitions to the start of both NFAs
        self.add_transition(new_start_state, old_start_state, None);
        self.add_transition(new_start_state, other.start_state + offset, None);

        // add new state and make it new accept_state
        self.states_count += 1;
        let new_accept_state = self.states_count;
        self.start_state = new_accept_state;

        // add 2 epsilon transitions to the start of both NFAs
        self.add_transition(old_accept_state, new_accept_state, None);
        self.add_transition(other.accept_state + offset, new_accept_state, None);
    }
}

impl Regex {
    pub fn new(pattern: String) -> Result<Self, String> {
        Ok(Self { pattern })
    }

    pub fn matches(s: &str) -> bool {
        todo!()
    }
}

enum RegexAst {
    Literal(char),
    KleeneStar(Box<RegexAst>),
    Either(Box<RegexAst>, Box<RegexAst>),
    Concat(Box<RegexAst>),
}

impl RegexAst {
    pub fn new(pattern: String) -> Result<Self, String> {
        todo!()
    }
}
