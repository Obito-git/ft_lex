use ast::RegexAstNode;
use nfa::Nfa;
use token::TokenSequence;

mod ast;
mod nfa;
mod token;

pub struct Regex {
    nfa: Nfa,
}

impl Regex {
    // TODO: err string? or?
    pub fn new(pattern: &str) -> Result<Self, String> {
        let nfa = RegexAstNode::new(pattern)?.to_nfa();

        Ok(Self { nfa })
    }

    pub fn is_exact_match(&self, s: &str) -> bool {
        self.nfa.is_exact_match(s)
    }
}
