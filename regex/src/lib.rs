use std::{collections::HashSet, usize};

mod ast;
mod nfa;
mod token;

pub struct Regex {
    pub pattern: String,
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
