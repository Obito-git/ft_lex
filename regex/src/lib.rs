use crate::token::tokenize;
use ast::RegexAstNode;
use nfa::Nfa;
#[cfg(test)]
use serde::Serialize;

mod ast;
mod nfa;
mod token;

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum RegexErr {
    UnexpectedToken {
        position: usize,
        found: char,
    },
    ExpectedClosingParenthesis {
        position: usize,
        found: Option<char>,
    },
    ExpectedClosingCurlyBracket {
        position: usize,
        found: Option<char>,
    },
    ExpectedClosingSquareBracket {
        position: usize,
    },
    UnexpectedEndOfExpression,
    QuantifierWithoutTarget {
        position: usize,
        quantifier: char,
    },
    EmptyRangeQuantifier,
    RangeQuantifierInvalidNumber {
        text: String,
    },
    RangeQuantifierMinExceedsMax {
        min: u16,
        max: u16,
    },
    InvalidRangeInCharacterSet {
        start: char,
        end: char,
    },
    RangeIsForbiddenForPosixClasses {
        position: usize,
    },
    UnknownPosixClassName {
        position: usize,
        name: String,
    },
    ExpectedClosingPosixClass {
        position: usize,
        found: Option<char>,
    },

    EscapedNothing,
}

pub struct Regex {
    nfa: Nfa,
}

impl Regex {
    pub fn new(pattern: &str) -> Result<Self, RegexErr> {
        let tokens = tokenize(pattern)?;
        let ast = RegexAstNode::new(tokens)?;
        let nfa = Nfa::from(ast);

        Ok(Self { nfa })
    }

    pub fn is_exact_match(&self, s: &str) -> bool {
        self.nfa.is_exact_match(s)
    }

    pub fn is_match(&self, s: &str) -> bool {
        self.nfa.is_match(s)
    }
}
