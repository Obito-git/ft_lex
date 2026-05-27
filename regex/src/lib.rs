use crate::token::tokenize;
use ast::RegexAstNode;
use nfa::Nfa;
#[cfg(test)]
use serde::{Serialize, Serializer};

mod ast;
mod nfa;
mod token;

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum RegexErr {
    UnexpectedToken(usize, char),
    UnexpectedTokenInTheEndOfExpr(usize, char),
    ExpectedClosingParenthesis(usize, Option<char>),
    ExpectedClosingCurlyBracket(usize, Option<char>),
    PrecedentTokenIsNotQuantifiable(usize, char),
    ExpressionCantStartWithQuantifier(usize, char),
    ExpectedClosingSquareBracket,
    UnexpectedEndOfExpression,
    EmptyRangeQuantifier,
    RangeQuantifierInvalidNumber(String),
    RangeQuantifierMinGreaterMax(u16, u16),
    InvalidRangeInCharacterSet(char, char),
    RangeIsForbiddenForPosixClasses(usize),
    UnknownPosixClassName(usize, String),

    ExpectedEndOfPosixClassSyntax(usize, Option<char>),
    InternalError, // TODO: delete, now I put it as stub when not sure

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
