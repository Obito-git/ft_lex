#[cfg(test)]
use serde::Serialize;
use std::fmt::{Display, Formatter};

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) struct TokenSequence {
    tokens: Vec<Token>,
}

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) enum Token {
    Literal(char),
    Star,
    Plus,
    QuestionMark,
    Pipe,
    LParen,
    RParen,
}

impl Token {
    pub fn is_quantifier(&self) -> bool {
        match self {
            Token::Star | Token::Plus | Token::QuestionMark => true,
            _ => false,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Literal(c) => write!(f, "{}", c),
            Token::Star => write!(f, "*"),
            Token::Plus => write!(f, "+"),
            Token::QuestionMark => write!(f, "?"),
            Token::Pipe => write!(f, "|"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
        }
    }
}

// TODO: should be either String, either &str
impl From<String> for TokenSequence {
    fn from(pattern: String) -> Self {
        if pattern.is_empty() {
            unimplemented!()
        }
        Self {
            tokens: pattern.chars().map(Token::from).collect(),
        }
    }
}

impl From<char> for Token {
    fn from(value: char) -> Self {
        match value {
            '*' => Token::Star,
            '+' => Token::Plus,
            '?' => Token::QuestionMark,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '|' => Token::Pipe,
            _ => Token::Literal(value),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("kaka")]
    fn parsing(#[case] pattern: String) {
        assert_eq!(pattern, "kaka".to_string())
    }
}
