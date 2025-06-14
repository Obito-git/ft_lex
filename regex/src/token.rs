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
    KleeneStar,
    Plus,
    QuestionMark,
    Alter,
    LParen,
    RParen,
}

impl Token {
    pub fn is_quantifier(&self) -> bool {
        match self {
            Token::KleeneStar | Token::Plus | Token::QuestionMark => true,
            _ => false,
        }
    }
}

impl TokenSequence {
    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Literal(c) => write!(f, "{}", c),
            Token::KleeneStar => write!(f, "*"),
            Token::Plus => write!(f, "+"),
            Token::QuestionMark => write!(f, "?"),
            Token::Alter => write!(f, "|"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
        }
    }
}

// TODO: should be either String, either &str
impl From<&str> for TokenSequence {
    fn from(pattern: &str) -> Self {
        if pattern.is_empty() {
            todo!()
        }
        Self {
            tokens: pattern.chars().map(Token::from).collect(),
        }
    }
}

impl From<char> for Token {
    fn from(value: char) -> Self {
        match value {
            '*' => Token::KleeneStar,
            '+' => Token::Plus,
            '?' => Token::QuestionMark,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '|' => Token::Alter,
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
