#[cfg(test)]
use serde::Serialize;
use std::fmt::{Display, Formatter};

// TODO: refactor all errors
#[derive(Debug, PartialEq)]
pub enum TokenParsingErr {
    EscapedNothing,
}

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) struct TokenSequence {
    pub(crate) tokens: Vec<Token>,
}

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) enum Token {
    Literal(char),
    Dot,
    Star,
    Plus,
    QuestionMark,
    Alter,
    LParen,
    RParen,
    LCurlyBracket,
    RCurlyBracket,
}

impl Token {
    pub fn is_quantifier(&self) -> bool {
        match self {
            Token::Star | Token::Plus | Token::QuestionMark | Token::LCurlyBracket => true,
            _ => false,
        }
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Token::Literal(_))
    }
}

impl TokenSequence {
    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", char::from(self))
    }
}

impl From<Token> for char {
    fn from(value: Token) -> Self {
        char::from(&value)
    }
}

impl From<&Token> for char {
    fn from(value: &Token) -> Self {
        match value {
            Token::Literal(c) => *c,
            Token::Star => '*',
            Token::Plus => '+',
            Token::QuestionMark => '?',
            Token::Alter => '|',
            Token::LParen => '(',
            Token::RParen => ')',
            Token::Dot => '.',
            Token::LCurlyBracket => '{',
            Token::RCurlyBracket => '}',
        }
    }
}

// TODO: should be either String, either &str
impl TryFrom<&str> for TokenSequence {
    type Error = TokenParsingErr;

    fn try_from(pattern: &str) -> Result<Self, Self::Error> {
        let mut tokens = Vec::with_capacity(pattern.len());
        let mut pattern_iter = pattern.chars();

        while let Some(cur) = pattern_iter.next() {
            if cur == '\\' {
                if let Some(escaped) = pattern_iter.next() {
                    tokens.push(Token::Literal(escaped));
                } else {
                    return Err(TokenParsingErr::EscapedNothing);
                }
            } else {
                tokens.push(Token::from(cur));
            }
        }

        Ok(Self { tokens })
    }
}

impl From<char> for Token {
    fn from(value: char) -> Self {
        match value {
            '{' => Token::LCurlyBracket,
            '}' => Token::RCurlyBracket,
            '.' => Token::Dot,
            '*' => Token::Star,
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

    #[test]
    fn test_parsing_of_all_special_tokens() {
        // given
        let pattern = "a.b*c+d?e|f(g)";
        let expected = TokenSequence {
            tokens: vec![
                Token::Literal('a'),
                Token::Dot,
                Token::Literal('b'),
                Token::Star,
                Token::Literal('c'),
                Token::Plus,
                Token::Literal('d'),
                Token::QuestionMark,
                Token::Literal('e'),
                Token::Alter,
                Token::Literal('f'),
                Token::LParen,
                Token::Literal('g'),
                Token::RParen,
            ],
        };

        // when
        let res = TokenSequence::try_from(pattern).unwrap();

        // then
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_with_escaped_special_characters() {
        // given
        let pattern = r"a\.b\*c\+d\?e\|f\(g\)";
        let expected = TokenSequence {
            tokens: vec![
                Token::Literal('a'),
                Token::Literal('.'),
                Token::Literal('b'),
                Token::Literal('*'),
                Token::Literal('c'),
                Token::Literal('+'),
                Token::Literal('d'),
                Token::Literal('?'),
                Token::Literal('e'),
                Token::Literal('|'),
                Token::Literal('f'),
                Token::Literal('('),
                Token::Literal('g'),
                Token::Literal(')'),
            ],
        };

        // when
        let res = TokenSequence::try_from(pattern).unwrap();

        // then
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_fails_on_trailing_escape_character() {
        // given
        let pattern = r"abc\";

        // when
        let res = TokenSequence::try_from(pattern);

        // then
        assert!(res.is_err());
        assert_eq!(res.unwrap_err(), TokenParsingErr::EscapedNothing);
    }

    #[test]
    fn test_parsing_empty_string_returns_empty_sequence() {
        // given
        let pattern = "";
        let expected = TokenSequence { tokens: vec![] };

        // when
        let res = TokenSequence::try_from(pattern).unwrap();

        // then
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_of_curly_brackets() {
        // given
        let pattern = "a{2,3}|b";
        let expected = TokenSequence {
            tokens: vec![
                Token::Literal('a'),
                Token::LCurlyBracket,
                Token::Literal('2'),
                Token::Literal(','),
                Token::Literal('3'),
                Token::RCurlyBracket,
                Token::Alter,
                Token::Literal('b'),
            ],
        };

        // when
        let res = TokenSequence::try_from(pattern).unwrap();

        // then
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_with_escaped_curly_brackets() {
        // given
        let pattern = r"a\{2,3\}";
        let expected = TokenSequence {
            tokens: vec![
                Token::Literal('a'),
                Token::Literal('{'),
                Token::Literal('2'),
                Token::Literal(','),
                Token::Literal('3'),
                Token::Literal('}'),
            ],
        };

        // when
        let res = TokenSequence::try_from(pattern).unwrap();

        // then
        assert_eq!(res, expected);
    }
}
