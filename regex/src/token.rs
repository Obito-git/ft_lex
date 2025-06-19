#[cfg(test)]
use serde::Serialize;
use std::{
    fmt::{Display, Formatter},
    iter::{Enumerate, Peekable},
    vec::IntoIter,
};

// TODO: refactor all errors
#[derive(Debug, PartialEq)]
pub enum TokenParsingErr {
    EscapedNothing,
}

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) enum Token {
    Escape,
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
    LSquareBracket,
    RSquareBracket,
}

#[derive(Clone, Debug)]
pub(crate) struct TokenSequence {
    tokens: Peekable<Enumerate<IntoIter<Token>>>,
    original_pattern: String,
    cur_idx: usize,
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
    pub fn new(tokens: Vec<Token>) -> Self {
        let original_pattern = tokens.iter().map(char::from).collect();

        Self {
            tokens: tokens.into_iter().enumerate().peekable(),
            original_pattern,
            cur_idx: 0,
        }
    }

    fn skip_escape(&mut self) {
        while self
            .tokens
            .peek()
            .map(|(_, token)| token == &Token::Escape)
            .unwrap_or(false)
        {
            self.cur_idx += 1;
            self.tokens.next();
        }
    }

    pub fn next(&mut self) -> Option<Token> {
        self.skip_escape();
        if let Some((idx, token)) = self.tokens.next() {
            self.cur_idx = idx;
            Some(token)
        } else {
            None
        }
    }
    pub fn next_enumerated(&mut self) -> Option<(usize, Token)> {
        self.skip_escape();
        if let Some((idx, token)) = self.tokens.next() {
            self.cur_idx = idx;
            Some((idx, token))
        } else {
            None
        }
    }

    pub fn peek(&mut self) -> Option<&Token> {
        self.skip_escape();
        if let Some((_, token)) = self.tokens.peek() {
            Some(token)
        } else {
            None
        }
    }

    pub fn peek_enumerated(&mut self) -> Option<(usize, &Token)> {
        self.skip_escape();
        if let Some((idx, token)) = self.tokens.peek() {
            Some((*idx, token))
        } else {
            None
        }
    }

    pub fn cur_pos(&self) -> usize {
        self.cur_idx
    }

    pub fn collect(&mut self) -> Vec<Token> {
        let mut res = Vec::with_capacity(self.original_pattern.len());
        while let Some(tok) = self.next() {
            res.push(tok);
        }
        res
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
            Token::LSquareBracket => '[',
            Token::RSquareBracket => ']',
            Token::Escape => '\\',
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
                    // not used, but helps to keep track of original position
                    // to be able to return right position for the error handling
                    tokens.push(Token::Escape);
                    tokens.push(Token::Literal(escaped));
                } else {
                    return Err(TokenParsingErr::EscapedNothing);
                }
            } else {
                tokens.push(Token::from(cur));
            }
        }
        Ok(TokenSequence::new(tokens))
    }
}

impl From<char> for Token {
    fn from(value: char) -> Self {
        match value {
            '{' => Token::LCurlyBracket,
            '}' => Token::RCurlyBracket,
            '[' => Token::LSquareBracket,
            ']' => Token::RSquareBracket,
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
    use Token::*;

    #[test]
    fn test_next_token_should_return_right_position() {
        // given
        let pattern = r"a.b*c+d?e|f(g){2}[\.i]";
        let mut sequence = TokenSequence::try_from(pattern).unwrap();
        let mut iter_count = 0usize;

        // when && then
        while let Some(token) = sequence.next() {
            assert_eq!(
                pattern.chars().nth(sequence.cur_idx).unwrap(),
                char::from(token)
            );
            iter_count += 1;
        }

        // then
        assert!(iter_count != 0);
    }
    #[test]
    fn test_parsing_of_all_special_tokens() {
        // given
        let pattern = r"a.b*c+d?e|f(g){2}[\.i]";
        let expected = vec![
            Literal('a'),
            Dot,
            Literal('b'),
            Star,
            Literal('c'),
            Plus,
            Literal('d'),
            QuestionMark,
            Literal('e'),
            Alter,
            Literal('f'),
            LParen,
            Literal('g'),
            RParen,
            LCurlyBracket,
            Literal('2'),
            RCurlyBracket,
            LSquareBracket,
            Literal('.'),
            Literal('i'),
            RSquareBracket,
        ];

        // when
        let res = TokenSequence::try_from(pattern).unwrap().collect();

        // then
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_with_escaped_special_characters() {
        // given
        let pattern = r"a\.b\*c\+d\?e\|f\(g\)";
        let expected = vec![
            Literal('a'),
            Literal('.'),
            Literal('b'),
            Literal('*'),
            Literal('c'),
            Literal('+'),
            Literal('d'),
            Literal('?'),
            Literal('e'),
            Literal('|'),
            Literal('f'),
            Literal('('),
            Literal('g'),
            Literal(')'),
        ];

        // when
        let res = TokenSequence::try_from(pattern).unwrap().collect();

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
        let expected = vec![];

        // when
        let res = TokenSequence::try_from(pattern).unwrap().collect();

        // then
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_of_curly_brackets() {
        // given
        let pattern = "a{2,3}|b";
        let expected = vec![
            Literal('a'),
            LCurlyBracket,
            Literal('2'),
            Literal(','),
            Literal('3'),
            RCurlyBracket,
            Alter,
            Literal('b'),
        ];

        // when
        let res = TokenSequence::try_from(pattern).unwrap().collect();

        // then
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_with_escaped_curly_brackets() {
        // given
        let pattern = r"a\{2,3\}";
        let expected = vec![
            Literal('a'),
            Literal('{'),
            Literal('2'),
            Literal(','),
            Literal('3'),
            Literal('}'),
        ];

        // when
        let res = TokenSequence::try_from(pattern).unwrap().collect();

        // then
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_of_square_brackets() {
        // given
        let pattern = "a[b-d]|c";
        let expected = vec![
            Literal('a'),
            LSquareBracket,
            Literal('b'),
            Literal('-'),
            Literal('d'),
            RSquareBracket,
            Alter,
            Literal('c'),
        ];

        // when
        let res = TokenSequence::try_from(pattern).unwrap().collect();

        // then
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_with_escaped_square_brackets() {
        // given
        let pattern = r"a\[b-d\]";
        let expected = vec![
            Literal('a'),
            Literal('['),
            Literal('b'),
            Literal('-'),
            Literal('d'),
            Literal(']'),
        ];

        // when
        let res = TokenSequence::try_from(pattern).unwrap().collect();

        // then
        assert_eq!(res, expected);
    }
}
