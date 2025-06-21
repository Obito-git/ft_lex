#[cfg(test)]
use serde::Serialize;
use std::fmt::{Display, Formatter};

// TODO: refactor all errors
#[derive(Debug, PartialEq)]
pub enum TokenParsingErr {
    EscapedNothing,
}

#[derive(Copy, Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) enum Token {
    BackSlash,
    Literal(char),
    Dot,
    Star,
    Plus,
    QuestionMark,
    Pipe,
    LParen,
    RParen,
    LCurlyBracket,
    RCurlyBracket,
    LSquareBracket,
    RSquareBracket,
    Caret,
    Colon,
    Dash,
}

impl Token {
    pub fn is_quantifier(&self) -> bool {
        matches!(
            self,
            Token::Star | Token::Plus | Token::QuestionMark | Token::LCurlyBracket
        )
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Token::Literal(_))
    }

    pub fn is_atom_start(&self) -> bool {
        matches!(
            self,
            Token::Literal(_)
                | Token::Dot
                | Token::LParen
                | Token::LSquareBracket
                | Token::Colon
                | Token::Dash
                | Token::RSquareBracket
        )
    }
}

#[derive(Clone, Debug)]
pub(crate) struct TokenSequence {
    //TODO: or iterator?
    tokens: Vec<Token>,
    pos: usize,
}

impl TokenSequence {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn cur_pos(&self) -> usize {
        self.pos
    }

    fn peek_enumerated_nth(&self, pos: usize) -> Option<(usize, &Token)> {
        if let Some(token) = self.tokens.get(pos) {
            if token == &Token::BackSlash {
                self.tokens.get(pos + 1).map(|t| (pos + 1, t))
            } else {
                Some((pos, token))
            }
        } else {
            None
        }
    }

    pub fn peek_enumerated(&self) -> Option<(usize, &Token)> {
        self.peek_enumerated_nth(self.pos)
    }

    pub fn next_enumerated(&mut self) -> Option<(usize, Token)> {
        let token_index = if self.tokens.get(self.pos) == Some(&Token::BackSlash) {
            self.pos + 1
        } else {
            self.pos
        };

        let token_to_return = self.tokens.get(token_index)?;

        self.pos = token_index + 1;

        Some((token_index, *token_to_return))
    }

    pub fn peek(&self) -> Option<&Token> {
        self.peek_enumerated().map(|(_index, token)| token)
    }

    pub fn peek_two(&self) -> (Option<&Token>, Option<&Token>) {
        (
            self.peek_enumerated_nth(self.pos).map(|(_, token)| token),
            self.peek_enumerated_nth(self.pos + 1)
                .map(|(_, token)| token),
        )
    }

    pub fn next(&mut self) -> Option<Token> {
        self.next_enumerated().map(|(_index, token)| token)
    }

    pub fn collect(&mut self) -> Vec<Token> {
        let mut res = Vec::new();
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
            Token::Pipe => '|',
            Token::LParen => '(',
            Token::RParen => ')',
            Token::Dot => '.',
            Token::LCurlyBracket => '{',
            Token::RCurlyBracket => '}',
            Token::LSquareBracket => '[',
            Token::RSquareBracket => ']',
            Token::BackSlash => '\\',
            Token::Caret => '^',
            Token::Colon => ':',
            Token::Dash => '-',
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
                    tokens.push(Token::BackSlash);
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
            '|' => Token::Pipe,
            '^' => Token::Caret,
            ':' => Token::Colon,
            '-' => Token::Dash,
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
        while let Some((pos, token)) = sequence.next_enumerated() {
            assert_eq!(pattern.chars().nth(pos).unwrap(), char::from(token));
            iter_count += 1;
        }

        // then
        assert_ne!(iter_count, 0);
    }
    #[test]
    fn test_parsing_of_all_special_tokens() {
        // given
        let pattern = r"a.b*c+d?e|f(g){2}[\.i]-:{}";
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
            Pipe,
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
            Dash,
            Colon,
            LCurlyBracket,
            RCurlyBracket,
        ];

        // when
        let res = TokenSequence::try_from(pattern).unwrap().collect();

        // then
        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_with_escaped_special_characters() {
        // given
        let pattern = r"a\.b\*c\+d\?e\|f\(g\)\:\-\[\]\{\}";
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
            Literal(':'),
            Literal('-'),
            Literal('['),
            Literal(']'),
            Literal('{'),
            Literal('}'),
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
            Pipe,
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
            Dash,
            Literal('d'),
            RSquareBracket,
            Pipe,
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
            Dash,
            Literal('d'),
            Literal(']'),
        ];

        // when
        let res = TokenSequence::try_from(pattern).unwrap().collect();

        // then
        assert_eq!(res, expected);
    }
}
