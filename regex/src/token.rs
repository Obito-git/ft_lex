#[cfg(test)]
use serde::Serialize;

use crate::RegexErr;
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) enum Token {
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
    Dollar,
}

impl Token {
    pub(crate) fn is_literal(&self) -> bool {
        matches!(self, Token::Literal(_))
    }

    pub(crate) fn can_start_concatenation(&self) -> bool {
        matches!(
            self,
            Token::Literal(_)
                | Token::Dot
                | Token::LParen
                | Token::LSquareBracket
                | Token::Colon
                | Token::Dollar
                | Token::Dash
                | Token::RSquareBracket
                | Token::Caret
        )
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
            Token::Caret => '^',
            Token::Colon => ':',
            Token::Dash => '-',
            Token::Dollar => '$',
        }
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
            '$' => Token::Dollar,
            _ => Token::Literal(value),
        }
    }
}

pub(crate) fn tokenize(pattern: &str) -> Result<Vec<Token>, RegexErr> {
    let mut tokens = Vec::with_capacity(pattern.len());
    let mut pattern_iter = pattern.chars();

    while let Some(cur) = pattern_iter.next() {
        if cur == '\\' {
            let escaped = pattern_iter.next().ok_or(RegexErr::EscapedNothing)?;
            tokens.push(Token::Literal(escaped));
        } else {
            tokens.push(Token::from(cur));
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn test_next_token_should_return_right_position() {
        let pattern = r"a.b*c+d?e|f(g){2}[\.i]";
        let tokens = tokenize(pattern).unwrap();

        assert!(!tokens.is_empty());
        assert_eq!(tokens[0], Literal('a'));
        assert_eq!(tokens[18], Literal('.'));
        assert_eq!(tokens[19], Literal('i'));
    }

    #[test]
    fn test_parsing_of_all_special_tokens() {
        let pattern = r"^a.b*c+d?e|f(g){2}[\.i]-:{}$";
        let expected = vec![
            Caret,
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
            Dollar,
        ];

        let res = tokenize(pattern).unwrap();

        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_with_escaped_special_characters() {
        let pattern = r"a\.b\*c\+d\?e\|f\(g\)\:\-\[\]\{\}\^\$";
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
            Literal('^'),
            Literal('$'),
        ];

        let res = tokenize(pattern).unwrap();

        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_fails_on_trailing_escape_character() {
        let pattern = r"abc\";

        let res = tokenize(pattern);

        assert!(res.is_err());
        assert_eq!(res.unwrap_err(), RegexErr::EscapedNothing);
    }

    #[test]
    fn test_parsing_empty_string_returns_empty_sequence() {
        let pattern = "";
        let expected = vec![];

        let res = tokenize(pattern).unwrap();

        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_of_curly_brackets() {
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

        let res = tokenize(pattern).unwrap();

        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_with_escaped_curly_brackets() {
        let pattern = r"a\{2,3\}";
        let expected = vec![
            Literal('a'),
            Literal('{'),
            Literal('2'),
            Literal(','),
            Literal('3'),
            Literal('}'),
        ];

        let res = tokenize(pattern).unwrap();

        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_of_square_brackets() {
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

        let res = tokenize(pattern).unwrap();

        assert_eq!(res, expected);
    }

    #[test]
    fn test_parsing_with_escaped_square_brackets() {
        let pattern = r"a\[b-d\]";
        let expected = vec![
            Literal('a'),
            Literal('['),
            Literal('b'),
            Dash,
            Literal('d'),
            Literal(']'),
        ];

        let res = tokenize(pattern).unwrap();

        assert_eq!(res, expected);
    }
}
