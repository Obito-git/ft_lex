#[cfg(test)]
use serde::Serialize;

use crate::RegexErr;
use crate::token::Token;

struct AstParser {
    tokens: Vec<Token>,
    pos: usize,
}

#[derive(Copy, Clone)]
enum QuantifierKind {
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
    Bounded,
}

impl QuantifierKind {
    fn from_token(token: Token) -> Option<Self> {
        match token {
            Token::Star => Some(Self::ZeroOrMore),
            Token::Plus => Some(Self::OneOrMore),
            Token::QuestionMark => Some(Self::ZeroOrOne),
            Token::LCurlyBracket => Some(Self::Bounded),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) enum BracketItem {
    Literal(char),
    Range { start: char, end: char },
    PosixClass(PosixClass),
}

impl AstParser {
    pub fn parse(tokens: Vec<Token>) -> Result<RegexAstNode, RegexErr> {
        if tokens.is_empty() {
            return Ok(RegexAstNode::Empty);
        }

        let mut parser = Self { tokens, pos: 0 };

        let parsed_ast = parser.parse_alternation()?;

        if let Some((pos, token_left)) = parser.next_enumerated() {
            Err(RegexErr::UnexpectedToken {
                position: pos,
                found: token_left.into(),
            })
        } else {
            Ok(parsed_ast)
        }
    }

    fn cur_pos(&self) -> usize {
        self.pos
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn peek_enumerated(&self) -> Option<(usize, &Token)> {
        self.peek().map(|token| (self.pos, token))
    }

    fn peek_two(&self) -> (Option<&Token>, Option<&Token>) {
        (self.tokens.get(self.pos), self.tokens.get(self.pos + 1))
    }

    fn next(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.pos).copied()?;
        self.pos += 1;
        Some(token)
    }

    fn next_enumerated(&mut self) -> Option<(usize, Token)> {
        let pos = self.pos;
        let token = self.next()?;
        Some((pos, token))
    }

    fn parse_alternation(&mut self) -> Result<RegexAstNode, RegexErr> {
        let mut left_node = self.parse_concatenation()?;

        while let Some(Token::Pipe) = self.peek() {
            self.next();

            let right_node = self.parse_concatenation()?;
            left_node = RegexAstNode::Alter(Box::new(left_node), Box::new(right_node));
        }

        Ok(left_node)
    }

    fn parse_concatenation(&mut self) -> Result<RegexAstNode, RegexErr> {
        let mut left_node = self.parse_quantifier()?;

        while self.peek().is_some_and(Token::can_start_concatenation) {
            let right_node = self.parse_quantifier()?;
            left_node = RegexAstNode::Concat(Box::new(left_node), Box::new(right_node));
        }

        Ok(left_node)
    }

    fn apply_quantifier(
        &mut self,
        node: RegexAstNode,
        quantifier: QuantifierKind,
    ) -> Result<RegexAstNode, RegexErr> {
        match quantifier {
            QuantifierKind::ZeroOrMore => Ok(RegexAstNode::Star(Box::new(node))),
            QuantifierKind::OneOrMore => Ok(RegexAstNode::OneOrMore(Box::new(node))),
            QuantifierKind::ZeroOrOne => Ok(RegexAstNode::ZeroOrOne(Box::new(node))),
            QuantifierKind::Bounded => {
                let mut content = Vec::new();
                while let Some(token) = self.peek() {
                    if !token.is_literal() {
                        break;
                    }
                    content.push(self.next().unwrap().into());
                }
                if self.peek() != Some(&Token::RCurlyBracket) {
                    return if let Some((idx, next)) = self.next_enumerated() {
                        Err(RegexErr::ExpectedClosingCurlyBracket {
                            position: idx,
                            found: Some(next.into()),
                        })
                    } else {
                        Err(RegexErr::ExpectedClosingCurlyBracket {
                            position: self.cur_pos(),
                            found: None,
                        })
                    };
                }
                self.next();
                let (n, m) = Self::parse_bounded_quantifier(content)?;
                Ok(RegexAstNode::Repeat {
                    node: Box::new(node),
                    lower_bound: n,
                    upper_bound: m,
                })
            }
        }
    }

    fn parse_quantifier(&mut self) -> Result<RegexAstNode, RegexErr> {
        let mut node = self.parse_literal_or_group()?;

        if let Some((idx, next_token)) = self.peek_enumerated() {
            if let Some(quantifier) = QuantifierKind::from_token(*next_token) {
                if !node.is_quantifiable() {
                    return Err(RegexErr::QuantifierWithoutTarget {
                        position: idx.checked_sub(1).unwrap_or(idx),
                        quantifier: next_token.into(),
                    });
                }
                self.next();
                node = self.apply_quantifier(node, quantifier)?;
            }
        }
        Ok(node)
    }

    fn parse_literal_or_group(&mut self) -> Result<RegexAstNode, RegexErr> {
        let (idx, token) = self
            .next_enumerated()
            .ok_or(RegexErr::UnexpectedEndOfExpression)?;
        if QuantifierKind::from_token(token).is_some() {
            Err(RegexErr::QuantifierWithoutTarget {
                position: idx,
                quantifier: (&token).into(),
            })?
        }
        match token {
            Token::Literal(c) => Ok(RegexAstNode::Literal(c)),
            Token::Colon => Ok(RegexAstNode::Literal(':')),
            Token::Dash => Ok(RegexAstNode::Literal('-')),
            Token::Dot => Ok(RegexAstNode::Wildcard),
            Token::Caret => Ok(RegexAstNode::StartOfLineAnchor),
            Token::Dollar => Ok(RegexAstNode::EndOfLineAnchor),
            Token::LSquareBracket => self.parse_square_bracket_expr(),
            Token::RSquareBracket => Ok(RegexAstNode::Literal(']')),
            Token::LParen => {
                if let Some(Token::RParen) = self.peek() {
                    self.next();
                    return Ok(RegexAstNode::Empty);
                }
                let nested_expr = self.parse_alternation()?;
                match self.next() {
                    Some(Token::RParen) => Ok(nested_expr),
                    Some(token) => Err(RegexErr::ExpectedClosingParenthesis {
                        position: self.cur_pos(),
                        found: Some(token.into()),
                    }),
                    None => Err(RegexErr::ExpectedClosingParenthesis {
                        position: self.cur_pos(),
                        found: None,
                    }),
                }
            }
            _ => Err(RegexErr::UnexpectedToken {
                position: idx,
                found: token.into(),
            }),
        }
    }

    fn parse_posix_class(&mut self) -> Result<PosixClass, RegexErr> {
        let mut class_name = String::new();
        let class_name_start_pos = self.cur_pos();

        while let Some(token) = self.next() {
            if matches!(token, Token::Colon) {
                break;
            }
            class_name.push(token.into());
        }

        let next = self.next();
        if next != Some(Token::RSquareBracket) {
            Err(RegexErr::ExpectedClosingPosixClass {
                position: self.cur_pos(),
                found: next.map(char::from),
            })?
        }

        PosixClass::try_from(class_name.as_str()).map_err(|_| RegexErr::UnknownPosixClassName {
            position: class_name_start_pos,
            name: class_name,
        })
    }

    fn parse_square_bracket_expr(&mut self) -> Result<RegexAstNode, RegexErr> {
        let mut items = Vec::new();
        let mut is_negated = false;

        if let Some(Token::Caret) = self.peek() {
            is_negated = true;
            self.next();
        }

        // in PCRE if ] is the first char it is treated as literal
        if let Some(Token::RSquareBracket) = self.peek() {
            items.push(BracketItem::Literal(']'));
            self.next();
        }

        while let Some(token) = self.peek() {
            if *token == Token::RSquareBracket {
                break;
            }
            if let (Some(Token::LSquareBracket), Some(Token::Colon)) = self.peek_two() {
                self.next();
                self.next();
                let posix_class = self.parse_posix_class()?;
                items.push(BracketItem::PosixClass(posix_class));
                if let Some((idx, Token::Dash)) = self.peek_enumerated() {
                    Err(RegexErr::RangeIsForbiddenForPosixClasses { position: idx })?
                }
                continue;
            }

            let start_token = self.next().unwrap();

            if let Some(Token::Dash) = self.peek() {
                self.next();

                if let (Some(Token::LSquareBracket), Some(Token::Colon)) = self.peek_two() {
                    Err(RegexErr::RangeIsForbiddenForPosixClasses {
                        position: self.cur_pos(),
                    })?
                }

                if let Some(end_token) = self.peek().copied() {
                    if end_token != Token::RSquareBracket {
                        self.next();

                        let start_char = char::from(&start_token);
                        let end_char = char::from(&end_token);

                        if start_char > end_char {
                            return Err(RegexErr::InvalidRangeInCharacterSet {
                                start: start_char,
                                end: end_char,
                            });
                        }

                        items.push(BracketItem::Range {
                            start: start_char,
                            end: end_char,
                        });
                        continue;
                    }
                }

                items.push(BracketItem::Literal(char::from(&start_token)));
                items.push(BracketItem::Literal('-'));
            } else {
                items.push(BracketItem::Literal(char::from(&start_token)));
            }
        }

        let next = self.next();
        if next != Some(Token::RSquareBracket) {
            Err(RegexErr::ExpectedClosingSquareBracket {
                position: self.cur_pos(),
            })?
        }

        Ok(RegexAstNode::BracketExpression {
            is_negated,
            expr: items,
        })
    }

    fn parse_bounded_quantifier(content: Vec<char>) -> Result<(u16, Option<u16>), RegexErr> {
        if content.is_empty() {
            return Err(RegexErr::EmptyRangeQuantifier);
        }

        let content = content.into_iter().collect::<String>();

        //TODO: test trim before and after
        if let Some((n_str, m_str)) = content.split_once(',') {
            let n = n_str.trim().parse::<u16>().map_err(|_| {
                RegexErr::RangeQuantifierInvalidNumber {
                    text: n_str.to_string(),
                }
            })?;
            if m_str.trim().is_empty() {
                Ok((n, None))
            } else {
                let m = m_str.trim().parse::<u16>().map_err(|_| {
                    RegexErr::RangeQuantifierInvalidNumber {
                        text: m_str.to_string(),
                    }
                })?;
                if n > m {
                    Err(RegexErr::RangeQuantifierMinExceedsMax { min: n, max: m })?
                }
                Ok((n, Some(m)))
            }
        } else {
            let n = content.trim().parse::<u16>().map_err(|_| {
                RegexErr::RangeQuantifierInvalidNumber {
                    text: content.to_string(),
                }
            })?;
            Ok((n, Some(n)))
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) enum PosixClass {
    Alnum,
    Alpha,
    Blank,
    Cntrl,
    Digit,
    Graph,
    Lower,
    Print,
    Punct,
    Space,
    Upper,
    Xdigit,
}

const POSIX_BLANK_CHARS: [char; 2] = [' ', '\t'];
const POSIX_SPACE_CHARS: [char; 6] = [' ', '\t', '\n', '\r', '\x0B', '\x0C'];
const POSIX_PUNCT_CHARS: &str = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";

impl TryFrom<&str> for PosixClass {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "alnum" => Ok(PosixClass::Alnum),
            "alpha" => Ok(PosixClass::Alpha),
            "blank" => Ok(PosixClass::Blank),
            "cntrl" => Ok(PosixClass::Cntrl),
            "digit" => Ok(PosixClass::Digit),
            "graph" => Ok(PosixClass::Graph),
            "lower" => Ok(PosixClass::Lower),
            "print" => Ok(PosixClass::Print),
            "punct" => Ok(PosixClass::Punct),
            "space" => Ok(PosixClass::Space),
            "upper" => Ok(PosixClass::Upper),
            "xdigit" => Ok(PosixClass::Xdigit),
            _ => Err(()),
        }
    }
}

impl PosixClass {
    pub(crate) fn for_each_char(&self, mut f: impl FnMut(char)) {
        match self {
            PosixClass::Alnum => {
                for c in 'a'..='z' {
                    f(c);
                }
                for c in 'A'..='Z' {
                    f(c);
                }
                for c in '0'..='9' {
                    f(c);
                }
            }
            PosixClass::Alpha => {
                for c in 'a'..='z' {
                    f(c);
                }
                for c in 'A'..='Z' {
                    f(c);
                }
            }
            PosixClass::Blank => {
                for c in POSIX_BLANK_CHARS {
                    f(c);
                }
            }
            PosixClass::Cntrl => {
                for c in 0u8..=31 {
                    f(c as char);
                }
                f(127 as char);
            }
            PosixClass::Digit => {
                for c in '0'..='9' {
                    f(c);
                }
            }
            PosixClass::Graph => {
                for c in '!'..='~' {
                    f(c);
                }
            }
            PosixClass::Lower => {
                for c in 'a'..='z' {
                    f(c);
                }
            }
            PosixClass::Print => {
                for c in ' '..='~' {
                    f(c);
                }
            }
            PosixClass::Punct => {
                for c in POSIX_PUNCT_CHARS.chars() {
                    f(c);
                }
            }
            PosixClass::Space => {
                for c in POSIX_SPACE_CHARS {
                    f(c);
                }
            }
            PosixClass::Upper => {
                for c in 'A'..='Z' {
                    f(c);
                }
            }
            PosixClass::Xdigit => {
                for c in '0'..='9' {
                    f(c);
                }
                for c in 'a'..='f' {
                    f(c);
                }
                for c in 'A'..='F' {
                    f(c);
                }
            }
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) enum RegexAstNode {
    Repeat {
        node: Box<RegexAstNode>,
        lower_bound: u16,
        upper_bound: Option<u16>,
    },
    BracketExpression {
        is_negated: bool,
        expr: Vec<BracketItem>,
    },
    Literal(char),
    ZeroOrOne(Box<RegexAstNode>),
    OneOrMore(Box<RegexAstNode>),
    Wildcard,
    Star(Box<RegexAstNode>),
    Alter(Box<RegexAstNode>, Box<RegexAstNode>),
    Concat(Box<RegexAstNode>, Box<RegexAstNode>),
    StartOfLineAnchor,
    EndOfLineAnchor,
    Empty,
}

impl RegexAstNode {
    // TODO: decide if I want to use self of &self. Probably I can drop reference cause the ast is
    // not used after building the nfa?

    pub(crate) fn new(tokens: Vec<Token>) -> Result<Self, RegexErr> {
        AstParser::parse(tokens)
    }

    //TODO: check all quantifiable tokens
    // TODO: all concat are quantifiable?
    fn is_quantifiable(&self) -> bool {
        !matches!(
            self,
            RegexAstNode::Star(_)
                | RegexAstNode::OneOrMore(_)
                | RegexAstNode::ZeroOrOne(_)
                | RegexAstNode::Repeat { .. }
                | RegexAstNode::StartOfLineAnchor
                | RegexAstNode::EndOfLineAnchor
        )
    }
}

impl TryFrom<Vec<Token>> for RegexAstNode {
    type Error = RegexErr;

    fn try_from(tokens: Vec<Token>) -> Result<Self, Self::Error> {
        AstParser::parse(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token::*;
    use lazy_static::lazy_static;
    use rstest::*;

    const SNAPSHOT_PATH: &str = "../tests/ast/";

    // Global setup for insta settings. In this way we don't need to add explicitly all settings in each test fn
    lazy_static! {
        static ref INSTA_SETTINGS: insta::Settings = {
            let mut set = insta::Settings::clone_current();
            set.set_snapshot_path(SNAPSHOT_PATH);
            set.set_prepend_module_to_snapshot(false);
            set
        };
    }

    fn to_pattern_string(tokens: &Vec<Token>) -> String {
        tokens.iter().map(|token| token.to_string()).collect()
    }

    fn snapshot_pattern_string(name: &str, tokens: &Vec<Token>) -> String {
        match name {
            "escaped_colon_dash_and_caret" => r"\^\:a\:-\-".to_string(),
            "escaped_special_chars_as_literals" => r"\^a\$".to_string(),
            "escaped_hyphen_in_middle" => r"[a\-b]".to_string(),
            "escaped_caret_at_start" => r"[\^ab]".to_string(),
            "escaped_closing_bracket" => r"[a\]]".to_string(),
            "escaped_opening_bracket" => r"[a\[]".to_string(),
            "escaped_backslash" => r"[a\\b]".to_string(),
            "unclosed_set_dangling_escape" => "[a\\\\".to_string(),
            _ => to_pattern_string(tokens),
        }
    }

    mod literals_and_concatenation {
        use super::*;

        #[test]
        fn single_literal() {
            // given
            let tokens = vec![Literal('a')]; // a
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn multiple_literals_are_a_concatenation() {
            // given
            let tokens = vec![Literal('a'), Literal('b'), Literal('3')]; // ab3
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn concatenation_of_all_atom_types() {
            // given
            let tokens = vec![
                Literal('a'),
                Dot,
                LParen,
                Literal('b'),
                RParen,
                LSquareBracket,
                Literal('c'),
                RSquareBracket,
                Dash,
                Colon,
            ]; // a.(b)[c]-:
            let pattern_string = to_pattern_string(&tokens);

            // when
            let ast_result = RegexAstNode::try_from(tokens);

            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[rstest]
        #[case("colon_as_literal_concat", vec![Literal('a'), Colon, Literal('b')])] // a:b
        #[case("dash_as_literal_concat", vec![Literal('a'), Dash, Literal('b')])] // a-b
        #[case("colon_with_quantifier", vec![Literal('a'), Colon, Literal('b'), Star])] // a:b*
        #[case("dash_with_alternation", vec![Literal('a'), Dash, Literal('b'), Pipe, Literal('c')])] // a-b|c
        #[case("colon_and_dash_in_group", vec![LParen, Literal('x'), Dash, Literal('y'), Colon, Literal('z'), RParen])] // (x-y:z)
        #[case("escaped_colon_dash_and_caret", vec![Literal('^'), Literal(':'), Literal('a'), Literal(':'), Dash, Literal('-')])] // \^\:a\:-
        fn bracket_special_chars_are_literals_outside_character_sets(
            #[case] name: &str,
            #[case] tokens: Vec<Token>,
        ) {
            let pattern_string = snapshot_pattern_string(name, &tokens);
            let ast_result = RegexAstNode::try_from(tokens);
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
        }

        #[rstest]
        #[case("colon_as_literal_concat", vec![Literal('a'), Colon, Literal('b')])] // a:b
        #[case("dash_as_literal_concat", vec![Literal('a'), Dash, Literal('b')])] // a-b
        #[case("escaped_special_chars_as_literals", vec![Literal('^'), Literal('a'), Literal('$')])] // \^a\$
        fn special_chars_are_literals(#[case] name: &str, #[case] tokens: Vec<Token>) {
            let pattern_string = snapshot_pattern_string(name, &tokens);
            let ast_result = RegexAstNode::try_from(tokens);
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
        }
    }

    mod wildcard {
        use super::*;

        #[test]
        fn dot_wildcard() {
            // given
            let tokens = vec![Dot]; // .
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[rstest]
        #[case("dot_works_with_all_quantifiers_case_1", vec![Dot, Star])] // .*
        #[case("dot_works_with_all_quantifiers_case_2", vec![Dot, Plus])] // .+
        #[case("dot_works_with_all_quantifiers_case_3", vec![Dot, QuestionMark])] // .?
        fn dot_works_with_all_quantifiers(#[case] name: &str, #[case] tokens: Vec<Token>) {
            // when
            let pattern_string = to_pattern_string(&tokens);
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
        }
    }

    mod alternation {
        use super::*;

        #[test]
        fn simple_alternation() {
            // given
            let tokens = vec![Literal('a'), Pipe, Literal('b')]; // a|b
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn alternation_with_concatenation() {
            // given
            let tokens = vec![Literal('a'), Pipe, Literal('b'), Literal('c')]; // a|bc
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn alternation_with_quantifier() {
            // given
            let tokens = vec![Literal('a'), Pipe, Literal('b'), Star]; // a|b*
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn starting_with_alter_fails() {
            //given
            let tokens = vec![Pipe, Literal('a')]; // |a
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn ending_with_alter_fails() {
            //given
            let tokens = vec![Literal('a'), Pipe]; // a|
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn double_alter_fails() {
            //given
            let tokens = vec![Literal('a'), Pipe, Pipe, Literal('b')]; // a||b
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }
    }

    mod groups_and_parentheses {
        use super::*;

        #[test]
        fn grouped_literals_with_quantifier() {
            // given
            let tokens = vec![LParen, Literal('1'), Literal('2'), RParen, Star]; // (12)*
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[rstest]
        #[case("group_works_with_all_quantifiers_case_1", vec![LParen, Literal('a'), RParen, Star])] // (a)*
        #[case("group_works_with_all_quantifiers_case_2", vec![LParen, Literal('a'), RParen, Plus])] // (a)+
        #[case("group_works_with_all_quantifiers_case_3", vec![LParen, Literal('a'), RParen, QuestionMark])] // (a)?
        fn group_works_with_all_quantifiers(#[case] name: &str, #[case] tokens: Vec<Token>) {
            //when
            let pattern_string = to_pattern_string(&tokens);
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
        }

        #[test]
        fn alternation_in_a_group_is_quantifiable() {
            // given
            let tokens = vec![LParen, Literal('a'), Pipe, Literal('b'), RParen, Star]; // (a|b)*
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn empty_group_is_valid() {
            // given
            let tokens = vec![LParen, RParen]; // ()
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn empty_group_with_quantifier_is_valid() {
            //given
            let tokens = vec![LParen, RParen, Star]; // ()*
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn nested_groups_are_valid() {
            // given
            let tokens = vec![
                // (a(b)*)
                LParen,
                Literal('a'),
                LParen,
                Literal('b'),
                RParen,
                Star,
                RParen,
            ];
            let pattern_string = to_pattern_string(&tokens);
            //when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn unclosed_parenthesis_fails() {
            //given
            let tokens = vec![Literal('a'), LParen, Literal('b')]; // a(b
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn unexpected_closing_parenthesis_fails() {
            //given
            let tokens = vec![Literal('a'), RParen]; // a)
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn starting_with_rparen_fails() {
            //given
            let tokens = vec![RParen, Literal('a')]; // )a
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }
    }

    mod quantifiers {
        use super::*;

        /// Tests for *?+
        mod standard_quantifiers {
            use super::*;

            #[test]
            fn literal_with_star_quantifier() {
                // given
                let tokens = vec![Literal('a'), Star]; // a*
                let pattern_string = to_pattern_string(&tokens);
                // when
                let ast_result = RegexAstNode::try_from(tokens);
                // then
                INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
            }

            #[test]
            fn literal_with_plus_quantifier() {
                // given
                let tokens = vec![Literal('b'), Plus]; // b+
                let pattern_string = to_pattern_string(&tokens);
                // when
                let ast_result = RegexAstNode::try_from(tokens);
                // then
                INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
            }

            #[test]
            fn literal_with_question_mark_quantifier() {
                // given
                let tokens = vec![Literal('c'), QuestionMark]; // c?
                let pattern_string = to_pattern_string(&tokens);
                // when
                let ast_result = RegexAstNode::try_from(tokens);
                // then
                INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
            }
        }

        /// Tests for bounded quantifiers like {n,m}
        mod bounded_quantifiers {
            use super::*;

            #[rstest]
            #[case("valid_bounded_exact", vec![Literal('a'), LCurlyBracket, Literal('3'), RCurlyBracket])] // a{3}
            #[case("valid_bounded_at_least", vec![Literal('a'), LCurlyBracket, Literal('5'),
            Literal(','), RCurlyBracket])] // a{5,}
            #[case("valid_bounded_range", vec![Literal('a'), LCurlyBracket, Literal('2'), Literal(','), Literal('4'), RCurlyBracket])] // a{2,4}
            #[case("valid_bounded_with_spaces", vec![Literal('a'), LCurlyBracket, Literal(' '), Literal('2'), Literal(' '), Literal(','), Literal(' '), Literal('4'), Literal(' '), RCurlyBracket])] // a{ 2 , 4 }
            fn valid_cases(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }

            #[rstest]
            #[case("invalid_bounded_min_greater_than_max", vec![Literal('a'), LCurlyBracket, Literal('5'), Literal(','), Literal('2'), RCurlyBracket])] // a{5,2}
            #[case("invalid_bounded_non_numeric", vec![Literal('a'), LCurlyBracket, Literal('b'), RCurlyBracket])] // a{b}
            #[case("invalid_bounded_unclosed", vec![Literal('a'), LCurlyBracket, Literal('3')])] // a{3
            #[case("invalid_starts_with_bounded", vec![LCurlyBracket, Literal('3'), RCurlyBracket])] // {3}
            fn invalid_cases(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }
        }

        mod invalid_placement {
            use super::*;

            #[test]
            fn only_quantifier_fails() {
                //given
                let tokens = vec![Star]; // *
                let pattern_string = to_pattern_string(&tokens);
                //when
                let ast_result = RegexAstNode::try_from(tokens);
                // then
                INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
            }

            #[test]
            fn dangling_plus_quantifier_fails() {
                //given
                let tokens = vec![Plus]; // +
                let pattern_string = to_pattern_string(&tokens);
                // when
                let ast_result = RegexAstNode::try_from(tokens);
                // then
                INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
            }

            #[test]
            fn quantifier_after_alternation_operator_fails() {
                //given
                let tokens = vec![Literal('a'), Pipe, Star]; // a|*
                let pattern_string = to_pattern_string(&tokens);
                // when
                let ast_result = RegexAstNode::try_from(tokens);
                // then
                INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
            }

            #[test]
            fn double_quantifier_fails() {
                //given
                let tokens = vec![Literal('a'), Star, Star]; // a**
                let pattern_string = to_pattern_string(&tokens);
                // when
                let ast_result = RegexAstNode::try_from(tokens);
                // then
                INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
            }

            #[test]
            fn quantifier_after_left_parenthesis_fails() {
                //given
                let tokens = vec![LParen, Star, Literal('1'), RParen]; // (*1)
                let pattern_string = to_pattern_string(&tokens);
                // when
                let ast_result = RegexAstNode::try_from(tokens);
                // then
                INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
            }

            #[rstest]
            #[case("starting_with_a_quantifier_fails_case_1", vec![Star, Literal('a')])] // *a
            #[case("starting_with_a_quantifier_fails_case_2", vec![Plus, Literal('a')])] // +a
            #[case("starting_with_a_quantifier_fails_case_3", vec![QuestionMark, Literal('a')])] // ?a
            fn starting_with_a_quantifier_fails(#[case] name: &str, #[case] tokens: Vec<Token>) {
                //when
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                // then
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }

            #[rstest]
            #[case("multiple_quantifiers_in_a_row_fails_case_1", vec![Literal('a'), Star, Plus])] // a*+
            #[case("multiple_quantifiers_in_a_row_fails_case_2", vec![Literal('a'), QuestionMark, Star])] // a?*
            #[case("multiple_quantifiers_in_a_row_fails_case_3", vec![Literal('a'), Plus, QuestionMark])] // a+?
            #[case("multiple_quantifiers_in_a_row_fails_case_4", vec![Literal('a'), Plus, LCurlyBracket, Literal('3'), RCurlyBracket])] // a+{3}
            #[case("multiple_quantifiers_in_a_row_fails_case_5", vec![Literal('a'), LCurlyBracket, Literal('3'), RCurlyBracket, Plus])] // a{3}+
            fn multiple_quantifiers_in_a_row_fails(#[case] name: &str, #[case] tokens: Vec<Token>) {
                // when
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                // then
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }
        }
    }

    #[cfg(test)]
    mod character_sets {
        use super::*;

        mod structure {
            use super::*;

            #[test]
            fn bracket_expression_keeps_literal_items() {
                let ast = RegexAstNode::try_from(vec![
                    LSquareBracket,
                    Literal('a'),
                    Literal('b'),
                    RSquareBracket,
                ])
                .unwrap();

                assert_eq!(
                    ast,
                    RegexAstNode::BracketExpression {
                        is_negated: false,
                        expr: vec![BracketItem::Literal('a'), BracketItem::Literal('b')],
                    }
                );
            }

            #[test]
            fn bracket_expression_keeps_range_items() {
                let ast = RegexAstNode::try_from(vec![
                    LSquareBracket,
                    Literal('a'),
                    Dash,
                    Literal('z'),
                    RSquareBracket,
                ])
                .unwrap();

                assert_eq!(
                    ast,
                    RegexAstNode::BracketExpression {
                        is_negated: false,
                        expr: vec![BracketItem::Range {
                            start: 'a',
                            end: 'z',
                        }],
                    }
                );
            }

            #[test]
            fn bracket_expression_keeps_posix_class_items() {
                let ast = RegexAstNode::try_from(vec![
                    LSquareBracket,
                    LSquareBracket,
                    Colon,
                    Literal('d'),
                    Literal('i'),
                    Literal('g'),
                    Literal('i'),
                    Literal('t'),
                    Colon,
                    RSquareBracket,
                    RSquareBracket,
                ])
                .unwrap();

                assert_eq!(
                    ast,
                    RegexAstNode::BracketExpression {
                        is_negated: false,
                        expr: vec![BracketItem::PosixClass(PosixClass::Digit)],
                    }
                );
            }
        }

        mod valid_syntax {
            use super::*;

            #[rstest]
            // --- Basic Sets and Ranges ---
            #[case("set_simple_literals", vec![LSquareBracket, Literal('a'), Literal('b'), Literal('c'), RSquareBracket])] // [abc]
            #[case("set_numeric_range", vec![LSquareBracket, Literal('0'), Dash, Literal('9'), RSquareBracket])] // [0-9]
            #[case("set_lowercase_range", vec![LSquareBracket, Literal('a'), Dash, Literal('z'), RSquareBracket])] // [a-z]
            #[case("set_uppercase_range", vec![LSquareBracket, Literal('A'), Dash, Literal('Z'), RSquareBracket])] // [A-Z]
            #[case("set_mixed_ranges_and_literals", vec![LSquareBracket, Literal('A'), Dash, Literal('F'), Literal('x'), Literal('0'), Dash, Literal('9'), RSquareBracket])] // [A-Fx0-9]
            #[case("set_single_char_range", vec![LSquareBracket, Literal('c'), Dash, Literal('c'), RSquareBracket])] // [c-c]
            #[case("valid_range_upper_to_lower", vec![LSquareBracket, Literal('Z'), Dash, Literal('a'), RSquareBracket])] // [Z-a]
            #[case("valid_range_digit_to_alpha", vec![LSquareBracket, Literal('5'), Dash, Literal('A'), RSquareBracket])] // [5-A]
            #[case("two_brackets_with_alteration", vec![LSquareBracket, Literal('Z'), Dash, Literal('a'), RSquareBracket,
                Pipe, LSquareBracket, Literal('c'), Dash, Literal('f'), RSquareBracket])] // [Z-a]|[c-f]

            // --- Negated Sets ---
            #[case("negated_set_simple_literals", vec![LSquareBracket, Caret, Literal('a'), Literal('b'), Literal('c'), RSquareBracket])] // [^abc]
            #[case("negated_set_range", vec![LSquareBracket,Caret, Literal('a'), Dash, Literal('z'), RSquareBracket])] // [^a-z]

            // --- Special Character Handling (as Literals) ---
            #[case("literal_hyphen_at_start", vec![LSquareBracket, Dash, Literal('a'), Literal('b'), RSquareBracket])] // [-ab]
            #[case("literal_hyphen_at_end", vec![LSquareBracket, Literal('a'), Literal('b'), Dash, RSquareBracket])] // [ab-]
            #[case("literal_hyphen_after_negation", vec![LSquareBracket, Caret, Dash, Literal('a'), Literal('b'), RSquareBracket])] // [^-ab]
            #[case("literal_caret_not_at_start", vec![LSquareBracket, Literal('a'), Literal('^'), Literal('b'), RSquareBracket])] // [a^b]
            #[case("literal_closing_bracket_at_start", vec![LSquareBracket, RSquareBracket, Literal('a'), RSquareBracket])] // []a]
            #[case("literal_closing_bracket_after_negation", vec![LSquareBracket, Caret, RSquareBracket, Literal('a'), RSquareBracket])] // [^]a]

            // --- Other Metacharacters Inside a Set ---
            #[case("metacharacters_as_literals", vec![LSquareBracket, Star, Plus, QuestionMark, Dot, LParen, RParen, LCurlyBracket, RCurlyBracket, Pipe, RSquareBracket])] // [*+?.(){}|]

            // --- Escaped Characters Inside a Set ---
            #[case("escaped_hyphen_in_middle", vec![LSquareBracket, Literal('a'), Literal('-'), Literal('b'), RSquareBracket])] // [a\-b]
            #[case("escaped_caret_at_start", vec![LSquareBracket, Literal('^'), Literal('a'), Literal('b'), RSquareBracket])] // [\^ab]
            #[case("escaped_closing_bracket", vec![LSquareBracket, Literal('a'), Literal(']'), RSquareBracket])] // [a\]]
            #[case("escaped_opening_bracket", vec![LSquareBracket, Literal('a'), Literal('['), RSquareBracket])] // [a\[]
            #[case("escaped_backslash", vec![LSquareBracket, Literal('a'), Literal('\\'), Literal('b'), RSquareBracket])] // [a\\b]

            // --- Quantifiable Sets ---
            #[case("set_with_quantifier", vec![LSquareBracket, Literal('a'), Literal('b'), RSquareBracket, Plus])] // [ab]+
            #[case("negated_set_with_quantifier", vec![LSquareBracket, Caret, Literal('a'), RSquareBracket, Star])] // [^a]*
            #[case("range_with_bounded_quantifier", vec![LSquareBracket, Literal('0'), Dash, Literal('9'), RSquareBracket, LCurlyBracket, Literal('2'), Literal(','), Literal('4'), RCurlyBracket])] // [0-9]{2,4}
            #[case("range_in_parent_with_alter_and_start", vec![LParen, LSquareBracket, Literal('Z'), Dash, Literal('a'), RSquareBracket, Pipe, Literal('2'), RParen, Star])] // ([Z-a]|2)*
            fn test_valid_sets(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = snapshot_pattern_string(name, &tokens);
                let ast_result = RegexAstNode::try_from(tokens);

                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }
        }

        mod invalid_syntax {
            use super::*;

            #[rstest]
            #[case("unclosed_set", vec![LSquareBracket, Literal('a'), Literal('b')])] // [ab
            #[case("empty_set", vec![LSquareBracket, RSquareBracket])] // []
            #[case("empty_negated_set", vec![LSquareBracket, Caret, RSquareBracket])] // [^]
            #[case("unclosed_negated_set", vec![LSquareBracket, Literal('^'), Literal('a')])] // [^a
            #[case("unclosed_set_with_range", vec![LSquareBracket, Literal('a'), Dash])] // [a-
            #[case("unclosed_set_dangling_escape", vec![LSquareBracket, Literal('a'), Literal('\\')])] // [a\
            #[case("unclosed_bracket_in_parent", vec![LParen, LSquareBracket, Literal('Z'), Dash, Literal('a')])] // ([Z-a
            #[case("unclosed_bracket_in_parent_2", vec![LParen, LSquareBracket, Literal('Z'), Dash, Literal('a'), RSquareBracket,
                Pipe, LSquareBracket, Literal('Z'), Dash, Literal('a')])] // ([Z-a]|[Z-a
            fn unclosed_sets_fail(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = snapshot_pattern_string(name, &tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }

            #[rstest]
            #[case("invalid_range_desc_alpha", vec![LSquareBracket, Literal('z'), Dash, Literal('a'), RSquareBracket])] // [z-a]
            #[case("invalid_range_lower_to_upper", vec![LSquareBracket, Literal('f'), Dash, Literal('A'), RSquareBracket])] // [f-A]
            #[case("invalid_range_alpha_to_digit", vec![LSquareBracket, Literal('z'), Dash, Literal('0'), RSquareBracket])] // [z-0]
            #[case("invalid_range_desc_numeric", vec![LSquareBracket, Literal('9'), Dash, Literal('0'), RSquareBracket])] // [9-0]
            fn invalid_ranges_fail(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }
        }
    }

    mod posix_character_classes {
        use super::*;

        mod valid_syntax {
            use super::*;

            #[rstest]
            #[case("posix_alnum", vec![LSquareBracket, LSquareBracket, Colon, Literal('a'), Literal('l'), Literal('n'), Literal('u'), Literal('m'),Colon, RSquareBracket, RSquareBracket])] // [[:alnum:]]
            #[case("posix_alpha", vec![LSquareBracket, LSquareBracket, Colon, Literal('a'), Literal('l'), Literal('p'), Literal('h'), Literal('a'), Colon, RSquareBracket, RSquareBracket])] // [[:alpha:]]
            #[case("posix_blank", vec![LSquareBracket, LSquareBracket, Colon, Literal('b'), Literal('l'), Literal('a'), Literal('n'), Literal('k'), Colon, RSquareBracket, RSquareBracket])] // [[:blank:]]
            #[case("posix_cntrl", vec![LSquareBracket, LSquareBracket, Colon, Literal('c'), Literal('n'), Literal('t'), Literal('r'), Literal('l'), Colon, RSquareBracket, RSquareBracket])] // [[:cntrl:]]
            #[case("posix_digit", vec![LSquareBracket, LSquareBracket, Colon, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), Colon, RSquareBracket, RSquareBracket])] // [[:digit:]]
            #[case("posix_graph", vec![LSquareBracket, LSquareBracket, Colon, Literal('g'), Literal('r'), Literal('a'), Literal('p'), Literal('h'), Colon, RSquareBracket, RSquareBracket])] // [[:graph:]]
            #[case("posix_lower", vec![LSquareBracket, LSquareBracket, Colon, Literal('l'), Literal('o'), Literal('w'), Literal('e'), Literal('r'), Colon, RSquareBracket, RSquareBracket])] // [[:lower:]]
            #[case("posix_print", vec![LSquareBracket, LSquareBracket, Colon, Literal('p'), Literal('r'), Literal('i'), Literal('n'), Literal('t'), Colon, RSquareBracket, RSquareBracket])] // [[:print:]]
            #[case("posix_punct", vec![LSquareBracket, LSquareBracket, Colon, Literal('p'), Literal('u'), Literal('n'), Literal('c'), Literal('t'), Colon, RSquareBracket, RSquareBracket])] // [[:punct:]]
            #[case("posix_space", vec![LSquareBracket, LSquareBracket, Colon, Literal('s'), Literal('p'), Literal('a'), Literal('c'), Literal('e'), Colon, RSquareBracket, RSquareBracket])] // [[:space:]]
            #[case("posix_upper", vec![LSquareBracket, LSquareBracket, Colon, Literal('u'), Literal('p'), Literal('p'), Literal('e'), Literal('r'), Colon, RSquareBracket, RSquareBracket])] // [[:upper:]]
            #[case("posix_xdigit", vec![LSquareBracket, LSquareBracket, Colon, Literal('x'), Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), Colon, RSquareBracket, RSquareBracket])] // [[:xdigit:]]
            fn test_standalone_posix_classes(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }

            #[rstest]
            #[case("posix_with_literals", vec![LSquareBracket, Literal('a'), Literal('b'), LSquareBracket, Colon, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), Colon, RSquareBracket, RSquareBracket])] // [ab[:digit:]]
            #[case("posix_with_range", vec![LSquareBracket, Literal('a'), Dash, Literal('f'), LSquareBracket, Colon, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), Colon, RSquareBracket, RSquareBracket])] // [a-f[:digit:]]
            #[case("multiple_posix_classes", vec![LSquareBracket, LSquareBracket, Colon, Literal('a'), Literal('l'), Literal('p'), Literal('h'), Literal('a'), Colon, RSquareBracket, LSquareBracket, Colon, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), Colon, RSquareBracket, RSquareBracket])] // [[:alpha:][:digit:]]
            #[case("negated_posix_class", vec![LSquareBracket, Caret, LSquareBracket, Colon, Literal('s'), Literal('p'), Literal('a'), Literal('c'), Literal('e'), Colon, RSquareBracket, RSquareBracket])] // [^[:space:]]
            #[case("negated_posix_with_literals", vec![LSquareBracket, Caret, Literal('a'), LSquareBracket, Colon, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), Colon, RSquareBracket, RSquareBracket])] // [^a[:digit:]]
            fn test_combined_posix_classes(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }

            #[rstest]
            #[case("quantified_posix", vec![LSquareBracket, LSquareBracket, Colon, Literal('a'), Literal('l'), Literal('p'), Literal('h'), Literal('a'), Colon, RSquareBracket, RSquareBracket, Plus])] // [[:alpha:]]+
            #[case("quantified_posix_with_literals", vec![LSquareBracket, Literal('X'), LSquareBracket, Colon, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), Colon, RSquareBracket, RSquareBracket, Star])] // [X[:digit:]]*
            fn test_quantified_posix_sets(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }
        }

        mod invalid_syntax {
            use super::*;

            #[rstest]
            #[case("misspelled_class_name", vec![LSquareBracket, LSquareBracket, Colon, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('s'), Colon, RSquareBracket, RSquareBracket])] // [[:digis:]]
            #[case("not_a_class_name", vec![LSquareBracket, LSquareBracket, Colon, Literal('f',), Literal('o'), Literal('o'), Colon, RSquareBracket, RSquareBracket])] // [[:foo:]]
            #[case("missing_closing_colon", vec![LSquareBracket, LSquareBracket, Colon, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), RSquareBracket, RSquareBracket])] // [[:digit]]
            #[case("missing_opening_colon", vec![LSquareBracket, LSquareBracket, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), Colon, RSquareBracket, RSquareBracket])] // [[digit:]]
            #[case("missing_both_colons", vec![LSquareBracket, LSquareBracket, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), RSquareBracket, RSquareBracket])] // [[digit]]
            #[case("empty_class_name", vec![LSquareBracket, LSquareBracket, Colon, Colon, RSquareBracket, RSquareBracket])] // [[:]]
            #[case("unclosed_posix_in_set", vec![LSquareBracket, Literal('a'), LSquareBracket, Colon, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), Colon, RSquareBracket])] // [a[:digit:]
            fn malformed_posix_classes_fail(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }

            #[rstest]
            #[case("range_from_char_to_posix", vec![LSquareBracket, Literal('a'), Dash, LSquareBracket, Colon, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), Colon, RSquareBracket, RSquareBracket])] // [a-[:digit:]]
            #[case("range_from_posix_to_char", vec![LSquareBracket, LSquareBracket, Colon, Literal('d'), Literal('i'), Literal('g'), Literal('i'), Literal('t'), Colon, RSquareBracket, Dash, Literal('z'), RSquareBracket])] // [[:digit:]-z]
            fn posix_class_in_a_range_fails(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }
        }
    }

    mod anchors {
        use super::*;

        #[rstest]
        #[case("anchor_at_start", vec![Caret, Literal('a')])] // ^a
        #[case("anchor_at_end", vec![Literal('a'), Dollar])] // a$
        #[case("anchors_at_both_ends", vec![Caret, Literal('a'), Dollar])] // ^a$
        #[case("anchor_in_middle_of_concatenation", vec![Literal('a'), Caret, Literal('b')])] // a^b
        #[case("anchor_in_alternation", vec![Literal('a'), Pipe, Caret, Literal('b')])] // a|^b
        #[case("alternation_of_anchors", vec![Caret, Pipe, Dollar])] // ^|$
        #[case("anchor_at_start_of_group", vec![LParen, Caret, Literal('a'), RParen])] // (^a)
        #[case("anchor_at_end_of_group", vec![LParen, Literal('a'), Dollar, RParen])] // (a$)
        #[case("not_valid_quantified_start_anchor", vec![Caret, Star])] // ^*
        #[case("not_valid_quantified_end_anchor", vec![Literal('a'), Dollar, Plus])] // a$+
        fn test_anchor_parsing(#[case] name: &str, #[case] tokens: Vec<Token>) {
            let pattern_string = to_pattern_string(&tokens);
            let ast_result = RegexAstNode::try_from(tokens);
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
        }
    }
}
