#[cfg(test)]
use serde::Serialize;

use crate::nfa::Nfa;
use crate::token::Token;
use crate::TokenSequence;
use std::collections::HashSet;
use std::iter::{Enumerate, Peekable};
use std::vec::IntoIter;

struct AstParser {
    seq: TokenSequence,
}

impl AstParser {
    pub fn parse(mut seq: TokenSequence) -> Result<RegexAstNode, SyntaxError> {
        if seq.peek().is_none() {
            return Ok(RegexAstNode::Empty);
        }

        let mut parser = Self { seq };

        let parsed_ast = parser.parse_alternation()?;

        if let Some((pos, token_left)) = parser.seq.next_enumerated() {
            Err(SyntaxError::UnexpectedToken(format!(
                "Unexpected token '{}' at position {}",
                token_left, pos
            )))
        } else {
            Ok(parsed_ast)
        }
    }

    fn parse_alternation(&mut self) -> Result<RegexAstNode, SyntaxError> {
        let mut left_node = self.parse_concatenation()?;

        while let Some(Token::Alter) = self.seq.peek() {
            self.seq.next();

            let right_node = self.parse_concatenation()?;
            left_node = RegexAstNode::Alter(Box::new(left_node), Box::new(right_node));
        }

        Ok(left_node)
    }

    fn parse_concatenation(&mut self) -> Result<RegexAstNode, SyntaxError> {
        let mut left_node = self.parse_quantifier()?;

        while self
            .seq
            .peek()
            .map_or(false, |t| matches!(t, Token::Literal(_) | Token::LParen))
        {
            let right_node = self.parse_quantifier()?;
            left_node = RegexAstNode::Concat(Box::new(left_node), Box::new(right_node));
        }

        Ok(left_node)
    }

    fn map_quantifier(&mut self, node: RegexAstNode) -> Result<RegexAstNode, SyntaxError> {
        match self.seq.next() {
            Some(Token::Star) => Ok(RegexAstNode::Star(Box::new(node))),
            Some(Token::Plus) => Ok(RegexAstNode::OneOrMore(Box::new(node))),
            Some(Token::QuestionMark) => Ok(RegexAstNode::ZeroOrOne(Box::new(node))),
            Some(Token::LCurlyBracket) => {
                let mut content = Vec::new();
                while let Some(token) = self.seq.peek() {
                    if !token.is_literal() {
                        break;
                    }
                    content.push(char::from(self.seq.next().unwrap()));
                }
                if self.seq.peek() != Some(&Token::RCurlyBracket) {
                    return if let Some((idx, _)) = self.seq.next_enumerated() {
                        Err(SyntaxError::ExpectedClosingCurlyBracket(idx))
                    } else {
                        Err(SyntaxError::UnexpectedEndOfExpression)
                    };
                }
                self.seq.next();
                let (n, m) = Self::parse_bounded_quantifier(content)?;
                Ok(RegexAstNode::Repeat {
                    node: Box::new(node),
                    lower_bound: n,
                    upper_bound: m,
                })
            }
            _ => Err(SyntaxError::UnexpectedLiteral), //TODO: fix
        }
    }

    fn parse_quantifier(&mut self) -> Result<RegexAstNode, SyntaxError> {
        let mut node = self.parse_literal_or_group()?;

        if let Some(next_token) = self.seq.peek() {
            if next_token.is_quantifier() {
                if !node.is_quantifiable() {
                    return Err(SyntaxError::PrecedentTokenIsNotQuantifiable);
                }
                node = self.map_quantifier(node)?;
            }
        }
        Ok(node)
    }

    fn parse_literal_or_group(&mut self) -> Result<RegexAstNode, SyntaxError> {
        match self.seq.next() {
            Some(Token::Literal(c)) => Ok(RegexAstNode::Literal(c)),
            Some(Token::Dot) => Ok(RegexAstNode::Wildcard),
            Some(Token::RSquareBracket) => self.parse_square_bracket_expr(),
            Some(Token::LParen) => {
                if let Some(Token::RParen) = self.seq.peek() {
                    self.seq.next();
                    return Ok(RegexAstNode::Empty);
                }
                let nested_expr = self.parse_alternation()?;
                let cur_pos = self.seq.cur_pos();
                match self.seq.next() {
                    Some(Token::RParen) => Ok(nested_expr),
                    Some(token) => Err(SyntaxError::UnexpectedToken(format!(
                        "Expected '{}', at position {}, got '{}'",
                        Token::RParen,
                        cur_pos,
                        token
                    ))),
                    _ => Err(SyntaxError::ExpectedClosingParenthesis),
                }
            }
            Some(Token::Star) | Some(Token::Plus) | Some(Token::QuestionMark) => {
                Err(SyntaxError::PrecedentTokenIsNotQuantifiable)
            }

            _ => Err(SyntaxError::UnexpectedLiteral),
        }
    }

    fn parse_square_bracket_expr(&mut self) -> Result<RegexAstNode, SyntaxError> {
        let mut expr = HashSet::new();
        let mut is_negated = false;

        if self.seq.peek() == Some(&Token::RSquareBracket) {
            // handle POSIX classes
        } else if self.seq.peek() == Some(&Token::Literal('^')) {
            is_negated = true;
            self.seq.next();
        }

        while let Some(token) = self.seq.peek() {
            if *token == Token::RSquareBracket {
                break;
            }

            let start_token = self.seq.next().unwrap();

            if self.seq.peek() == Some(&Token::Literal('-')) {
                self.seq.next();

                if let Some(end_token) = self.seq.peek().cloned() {
                    if end_token != Token::RSquareBracket {
                        self.seq.next();

                        let start_char = char::from(&start_token);
                        let end_char = char::from(&end_token);

                        if start_char > end_char {
                            return Err(SyntaxError::InvalidRangeInCharacterSet);
                        }

                        for c_val in start_char as u32..=end_char as u32 {
                            expr.insert(std::char::from_u32(c_val).unwrap());
                        }
                        continue;
                    }
                }

                expr.insert(char::from(&start_token));
                expr.insert('-');
            } else {
                expr.insert(char::from(&start_token));
            }
        }

        let next = self.seq.next();
        if next != Some(Token::RSquareBracket) {
            Err(SyntaxError::UnexpectedEndOfExpression)?
        }

        Ok(RegexAstNode::BracketExpression { is_negated, expr })
    }

    fn parse_bounded_quantifier(content: Vec<char>) -> Result<(u16, Option<u16>), SyntaxError> {
        if content.is_empty() {
            return Err(SyntaxError::QuantifierInvalidFormat);
        }

        let content = content.into_iter().collect::<String>();

        //TODO: test trim before and after
        if let Some((n_str, m_str)) = content.split_once(',') {
            let n = n_str
                .trim()
                .parse::<u16>()
                .map_err(|_| SyntaxError::QuantifierInvalidNumber(n_str.to_string()))?;
            if m_str.trim().is_empty() {
                Ok((n, None))
            } else {
                let m = m_str
                    .trim()
                    .parse::<u16>()
                    .map_err(|_| SyntaxError::QuantifierInvalidNumber(m_str.to_string()))?;
                if n > m {
                    Err(SyntaxError::QuantifierMinGreaterMax)?
                }
                Ok((n, Some(m)))
            }
        } else {
            let n = content
                .trim()
                .parse::<u16>()
                .map_err(|_| SyntaxError::QuantifierInvalidNumber(content.to_string()))?;
            Ok((n, Some(n)))
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
        expr: HashSet<char>,
    },
    Literal(char),
    ZeroOrOne(Box<RegexAstNode>),
    OneOrMore(Box<RegexAstNode>),
    Wildcard,
    Star(Box<RegexAstNode>),
    Alter(Box<RegexAstNode>, Box<RegexAstNode>),
    Concat(Box<RegexAstNode>, Box<RegexAstNode>),
    Empty,
}

impl RegexAstNode {
    pub(crate) fn to_nfa(&self) -> Nfa {
        match self {
            RegexAstNode::Literal(c) => Nfa::from_char(*c),
            RegexAstNode::Wildcard => Nfa::from_wildcard(),
            RegexAstNode::Concat(left, right) => {
                let mut left_nfa = left.to_nfa();
                let right_nfa = right.to_nfa();

                left_nfa.concatenate(&right_nfa);
                left_nfa
            }
            RegexAstNode::Alter(left, right) => {
                let mut left_nfa = left.to_nfa();
                let right_nfa = right.to_nfa();

                left_nfa.alternate(&right_nfa);
                left_nfa
            }
            RegexAstNode::Star(regex_ast_node) => {
                let mut nfa_child = regex_ast_node.to_nfa();

                nfa_child.kleene_star();
                nfa_child
            }
            RegexAstNode::Empty => Nfa::from_epsilon(),
            RegexAstNode::ZeroOrOne(_) => todo!(),
            RegexAstNode::OneOrMore(_) => todo!(),
            RegexAstNode::Repeat {
                node,
                lower_bound,
                upper_bound,
            } => Nfa::from_range(&node.to_nfa(), *lower_bound, *upper_bound),
            RegexAstNode::BracketExpression { is_negated, expr } => todo!(),
        }
    }

    // TODO: err == string?
    pub(crate) fn new(pattern: &str) -> Result<Self, String> {
        let sequence = TokenSequence::try_from(pattern).unwrap(); //TODO: map err
                                                                  //TODO: display?
        AstParser::parse(sequence).map_err(|e| format!("{e:?}"))
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
        )
    }
}

impl TryFrom<Vec<Token>> for RegexAstNode {
    type Error = SyntaxError;

    fn try_from(tokens: Vec<Token>) -> Result<Self, Self::Error> {
        if tokens.is_empty() {
            return Ok(RegexAstNode::Empty);
        }
        AstParser::parse(TokenSequence::new(tokens))
    }
}

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) enum SyntaxError {
    UnexpectedToken(String),
    ExpectedClosingParenthesis,
    ExpectedClosingCurlyBracket(usize),
    PrecedentTokenIsNotQuantifiable,
    UnexpectedLiteral,
    UnexpectedEndOfExpression,
    QuantifierInvalidFormat,
    QuantifierInvalidNumber(String),
    QuantifierMinGreaterMax,
    InvalidRangeInCharacterSet,
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
            let tokens = vec![Literal('a'), Alter, Literal('b')]; // a|b
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn alternation_with_concatenation() {
            // given
            let tokens = vec![Literal('a'), Alter, Literal('b'), Literal('c')]; // a|bc
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn alternation_with_quantifier() {
            // given
            let tokens = vec![Literal('a'), Alter, Literal('b'), Star]; // a|b*
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn starting_with_alter_fails() {
            //given
            let tokens = vec![Alter, Literal('a')]; // |a
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn ending_with_alter_fails() {
            //given
            let tokens = vec![Literal('a'), Alter]; // a|
            let pattern_string = to_pattern_string(&tokens);
            // when
            let ast_result = RegexAstNode::try_from(tokens);
            // then
            INSTA_SETTINGS.bind(|| insta::assert_yaml_snapshot!((pattern_string, ast_result)));
        }

        #[test]
        fn double_alter_fails() {
            //given
            let tokens = vec![Literal('a'), Alter, Alter, Literal('b')]; // a||b
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
            let tokens = vec![LParen, Literal('a'), Alter, Literal('b'), RParen, Star]; // (a|b)*
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
            #[case("valid_bounded_at_least", vec![Literal('a'), LCurlyBracket, Literal('5'), Literal(','), RCurlyBracket])] // a{5,}
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
                let tokens = vec![Literal('a'), Alter, Star]; // a|*
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

    /*
    #[cfg(test)]
    mod character_sets {
        use super::*;

        mod valid_syntax {
            use super::*;

            #[rstest]
            // --- Basic Sets and Ranges ---
            #[case("set_simple_literals", vec![LSquareBracket, Literal('a'), Literal('b'), Literal('c'), RSquareBracket])] // [abc]
            #[case("set_numeric_range", vec![LSquareBracket, Literal('0'), Literal('-'), Literal('9'), RSquareBracket])] // [0-9]
            #[case("set_lowercase_range", vec![LSquareBracket, Literal('a'), Literal('-'), Literal('z'), RSquareBracket])] // [a-z]
            #[case("set_uppercase_range", vec![LSquareBracket, Literal('A'), Literal('-'), Literal('Z'), RSquareBracket])] // [A-Z]
            #[case("set_mixed_ranges_and_literals", vec![LSquareBracket, Literal('A'), Literal('-'), Literal('F'), Literal('x'), Literal('0'), Literal('-'), Literal('9'), RSquareBracket])] // [A-Fx0-9]
            #[case("set_single_char_range", vec![LSquareBracket, Literal('c'), Literal('-'), Literal('c'), RSquareBracket])] // [c-c]

            // --- Negated Sets ---
            #[case("negated_set_simple_literals", vec![LSquareBracket, Literal('^'), Literal('a'), Literal('b'), Literal('c'), RSquareBracket])] // [^abc]
            #[case("negated_set_range", vec![LSquareBracket, Literal('^'), Literal('a'), Literal('-'), Literal('z'), RSquareBracket])] // [^a-z]

            // --- Special Character Handling (as Literals) ---
            #[case("literal_hyphen_at_start", vec![LSquareBracket, Literal('-'), Literal('a'), Literal('b'), RSquareBracket])] // [-ab]
            #[case("literal_hyphen_at_end", vec![LSquareBracket, Literal('a'), Literal('b'), Literal('-'), RSquareBracket])] // [ab-]
            #[case("literal_hyphen_after_negation", vec![LSquareBracket, Literal('^'), Literal('-'), Literal('a'), Literal('b'), RSquareBracket])] // [^-ab]
            #[case("literal_caret_not_at_start", vec![LSquareBracket, Literal('a'), Literal('^'), Literal('b'), RSquareBracket])] // [a^b]
            #[case("literal_closing_bracket_at_start", vec![LSquareBracket, RSquareBracket, Literal('a'), RSquareBracket])] // []a]
            #[case("literal_closing_bracket_after_negation", vec![LSquareBracket, Literal('^'), RSquareBracket, Literal('a'), RSquareBracket])] // [^]a]

            // --- Other Metacharacters Inside a Set ---
            #[case("metacharacters_as_literals", vec![LSquareBracket, Star, Plus, QuestionMark, Dot, LParen, RParen, LCurlyBracket, RCurlyBracket, Alter, RSquareBracket])] // [*+?.(){}|]

            // --- Escaped Characters Inside a Set ---
            #[case("escaped_hyphen_in_middle", vec![LSquareBracket, Literal('a'), Literal('\\'), Literal('-'), Literal('b'), RSquareBracket])] // [a\-b]
            #[case("escaped_caret_at_start", vec![LSquareBracket, Literal('\\'), Literal('^'), Literal('a'), Literal('b'), RSquareBracket])] // [\^ab]
            #[case("escaped_closing_bracket", vec![LSquareBracket, Literal('a'), Literal('\\'), RSquareBracket, RSquareBracket])] // [a\]]
            #[case("escaped_opening_bracket", vec![LSquareBracket, Literal('a'), Literal('\\'), LSquareBracket, RSquareBracket])] // [a\[]
            #[case("escaped_backslash", vec![LSquareBracket, Literal('a'), Literal('\\'), Literal('\\'), Literal('b'), RSquareBracket])] // [a\\b]

            // --- Quantifiable Sets ---
            #[case("set_with_quantifier", vec![LSquareBracket, Literal('a'), Literal('b'), RSquareBracket, Plus])] // [ab]+
            #[case("negated_set_with_quantifier", vec![LSquareBracket, Literal('^'), Literal('a'), RSquareBracket, Star])] // [^a]*
            #[case("range_with_bounded_quantifier", vec![LSquareBracket, Literal('0'), Literal('-'), Literal('9'), RSquareBracket, LCurlyBracket, Literal('2'), Literal(','), Literal('4'), RCurlyBracket])] // [0-9]{2,4}
            fn test_valid_sets(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);

                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }
        }

        /// Groups tests for all invalid or malformed character set syntax.
        mod invalid_syntax {
            use super::*;

            #[rstest]
            #[case("unclosed_set", vec![LSquareBracket, Literal('a'), Literal('b')])] // [ab
            #[case("unclosed_negated_set", vec![LSquareBracket, Literal('^'), Literal('a')])] // [^a
            #[case("unclosed_set_with_range", vec![LSquareBracket, Literal('a'), Literal('-')])] // [a-
            #[case("unclosed_set_dangling_escape", vec![LSquareBracket, Literal('a'), Literal('\\')])] // [a\
            fn unclosed_sets_fail(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }

            #[rstest]
            // Your requested cases
            #[case("invalid_range_desc_alpha", vec![LSquareBracket, Literal('z'), Literal('-'), Literal('a'), RSquareBracket])] // [z-a]
            #[case("invalid_range_lower_to_upper", vec![LSquareBracket, Literal('f'), Literal('-'), Literal('A'), RSquareBracket])] // [f-A]
            #[case("invalid_range_alpha_to_digit", vec![LSquareBracket, Literal('z'), Literal('-'), Literal('0'), RSquareBracket])] // [z-0]
            // Additional robust cases
            #[case("invalid_range_desc_numeric", vec![LSquareBracket, Literal('9'), Literal('-'), Literal('0'), RSquareBracket])] // [9-0]
            #[case("invalid_range_upper_to_lower", vec![LSquareBracket, Literal('Z'), Literal('-'), Literal('a'), RSquareBracket])] // [Z-a]
            #[case("invalid_range_digit_to_alpha", vec![LSquareBracket, Literal('5'), Literal('-'), Literal('A'), RSquareBracket])] // [5-A]
            fn invalid_ranges_fail(#[case] name: &str, #[case] tokens: Vec<Token>) {
                let pattern_string = to_pattern_string(&tokens);
                let ast_result = RegexAstNode::try_from(tokens);
                INSTA_SETTINGS
                    .bind(|| insta::assert_yaml_snapshot!(name, (pattern_string, ast_result)));
            }
        }
    }
    */
}
