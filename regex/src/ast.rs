#[cfg(test)]
use serde::Serialize;

use crate::nfa::Nfa;
use crate::token::Token;
use crate::TokenSequence;
use std::iter::{Enumerate, Peekable};
use std::slice::Iter;

struct AstParser<'a> {
    tokens: Peekable<Enumerate<Iter<'a, Token>>>,
}

impl<'a> AstParser<'a> {
    pub fn parse(tokens: &'a [Token]) -> Result<RegexAstNode, SyntaxError> {
        let mut parser = Self {
            tokens: tokens.iter().enumerate().peekable(),
        };

        if tokens.is_empty() {
            return Ok(RegexAstNode::Empty);
        }

        let parsed_ast = parser.parse_alternation()?;

        if let Some((pos, token_left)) = parser.tokens.peek() {
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

        while let Some((_, Token::Alter)) = self.tokens.peek() {
            self.tokens.next();

            let right_node = self.parse_concatenation()?;
            left_node = RegexAstNode::Alter(Box::new(left_node), Box::new(right_node));
        }

        Ok(left_node)
    }

    fn parse_concatenation(&mut self) -> Result<RegexAstNode, SyntaxError> {
        let mut left_node = self.parse_quantifier()?;

        while self.tokens.peek().map_or(false, |t| {
            matches!(t, (_, Token::Literal(_)) | (_, Token::LParen))
        }) {
            let right_node = self.parse_quantifier()?;
            left_node = RegexAstNode::Concat(Box::new(left_node), Box::new(right_node));
        }

        Ok(left_node)
    }

    fn parse_quantifier(&mut self) -> Result<RegexAstNode, SyntaxError> {
        let mut node = self.parse_literal_or_group()?;

        if let Some((_, token)) = self.tokens.peek() {
            if token.is_quantifier() {
                if !node.is_quantifiable() {
                    return Err(SyntaxError::PrecedentTokenIsNotQuantifiable);
                }
                if let Some(quantifier_node) = RegexAstNode::from_quantifier(token, node.clone()) {
                    self.tokens.next();
                    node = quantifier_node;
                }
            }
        }
        Ok(node)
    }

    fn parse_literal_or_group(&mut self) -> Result<RegexAstNode, SyntaxError> {
        match self.tokens.next() {
            Some((_, Token::Literal(c))) => Ok(RegexAstNode::Literal(*c)),
            Some((_, Token::Dot)) => Ok(RegexAstNode::Wildcard),
            Some((_, Token::LParen)) => {
                if let Some((_, Token::RParen)) = self.tokens.peek() {
                    self.tokens.next();
                    return Ok(RegexAstNode::Empty);
                }
                let nested_expr = self.parse_alternation()?;
                match self.tokens.next() {
                    Some((_, Token::RParen)) => Ok(nested_expr),
                    Some((pos, token)) => Err(SyntaxError::UnexpectedToken(format!(
                        "Expected '{}', at position {}, got '{}'",
                        Token::RParen,
                        pos,
                        token
                    ))),
                    _ => Err(SyntaxError::ExpectedClosingParenthesis),
                }
            }
            Some((_, Token::Star)) | Some((_, Token::Plus)) | Some((_, Token::QuestionMark)) => {
                Err(SyntaxError::PrecedentTokenIsNotQuantifiable)
            }

            _ => Err(SyntaxError::UnexpectedLiteral),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) enum RegexAstNode {
    ZeroOrOne(Box<RegexAstNode>),
    OneOrMore(Box<RegexAstNode>),
    Literal(char),
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
                let mut left_nfa = RegexAstNode::to_nfa(left);
                let right_nfa = RegexAstNode::to_nfa(right);

                left_nfa.concatenate(&right_nfa);
                left_nfa
            }
            RegexAstNode::Alter(left, right) => {
                let mut left_nfa = RegexAstNode::to_nfa(left);
                let right_nfa = RegexAstNode::to_nfa(right);

                left_nfa.alternate(&right_nfa);
                left_nfa
            }
            RegexAstNode::Star(regex_ast_node) => {
                let mut nfa_child = RegexAstNode::to_nfa(regex_ast_node);

                nfa_child.kleene_star();
                nfa_child
            }
            RegexAstNode::Empty => Nfa::from_epsilon(),
            RegexAstNode::ZeroOrOne(_) => todo!(),
            RegexAstNode::OneOrMore(_) => todo!(),
        }
    }

    // TODO: err == string?
    pub(crate) fn new(pattern: &str) -> Result<Self, String> {
        let sequence = TokenSequence::try_from(pattern).unwrap(); //TODO: map err
                                                                  //TODO: display?
        AstParser::parse(sequence.tokens()).map_err(|e| format!("{e:?}"))
    }

    //TODO: check all quantifiable tokens
    // TODO: all concat are quantifiable?
    fn is_quantifiable(&self) -> bool {
        !matches!(
            self,
            RegexAstNode::Star(_) | RegexAstNode::OneOrMore(_) | RegexAstNode::ZeroOrOne(_)
        )
    }

    fn from_quantifier(token: &Token, node: RegexAstNode) -> Option<Self> {
        match token {
            Token::Star => Some(RegexAstNode::Star(Box::new(node))),
            Token::Plus => Some(RegexAstNode::OneOrMore(Box::new(node))),
            Token::QuestionMark => Some(RegexAstNode::ZeroOrOne(Box::new(node))),
            _ => None,
        }
    }
}

impl TryFrom<&Vec<Token>> for RegexAstNode {
    type Error = SyntaxError;

    fn try_from(tokens: &Vec<Token>) -> Result<Self, Self::Error> {
        if tokens.is_empty() {
            return Ok(RegexAstNode::Empty);
        }
        AstParser::parse(&tokens)
    }
}

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub(crate) enum SyntaxError {
    UnexpectedToken(String),
    ExpectedClosingParenthesis,
    PrecedentTokenIsNotQuantifiable,
    UnexpectedLiteral,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token::{Alter, Dot, LParen, Literal, Plus, QuestionMark, RParen, Star};
    use lazy_static::lazy_static;
    use rstest::*;

    const SNAPSHOT_PATH: &str = "../tests/ast/";

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

    mod valid_patterns {
        use super::*;

        #[test]
        fn single_literal() {
            // given
            let tokens = vec![Literal('a')];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn multiple_literals_are_a_concatenation() {
            // given
            let tokens = vec![Literal('a'), Literal('b'), Literal('3')];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn literal_with_star_quantifier() {
            // given
            let tokens = vec![Literal('a'), Star];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn literal_with_plus_quantifier() {
            // given
            let tokens = vec![Literal('b'), Plus];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn literal_with_question_mark_quantifier() {
            // given
            let tokens = vec![Literal('c'), QuestionMark];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn dot_wildcard() {
            // given
            let tokens = vec![Dot];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn simple_alternation() {
            // given
            let tokens = vec![Literal('a'), Alter, Literal('b')];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn alternation_with_concatenation() {
            // given
            let tokens = vec![Literal('a'), Alter, Literal('b'), Literal('c')];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn alternation_with_quantifier() {
            // given
            let tokens = vec![Literal('a'), Alter, Literal('b'), Star];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn grouped_literals_with_quantifier() {
            // given
            let tokens = vec![LParen, Literal('1'), Literal('2'), RParen, Star];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[rstest]
        #[case("dot_works_with_all_quantifiers_case_1", vec![Dot, Star])]
        #[case("dot_works_with_all_quantifiers_case_2", vec![Dot, Plus])]
        #[case("dot_works_with_all_quantifiers_case_3", vec![Dot, QuestionMark])]
        fn dot_works_with_all_quantifiers(#[case] name: &str, #[case] tokens: Vec<Token>) {
            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS.bind(|| {
                insta::assert_yaml_snapshot!(name, (to_pattern_string(&tokens), ast_result))
            });
        }

        #[rstest]
        #[case("group_works_with_all_quantifiers_case_1", vec![LParen, Literal('a'), RParen, Star])]
        #[case("group_works_with_all_quantifiers_case_2", vec![LParen, Literal('a'), RParen, Plus])]
        #[case("group_works_with_all_quantifiers_case_3", vec![LParen, Literal('a'), RParen, QuestionMark])]
        fn group_works_with_all_quantifiers(#[case] name: &str, #[case] tokens: Vec<Token>) {
            //when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS.bind(|| {
                insta::assert_yaml_snapshot!(name, (to_pattern_string(&tokens), ast_result))
            });
        }

        #[test]
        fn alternation_in_a_group_is_quantifiable() {
            // given
            let tokens = vec![LParen, Literal('a'), Alter, Literal('b'), RParen, Star];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn empty_group_is_valid() {
            // given
            let tokens = vec![LParen, RParen];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn empty_group_with_quantifier_is_valid() {
            //given
            let tokens = vec![LParen, RParen, Star];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn nested_groups_are_valid() {
            // given
            let tokens = vec![
                LParen,
                Literal('a'),
                LParen,
                Literal('b'),
                RParen,
                Star,
                RParen,
            ];

            //when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }
    }

    mod invalid_patterns {
        use super::*;

        #[test]
        fn only_quantifier_fails() {
            //given
            let tokens = vec![Star];

            //when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn dangling_plus_quantifier_fails() {
            //given
            let tokens = vec![Plus];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn quantifier_after_alternation_operator_fails() {
            //given
            let tokens = vec![Literal('a'), Alter, Star];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn double_quantifier_fails() {
            //given
            let tokens = vec![Literal('a'), Star, Star];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn unclosed_parenthesis_fails() {
            //given
            let tokens = vec![Literal('a'), LParen, Literal('b')];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn unexpected_closing_parenthesis_fails() {
            //given
            let tokens = vec![Literal('a'), RParen];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn quantifier_after_left_parenthesis_fails() {
            //given
            let tokens = vec![LParen, Star, Literal('1'), RParen];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[rstest]
        #[case("starting_with_a_quantifier_fails_case_1", vec![Star, Literal('a')])]
        #[case("starting_with_a_quantifier_fails_case_2", vec![Plus, Literal('a')])]
        #[case("starting_with_a_quantifier_fails_case_3", vec![QuestionMark, Literal('a')])]
        fn starting_with_a_quantifier_fails(#[case] name: &str, #[case] tokens: Vec<Token>) {
            //when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS.bind(|| {
                insta::assert_yaml_snapshot!(name, (to_pattern_string(&tokens), ast_result))
            });
        }

        #[test]
        fn starting_with_rparen_fails() {
            //given
            let tokens = vec![RParen, Literal('a')];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn starting_with_alter_fails() {
            //given
            let tokens = vec![Alter, Literal('a')];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[test]
        fn ending_with_alter_fails() {
            //given
            let tokens = vec![Literal('a'), Alter];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }

        #[rstest]
        #[case("quantifier_after_alter_fails_case_1", vec![Literal('a'), Alter, Star])]
        #[case("quantifier_after_alter_fails_case_2", vec![Literal('a'), Alter, Plus])]
        #[case("quantifier_after_alter_fails_case_3", vec![Literal('a'), Alter, QuestionMark])]
        fn quantifier_after_alter_fails(#[case] name: &str, #[case] tokens: Vec<Token>) {
            //when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS.bind(|| {
                insta::assert_yaml_snapshot!(name, (to_pattern_string(&tokens), ast_result))
            });
        }

        #[rstest]
        #[case("quantifier_or_alter_after_lparen_fails_case_1", vec![LParen, Star, Literal('a'), RParen])]
        #[case("quantifier_or_alter_after_lparen_fails_case_2", vec![LParen, Plus, Literal('a'), RParen])]
        #[case("quantifier_or_alter_after_lparen_fails_case_3", vec![LParen, QuestionMark, Literal('a'), RParen])]
        #[case("quantifier_or_alter_after_lparen_fails_case_4", vec![LParen, Alter, Literal('a'), RParen])]
        fn quantifier_or_alter_after_lparen_fails(#[case] name: &str, #[case] tokens: Vec<Token>) {
            //when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS.bind(|| {
                insta::assert_yaml_snapshot!(name, (to_pattern_string(&tokens), ast_result))
            });
        }

        #[rstest]
        #[case("multiple_quantifiers_in_a_row_fails_case_1", vec![Literal('a'), Star, Plus])]
        #[case("multiple_quantifiers_in_a_row_fails_case_2", vec![Literal('a'), QuestionMark, Star])]
        #[case("multiple_quantifiers_in_a_row_fails_case_3", vec![Literal('a'), Plus, QuestionMark])]
        fn multiple_quantifiers_in_a_row_fails(#[case] name: &str, #[case] tokens: Vec<Token>) {
            // when
            let ast_result = RegexAstNode::try_from(&tokens);
            // then
            INSTA_SETTINGS.bind(|| {
                insta::assert_yaml_snapshot!(name, (to_pattern_string(&tokens), ast_result))
            });
        }

        #[test]
        fn double_alter_fails() {
            //given
            let tokens = vec![Literal('a'), Alter, Alter, Literal('b')];

            // when
            let ast_result = RegexAstNode::try_from(&tokens);

            // then
            INSTA_SETTINGS
                .bind(|| insta::assert_yaml_snapshot!((to_pattern_string(&tokens), ast_result)));
        }
    }
}
