#[cfg(test)]
use serde::Serialize;

use crate::token::Token;
use std::iter::{Enumerate, Peekable};
use std::slice::Iter;

struct AstParser<'a> {
    tokens: Peekable<Enumerate<Iter<'a, Token>>>,
}

impl<'a> AstParser<'a> {
    fn parse(tokens: &'a [Token]) -> Result<RegexAstNode, SyntaxError> {
        let mut parser = Self {
            tokens: tokens.iter().enumerate().peekable(),
        };
        let parsed = parser.parse_expression()?;
        if let Some((pos, token_left)) = parser.tokens.peek() {
            let syntax_err = if token_left.is_quantifier() {
                SyntaxError::PrecedentTokenIsNotQuantifiable
            } else {
                SyntaxError::UnexpectedToken(format!("'{}', position {}", token_left, pos))
            };
            Err(syntax_err)
        } else {
            Ok(parsed)
        }
    }

    fn parse_expression(&mut self) -> Result<RegexAstNode, SyntaxError> {
        let head_node = self.parse_term()?;

        Ok(head_node)
    }

    fn parse_term(&mut self) -> Result<RegexAstNode, SyntaxError> {
        let mut left_node = self.parse_factor()?;

        while self.tokens.peek().map_or(false, |t| {
            matches!(t, (_, Token::Literal(_)) | (_, Token::LParen))
        }) {
            let right_ast = self.parse_factor()?; // Parse the subsequent factor
            left_node = RegexAstNode::Concat(Box::new(left_node), Box::new(right_ast));
        }
        Ok(left_node)
    }

    fn parse_factor(&mut self) -> Result<RegexAstNode, SyntaxError> {
        let atom = self.parse_atom()?;

        if let Some((_, token)) = self.tokens.peek() {
            match (atom.is_quantifiable(), token.is_quantifier()) {
                (true, true) => {
                    self.tokens.next();
                    Ok(RegexAstNode::Star(Box::new(atom)))
                }
                (false, true) => Err(SyntaxError::PrecedentTokenIsNotQuantifiable),
                (_, _) => Ok(atom),
            }
        } else {
            Ok(atom)
        }
    }

    fn parse_atom(&mut self) -> Result<RegexAstNode, SyntaxError> {
        match self.tokens.next() {
            Some((_, Token::Literal(c))) => Ok(RegexAstNode::Literal(*c)),
            Some((_, Token::LParen)) => {
                let nested_expr = self.parse_expression()?;
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
enum RegexAstNode {
    Literal(char),
    Star(Box<RegexAstNode>),
    Alter(Box<RegexAstNode>, Box<RegexAstNode>),
    Concat(Box<RegexAstNode>, Box<RegexAstNode>),
    Empty,
}

impl RegexAstNode {
    //TODO: check all quantifiable tokens
    // TODO: all concat are quantifiable?
    pub fn is_quantifiable(&self) -> bool {
        match self {
            RegexAstNode::Literal(_) | RegexAstNode::Concat(_, _) => true,
            _ => false,
        }
    }
}

impl TryFrom<Vec<Token>> for RegexAstNode {
    type Error = SyntaxError;

    fn try_from(tokens: Vec<Token>) -> Result<Self, Self::Error> {
        if tokens.is_empty() {
            return Ok(RegexAstNode::Empty);
        }
        AstParser::parse(tokens.as_slice())
    }
}

#[derive(Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
enum SyntaxError {
    UnexpectedToken(String),
    ExpectedClosingParenthesis,
    PrecedentTokenIsNotQuantifiable,
    UnexpectedLiteral, // Improve
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    fn trash_can_todo_change_name() {}
}

/*
#[cfg(test)]
mod tests {
    use super::*;

    mod literals_tests {
        use super::*;

        #[test]
        fn single_literal() {
            // given
            let tokens = vec![Token::Literal('a')];
            let expected_res = Ok(RegexAstNode::Literal('a'));

            // when
            let ast = RegexAstNode::try_from(tokens);

            //then
            assert_eq!(expected_res, ast)
        }

        #[test]
        fn multiple_literals() {
            // given
            let tokens = vec![
                Token::Literal('a'),
                Token::Literal('b'),
                Token::Literal('3'),
            ];
            let expected_res = Ok(RegexAstNode::Concat(
                Box::new(RegexAstNode::Concat(
                    Box::new(RegexAstNode::Literal('a')),
                    Box::new(RegexAstNode::Literal('b')),
                )),
                Box::new(RegexAstNode::Literal('3')),
            ));

            // when
            let ast = RegexAstNode::try_from(tokens);

            //then
            assert_eq!(expected_res, ast)
        }
    }

    mod quantifiers_tests {
        use super::*;
        use crate::token::Token::Literal;

        #[test]
        fn single_literal_with_quantifiers() {
            // given
            let tokens = vec![Token::Literal('a'), Token::Star];
            let expected_res = Ok(RegexAstNode::Star(Box::new(RegexAstNode::Literal('a'))));

            // when
            let ast = RegexAstNode::try_from(tokens);

            //then
            assert_eq!(expected_res, ast)
        }

        #[test]
        fn literals_in_parentheses_with_start() {
            // given
            let tokens = vec![
                Token::LParen,
                Literal('1'),
                Literal('2'),
                Token::RParen,
                Token::Star,
            ];
            let expected_res = Ok(RegexAstNode::Star(Box::new(RegexAstNode::Concat(
                Box::new(RegexAstNode::Literal('1')),
                Box::new(RegexAstNode::Literal('2')),
            ))));

            // when
            let ast = RegexAstNode::try_from(tokens);

            //then
            assert_eq!(expected_res, ast)
        }

        #[test]
        fn multiple_quantifier_should_fail() {
            // given
            let tokens = vec![Token::Literal('a'), Token::Star, Token::Star];
            let expected_res = Err(SyntaxError::PrecedentTokenIsNotQuantifiable);

            // when
            let ast = RegexAstNode::try_from(tokens);

            //then
            assert_eq!(expected_res, ast)
        }

        #[test]
        fn multiple_quantifier_with_trained_literal_should_fail() {
            // given
            let tokens = vec![
                Token::Literal('a'),
                Token::Star,
                Token::Star,
                Token::Literal('b'),
            ];
            let expected_res = Err(SyntaxError::PrecedentTokenIsNotQuantifiable);

            // when
            let ast = RegexAstNode::try_from(tokens);

            //then
            assert_eq!(expected_res, ast)
        }

        #[test]
        fn only_quantifier_should_fail() {
            // given
            let tokens = vec![Token::Star];
            let expected_res = Err(SyntaxError::PrecedentTokenIsNotQuantifiable);

            // when
            let ast = RegexAstNode::try_from(tokens);

            //then
            assert_eq!(expected_res, ast)
        }

        #[test]
        fn star_after_left_parentheses_should_fail() {
            // given
            let tokens = vec![Token::LParen, Token::Star, Literal('1'), Token::RParen];
            let expected_res = Err(SyntaxError::PrecedentTokenIsNotQuantifiable);

            // when
            let ast = RegexAstNode::try_from(tokens);

            //then
            assert_eq!(expected_res, ast)
        }
    }

    mod parentheses_tests {
        use super::*;

        #[test]
        fn unexpected_closing_parentheses_should_fail() {
            // given
            let tokens = vec![Token::Literal('a'), Token::RParen];
            let expected_res = Err(SyntaxError::UnexpectedToken("')', position 1".to_string()));

            // when
            let ast = RegexAstNode::try_from(tokens);

            //then
            assert_eq!(expected_res, ast)
        }

        #[test]
        fn unclosed_parentheses_should_fail() {
            // given
            let tokens = vec![
                Token::Literal('a'),
                Token::LParen,
                Token::Literal('a'),
                Token::Star,
            ];
            let expected_res = Err(SyntaxError::ExpectedClosingParenthesis);

            // when
            let ast = RegexAstNode::try_from(tokens);

            //then
            assert_eq!(expected_res, ast)
        }
    }
}


 */
