use crate::tokenizer::cursor::CursorPosition;
#[cfg(test)]
use serde::Serialize;

#[derive(Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum LexErrorKind {
    NewLineExpected,
    UnterminatedCommentBlock,
    UnterminatedCodeBlock,
    UnexpectedEscapedChar(Option<char>),
    UnbalancedBrackets,
    UnexpectedToken { token: String, msg: String },
    CodeBlockAfterRulesDetected { first_rule: CursorPosition },
    CodeLineAfterRulesDetected { first_rule: CursorPosition },
    NoSeparatorsFound,
    Internal(String),
}

#[derive(Debug)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct LexError {
    kind: LexErrorKind,
    position: Option<CursorPosition>,
}

impl LexError {
    pub fn new(kind: LexErrorKind, position: CursorPosition) -> Self {
        LexError {
            kind,
            position: Some(position),
        }
    }

    pub fn new_general(kind: LexErrorKind) -> Self {
        LexError {
            kind,
            position: None,
        }
    }

    pub fn new_internal(message: String) -> Self {
        LexError {
            kind: LexErrorKind::Internal(message),
            position: None,
        }
    }

    pub fn kind(&self) -> &LexErrorKind {
        &self.kind
    }

    pub fn position(&self) -> Option<&CursorPosition> {
        self.position.as_ref()
    }
}
