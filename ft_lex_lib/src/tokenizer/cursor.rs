#[cfg(test)]
use serde::Serialize;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Copy, Clone, PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub struct CursorPosition {
    pub cur_line: usize,
    pub cur_pos: usize,
}

pub(super) struct Cursor<'a> {
    data: Peekable<Chars<'a>>,
    cursor_pos: CursorPosition,
}

impl CursorPosition {
    pub fn new(cur_line: usize, cur_pos: usize) -> Self {
        CursorPosition { cur_line, cur_pos }
    }

    pub fn prev(&self) -> Self {
        CursorPosition {
            cur_line: self.cur_line,
            cur_pos: self.cur_pos.checked_sub(1).unwrap_or(0),
        }
    }
}

impl Default for CursorPosition {
    fn default() -> Self {
        Self {
            cur_line: 1,
            cur_pos: 0,
        }
    }
}

impl<'a> Cursor<'a> {
    pub fn new(data: Peekable<Chars<'a>>) -> Self {
        Self {
            data,
            cursor_pos: CursorPosition::default(),
        }
    }

    pub fn skip_spaces_and_tabs(&mut self) {
        while let Some(&c) = self.data.peek() {
            if c == ' ' || c == '\t' {
                self.next();
            } else {
                break;
            }
        }
    }

    pub fn skip_white_spaces(&mut self) {
        while let Some(c) = self.data.peek() {
            if c.is_whitespace() {
                self.next();
            } else {
                break;
            }
        }
    }

    pub fn next(&mut self) -> Option<char> {
        if let Some(n) = self.data.next() {
            self.cursor_pos.cur_pos += 1;
            if n == '\n' {
                self.cursor_pos.cur_pos = 0;
                self.cursor_pos.cur_line += 1;
            }
            Some(n)
        } else {
            None
        }
    }

    pub fn next_until_end_of_line(&mut self) -> Vec<char> {
        let mut buf = Vec::with_capacity(16);

        while let Some(&c) = self.data.peek() {
            if c != '\n' {
                buf.push(c);
                self.next();
            } else {
                break;
            }
        }

        buf
    }

    pub fn collect(&mut self) -> Vec<char> {
        self.data.clone().collect()
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.data.peek()
    }

    pub fn peek_two(&mut self) -> (Option<char>, Option<char>) {
        let mut it = self.data.clone();
        (it.next(), it.next())
    }

    pub fn get_position(&self) -> CursorPosition {
        self.cursor_pos
    }

    pub fn is_on_line_beginning(&self) -> bool {
        self.cursor_pos.cur_pos == 0
    }

    pub fn is_eof(&mut self) -> bool {
        self.data.peek().is_none()
    }
}
