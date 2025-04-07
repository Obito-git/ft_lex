pub(crate) mod cursor;
use crate::error::{LexError, LexErrorKind};
use crate::tokenizer::cursor::{Cursor, CursorPosition};
#[cfg(test)]
use serde::Serialize;

type Pair = (CursorPosition, Vec<char>, Vec<char>);

#[derive(PartialEq, Debug)]
#[cfg_attr(test, derive(Serialize))]
pub enum Token {
    PercentPercent(CursorPosition),
    CodeBlock(CursorPosition, Vec<char>),
    Definition(CursorPosition, Vec<char>, Vec<char>),
    Rule(CursorPosition, Vec<char>, Vec<char>),
    MultilineComment(CursorPosition, Vec<char>),
}

pub struct LexFileTokenizer<'a> {
    cursor: Cursor<'a>,
    res: Vec<Token>,
    percent_percent_count: usize,
}

impl<'a> LexFileTokenizer<'a> {
    fn assert_next_is_newline(&mut self) -> Result<(), LexError> {
        if self.cursor.peek() != Some(&'\n') {
            self.cursor.next();
            Err(LexError::new(
                LexErrorKind::NewLineExpected,
                self.cursor.get_position(),
            ))
        } else {
            Ok(())
        }
    }

    fn assert_next_is_newline_or_none(&mut self) -> Result<(), LexError> {
        let next = self.cursor.peek();
        if next != Some(&'\n') && next != None {
            self.cursor.next();
            Err(LexError::new(
                LexErrorKind::NewLineExpected,
                self.cursor.get_position(),
            ))
        } else {
            Ok(())
        }
    }

    fn read_code_block(&mut self) -> Result<(), LexError> {
        let mut is_str_mode = false;
        let mut buffer = Vec::with_capacity(16);
        let pos = self.cursor.get_position();

        //TODO: handle escaped " with / inside string
        loop {
            if self.cursor.peek_two() == (Some('%'), Some('}')) && !is_str_mode {
                self.cursor.next();
                self.cursor.next();
                self.res.push(Token::CodeBlock(pos, buffer));
                self.cursor.skip_spaces_and_tabs();
                self.assert_next_is_newline()?;
                break;
            }
            match self.cursor.next() {
                Some('"') => {
                    is_str_mode = !is_str_mode;
                    buffer.push('"');
                }
                Some(c) => buffer.push(c),
                None => return Err(LexError::new(LexErrorKind::UnterminatedCodeBlock, pos)),
            }
        }
        Ok(())
    }

    //TODO: handle unclosed code block }
    fn read_pair(&mut self) -> Result<Pair, LexError> {
        let position = self.cursor.get_position();
        let mut definition_name = Vec::with_capacity(4);
        //TODO: review carefully when parantesses
        //TODO: review carefully when [] ][ are escaped for regex
        //TODO: review escaped " inside ""
        let mut regex_depth = 0;
        let mut is_str_mode = false;
        let mut prev_char = '0';

        while let Some(&c) = self.cursor.peek() {
            match (c, is_str_mode, regex_depth == 0) {
                (']', false, _) => regex_depth += 1,
                ('[', false, _) => regex_depth -= 1,
                ('"', _, _) => {
                    if prev_char != '\\' {
                        is_str_mode = !is_str_mode
                    }
                }
                ('\n' | ' ' | '\t', false, true) => break,
                _ => {}
            }
            self.cursor.next();
            prev_char = c;
            definition_name.push(c)
        }

        self.cursor.skip_spaces_and_tabs();

        //TODO: very naive handing, change
        let mut is_str_mode = false;
        let multiline_code_block = self.cursor.peek() == Some(&'{');
        let mut bracket_depth = 0;
        let val_start_pos = self.cursor.get_position();
        let mut value = Vec::with_capacity(32);

        if multiline_code_block {
            // Consume the opening '{' and start depth at 1
            self.cursor.next(); // Consume '{'
            value.push('{'); // Add '{' to the value
            bracket_depth = 1;
        }

        loop {
            let current_char_pos = self.cursor.get_position();
            let next_char = self.cursor.peek().cloned();

            match next_char {
                Some('"') => {
                    self.cursor.next();
                    value.push('"');
                    is_str_mode = !is_str_mode;
                }
                //TODO: test if action ends with "something/(EOF)
                Some('\\') if is_str_mode => {
                    self.cursor.next();
                    value.push('\\');
                    if let Some(escaped) = self.cursor.next() {
                        value.push(escaped);
                    } else {
                        //TODO: test
                        return Err(LexError::new(
                            LexErrorKind::UnterminatedString,
                            current_char_pos,
                        ));
                    }
                }
                Some('{') if !is_str_mode && multiline_code_block => {
                    self.cursor.next();
                    value.push('{');
                    bracket_depth += 1;
                }
                Some('}') if !is_str_mode && multiline_code_block => {
                    self.cursor.next();
                    value.push('}');
                    bracket_depth -= 1;
                    if bracket_depth == 0 {
                        value.append(&mut self.cursor.next_until_end_of_line());
                        self.cursor.next();
                        break;
                    }
                }
                Some('\n') if !multiline_code_block => {
                    break;
                }
                Some(c) => {
                    self.cursor.next();
                    value.push(c);
                }
                None => {
                    if multiline_code_block && bracket_depth != 0 {
                        return Err(LexError::new(
                            LexErrorKind::UnterminatedCodeBlock,
                            val_start_pos, // Error started at the opening '{'
                        ));
                    }
                    if is_str_mode {
                        return Err(LexError::new(
                            LexErrorKind::UnterminatedString,
                            val_start_pos, // Or find the opening '"'? Harder.
                        ));
                    }
                    // EOF is fine if the block/line terminated correctly or was empty.
                    break; // Exit loop, EOF reached
                }
            }
        }

        Ok((position, definition_name, value))
    }

    fn read_multi_comment_block(&mut self) -> Result<(), LexError> {
        let mut buffer = Vec::with_capacity(16);
        let pos = self.cursor.get_position();

        loop {
            if self.cursor.peek_two() == (Some('*'), Some('/')) {
                buffer.push(self.cursor.next().unwrap());
                buffer.push(self.cursor.next().unwrap());
                self.res.push(Token::MultilineComment(pos, buffer));
                break;
            }
            match self.cursor.next() {
                None => return Err(LexError::new(LexErrorKind::UnterminatedCommentBlock, pos)),
                Some(c) => buffer.push(c),
            }
        }
        Ok(())
    }

    fn handle_section_end(&mut self) -> Result<(), LexError> {
        self.res
            .push(Token::PercentPercent(self.cursor.get_position()));
        self.cursor.next();
        self.cursor.next();
        self.percent_percent_count += 1;
        /* lex ignores everything on the same line after %%,
         * but we ignore whitespaces and return an error when find something. */
        self.cursor.skip_spaces_and_tabs();
        self.assert_next_is_newline_or_none()?;
        self.cursor.next();
        Ok(())
    }

    fn read_first_section(&mut self) -> Result<(), LexError> {
        loop {
            self.cursor.skip_white_spaces();
            let cursor_is_on_line_start = self.cursor.is_on_line_beginning();
            match self.cursor.peek_two() {
                (Some('%'), Some('{')) => {
                    self.cursor.next();
                    self.cursor.next();
                    self.read_code_block()?
                }
                (Some('%'), Some('%')) => {
                    self.handle_section_end()?;
                    break;
                }
                (Some('/'), Some('*')) => self.read_multi_comment_block()?,
                (Some(_), _) if cursor_is_on_line_start => {
                    let (pos, key, value) = self.read_pair()?;
                    self.res.push(Token::Definition(pos, key, value))
                }
                (Some(_), _) if !cursor_is_on_line_start => self.res.push(Token::CodeBlock(
                    self.cursor.get_position(),
                    self.cursor.next_until_end_of_line(),
                )),
                _ => return Err(LexError::new_general(LexErrorKind::NoSeparatorsFound)),
            }
        }
        Ok(())
    }

    fn read_second_section(&mut self) -> Result<(), LexError> {
        let mut rule_seen = false;
        loop {
            self.cursor.skip_white_spaces();
            let cursor_is_on_line_start = self.cursor.is_on_line_beginning();
            let first_two = self.cursor.peek_two();
            
            if self.cursor.is_eof() {
                break;
            }

            if first_two == (Some('%'), Some('%')) {
                self.handle_section_end()?;
                break;
            }

            if first_two == (Some('%'), Some('{')) {
                self.cursor.next();
                self.cursor.next();
                if rule_seen {
                    //TODO: Warning? By lex docs it is undefined behaviour
                }
                self.read_code_block()?;
                continue;
            }

            //TODO: check if conflicts with rule regex
            if first_two == (Some('/'), Some('*')) {
                if rule_seen {
                    //TODO: need to handle?
                }
                self.read_multi_comment_block()?;
                continue;
            }

            if !cursor_is_on_line_start && !rule_seen {
                self.res.push(Token::CodeBlock(
                    self.cursor.get_position(),
                    self.cursor.next_until_end_of_line(),
                ));
            } else {
                let (pos, key, value) = self.read_pair()?;
                self.res.push(Token::Rule(pos, key, value));
                rule_seen = true;
            }
        }
        Ok(())
    }

    fn analyze(&mut self) -> Result<(), LexError> {
        self.read_first_section()?;
        self.read_second_section()?;

        // User code section
        let code_block_position = self.cursor.get_position();
        let rest = self.cursor.collect();
        if !rest.is_empty() {
            self.res.push(Token::CodeBlock(code_block_position, rest));
        }
        if self.percent_percent_count == 0 {
            Err(LexError::new_general(LexErrorKind::NoSeparatorsFound))
        } else {
            Ok(())
        }
    }

    pub fn parse(s: &'a String) -> Result<Vec<Token>, LexError> {
        let mut parser = Self {
            cursor: Cursor::new(s.chars().peekable()),
            res: vec![],
            percent_percent_count: 0,
        };

        parser.analyze()?;

        Ok(parser.res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;
    use std::fs;
    use std::path::PathBuf;

    mod definition_table_test {
        use super::*;
        use insta::with_settings;
        //TODO: test spaces and tabs after code block
        //TODO: test error when any line starts with whitespace

        #[rstest]
        /// Parameterized test verifying `LexFileTokenizer` output for `.l` files.
        /// Uses `rstest` to discover files in `test_config/tokenizer/**/*.l`.
        /// Asserts the tokenization result against an `insta` YAML snapshot.
        fn load_lex_configuration_files(#[files("test_config/tokenizer/**/*.l")] path: PathBuf) {
            // given
            let input_content = fs::read_to_string(&path)
                .unwrap_or_else(|err| panic!("Error: Could not read input {:?}: {}", path, err));

            // the name of the generated snapshot
            let test_name = format!("{path:?}")
                .split_once("test_config/tokenizer/")
                .expect(
                    "Failed to find 'test_config/tokenizer/' segment in the formatted path string",
                )
                .1 // Get the part after the delimiter (e.g., "subdir/file.l")
                .replace(['/', '.', '"', '\\'], "_");

            // when
            let result: Result<Vec<Token>, LexError> = LexFileTokenizer::parse(&input_content);

            // then
            with_settings!(
                {
                    snapshot_path => "../../test_config/result_snaps/tokenizer", // Centralized snapshot location
                    prepend_module_to_snapshot => false, // Avoid module path prefix in snapshot filename
                },
                {
                    // Assert tokenization result (Ok or Err) against the YAML snapshot.
                    // Run `cargo insta review` to approve changes on first run or after modifications.
                    insta::assert_yaml_snapshot!(test_name, result);
                }
            );
        }
    }
}
