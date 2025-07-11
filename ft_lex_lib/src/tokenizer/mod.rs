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
    EndCodeBlock(CursorPosition, Vec<char>),
}

enum CodeBlockType {
    C,
    Lex,
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

    fn read_escaped_char(&mut self) -> Result<char, LexErrorKind> {
        match self.cursor.next() {
            Some('\n') => Err(LexErrorKind::UnexpectedEscapedChar(Some('\n'))),
            None => Err(LexErrorKind::UnexpectedEscapedChar(None)),
            Some(c) => Ok(c),
        }
    }

    fn read_code_block_internal(
        &mut self,
        block_type: CodeBlockType,
    ) -> Result<Vec<char>, LexError> {
        let mut is_str_mode = false;
        let start_pos = self.cursor.get_position();
        let mut res = Vec::with_capacity(32);
        let mut bracket_depth = 1;

        match block_type {
            CodeBlockType::C => {
                if self.cursor.next() != Some('{') {
                    return Err(LexError::new(
                        LexErrorKind::Internal("Expected '{' to start C block".to_string()),
                        start_pos,
                    ));
                }
                res.push('{');
            }
            CodeBlockType::Lex => {
                if let (Some('%'), Some('{')) = (self.cursor.next(), self.cursor.next()) {
                    res.push('%');
                    res.push('{');
                } else {
                    return Err(LexError::new(
                        LexErrorKind::Internal("Expected '{%' to start Lex block".to_string()),
                        start_pos,
                    ));
                }
            }
        }

        loop {
            let current_char_pos = self.cursor.get_position();
            let next_char = self.cursor.peek().cloned();

            match self.cursor.peek_two() {
                (Some('/'), Some('/')) if !is_str_mode => {
                    res.append(&mut self.cursor.next_until_end_of_line());
                    continue;
                }
                (Some('/'), Some('*')) if !is_str_mode => {
                    res.append(&mut self.read_c_multi_line_comment()?);
                    continue;
                }
                (_, _) => {}
            }

            match next_char {
                Some('"') => {
                    self.cursor.next();
                    res.push('"');
                    is_str_mode = !is_str_mode;
                }
                Some('\\') if is_str_mode => {
                    self.cursor.next();
                    res.push('\\');
                    res.push(
                        self.read_escaped_char()
                            .map_err(|e| LexError::new(e, current_char_pos))?,
                    );
                }
                Some('{') if !is_str_mode => {
                    self.cursor.next();
                    res.push('{');
                    bracket_depth += 1;
                }
                Some('}') if !is_str_mode => {
                    self.cursor.next();
                    res.push('}');
                    bracket_depth -= 1;
                }
                Some(c) => {
                    self.cursor.next();
                    res.push(c);
                }
                None => {
                    if bracket_depth != 0 {
                        return Err(LexError::new(
                            LexErrorKind::UnterminatedCodeBlock,
                            start_pos,
                        ));
                    }
                    break;
                }
            }
            if bracket_depth == 0 {
                match block_type {
                    CodeBlockType::C => {
                        res.append(&mut self.cursor.next_until_end_of_line());
                        self.cursor.next();
                    }
                    CodeBlockType::Lex => {
                        let last_two = res.iter().rev().take(2).rev().copied().collect::<Vec<_>>();
                        if last_two != vec!['%', '}'] {
                            return Err(LexError::new(
                                LexErrorKind::UnexpectedToken {
                                    token: format!("{}", last_two.iter().collect::<String>()),
                                    msg: "The right sequence to close code block is \"%}\""
                                        .to_string(),
                                },
                                current_char_pos.prev(),
                            ));
                        }
                        self.cursor.skip_spaces_and_tabs();
                        self.assert_next_is_newline()?;
                    }
                }
                break;
            }
        }
        Ok(res)
    }

    fn read_c_code_block(&mut self) -> Result<Vec<char>, LexError> {
        self.read_code_block_internal(CodeBlockType::C)
    }

    fn read_lex_code_block(&mut self) -> Result<Vec<char>, LexError> {
        self.read_code_block_internal(CodeBlockType::Lex)
    }

    // It reads regex like expressions, but doesn't validate it. It is done on parsing level
    fn read_regex_like_expression(&mut self) -> Result<Vec<char>, LexError> {
        let mut key = Vec::with_capacity(16);
        let mut delimiter_stack = Vec::with_capacity(8);
        let start_position = self.cursor.get_position();

        while let Some(&c) = self.cursor.peek() {
            let current_delimiter = delimiter_stack.last().cloned();
            match c {
                '\\' => {
                    self.cursor.next();
                    key.push('\\');
                    key.push(
                        self.read_escaped_char()
                            .map_err(|e| LexError::new(e, start_position))?,
                    );
                }
                _ if current_delimiter == Some('[') => {
                    self.cursor.next();
                    key.push(c);
                    if c == ']' {
                        delimiter_stack.pop();
                    }
                }
                _ if current_delimiter == Some('(') => {
                    self.cursor.next();
                    key.push(c);
                    if c == ')' {
                        delimiter_stack.pop();
                    }
                }
                _ if current_delimiter == Some('"') => {
                    self.cursor.next();
                    key.push(c);
                    if c == '"' {
                        delimiter_stack.pop();
                    }
                }
                _ if current_delimiter == Some('{') => {
                    self.cursor.next();
                    key.push(c);
                    if c == '}' {
                        delimiter_stack.pop();
                    }
                }
                '[' | '(' | '{' | '"' => {
                    self.cursor.next();
                    delimiter_stack.push(c);
                    key.push(c);
                }
                '\n' | ' ' | '\t' if delimiter_stack.is_empty() => {
                    break;
                }
                c => {
                    self.cursor.next();
                    key.push(c)
                }
            }
        }
        // probably unreachable, but I'll keep just in case
        if !delimiter_stack.is_empty() {
            Err(LexError::new(
                LexErrorKind::UnbalancedBrackets,
                self.cursor.get_position(),
            ))
        } else {
            Ok(key)
        }
    }

    fn read_rule(&mut self) -> Result<Pair, LexError> {
        let start_position = self.cursor.get_position();

        let key = self.read_regex_like_expression()?;
        self.cursor.skip_spaces_and_tabs();
        let value = {
            if self.cursor.peek() == Some(&'{') {
                self.read_c_code_block()?
            } else {
                self.cursor.next_until_end_of_line()
            }
        };
        Ok((start_position, key, value))
    }

    fn read_definition(&mut self) -> Result<Pair, LexError> {
        let start_position = self.cursor.get_position();

        let key = self.read_regex_like_expression()?;
        self.cursor.skip_spaces_and_tabs();
        let value = self.cursor.next_until_end_of_line();
        Ok((start_position, key, value))
    }

    fn read_c_multi_line_comment(&mut self) -> Result<Vec<char>, LexError> {
        let mut buffer = Vec::with_capacity(32);
        let pos = self.cursor.get_position();

        if self.cursor.peek_two() != ((Some('/'), Some('*'))) {
            return Err(LexError::new(
                LexErrorKind::Internal(
                    "Trying to read multi-line comment, but the block doesn't start with \"/*\""
                        .to_string(),
                ),
                self.cursor.get_position(),
            ));
        }

        loop {
            if self.cursor.peek_two() == (Some('*'), Some('/')) {
                buffer.push(self.cursor.next().unwrap());
                buffer.push(self.cursor.next().unwrap());
                break;
            }
            match self.cursor.next() {
                None => return Err(LexError::new(LexErrorKind::UnterminatedCommentBlock, pos)),
                Some(c) => buffer.push(c),
            }
        }
        Ok(buffer)
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

    fn tokenize_first_section(&mut self) -> Result<(), LexError> {
        loop {
            self.cursor.skip_white_spaces();
            let cursor_is_on_line_beginning = self.cursor.is_on_line_beginning();
            let cursor_pos = self.cursor.get_position();
            match self.cursor.peek_two() {
                (Some('%'), Some('{')) => {
                    let code = self.read_lex_code_block()?;
                    self.res.push(Token::CodeBlock(cursor_pos, code))
                }
                (Some('%'), Some('%')) => {
                    self.handle_section_end()?;
                    break;
                }
                (Some('/'), Some('*')) => {
                    let mut comment = self.read_c_multi_line_comment()?;
                    comment.extend(self.cursor.next_until_end_of_line());
                    self.res.push(Token::MultilineComment(cursor_pos, comment));
                }
                (Some(_), _) if cursor_is_on_line_beginning => {
                    let (pos, key, value) = self.read_definition()?;
                    self.res.push(Token::Definition(pos, key, value))
                }
                (Some(_), _) if !cursor_is_on_line_beginning => self.res.push(Token::CodeBlock(
                    cursor_pos,
                    self.cursor.next_until_end_of_line(),
                )),
                _ => return Err(LexError::new_general(LexErrorKind::NoSeparatorsFound)),
            }
        }
        Ok(())
    }

    fn tokenize_second_section(&mut self) -> Result<(), LexError> {
        let mut first_rule = None;
        loop {
            self.cursor.skip_white_spaces();
            let cursor_is_on_line_start = self.cursor.is_on_line_beginning();
            let cursor_pos = self.cursor.get_position();
            let first_two = self.cursor.peek_two();

            if self.cursor.is_eof() {
                break;
            }

            if first_two == (Some('%'), Some('%')) {
                self.handle_section_end()?;
                break;
            }

            if first_two == (Some('%'), Some('{')) {
                if let Some(first_rule_pos) = first_rule {
                    return Err(LexError::new(
                        LexErrorKind::CodeBlockAfterRulesDetected {
                            first_rule: first_rule_pos,
                        },
                        cursor_pos,
                    ));
                }
                let code = self.read_lex_code_block()?;
                self.res.push(Token::CodeBlock(cursor_pos, code));
                continue;
            }

            if !cursor_is_on_line_start {
                if let Some(first_rule_pos) = first_rule {
                    return Err(LexError::new(
                        LexErrorKind::CodeLineAfterRulesDetected {
                            first_rule: first_rule_pos,
                        },
                        cursor_pos,
                    ));
                }
                self.res.push(Token::CodeBlock(
                    self.cursor.get_position(),
                    self.cursor.next_until_end_of_line(),
                ));
            } else {
                let (pos, key, value) = self.read_rule()?;
                self.res.push(Token::Rule(pos, key, value));
                if first_rule.is_none() {
                    first_rule = Some(pos);
                }
            }
        }
        Ok(())
    }

    fn start(&mut self) -> Result<(), LexError> {
        self.tokenize_first_section()?;
        self.tokenize_second_section()?;

        self.cursor.skip_white_spaces();

        // User code section
        let code_block_position = self.cursor.get_position();
        let rest = self.cursor.collect();
        if !rest.is_empty() {
            self.res
                .push(Token::EndCodeBlock(code_block_position, rest));
        }
        if self.percent_percent_count == 0 {
            Err(LexError::new_general(LexErrorKind::NoSeparatorsFound))
        } else {
            Ok(())
        }
    }

    pub fn tokenize(s: &'a String) -> Result<Vec<Token>, LexError> {
        let mut parser = Self {
            cursor: Cursor::new(s.chars().peekable()),
            res: vec![],
            percent_percent_count: 0,
        };

        parser.start()?;

        Ok(parser.res)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::with_settings;
    use lazy_static::lazy_static;
    use regex::Regex;
    use rstest::rstest;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::Duration;

    const BASE_DIR: &str = "test_config/tokenizer/";
    const TEST_CONFIG_FILENAME_REGEX: &str = r"^\d{2}_(ok|err)_.+\.l\.example$";
    const SNAPSHOT_BASE_FOLDER: &str = "../../../test_config/result_snaps/tokenizer/";

    lazy_static! {
        static ref test_file_name: Regex = Regex::new(TEST_CONFIG_FILENAME_REGEX).unwrap();
    }

    /// Splits related path to the conf file to path and file name.
    /// # Arguments
    ///
    /// * `rel_path_to_conf`: relative path to the config file
    ///
    /// returns: (String, String) -> (path, file_name)
    fn split_relative_path(rel_path_to_conf: &str) -> (String, String) {
        let path = Path::new(rel_path_to_conf);

        let file_name = path
            .file_name()
            .and_then(std::ffi::OsStr::to_str)
            .map(String::from)
            .expect(&format!("Can't get file_name from: {}", rel_path_to_conf));

        let folder_path = path
            .parent() // Returns Option<&Path>
            .filter(|p| !p.as_os_str().is_empty())
            .and_then(Path::to_str)
            .map(String::from)
            // If there's no parent (path was just "filename.ext"), default to "."
            .unwrap_or_else(|| ".".to_string());

        (folder_path, file_name)
    }

    /// Parameterized test verifying tokenizer output for lex/flex configuration files.
    ///
    /// Uses `rstest` to discover test files within the `test_config/tokenizer/**/*` directory.
    /// Each discovered file must adhere to the specific naming convention:
    /// `<NN>_<TYPE>_<NAME>.lex.example`
    ///
    /// Where:
    ///   - `<NN>`: A two-digit number (e.g., `01`-`99`), representing the test case identifier.
    ///   - `<TYPE>`: Specifies the expected outcome - either `ok` (successful tokenization
    ///             is expected) or `err` (indicating a tokenization error
    ///             is expected).
    ///   - `<NAME>`: A descriptive name for the test configuration (e.g., `basic_rules`,
    ///             `unterminated_comment`).
    ///   - `.lex.example`: The required file extension. (The `.example` suffix is used to
    ///                     prevent misidentification of the project's language by GitHub).
    ///
    /// The test tokenizes the content of each matched configuration file. The resulting
    /// sequence of tokens (or the resulting error) is then asserted against an `insta`
    /// YAML snapshot using `insta::assert_yaml_snapshot!`.
    ///
    /// Snapshot files (`.snap`) are stored in a parallel directory structure under
    /// `result_snaps`. For an input file like
    /// `test_config/tokenizer/feature_x/05_ok_keywords.lex.example`, the corresponding snapshot
    /// will be located at
    /// `test_config/tokenizer/result_snaps/feature_x/05_ok_keywords.snap`.
    ///
    /// **Snapshot Workflow:**
    /// When running these tests for the first time, or if the tokenizer's output changes for
    /// an existing test case, `insta::assert_yaml_snapshot!` will serialize the new result
    /// to a `.snap.new` file. These new or updated snapshots are considered 'pending' and
    /// must be manually reviewed and accepted (or rejected) using the command:
    /// `cargo insta review`
    ///
    /// Once a snapshot is accepted via the review process, the `.snap.new` file is renamed
    /// to `.snap`, becoming the official reference for future runs. Subsequent test executions
    /// will compare the tokenizer's output directly against the content of the corresponding
    /// accepted `.snap` file.
    #[rstest]
    #[timeout(Duration::from_secs(1))]
    fn load_lex_configuration_files(#[files("../test_config/tokenizer/**/*")] path: PathBuf) {
        // given
        let input_content = fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("Error: Can't read input {:?}: {}", path, err));

        let config_file_path = format!("{path:?}")
            .split_once(BASE_DIR)
            .expect("Failed to find 'test_config/tokenizer/' segment in the formatted path string")
            .1
            .trim_end_matches("\"")
            .to_string();

        let (conf_rel_path, mut conf_name) = split_relative_path(&config_file_path);

        if !test_file_name.is_match(&conf_name) {
            panic!(
                "Test config \"{conf_name}\": should match regex \"{TEST_CONFIG_FILENAME_REGEX}\""
            )
        }

        conf_name = conf_name.trim_end_matches(".l.example").to_string();

        let snapshot_path = format!("{SNAPSHOT_BASE_FOLDER}/{conf_rel_path}");

        // when
        let result: Result<Vec<Token>, LexError> = LexFileTokenizer::tokenize(&input_content);

        // then
        with_settings!(
            {
                snapshot_path => snapshot_path,
                prepend_module_to_snapshot => false, // Avoid module path prefix in snapshot filename
            },
            {
                // Assert tokenization result (Ok or Err) against the YAML snapshot.
                // Run `cargo insta review` to approve changes on first run or after modifications.
                insta::assert_yaml_snapshot!(conf_name, result);
            }
        );
    }
}
