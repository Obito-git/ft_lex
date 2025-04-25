use std::process::exit;
use std::{env, fs};
use ft_lex_lib::error::{LexError, LexErrorKind};
use ft_lex_lib::tokenizer::LexFileTokenizer;

//TODO: just basic example by AI, to be refactored
pub fn format_lex_error(error: &LexError, source: &str, filename: String) -> String {
    let mut message = String::new();

    if let Some(pos) = error.position() {
        message.push_str(&format!("{}:{}:{}: ", filename, pos.cur_line, pos.cur_pos));
    }

    match error.kind() {
        LexErrorKind::NewLineExpected => {
            message.push_str("Error: Expected a newline character");
        }
        LexErrorKind::Internal(msg) => {
            message.push_str(&format!("Internal Lexer Error: {}", msg));
            return message;
        }
        _ => unimplemented!()
    }
    message.push('\n');

    if let Some(pos) = error.position() {
        if let Some(line_str) = source.lines().nth(pos.cur_line.saturating_sub(1)) {
            message.push_str(line_str);
            message.push('\n');
            let pointer_space = " ".repeat(pos.cur_pos.saturating_sub(1));
            message.push_str(&pointer_space);
            message.push('^');
            message.push('\n');
        } else {
            message.push_str("(Could not retrieve source line for context)\n");
        }
    }

    message
}


fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() != 1 {
        eprintln!("The program takes exactly one argument - the .l file location");
        exit(1);
    }
    match fs::read_to_string(args[0].clone()) {
        Ok(data) => match LexFileTokenizer::tokenize(&data) {
            Ok(tokens) => println!("{:?}", tokens),
            Err(e) => println!("{}", format_lex_error(&e, &data, args[0].clone())),
        },
        Err(err) => {
            eprintln!("Can't read the file: {err}")
        }
    }
}
