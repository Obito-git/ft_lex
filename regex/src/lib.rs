mod ast;
mod token;

pub struct Regex {
    pub pattern: String,
}

impl Regex {
    pub fn new(pattern: String) -> Result<Self, String> {
        Ok(Self { pattern })
    }
}

enum RegexAst {
    Literal(char),
    Star(Box<RegexAst>),
    Either(Box<RegexAst>, Box<RegexAst>),
    Concat(Box<RegexAst>),
}

impl RegexAst {
    pub fn new(pattern: String) -> Result<Self, String> {
        todo!()
    }
}
