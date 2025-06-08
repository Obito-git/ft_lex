use crate::ArgLiteErr;

#[derive(Debug)]
pub struct StrType {
    pub name: String,
    pub alias: char,
    pub is_mandatory: bool,
    pub description: String,
    pub value: Option<String>,
}

#[derive(Debug)]
pub struct FlagType {
    pub name: String,
    pub alias: char,
    pub description: String,
    pub is_present: bool,
}

#[derive(Debug)]
pub enum DefinedArg<'a> {
    Str(&'a mut StrType),
    Flag(&'a mut FlagType),
}

impl DefinedArg<'_> {
    pub(crate) fn name(&self) -> &String {
        match self {
            DefinedArg::Str(arg_type) => &arg_type.name,
            DefinedArg::Flag(arg_type) => &arg_type.name,
        }
    }

    pub(crate) fn alias(&self) -> char {
        match self {
            DefinedArg::Str(arg_type) => arg_type.alias,
            DefinedArg::Flag(arg_type) => arg_type.alias,
        }
    }

    pub(crate) fn description(&self) -> &String {
        match self {
            DefinedArg::Str(arg_type) => &arg_type.description,
            DefinedArg::Flag(arg_type) => &arg_type.description,
        }
    }

    // TODO: find more elegant way
    pub(crate) fn set_present(&mut self, value: Option<String>) -> Result<(), ArgLiteErr> {
        match (self, value) {
            (DefinedArg::Str(arg_type), Some(value)) => arg_type.value = Some(value),
            (DefinedArg::Flag(arg_type), None) => arg_type.is_present = true,
            _ => unreachable!(),
        }
        Ok(())
    }

    pub(crate) fn is_mandatory(&self) -> bool {
        match self {
            DefinedArg::Str(arg_type) => arg_type.is_mandatory,
            DefinedArg::Flag(_) => false,
        }
    }

    pub(crate) fn has_value(&self) -> bool {
        match self {
            DefinedArg::Str(arg_type) => arg_type.value.is_some(),
            DefinedArg::Flag(_) => true,
        }
    }

    pub(crate) fn is_flag(&self) -> bool {
        match self {
            DefinedArg::Str(_) => false,
            DefinedArg::Flag(_) => true,
        }
    }
}
