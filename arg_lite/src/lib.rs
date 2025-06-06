use std::{
    collections::{BTreeMap, HashMap},
    str::FromStr,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgLiteErr {
    AmbigousName(String),
    AmbigousAlias(char),
    MandatoryArgIsntProvided(Vec<String>),
}

pub struct ArgLite<'a> {
    arg_names: BTreeMap<String, usize>,
    aliases: HashMap<char, usize>,
    args: Vec<DefinedArg<'a>>,
}

impl<'a> ArgLite<'a> {
    fn new(args: Vec<DefinedArg<'a>>) -> Result<Self, ArgLiteErr> {
        let mut arg_names = BTreeMap::new();
        let mut aliases = HashMap::new();

        for (i, a) in args.iter().enumerate() {
            if arg_names.insert(a.name(), i).is_some() {
                Err(ArgLiteErr::AmbigousName(a.name()))?;
            }
            if aliases.insert(a.alias(), i).is_some() {
                Err(ArgLiteErr::AmbigousAlias(a.alias()))?;
            }
        }
        Ok(Self {
            arg_names,
            aliases,
            args,
        })
    }

    fn assert_all_provided(&self) -> Result<(), ArgLiteErr> {
        let mut errors = vec![];
        for arg in &self.args {
            if arg.is_mandatory() && !arg.has_value() {
                errors.push(arg.name());
            }
        }
        if !errors.is_empty() {
            Err(ArgLiteErr::MandatoryArgIsntProvided(errors))
        } else {
            Ok(())
        }
    }

    pub fn parse(
        defined_args: Vec<DefinedArg>,
        source: impl Iterator<Item = String>,
    ) -> Result<(), ArgLiteErr> {
        let arg_lite = ArgLite::new(defined_args)?;

        arg_lite.assert_all_provided()?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct ArgType<T: FromStr>
where
    <T as FromStr>::Err: std::fmt::Debug,
{
    pub name: String,
    pub alias: char,
    pub is_mandatory: bool,
    pub description: String,
    pub value: Option<T>,
}

#[derive(Debug)]
pub enum DefinedArg<'a> {
    Str(&'a mut ArgType<String>),
    Bool(&'a mut ArgType<bool>),
}

impl<'a> DefinedArg<'a> {
    fn name(&self) -> String {
        match self {
            DefinedArg::Str(arg_type) => arg_type.name.clone(),
            DefinedArg::Bool(arg_type) => arg_type.name.clone(),
        }
    }

    fn alias(&self) -> char {
        match self {
            DefinedArg::Str(arg_type) => arg_type.alias,
            DefinedArg::Bool(arg_type) => arg_type.alias,
        }
    }

    fn set_value(&mut self, value: String) -> Result<(), String> {
        match self {
            DefinedArg::Str(arg_type) => arg_type.value = Some(value),
            DefinedArg::Bool(arg_type) => todo!(),
        }
        Ok(())
    }

    fn is_mandatory(&self) -> bool {
        match self {
            DefinedArg::Str(arg_type) => arg_type.is_mandatory,
            DefinedArg::Bool(arg_type) => arg_type.is_mandatory,
        }
    }

    fn has_value(&self) -> bool {
        match self {
            DefinedArg::Str(arg_type) => arg_type.value.is_some(),
            DefinedArg::Bool(arg_type) => arg_type.value.is_some(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{ArgLite, ArgLiteErr, ArgType, DefinedArg};

    #[test]
    fn ambigous_arg_name() {
        // given
        let arg_name = "test".to_string();
        let mut arg1 = ArgType {
            name: arg_name.clone(),
            alias: 't',
            is_mandatory: true,
            description: "Test arg desc".to_string(),
            value: None,
        };
        let mut arg2 = ArgType {
            name: arg_name.clone(),
            alias: 'p',
            is_mandatory: true,
            description: "Test arg desc".to_string(),
            value: None,
        };

        // when
        let res = ArgLite::parse(
            vec![DefinedArg::Str(&mut arg1), DefinedArg::Str(&mut arg2)],
            vec![].into_iter(),
        );

        // then
        assert_eq!(res, Err(ArgLiteErr::AmbigousName(arg_name)));
    }

    //TODO: add multiple
    #[test]
    fn one_mandatory_arg_isnt_provided() {
        // given
        let arg_name = "test".to_string();
        let mut arg1 = ArgType {
            name: arg_name.clone(),
            alias: 't',
            is_mandatory: true,
            description: "Test arg desc".to_string(),
            value: None,
        };

        // when
        let res = ArgLite::parse(vec![DefinedArg::Str(&mut arg1)], vec![].into_iter());

        // then
        assert_eq!(
            res,
            Err(ArgLiteErr::MandatoryArgIsntProvided(vec![arg_name]))
        );
    }
}
