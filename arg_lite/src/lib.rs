use std::collections::{BTreeMap, HashMap};

use args::DefinedArg;

pub mod args;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgLiteErr {
    AmbigousName(String),
    AmbigousAlias(char),
    MandatoryArgIsntProvided(Vec<String>),
    UnknownOption(String),
    UnknownAlias(char),
    ExpectedValueButGotNothing(String),
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

    fn parse_internal(
        defined_args: Vec<DefinedArg>,
        mut source: impl Iterator<Item = String>,
    ) -> Result<Vec<String>, ArgLiteErr> {
        let mut arg_lite = ArgLite::new(defined_args)?;
        while let Some(cur) = source.next() {
            if let Some(arg_name) = cur.strip_prefix("--") {
                if let Some(def_idx) = arg_lite.arg_names.get(arg_name) {
                    let mut def = &mut arg_lite.args[*def_idx];
                    if def.is_flag() {
                        def.set_present(None)?;
                    } else {
                        if let Some(value) = source.next() {
                            def.set_present(Some(value))?;
                        } else {
                            Err(ArgLiteErr::ExpectedValueButGotNothing(arg_name.to_string()))?
                        }
                    }
                } else {
                    Err(ArgLiteErr::UnknownOption(cur))?
                }
            } else if cur.starts_with("-") {
            } else {
                return Ok(std::iter::once(cur).chain(source).collect());
            }
        }

        arg_lite.assert_all_provided()?;
        Ok(vec![])
    }

    pub fn parse(defined_args: Vec<DefinedArg>) -> Result<Vec<String>, ArgLiteErr> {
        let a = std::env::args().skip(1).peekable();
        ArgLite::parse_internal(defined_args, std::env::args().skip(1).peekable())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::args::*;

    mod ambigous_values {

        use super::*;

        #[test]
        fn ambigous_arg_name_expect_err() {
            // given
            let arg_name = "test".to_string();
            let mut arg1 = StrType {
                name: arg_name.clone(),
                alias: 't',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: None,
            };
            let mut arg2 = StrType {
                name: arg_name.clone(),
                alias: 'p',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: None,
            };

            // when
            let res = ArgLite::parse_internal(
                vec![DefinedArg::Str(&mut arg1), DefinedArg::Str(&mut arg2)],
                vec![].into_iter(),
            );

            // then
            assert_eq!(res, Err(ArgLiteErr::AmbigousName(arg_name)));
        }

        #[test]
        fn ambigous_arg_alias_expect_err() {
            // given
            let arg_alias = 't';
            let mut arg1 = StrType {
                name: "arg1".to_string(),
                alias: arg_alias,
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: None,
            };
            let mut arg2 = StrType {
                name: "arg2".to_string(),
                alias: arg_alias,
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: None,
            };

            // when
            let res = ArgLite::parse_internal(
                vec![DefinedArg::Str(&mut arg1), DefinedArg::Str(&mut arg2)],
                vec![].into_iter(),
            );

            // then
            assert_eq!(res, Err(ArgLiteErr::AmbigousAlias(arg_alias)));
        }
    }
    mod mandatory_args {
        use super::*;

        #[test]
        fn one_mandatory_arg_isnt_provided_expect_err() {
            // given
            let arg_name = "test".to_string();
            let mut mandatory_arg = StrType {
                name: arg_name.clone(),
                alias: 't',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: None,
            };

            // when
            let res = ArgLite::parse_internal(
                vec![DefinedArg::Str(&mut mandatory_arg)],
                vec![].into_iter(),
            );

            // then
            assert_eq!(
                res,
                Err(ArgLiteErr::MandatoryArgIsntProvided(vec![arg_name]))
            );
        }

        #[test]
        fn zero_mandotory_should_ok() {
            // given
            let mut optional_arg = StrType {
                name: "arg".to_string(),
                alias: 't',
                is_mandatory: false,
                description: "Test arg desc".to_string(),
                value: None,
            };

            // when
            let res = ArgLite::parse_internal(
                vec![DefinedArg::Str(&mut optional_arg)],
                vec![].into_iter(),
            );

            // then
            assert_eq!(res, Ok(vec![]))
        }

        #[test]
        fn few_mandotory_should_expect_err() {
            // given
            let arg1_name = "arg1".to_string();
            let arg2_name = "arg2".to_string();
            let mut mandatory_arg1 = StrType {
                name: arg1_name.clone(),
                alias: 'k',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: None,
            };
            let mut optional_flag = FlagType {
                name: "arg3".to_string(),
                alias: 't',
                description: "Test arg desc".to_string(),
                is_present: false,
            };
            let mut mandatory_arg2 = StrType {
                name: arg2_name.clone(),
                alias: 'o',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: None,
            };

            // when
            let res = ArgLite::parse_internal(
                vec![
                    DefinedArg::Str(&mut mandatory_arg1),
                    DefinedArg::Flag(&mut optional_flag),
                    DefinedArg::Str(&mut mandatory_arg2),
                ],
                vec![].into_iter(),
            );

            // then
            assert_eq!(
                res,
                Err(ArgLiteErr::MandatoryArgIsntProvided(vec![
                    arg1_name, arg2_name
                ]))
            );
        }

        #[test]
        fn mandotory_but_there_is_an_initial_val_should_ok() {
            // given
            let mut optional_arg = StrType {
                name: "arg".to_string(),
                alias: 't',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: Some("I'm mandatory but with val".to_string()),
            };

            // when
            let res = ArgLite::parse_internal(
                vec![DefinedArg::Str(&mut optional_arg)],
                vec![].into_iter(),
            );

            // then
            assert!(res.is_ok())
        }
    }

    mod arguments {
        use super::*;

        #[test]
        fn should_return_arguments() {
            //given
            let args = vec![
                "tomate".to_string(),
                "carotte".to_string(),
                "concombre".to_string(),
            ];
            let expected = Ok(args.clone());

            //when
            let res = ArgLite::parse_internal(vec![], args.into_iter());

            //then
            assert_eq!(res, expected);
        }
        #[test]
        fn should_ignore_flags_after_first_arg() {
            //given
            let args = vec![
                "tomate".to_string(),
                "--file-name".to_string(),
                "the prev one goes after arg, so treated as arg but not option".to_string(),
            ];
            let expected = Ok(args.clone());

            //when
            let res = ArgLite::parse_internal(vec![], args.into_iter());

            //then
            assert_eq!(res, expected);
        }
        #[test]
        fn should_ignore_flags_alias_after_first_arg() {
            //given
            let args = vec![
                "tomate".to_string(),
                "-f".to_string(),
                "the prev one goes after arg, so treated as arg but not option".to_string(),
            ];
            let expected = Ok(args.clone());

            //when
            let res = ArgLite::parse_internal(vec![], args.into_iter());

            //then
            assert_eq!(res, expected);
        }

        #[test]
        fn should_return_nothing_when_no_args() {
            //given
            let only_options = vec!["-f".to_string(), "--ecole42".to_string()];
            let mut arg1 = FlagType {
                name: "file".to_string(),
                alias: 'f',
                description: "Test arg desc".to_string(),
                is_present: false,
            };
            let mut arg2 = FlagType {
                name: "ecole42".to_string(),
                alias: 'e',
                description: "Test arg desc".to_string(),
                is_present: false,
            };

            // when
            let res = ArgLite::parse_internal(
                vec![DefinedArg::Flag(&mut arg1), DefinedArg::Flag(&mut arg2)],
                only_options.into_iter(),
            );

            //then
            assert!(res.unwrap().is_empty())
        }

        #[test]
        fn should_return_only_args_after_options() {
            //given
            let only_options = vec![
                "-f".to_string(),
                "--ecole42".to_string(),
                "hello".to_string(),
            ];
            let mut arg1 = FlagType {
                name: "file".to_string(),
                alias: 'f',
                description: "Test arg desc".to_string(),
                is_present: false,
            };
            let mut arg2 = FlagType {
                name: "ecole42".to_string(),
                alias: 'e',
                description: "Test arg desc".to_string(),
                is_present: false,
            };

            // when
            let res = ArgLite::parse_internal(
                vec![DefinedArg::Flag(&mut arg1), DefinedArg::Flag(&mut arg2)],
                only_options.into_iter(),
            );

            //then
            assert_eq!(res, Ok(vec!["hello".to_string()]))
        }
    }

    mod str_types {
        use super::*;

        #[test]
        fn should_fail_when_there_is_no_value() {
            // given
            let name = "arg".to_string();
            let mut optional_arg = StrType {
                name: name.clone(),
                alias: 't',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: Some("I'm mandatory but with val".to_string()),
            };
            let args = vec!["--arg".to_string()];

            // when
            let res =
                ArgLite::parse_internal(vec![DefinedArg::Str(&mut optional_arg)], args.into_iter());

            // then
            assert_eq!(res, Err(ArgLiteErr::ExpectedValueButGotNothing(name)));
        }
    }
}
