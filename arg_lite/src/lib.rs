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
    ValuesAreForbiddenWithMultiAliasSyntax(char),
}

pub struct ArgLite<'a, I>
where
    I: Iterator<Item = String>,
{
    arg_names: BTreeMap<String, usize>,
    aliases: HashMap<char, usize>,
    args: Vec<DefinedArg<'a>>,
    source: I,
}

impl<'a, I> ArgLite<'a, I>
where
    I: Iterator<Item = String>,
{
    fn new(args: Vec<DefinedArg<'a>>, source: I) -> Result<Self, ArgLiteErr> {
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
            source,
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

    //TODO: improve nested if
    fn handle_named_arg(&mut self, def_idx: usize) -> Result<(), ArgLiteErr> {
        let def = &mut self.args[def_idx];
        if def.is_flag() {
            def.set_present(None)?;
        } else {
            if let Some(value) = self.source.next() {
                def.set_present(Some(value))?;
            } else {
                Err(ArgLiteErr::ExpectedValueButGotNothing(def.name()))?
            }
        }
        Ok(())
    }

    fn handle_alias_arg(&mut self, def_idx: usize, has_one_alias: bool) -> Result<(), ArgLiteErr> {
        let def = &mut self.args[def_idx];
        if def.is_flag() {
            def.set_present(None)?;
        } else {
            if !has_one_alias {
                Err(ArgLiteErr::ValuesAreForbiddenWithMultiAliasSyntax(
                    def.alias(),
                ))?;
            }
            if let Some(value) = self.source.next() {
                def.set_present(Some(value))?;
            } else {
                Err(ArgLiteErr::ExpectedValueButGotNothing(def.name()))?
            }
        }
        Ok(())
    }

    fn parse_internal(defined_args: Vec<DefinedArg>, source: I) -> Result<Vec<String>, ArgLiteErr> {
        let mut arg_lite = ArgLite::new(defined_args, source)?;
        while let Some(cur) = arg_lite.source.next() {
            if let Some(arg_name) = cur.strip_prefix("--") {
                let arg_name = arg_name.to_string();
                let def_idx = arg_lite.arg_names.get(&arg_name).copied();
                def_idx
                    .map(|def_idx| arg_lite.handle_named_arg(def_idx))
                    .unwrap_or(Err(ArgLiteErr::UnknownOption(arg_name.to_string())))?;
            } else if let Some(arg_alias) = cur.strip_prefix("-") {
                let aliases_arg = arg_alias.to_string();
                let has_one_alias = aliases_arg.len() == 1;
                for alias in aliases_arg.chars() {
                    let def_idx = arg_lite.aliases.get(&alias).copied();
                    def_idx
                        .map(|def_idx| arg_lite.handle_alias_arg(def_idx, has_one_alias))
                        .unwrap_or(Err(ArgLiteErr::UnknownOption(aliases_arg.to_string())))?;
                }
            } else {
                return Ok(std::iter::once(cur).chain(arg_lite.source).collect());
            }
        }

        arg_lite.assert_all_provided()?;
        Ok(vec![])
    }

    pub fn parse(defined_args: Vec<DefinedArg>) -> Result<Vec<String>, ArgLiteErr> {
        ArgLite::parse_internal(defined_args, std::env::args().skip(1).peekable())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::args::*;
    use rstest::*;

    mod wrong_names_and_aliases {
        use super::*;

        #[rstest]
        #[case(vec!["--doesnt-exist"], "doesnt-exist")]
        #[case(vec!["--doesnt-exist", "--should-not-appear"], "doesnt-exist")]
        #[case(vec!["--doesnt-exist", "-k", "--should-not-appear"], "doesnt-exist")]
        fn wrong_names(#[case] input: Vec<&str>, #[case] expected_err: String) {
            //given
            let input = input.into_iter().map(|v| v.to_string()).collect::<Vec<_>>();

            //when
            let res = ArgLite::parse_internal(vec![], input.into_iter());

            //then
            assert_eq!(res, Err(ArgLiteErr::UnknownOption(expected_err)))
        }

        #[rstest]
        #[case(vec!["-k"], "k")]
        #[case(vec!["-d", "--should-not-appear"], "d")]
        #[case(vec!["-d", "-k", "--should-not-appear"], "d")]
        fn wrong_aliases(#[case] input: Vec<&str>, #[case] expected_err: String) {
            //given
            let input = input.into_iter().map(|v| v.to_string()).collect::<Vec<_>>();

            //when
            let res = ArgLite::parse_internal(vec![], input.into_iter());

            //then
            assert_eq!(res, Err(ArgLiteErr::UnknownOption(expected_err)))
        }
    }

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
                std::iter::empty(),
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
                std::iter::empty(),
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
                std::iter::empty(),
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
                std::iter::empty(),
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
                std::iter::empty(),
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
                std::iter::empty(),
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
            let input = vec![
                "tomate".to_string(),
                "carotte".to_string(),
                "concombre".to_string(),
            ];
            let expected = Ok(input.clone());

            //when
            let res = ArgLite::parse_internal(vec![], input.into_iter());

            //then
            assert_eq!(res, expected);
        }
        #[test]
        fn should_ignore_flags_after_first_arg() {
            //given
            let input = vec![
                "tomate".to_string(),
                "--file-name".to_string(),
                "the prev one goes after normal arg, so treated as the regular arg but not as the named one"
                    .to_string(),
            ];
            let expected = Ok(input.clone());

            //when
            let res = ArgLite::parse_internal(vec![], input.into_iter());

            //then
            assert_eq!(res, expected);
        }
        #[test]
        fn should_ignore_flags_alias_after_first_arg() {
            //given
            let input = vec![
                "tomate".to_string(),
                "-f".to_string(),
                "the prev one goes after normal arg, so treated as the regular arg but not as the named one"
                    .to_string(),
            ];
            let expected = Ok(input.clone());

            //when
            let res = ArgLite::parse_internal(vec![], input.into_iter());

            //then
            assert_eq!(res, expected);
        }

        #[test]
        fn should_return_nothing_when_only_named_args() {
            //given
            let input = vec!["-f".to_string(), "--ecole42".to_string()];
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
                input.into_iter(),
            );

            //then
            assert!(res.unwrap().is_empty())
        }

        #[test]
        fn should_return_only_arg_after_flags() {
            //given
            let input = vec![
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
                input.into_iter(),
            );

            //then
            assert_eq!(res, Ok(vec!["hello".to_string()]))
        }
    }

    mod str_types {
        use super::*;

        #[rstest]
        #[case(vec!["--arg".to_string()])]
        #[case(vec!["-t".to_string()])]
        fn should_fail_when_there_is_no_value(#[case] input: Vec<String>) {
            // given
            let name = "arg".to_string();
            let mut arg_type = StrType {
                name: name.clone(),
                alias: 't',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: Some("I'm mandatory but with val".to_string()),
            };

            // when
            let res =
                ArgLite::parse_internal(vec![DefinedArg::Str(&mut arg_type)], input.into_iter());

            // then
            assert_eq!(res, Err(ArgLiteErr::ExpectedValueButGotNothing(name)));
        }

        #[rstest]
        #[case(vec!["--arg", "value"], "value")]
        #[case(vec!["-t", "value"], "value")]
        fn should_parse_value(#[case] input: Vec<&str>, #[case] expected: String) {
            // given
            let input = input.into_iter().map(|v| v.to_string()).collect::<Vec<_>>();
            let name = "arg".to_string();
            let mut arg_type = StrType {
                name: name.clone(),
                alias: 't',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: None,
            };

            // when
            let res =
                ArgLite::parse_internal(vec![DefinedArg::Str(&mut arg_type)], input.into_iter());

            // then
            assert!(res.unwrap().is_empty());
            assert_eq!(arg_type.value, Some(expected))
        }

        #[rstest]
        #[case(vec!["--arg", "value"], "value")]
        #[case(vec!["-t", "value"], "value")]
        fn should_override_prev_value(#[case] input: Vec<&str>, #[case] expected: String) {
            // given
            let input = input.into_iter().map(|v| v.to_string()).collect::<Vec<_>>();
            let name = "arg".to_string();
            let mut arg_type = StrType {
                name: name.clone(),
                alias: 't',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: Some("some default value".to_string()),
            };

            // when
            let res =
                ArgLite::parse_internal(vec![DefinedArg::Str(&mut arg_type)], input.into_iter());

            // then
            assert!(res.unwrap().is_empty());
            assert_eq!(arg_type.value, Some(expected))
        }

        #[rstest]
        #[case(vec!["--arg", "value", "-f", "test.txt"], "value", "test.txt")]
        #[case(vec!["-t", "value", "--file-name", "test.txt"], "value", "test.txt")]
        fn should_work_with_multiple_different_named_args(
            #[case] input: Vec<&str>,
            #[case] expected_first: String,
            #[case] expected_second: String,
        ) {
            // given
            let input = input.into_iter().map(|v| v.to_string()).collect::<Vec<_>>();
            let mut arg_type1 = StrType {
                name: "arg".to_string(),
                alias: 't',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: Some("some default value".to_string()),
            };
            let mut arg_type2 = StrType {
                name: "file-name".to_string(),
                alias: 'f',
                is_mandatory: true,
                description: "Test arg desc".to_string(),
                value: Some("some default value".to_string()),
            };

            // when
            let res = ArgLite::parse_internal(
                vec![
                    DefinedArg::Str(&mut arg_type2),
                    DefinedArg::Str(&mut arg_type1),
                ],
                input.into_iter(),
            );

            // then
            assert!(res.unwrap().is_empty());
            assert_eq!(arg_type1.value, Some(expected_first));
            assert_eq!(arg_type2.value, Some(expected_second))
        }

        /*
        * multiple aliases
                #[rstest]
                #[case(vec!["--arg", "value", "-f", "test.txt"], "value", "test.txt")]
                #[case(vec!["-t", "value", "--file-name", "test.txt"], "value", "test.txt")]
                fn should_work_with_multiple_different_named_args(
                    #[case] input: Vec<&str>,
                    #[case] expected_first: String,
                    #[case] expected_second: String,
                ) {
                }
        */
    }
}
