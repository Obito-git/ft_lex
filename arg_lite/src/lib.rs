use std::collections::{BTreeMap, HashMap};

use args::DefinedArg;

pub mod args;

const OPT_NAME: &str = "name";
const ALIAS: &str = "alias";
const DESCRIPTION: &str = "description";
const REQ: &str = "req";
const DEFAULT: &str = "default";
const FLAGS: &str = "Flags (with no value required):";
const VALUE_OPTION: &str = "Option (expects a value as next argument):";

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArgLiteErr {
    AmbigousName(String),
    AmbigousAlias(char),
    MandatoryArgIsntProvided(Vec<String>),
    UnknownOption(String),
    UnknownAlias(char),
    ExpectedValueButGotNothing(String),
    ValuesAreForbiddenWithMultiAliasSyntax(char),
    HelpIsReserved,
}

pub struct ArgLite<'a, I>
where
    I: Iterator<Item = String>,
{
    arg_names: BTreeMap<String, usize>,
    aliases: HashMap<char, usize>,
    args: Vec<DefinedArg<'a>>,
    source: I,
    usage: Option<String>,
}

impl<'a, I> ArgLite<'a, I>
where
    I: Iterator<Item = String>,
{
    fn new(
        usage: Option<String>,
        args: Vec<DefinedArg<'a>>,
        source: I,
    ) -> Result<Self, ArgLiteErr> {
        let mut arg_names = BTreeMap::new();
        let mut aliases = HashMap::new();

        for (i, a) in args.iter().enumerate() {
            if a.name() == "help" || a.alias() == 'h' {
                Err(ArgLiteErr::HelpIsReserved)?;
            }
            if arg_names.insert(a.name().clone(), i).is_some() {
                Err(ArgLiteErr::AmbigousName(a.name().to_string()))?;
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
            usage,
        })
    }

    fn assert_all_provided(&self) -> Result<(), ArgLiteErr> {
        let mut errors = vec![];
        for arg in &self.args {
            if arg.is_mandatory() && !arg.has_value() {
                errors.push(arg.name().clone());
            }
        }
        if !errors.is_empty() {
            Err(ArgLiteErr::MandatoryArgIsntProvided(errors))
        } else {
            Ok(())
        }
    }

    fn get_longest_name(it: impl Iterator<Item = &'a DefinedArg<'a>>) -> usize {
        it.map(|defined_arg| defined_arg.name().len())
            .chain(std::iter::once(OPT_NAME.len()))
            .max()
            .unwrap_or(0)
    }

    fn map_help_line(
        name: &str,
        alias: &str,
        description: &str,
        is_mandatory: Option<String>,
        longest: usize,
    ) -> String {
        format!(
            "{}{}\t{}\t{}{}",
            name,
            " ".repeat(longest - name.len()),
            alias,
            is_mandatory.map(|v| format!("{v}\t")).unwrap_or_default(),
            description
        )
    }

    fn generate_help(&self) -> Vec<String> {
        let mut res = Vec::with_capacity(self.args.len());

        if let Some(usage) = &self.usage {
            res.push(usage.to_string());
            res.push(String::new());
        }

        let flags_it = self
            .arg_names
            .values()
            .map(|idx| &self.args[*idx])
            .filter(|v| v.is_flag());
        let longest_flag_name = ArgLite::<I>::get_longest_name(flags_it.clone());

        res.push(FLAGS.to_string());
        res.push(ArgLite::<I>::map_help_line(
            OPT_NAME,
            ALIAS,
            DESCRIPTION,
            None,
            longest_flag_name,
        ));

        for arg in flags_it {
            res.push(ArgLite::<I>::map_help_line(
                arg.name(),
                &arg.alias().to_string(),
                arg.description(),
                None,
                longest_flag_name,
            ));
        }

        let opt_it = self
            .arg_names
            .values()
            .map(|idx| &self.args[*idx])
            .filter(|v| !v.is_flag());
        let longest_opt_name = ArgLite::<I>::get_longest_name(opt_it.clone());

        res.push(String::new());
        res.push(VALUE_OPTION.to_string());
        res.push(ArgLite::<I>::map_help_line(
            OPT_NAME,
            ALIAS,
            DESCRIPTION,
            Some(REQ.to_string()),
            longest_opt_name,
        ));

        for arg in opt_it {
            res.push(ArgLite::<I>::map_help_line(
                arg.name(),
                &arg.alias().to_string(),
                arg.description(),
                Some(arg.is_mandatory().to_string()),
                longest_opt_name,
            ));
        }

        res
    }

    //TODO: improve nested if
    fn handle_named_arg(&mut self, def_idx: usize) -> Result<(), ArgLiteErr> {
        let def = &mut self.args[def_idx];
        if def.is_flag() {
            def.set_present(None)?;
        } else if let Some(value) = self.source.next() {
            def.set_present(Some(value))?;
        } else {
            Err(ArgLiteErr::ExpectedValueButGotNothing(
                def.name().to_string(),
            ))?
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
                Err(ArgLiteErr::ExpectedValueButGotNothing(
                    def.name().to_string(),
                ))?
            }
        }
        Ok(())
    }

    fn parse_internal(
        usage: Option<String>,
        defined_args: Vec<DefinedArg>,
        source: I,
    ) -> Result<Vec<String>, ArgLiteErr> {
        let mut arg_lite = ArgLite::new(usage, defined_args, source)?;
        while let Some(cur) = arg_lite.source.next() {
            if let Some(arg_name) = cur.strip_prefix("--") {
                let arg_name = arg_name.to_string();
                let def_idx = arg_lite.arg_names.get(&arg_name).copied();
                def_idx
                    .map(|def_idx| arg_lite.handle_named_arg(def_idx))
                    .unwrap_or(Err(ArgLiteErr::UnknownOption(arg_name)))?;
            } else if let Some(arg_alias) = cur.strip_prefix("-") {
                let has_one_alias = arg_alias.len() == 1;
                for alias in arg_alias.chars() {
                    let def_idx = arg_lite.aliases.get(&alias).copied();
                    def_idx
                        .map(|def_idx| arg_lite.handle_alias_arg(def_idx, has_one_alias))
                        .unwrap_or(Err(ArgLiteErr::UnknownOption(arg_alias.to_string())))?;
                }
            } else {
                return Ok(std::iter::once(cur).chain(arg_lite.source).collect());
            }
        }

        arg_lite.assert_all_provided()?;
        Ok(vec![])
    }

    pub fn parse(
        defined_args: Vec<DefinedArg>,
        usage: Option<String>,
    ) -> Result<Vec<String>, ArgLiteErr> {
        ArgLite::parse_internal(usage, defined_args, std::env::args().skip(1).peekable())
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
        #[case("help", 'k')]
        #[case("arg", 'h')]
        #[case("help", 'h')]
        fn usage_of_reserved_name_and_aliases_for_help(#[case] name: String, #[case] alias: char) {
            // given
            let mut defined_arg = StrType {
                name,
                alias,
                is_mandatory: false,
                description: "not valid".to_string(),
                value: None,
            };

            //when
            let res = ArgLite::parse_internal(
                None,
                vec![DefinedArg::Str(&mut defined_arg)],
                std::iter::empty(),
            );

            //then
            assert_eq!(res, Err(ArgLiteErr::HelpIsReserved))
        }

        #[rstest]
        #[case(vec!["--doesnt-exist"], "doesnt-exist")]
        #[case(vec!["--doesnt-exist", "--should-not-appear"], "doesnt-exist")]
        #[case(vec!["--doesnt-exist", "-k", "--should-not-appear"], "doesnt-exist")]
        fn wrong_names(#[case] input: Vec<&str>, #[case] expected_err: String) {
            //given
            let input = input.into_iter().map(|v| v.to_string()).collect::<Vec<_>>();

            //when
            let res = ArgLite::parse_internal(None, vec![], input.into_iter());

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
            let res = ArgLite::parse_internal(None, vec![], input.into_iter());

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
                None,
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
                None,
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
                None,
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
                None,
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
                None,
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
                None,
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
            let res = ArgLite::parse_internal(None, vec![], input.into_iter());

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
            let res = ArgLite::parse_internal(None, vec![], input.into_iter());

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
            let res = ArgLite::parse_internal(None, vec![], input.into_iter());

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
                None,
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
                None,
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
            let res = ArgLite::parse_internal(
                None,
                vec![DefinedArg::Str(&mut arg_type)],
                input.into_iter(),
            );

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
            let res = ArgLite::parse_internal(
                None,
                vec![DefinedArg::Str(&mut arg_type)],
                input.into_iter(),
            );

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
            let res = ArgLite::parse_internal(
                None,
                vec![DefinedArg::Str(&mut arg_type)],
                input.into_iter(),
            );

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
                None,
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

        #[test]
        fn should_work_with_glewed_flags() {
            // given
            let mut flag_f = FlagType {
                name: "f".to_string(),
                alias: 'f',
                description: "f arg".to_string(),
                is_present: false,
            };
            let mut flag_t = FlagType {
                name: "t".to_string(),
                alias: 't',
                description: "t arg".to_string(),
                is_present: false,
            };
            let mut flag_p = FlagType {
                name: "p".to_string(),
                alias: 'p',
                description: "p arg".to_string(),
                is_present: false,
            };
            let input = vec!["-tp".to_string()];

            // when
            let res = ArgLite::parse_internal(
                None,
                vec![
                    DefinedArg::Flag(&mut flag_f),
                    DefinedArg::Flag(&mut flag_t),
                    DefinedArg::Flag(&mut flag_p),
                ],
                input.into_iter(),
            );

            // then
            assert!(res.unwrap().is_empty());
            assert!(!flag_f.is_present);
            assert!(flag_t.is_present);
            assert!(flag_p.is_present);
        }

        #[test]
        fn named_arg_with_value_should_fail_when_in_glewed_flags() {
            // given
            let mut flag_f = FlagType {
                name: "f".to_string(),
                alias: 'f',
                description: "f arg".to_string(),
                is_present: false,
            };
            let mut expects_value = StrType {
                name: "p".to_string(),
                alias: 'p',
                description: "p arg".to_string(),
                is_mandatory: false,
                value: None,
            };
            let input = vec!["-fp".to_string()];

            // when
            let res = ArgLite::parse_internal(
                None,
                vec![
                    DefinedArg::Flag(&mut flag_f),
                    DefinedArg::Str(&mut expects_value),
                ],
                input.into_iter(),
            );

            // then
            assert_eq!(
                res,
                Err(ArgLiteErr::ValuesAreForbiddenWithMultiAliasSyntax('p'))
            )
        }
    }

    /*
    mod generate_help {

        use super::*;

        #[test]
        fn generate_help_with_no_usage() {
            // given
            let mut flag_f = FlagType {
                name: "filename".to_string(),
                alias: 'f',
                description: "here is the description of the filename flag".to_string(),
                is_present: false,
            };
            let mut expects_value = StrType {
                name: "path".to_string(),
                alias: 'p',
                description: "GGGstrtype arg".to_string(),
                is_mandatory: false,
                value: None,
            };
            let arg_lite = ArgLite::new(
                None,
                vec![
                    DefinedArg::Flag(&mut flag_f),
                    DefinedArg::Str(&mut expects_value),
                ],
                std::iter::empty(),
            )
            .unwrap();

            // when
            let help_content = arg_lite.generate_help();

            // then
            help_content.iter().for_each(|line| println!("{}", line));
        }
    }
    */
}
