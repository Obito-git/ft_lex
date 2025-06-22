use std::collections::{HashSet, VecDeque};

#[derive(Debug, Clone, PartialEq, Eq)]
enum TransitionKind {
    Symbol(char),
    Epsilon,
    Wildcard,
    WildcardExcept(HashSet<char>),
}

#[derive(Clone)]
struct NfaTransition {
    pub start: usize,
    pub end: usize,
    pub kind: TransitionKind,
}

#[derive(Clone)]
pub(crate) struct Nfa {
    states_count: usize,             // for now only simple counter
    transitions: Vec<NfaTransition>, // TODO: not vec, probably map or ?
    start_state: usize,
    accept_state: usize,
}

impl NfaTransition {
    fn matches(&self, c: char) -> bool {
        match &self.kind {
            TransitionKind::Symbol(symbol) => *symbol == c,
            TransitionKind::Epsilon => false,
            TransitionKind::Wildcard => c != '\n',
            TransitionKind::WildcardExcept(set) => !set.contains(&c),
        }
    }

    fn is_epsilon(&self) -> bool {
        self.kind == TransitionKind::Epsilon
    }
}

impl Nfa {
    fn add_transition(&mut self, start: usize, end: usize, kind: TransitionKind) {
        self.transitions.push(NfaTransition { start, end, kind });
    }

    fn add_state(&mut self) -> usize {
        self.states_count += 1;
        self.states_count
    }

    fn new() -> Self {
        Nfa {
            transitions: vec![],
            start_state: 0,
            accept_state: 1,
            states_count: 2,
        }
    }

    pub fn from_epsilon() -> Self {
        let mut nfa = Nfa::new();
        nfa.add_transition(nfa.start_state, nfa.accept_state, TransitionKind::Epsilon);
        nfa
    }

    pub fn from_wildcard() -> Self {
        let mut nfa = Nfa::new();
        nfa.add_transition(nfa.start_state, nfa.accept_state, TransitionKind::Wildcard);
        nfa
    }

    pub fn from_char(c: char) -> Self {
        let mut nfa = Nfa::new();
        nfa.add_transition(0, 1, TransitionKind::Symbol(c));
        nfa
    }

    pub fn from_char_set(is_negated: bool, set: &HashSet<char>) -> Self {
        let mut nfa = Self::new();
        let start = nfa.start_state;
        let accept = nfa.accept_state;

        if !is_negated {
            for c in set {
                nfa.add_transition(start, accept, TransitionKind::Symbol(*c));
            }
        } else {
            nfa.add_transition(start, accept, TransitionKind::WildcardExcept(set.clone()));
        }

        nfa
    }

    pub fn concatenate(&mut self, other: &Self) {
        let offset = self.states_count;
        let old_accept = self.accept_state;

        // merge states count
        self.states_count += other.states_count;

        // merge transitions but update including offsets
        for t in &other.transitions {
            self.add_transition(t.start + offset, t.end + offset, t.kind.clone());
        }

        // connect two machines using the epsilon
        self.add_transition(
            old_accept,
            other.start_state + offset,
            TransitionKind::Epsilon,
        );

        // new accept state is the other's accept state but need to respect the offset
        self.accept_state = other.accept_state + offset;
    }

    pub fn alternate(&mut self, other: &Self) {
        let old_start = self.start_state;
        let old_accept = self.accept_state;
        let offset = self.states_count;

        self.states_count += other.states_count;

        // merge transitions but update including offsets
        for t in &other.transitions {
            self.add_transition(t.start + offset, t.end + offset, t.kind.clone());
        }

        // add new state and make it new start state
        let new_start_state = self.add_state();
        self.start_state = new_start_state;

        // add 2 epsilon transitions to the start of both NFAs
        self.add_transition(new_start_state, old_start, TransitionKind::Epsilon);
        self.add_transition(
            new_start_state,
            other.start_state + offset,
            TransitionKind::Epsilon,
        );

        // add new state and make it new accept_state
        let new_accept_state = self.add_state();
        self.accept_state = new_accept_state;

        // add 2 epsilon transitions from old accepts to the new one
        self.add_transition(old_accept, new_accept_state, TransitionKind::Epsilon);
        self.add_transition(
            other.accept_state + offset,
            new_accept_state,
            TransitionKind::Epsilon,
        );
    }

    pub fn kleene_star(&mut self) {
        let old_start = self.start_state;
        let old_accept = self.accept_state;

        let new_start = self.add_state();
        let new_accept = self.add_state();

        self.start_state = new_start;
        self.accept_state = new_accept;

        // epsilon from old accept to old start
        self.add_transition(old_accept, old_start, TransitionKind::Epsilon);

        // epsilons from new start to old start and new accept
        self.add_transition(new_start, old_start, TransitionKind::Epsilon);
        self.add_transition(new_start, new_accept, TransitionKind::Epsilon);

        // epsilon from old accept to new accept
        self.add_transition(old_accept, new_accept, TransitionKind::Epsilon);
    }

    pub fn one_or_more(&mut self) {
        self.add_transition(self.accept_state, self.start_state, TransitionKind::Epsilon);
    }

    pub fn zero_or_one(&mut self) {
        let old_start = self.start_state;
        let old_accept = self.accept_state;

        let new_start = self.add_state();
        let new_accept = self.add_state();

        self.start_state = new_start;
        self.accept_state = new_accept;

        // epsilon from new start to old start
        self.add_transition(new_start, old_start, TransitionKind::Epsilon);

        // epsilon from new start to new accept
        self.add_transition(new_start, new_accept, TransitionKind::Epsilon);

        // epsilon from old accept to new accept
        self.add_transition(old_accept, new_accept, TransitionKind::Epsilon);
    }

    // we assume that lower and upper are validated
    pub fn from_range(base: &Self, lower: u16, upper: Option<u16>) -> Self {
        // lower 0 is just an epsilon, and epsilon can be start for other as well
        let mut res = Self::from_epsilon();
        // add base "lower" times, a{3.. == aaa
        for _ in 0..lower {
            res.concatenate(base);
        }

        if let Some(upper) = upper {
            // upper bound same as ?, for example a{2,4} == aaa?a?
            let mut optional_base = base.clone();
            optional_base.zero_or_one();
            for _ in 0..(upper - lower) {
                res.concatenate(&optional_base);
            }
        } else {
            // {n,} means n times base and base*
            // a{2,} == aaa*. 2 a are mandatory and a zero or more
            let mut star_base = base.clone();
            star_base.kleene_star();
            res.concatenate(&star_base);
        }

        res
    }

    fn follow_epsilons(&self, initial_states: &[usize]) -> HashSet<usize> {
        let mut reachable = HashSet::new();
        let mut queue: VecDeque<usize> = VecDeque::new();

        for &state in initial_states {
            if reachable.insert(state) {
                queue.push_back(state);
            }
        }

        while let Some(cur) = queue.pop_front() {
            for t in &self.transitions {
                if t.start == cur && t.is_epsilon() && reachable.insert(t.end) {
                    queue.push_back(t.end);
                }
            }
        }

        reachable
    }

    pub(crate) fn is_exact_match(&self, s: &str) -> bool {
        let mut current_states = Self::follow_epsilons(self, &[self.start_state]);
        for c in s.chars() {
            let next_states_after_char: Vec<usize> = current_states
                .iter()
                .flat_map(|&state| {
                    self.transitions
                        .iter()
                        .filter(move |t| t.start == state && t.matches(c))
                        .map(|t| t.end)
                })
                .collect();

            current_states = self.follow_epsilons(&next_states_after_char);
            if current_states.is_empty() {
                return false;
            }
        }

        current_states.contains(&self.accept_state)
    }

    //TODO: Do I need it in NFA? probably only in DFA
    pub(crate) fn find(&self, s: &str) -> Option<()> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    mod from_char {
        use super::*;

        #[test]
        fn should_match_nfa_char_and_dont_any_other() {
            for i in 32..=126 {
                // build nfa for each char
                let pattern_char = i as u8 as char;
                let nfa = Nfa::from_char(pattern_char);

                // should match the same char
                assert!(nfa.is_exact_match(&pattern_char.to_string()));

                // shouldn't match any other one
                for x in 32..=126 {
                    let not_pattern_char = x as u8 as char;
                    if i != x {
                        assert!(!nfa.is_exact_match(&not_pattern_char.to_string()));
                    }
                }
            }
        }

        #[rstest]
        #[case('a', "aa")]
        #[case('a', "aaa")]
        #[case('a', "bb")]
        #[case('a', "ab")]
        #[case('a', "ba")]
        #[case('a', "bab")]
        #[case('a', "")]
        #[case('1', "11")]
        #[case('1', "222")]
        #[case('@', "@.@")]
        #[case('$', "$$")]
        fn shouldnt_match_any_string(#[case] nfa_char: char, #[case] s: &str) {
            //given
            let nfa = Nfa::from_char(nfa_char);

            //when
            let matched = nfa.is_exact_match(s);

            //then
            assert!(!matched);
        }
    }

    mod concatenate {
        use super::*;

        #[rstest]
        #[case('a', 'b', "ab")]
        #[case('c', 'd', "cd")]
        #[case('1', '2', "12")]
        #[case('$', '%', "$%")]
        #[case('x', '9', "x9")]
        #[case('7', '&', "7&")]
        #[case('z', 'z', "zz")]
        fn should_match_concatenated_string(
            #[case] char1: char,
            #[case] char2: char,
            #[case] expected_str: &str,
        ) {
            //given
            let mut nfa1 = Nfa::from_char(char1);
            let nfa2 = Nfa::from_char(char2);

            //when
            nfa1.concatenate(&nfa2);
            let matched = nfa1.is_exact_match(expected_str);

            //then
            assert!(matched);
        }

        #[rstest]
        #[case('a', 'b', "a")]
        #[case('a', 'b', "abc")]
        #[case('x', 'y', "xy ")]
        #[case('a', 'b', "")]
        #[case('a', 'b', "ba")]
        #[case('1', '2', "21")]
        #[case('a', 'b', "b")]
        #[case('a', 'b', "xy")]
        #[case('a', 'b', "a_b")]
        #[case('a', 'b', "aa")]
        #[case('a', 'b', "bb")]
        fn should_not_match_concatenated_string(
            #[case] char1: char,
            #[case] char2: char,
            #[case] expected_str: &str,
        ) {
            //given
            let mut nfa1 = Nfa::from_char(char1);
            let nfa2 = Nfa::from_char(char2);

            //when
            nfa1.concatenate(&nfa2);
            let matched = nfa1.is_exact_match(expected_str);

            //then
            assert!(!matched);
        }
    }

    mod alternate {
        use super::*;

        #[rstest]
        #[case('a', 'b', "a")]
        #[case('a', 'b', "b")]
        #[case('1', '2', "1")]
        #[case('1', '2', "2")]
        #[case('$', '%', "$")]
        #[case('$', '%', "%")]
        #[case('z', 'z', "z")]
        fn should_match_alternate_string(
            #[case] char1: char,
            #[case] char2: char,
            #[case] input_str: &str,
        ) {
            //given
            let mut nfa1 = Nfa::from_char(char1);
            let nfa2 = Nfa::from_char(char2);

            //when
            nfa1.alternate(&nfa2);
            let matched = nfa1.is_exact_match(input_str);

            //then
            assert!(matched);
        }

        #[rstest]
        #[case('a', 'b', "c")]
        #[case('1', '2', "3")]
        #[case('a', 'b', "ab")]
        #[case('a', 'b', "ba")]
        #[case('a', 'b', "aa")]
        #[case('a', 'b', "bb")]
        #[case('c', 'd', "cc")]
        #[case('x', 'y', "yz")]
        #[case('a', 'b', "")]
        fn shouldnt_match_alternate_string(
            #[case] char1: char,
            #[case] char2: char,
            #[case] input_str: &str,
        ) {
            //given
            let mut nfa1 = Nfa::from_char(char1);
            let nfa2 = Nfa::from_char(char2);

            //when
            nfa1.alternate(&nfa2);
            let matched = nfa1.is_exact_match(input_str);

            //then
            assert!(!matched);
        }
    }

    mod kleene_star {
        use super::*;

        #[rstest]
        #[case('a', "")]
        #[case('a', "a")]
        #[case('b', "bb")]
        #[case('c', "ccccc")]
        #[case('1', "1111")]
        #[case('$', "$$")]
        fn should_match_kleene_star_string(#[case] nfa_char: char, #[case] input_str: &str) {
            //given
            let mut base_nfa = Nfa::from_char(nfa_char);

            //when
            base_nfa.kleene_star();
            let matched = base_nfa.is_exact_match(input_str);

            //then
            assert!(matched);
        }
        #[rstest]
        #[case('a', "b")]
        #[case('a', "bbb")]
        #[case('a', "aaab")]
        #[case('a', "baaa")]
        #[case('a', "aabaa")]
        #[case('a', "bc")]
        #[case('1', "1121")]
        #[case('@', "@@.@@")]
        fn shouldnt_match_kleene_star_string(#[case] nfa_char: char, #[case] input_str: &str) {
            //given
            let mut base_nfa = Nfa::from_char(nfa_char);

            //when
            base_nfa.kleene_star();
            let matched = base_nfa.is_exact_match(input_str);

            //then
            assert!(!matched);
        }
    }

    // This module would be inside your main test module, next to the other Kleene star tests.
    mod kleene_star_with_alternation {
        use super::*;
        use rstest::rstest;

        #[rstest]
        #[case('a', 'b', "")]
        #[case('a', 'b', "a")]
        #[case('a', 'b', "b")]
        #[case('x', 'y', "xx")]
        #[case('x', 'y', "yyyy")]
        #[case('a', 'b', "ab")]
        #[case('a', 'b', "ba")]
        #[case('0', '1', "010101")]
        #[case('0', '1', "111000101")]
        #[case('#', '@', "#@#@##@@")]
        fn should_match_alternation_with_kleene_star(
            #[case] c1: char,
            #[case] c2: char,
            #[case] input: &str,
        ) {
            // given: build (c1|c2)*
            let mut nfa1 = Nfa::from_char(c1);
            let nfa2 = Nfa::from_char(c2);
            nfa1.alternate(&nfa2);
            nfa1.kleene_star();

            // when
            let matched = nfa1.is_exact_match(input);

            // then
            assert!(matched);
        }

        #[rstest]
        #[case('a', 'b', "c")]
        #[case('a', 'b', "ac")]
        #[case('a', 'b', "abc")]
        #[case('a', 'b', "ca")]
        #[case('a', 'b', "bca")]
        #[case('x', 'y', "xxyzyxx")]
        #[case('0', '1', "0001112000111")]
        #[case('a', 'b', "a b a")]
        fn shouldnt_match_alternation_with_kleene_star(
            #[case] c1: char,
            #[case] c2: char,
            #[case] input: &str,
        ) {
            // given: build (c1|c2)*
            let mut nfa1 = Nfa::from_char(c1);
            let nfa2 = Nfa::from_char(c2);
            nfa1.alternate(&nfa2);

            // when
            nfa1.kleene_star();
            let matched = nfa1.is_exact_match(input);

            // then
            assert!(!matched);
        }
    }

    mod wildcard {
        use super::*;

        #[test]
        fn should_match_every_char_except_newline() {
            // given
            let nfa = Nfa::from_wildcard();

            // when && then
            // should not match the new line
            assert!(!nfa.is_exact_match("\n"));

            // should match any other single printable ASCII character
            for code_point in 32..=126 {
                let c = code_point as u8 as char;
                if c != '\n' {
                    assert!(nfa.is_exact_match(&c.to_string()));
                }
            }
        }

        #[rstest]
        #[case("a.c", "abc")]
        #[case("a.c", "axc")]
        #[case("a.c", "a$c")]
        #[case("..", "ab")]
        #[case("..", "12")]
        #[case(".*", "")]
        #[case(".*", "abc")]
        #[case(".*", "123!@#$")]
        #[case(".+", "a")]
        #[case(".+", "abc")]
        #[case(".+", " a b ")]
        #[case("a.+c", "ab_c")]
        #[case("a.+c", "a--c")]
        #[case(".?", "")]
        #[case(".?", "a")]
        #[case(".?", "$")]
        #[case("a?c", "ac")]
        #[case("a.?c", "ac")]
        #[case("a.?c", "abc")]
        fn should_match_wildcard_patterns(#[case] pattern: &str, #[case] input_str: &str) {
            // given
            let nfa = match pattern {
                "a.c" => {
                    let mut nfa = Nfa::from_char('a');
                    nfa.concatenate(&Nfa::from_wildcard());
                    nfa.concatenate(&Nfa::from_char('c'));
                    nfa
                }
                ".." => {
                    let mut nfa = Nfa::from_wildcard();
                    nfa.concatenate(&Nfa::from_wildcard());
                    nfa
                }
                ".*" => {
                    let mut nfa = Nfa::from_wildcard();
                    nfa.kleene_star();
                    nfa
                }
                ".+" => {
                    let mut nfa = Nfa::from_wildcard();
                    nfa.one_or_more();
                    nfa
                }
                ".?" => {
                    let mut nfa = Nfa::from_wildcard();
                    nfa.zero_or_one();
                    nfa
                }
                "a.+c" => {
                    let mut nfa = Nfa::from_char('a');
                    let mut dot_plus_nfa = Nfa::from_wildcard();
                    dot_plus_nfa.one_or_more();
                    nfa.concatenate(&dot_plus_nfa);
                    nfa.concatenate(&Nfa::from_char('c'));
                    nfa
                }
                "a?c" => {
                    let mut nfa = Nfa::from_char('a');
                    nfa.zero_or_one();
                    nfa.concatenate(&Nfa::from_char('c'));
                    nfa
                }
                "a.?c" => {
                    let mut nfa = Nfa::from_char('a');
                    let mut dot_q_nfa = Nfa::from_wildcard();
                    dot_q_nfa.zero_or_one();
                    nfa.concatenate(&dot_q_nfa);
                    nfa.concatenate(&Nfa::from_char('c'));
                    nfa
                }
                _ => unreachable!(),
            };

            // when
            let matched = nfa.is_exact_match(input_str);

            // then
            assert!(matched);
        }

        #[rstest]
        #[case("a.c", "a\nc")]
        #[case(".", "\n")]
        #[case(".*", "abc\ndef")]
        #[case("a.c", "ac")]
        #[case("a.c", "abbc")]
        #[case("..", "a")]
        fn shouldnt_match_wildcard_patterns(#[case] pattern: &str, #[case] input_str: &str) {
            // given
            let nfa = match pattern {
                "a.c" => {
                    let mut nfa = Nfa::from_char('a');
                    nfa.concatenate(&Nfa::from_wildcard());
                    nfa.concatenate(&Nfa::from_char('c'));
                    nfa
                }
                "." => Nfa::from_wildcard(),
                ".*" => {
                    let mut nfa = Nfa::from_wildcard();
                    nfa.kleene_star();
                    nfa
                }
                ".." => {
                    let mut nfa = Nfa::from_wildcard();
                    nfa.concatenate(&Nfa::from_wildcard());
                    nfa
                }
                _ => unreachable!(),
            };

            // when
            let matched = nfa.is_exact_match(input_str);

            // then
            assert!(!matched);
        }
    }

    mod zero_or_one {
        use super::*;

        #[rstest]
        #[case('a', "")]
        #[case('a', "a")]
        #[case('b', "b")]
        #[case('c', "")]
        #[case('1', "1")]
        #[case('$', "")]
        #[case('@', "@")]
        fn should_match_zero_or_one(#[case] nfa_char: char, #[case] input_str: &str) {
            // given
            let mut base_nfa = Nfa::from_char(nfa_char);

            // when
            base_nfa.zero_or_one();
            let matched = base_nfa.is_exact_match(input_str);

            // then
            assert!(matched);
        }

        #[rstest]
        #[case('a', "aa")]
        #[case('b', "bb")]
        #[case('1', "11")]
        #[case('a', "b")]
        #[case('c', "d")]
        #[case('a', "ab")]
        #[case('a', "ba")]
        #[case('a', "bab")]
        #[case('$', "a$")]
        fn shouldnt_match_zero_or_one(#[case] nfa_char: char, #[case] input_str: &str) {
            // given
            let mut base_nfa = Nfa::from_char(nfa_char);

            // when
            base_nfa.zero_or_one();
            let matched = base_nfa.is_exact_match(input_str);

            // then
            assert!(!matched);
        }
    }

    mod zero_or_one_with_alternation {
        use super::*;

        #[rstest]
        #[case('a', 'b', "")]
        #[case('a', 'b', "a")]
        #[case('a', 'b', "b")]
        #[case('x', 'y', "y")]
        #[case('1', '2', "")]
        #[case('#', '$', "#")]
        fn should_match_alternation_with_zero_or_one(
            #[case] char1: char,
            #[case] char2: char,
            #[case] input_str: &str,
        ) {
            // given: build (char1|char2)
            let mut nfa1 = Nfa::from_char(char1);
            let nfa2 = Nfa::from_char(char2);
            nfa1.alternate(&nfa2);

            // when
            nfa1.zero_or_one();
            let matched = nfa1.is_exact_match(input_str);

            // then
            assert!(matched);
        }

        #[rstest]
        #[case('a', 'b', "c")]
        #[case('a', 'b', "aa")]
        #[case('a', 'b', "bb")]
        #[case('a', 'b', "ab")]
        #[case('a', 'b', "ba")]
        #[case('x', 'y', "xa")]
        #[case('1', '2', "1a")]
        fn shouldnt_match_alternation_with_zero_or_one(
            #[case] char1: char,
            #[case] char2: char,
            #[case] input_str: &str,
        ) {
            // given: build (char1|char2)
            let mut nfa1 = Nfa::from_char(char1);
            let nfa2 = Nfa::from_char(char2);
            nfa1.alternate(&nfa2);

            // when
            nfa1.zero_or_one();
            let matched = nfa1.is_exact_match(input_str);

            // then
            assert!(!matched);
        }
    }

    mod one_or_more {
        use super::*;

        #[rstest]
        #[case('a', "a")]
        #[case('b', "bb")]
        #[case('c', "ccccc")]
        #[case('1', "1111")]
        #[case('$', "$$")]
        fn should_match_one_or_more(#[case] nfa_char: char, #[case] input_str: &str) {
            // given
            let mut base_nfa = Nfa::from_char(nfa_char);

            // when
            base_nfa.one_or_more();
            let matched = base_nfa.is_exact_match(input_str);

            // then
            assert!(matched);
        }

        #[rstest]
        #[case('a', "")]
        #[case('a', "b")]
        #[case('c', "d")]
        #[case('a', "aaab")]
        #[case('a', "baaa")]
        #[case('a', "aabaa")]
        #[case('1', "1121")]
        #[case('@', "@@.@@")]
        fn shouldnt_match_one_or_more(#[case] nfa_char: char, #[case] input_str: &str) {
            // given
            let mut base_nfa = Nfa::from_char(nfa_char);

            // when
            base_nfa.one_or_more();
            let matched = base_nfa.is_exact_match(input_str);

            // then
            assert!(!matched);
        }
    }

    mod one_or_more_with_alternation {
        use super::*;

        #[rstest]
        #[case('a', 'b', "a")]
        #[case('a', 'b', "b")]
        #[case('x', 'y', "xx")]
        #[case('x', 'y', "yyyy")]
        #[case('a', 'b', "ab")]
        #[case('a', 'b', "ba")]
        #[case('0', '1', "010101")]
        #[case('0', '1', "111000101")]
        #[case('#', '@', "#@#@##@@")]
        fn should_match_alternation_with_one_or_more(
            #[case] char1: char,
            #[case] char2: char,
            #[case] input_str: &str,
        ) {
            // given
            let mut nfa1 = Nfa::from_char(char1);
            let nfa2 = Nfa::from_char(char2);
            nfa1.alternate(&nfa2);

            // when
            nfa1.one_or_more();
            let matched = nfa1.is_exact_match(input_str);

            // then
            assert!(matched);
        }

        #[rstest]
        #[case('a', 'b', "")]
        #[case('a', 'b', "c")]
        #[case('a', 'b', "ac")]
        #[case('a', 'b', "abc")]
        #[case('a', 'b', "ca")]
        #[case('a', 'b', "bca")]
        #[case('x', 'y', "xxyzyxx")]
        #[case('0', '1', "0001112000111")]
        fn shouldnt_match_alternation_with_one_or_more(
            #[case] char1: char,
            #[case] char2: char,
            #[case] input_str: &str,
        ) {
            // given
            let mut nfa1 = Nfa::from_char(char1);
            let nfa2 = Nfa::from_char(char2);
            nfa1.alternate(&nfa2);

            // when
            nfa1.one_or_more();
            let matched = nfa1.is_exact_match(input_str);

            // then
            assert!(!matched);
        }
    }

    mod empty_group {
        use super::*;
        use rstest::rstest;

        #[test]
        fn standalone_empty_group_matches_only_empty_string() {
            // given: An NFA for an empty group `()`
            let nfa = Nfa::from_epsilon();

            // then should match empty
            assert!(nfa.is_exact_match(""));

            // then shouldn't match non empty
            assert!(!nfa.is_exact_match("a"));
            assert!(!nfa.is_exact_match(" "));
        }

        #[test]
        fn applying_quantifiers_to_empty_group_is_a_no_op() {
            // given: An NFA for an empty group `()`
            let mut nfa_star = Nfa::from_epsilon();
            let mut nfa_plus = Nfa::from_epsilon();
            let mut nfa_q_mark = Nfa::from_epsilon();

            // when: We apply all three major quantifiers
            nfa_star.kleene_star(); // ()*
            nfa_plus.one_or_more(); // ()+
            nfa_q_mark.zero_or_one(); // ()?

            // then: All of them should still only match the empty string
            assert!(nfa_star.is_exact_match(""));
            assert!(!nfa_star.is_exact_match("a"));

            assert!(nfa_plus.is_exact_match(""));
            assert!(!nfa_plus.is_exact_match("a"));

            assert!(nfa_q_mark.is_exact_match(""));
            assert!(!nfa_q_mark.is_exact_match("a"));
        }

        #[rstest]
        #[case('a', 'b', "ab")] // for a()b
        #[case('x', 'y', "xy")] // for x()y
        fn concatenation_with_empty_group_is_a_no_op(
            #[case] c1: char,
            #[case] c2: char,
            #[case] input: &str,
        ) {
            // given: Build an NFA for c1()c2
            let mut nfa = Nfa::from_char(c1);
            nfa.concatenate(&Nfa::from_epsilon());
            nfa.concatenate(&Nfa::from_char(c2));

            // then
            assert!(nfa.is_exact_match(input));
            assert!(!nfa.is_exact_match(""));
            assert!(!nfa.is_exact_match(&c1.to_string()));
            assert!(!nfa.is_exact_match(&c2.to_string()));
        }

        #[rstest]
        #[case('a', "a")]
        #[case('a', "")]
        #[case('b', "b")]
        #[case('b', "")]
        fn alternation_with_empty_group_adds_empty_match(#[case] c: char, #[case] input: &str) {
            // given: Build an NFA for c|()
            let mut nfa = Nfa::from_char(c);
            nfa.alternate(&Nfa::from_epsilon());

            // then
            assert!(nfa.is_exact_match(input));
        }

        #[rstest]
        #[case('a', "b")]
        #[case('a', "aa")]
        fn alternation_with_empty_group_doesnt_match_others(#[case] c: char, #[case] input: &str) {
            // given: Build an NFA for c|()
            let mut nfa = Nfa::from_char(c);
            nfa.alternate(&Nfa::from_epsilon());

            // then
            assert!(!nfa.is_exact_match(input));
        }
    }

    mod range_quantifier {
        use super::*;

        fn nfa_from_range(base_char: char, lower: u16, upper: Option<u16>) -> Nfa {
            let child_nfa = Nfa::from_char(base_char);
            Nfa::from_range(&child_nfa, lower, upper)
        }

        // covers {n}
        mod exact_repetition {
            use super::*;

            #[rstest]
            #[case('a', 3, "aaa")]
            #[case('b', 5, "bbbbb")]
            #[case('1', 1, "1")]
            fn should_match_exact_repetitions(
                #[case] c: char,
                #[case] n: u16,
                #[case] input: &str,
            ) {
                let nfa = nfa_from_range(c, n, Some(n));
                assert!(nfa.is_exact_match(input));
            }

            #[rstest]
            #[case('a', 3, "aa")] // too few
            #[case('a', 3, "aaaa")] // too many
            #[case('b', 1, "")] // empty string
            #[case('c', 2, "ca")] // wrong character
            fn should_not_match_incorrect_exact_repetitions(
                #[case] c: char,
                #[case] n: u16,
                #[case] input: &str,
            ) {
                let nfa = nfa_from_range(c, n, Some(n));
                assert!(!nfa.is_exact_match(input));
            }

            #[test]
            fn should_match_zero_repetitions_as_empty_string() {
                // `a{0}` case
                let nfa = nfa_from_range('a', 0, Some(0));
                assert!(nfa.is_exact_match(""));
                assert!(!nfa.is_exact_match("a"));
            }
        }

        // covers {n,}
        mod at_least_repetition {
            use super::*;

            #[rstest]
            #[case('a', 2, "aa")] // exactly n
            #[case('a', 2, "aaa")] // n + 1
            #[case('a', 2, "aaaaa")] // n + many
            #[case('b', 1, "b")] // {1,} is same as +
            #[case('c', 0, "")] // {0,} is same as *
            #[case('c', 0, "c")]
            #[case('c', 0, "ccc")]
            fn should_match_at_least_n_repetitions(
                #[case] c: char,
                #[case] n: u16,
                #[case] input: &str,
            ) {
                let nfa = nfa_from_range(c, n, None);
                assert!(nfa.is_exact_match(input));
            }

            #[rstest]
            #[case('a', 2, "a")] // too few
            #[case('a', 2, "")]
            #[case('b', 1, "")] // {1,} does not match empty
            #[case('c', 3, "cdc")]
            fn should_not_match_fewer_than_n_repetitions(
                #[case] c: char,
                #[case] n: u16,
                #[case] input: &str,
            ) {
                let nfa = nfa_from_range(c, n, None);
                assert!(!nfa.is_exact_match(input));
            }
        }

        // covers {n,m}
        mod range_repetition {
            use super::*;

            #[rstest]
            #[case('a', 2, 4, "aa")] // lower bound
            #[case('a', 2, 4, "aaa")] // in between
            #[case('a', 2, 4, "aaaa")] // upper bound
            #[case('b', 0, 2, "")] // zero is allowed
            #[case('b', 0, 2, "b")]
            #[case('b', 0, 2, "bb")]
            fn should_match_repetitions_in_range(
                #[case] c: char,
                #[case] n: u16,
                #[case] m: u16,
                #[case] input: &str,
            ) {
                let nfa = nfa_from_range(c, n, Some(m));
                assert!(nfa.is_exact_match(input));
            }

            #[rstest]
            #[case('a', 2, 4, "a")] // below range
            #[case('a', 2, 4, "")]
            #[case('a', 2, 4, "aaaaa")] // above range
            #[case('b', 0, 2, "bbb")] // above range
            fn should_not_match_repetitions_outside_range(
                #[case] c: char,
                #[case] n: u16,
                #[case] m: u16,
                #[case] input: &str,
            ) {
                let nfa = nfa_from_range(c, n, Some(m));
                assert!(!nfa.is_exact_match(input));
            }
        }
    }

    mod character_set {
        use super::*;
        use std::collections::HashSet;

        mod positive_sets {
            use super::*;

            #[rstest]
            // Simple set
            #[case(HashSet::from(['a', 'b', 'c']), "a")]
            #[case(HashSet::from(['a', 'b', 'c']), "c")]
            #[case(HashSet::from(['0', '1', '2', '3']), "2")]
            #[case(HashSet::from(['*', '+', '.']), ".")]
            #[case(HashSet::from(['*', '+', '.']), "*")]
            fn should_match_any_single_char_from_the_set(
                #[case] set: HashSet<char>,
                #[case] input: &str,
            ) {
                // given
                let nfa = Nfa::from_char_set(false, &set);
                // then
                assert!(nfa.is_exact_match(input));
            }

            #[rstest]
            #[case(HashSet::from(['a', 'b', 'c']), "d")]
            #[case(HashSet::from(['0', '1', '2']), "3")]
            #[case(HashSet::from(['a', 'b']), "*")]
            fn should_not_match_any_char_outside_the_set(
                #[case] set: HashSet<char>,
                #[case] input: &str,
            ) {
                // given
                let nfa = Nfa::from_char_set(false, &set);
                // then
                assert!(!nfa.is_exact_match(input));
            }

            #[rstest]
            #[case(HashSet::from(['a', 'b', 'c']), "ab")]
            #[case(HashSet::from(['a', 'b', 'c']), "aa")]
            #[case(HashSet::from(['a', 'b', 'c']), "")]
            fn should_not_match_multi_char_or_empty_strings(
                #[case] set: HashSet<char>,
                #[case] input: &str,
            ) {
                // given
                let nfa = Nfa::from_char_set(false, &set);
                // then
                assert!(!nfa.is_exact_match(input));
            }
        }

        mod with_other_operations {
            use super::*;

            #[test]
            fn should_work_with_concatenation() {
                // given: an NFA for `[ab]c`
                let set = HashSet::from(['a', 'b']);
                let mut nfa = Nfa::from_char_set(false, &set);
                let nfa_c = Nfa::from_char('c');
                nfa.concatenate(&nfa_c);

                // then
                assert!(nfa.is_exact_match("ac"));
                assert!(nfa.is_exact_match("bc"));
                assert!(!nfa.is_exact_match("c"));
                assert!(!nfa.is_exact_match("abc"));
            }

            #[test]
            fn should_work_with_alternation() {
                // given: an NFA for `[ab]|c`
                let set = HashSet::from(['a', 'b']);
                let mut nfa = Nfa::from_char_set(false, &set);
                let nfa_c = Nfa::from_char('c');
                nfa.alternate(&nfa_c);

                // then
                assert!(nfa.is_exact_match("a"));
                assert!(nfa.is_exact_match("b"));
                assert!(nfa.is_exact_match("c"));
                assert!(!nfa.is_exact_match("d"));
                assert!(!nfa.is_exact_match("ac"));
            }

            #[test]
            fn should_work_with_kleene_star() {
                // given: an NFA for `[ab]*`
                let set = HashSet::from(['a', 'b']);
                let mut nfa = Nfa::from_char_set(false, &set);
                nfa.kleene_star();

                // then
                assert!(nfa.is_exact_match(""));
                assert!(nfa.is_exact_match("a"));
                assert!(nfa.is_exact_match("b"));
                assert!(nfa.is_exact_match("aa"));
                assert!(nfa.is_exact_match("bb"));
                assert!(nfa.is_exact_match("ab"));
                assert!(nfa.is_exact_match("baba"));
                assert!(!nfa.is_exact_match("c"));
                assert!(!nfa.is_exact_match("ac"));
            }

            #[test]
            fn should_work_with_one_or_more() {
                // given: an NFA for `[ab]+`
                let set = HashSet::from(['a', 'b']);
                let mut nfa = Nfa::from_char_set(false, &set);
                nfa.one_or_more();

                // then
                assert!(nfa.is_exact_match("a"));
                assert!(nfa.is_exact_match("b"));
                assert!(nfa.is_exact_match("ababa"));
                assert!(!nfa.is_exact_match(""));
                assert!(!nfa.is_exact_match("c"));
            }

            #[test]
            fn should_work_with_zero_or_one() {
                // given: an NFA for `[ab]?`
                let set = HashSet::from(['a', 'b']);
                let mut nfa = Nfa::from_char_set(false, &set);
                nfa.zero_or_one();

                // then
                assert!(nfa.is_exact_match(""));
                assert!(nfa.is_exact_match("a"));
                assert!(nfa.is_exact_match("b"));
                assert!(!nfa.is_exact_match("aa"));
                assert!(!nfa.is_exact_match("c"));
            }
        }

        mod negated_sets {
            use super::*;
            use std::collections::HashSet;

            #[rstest]
            #[case(HashSet::from(['a', 'b', 'c']), "d")]
            #[case(HashSet::from(['A', 'B', 'C']), "a")]
            #[case(HashSet::from(['0', '1', '2']), "9")]
            #[case(HashSet::from(['a', 'b']), "\n")]
            #[case(HashSet::from(['a', 'b']), "*")]
            fn should_match_any_single_char_outside_the_set(
                #[case] set: HashSet<char>,
                #[case] input: &str,
            ) {
                // given
                let nfa = Nfa::from_char_set(true, &set);
                // then
                assert!(nfa.is_exact_match(input));
            }

            #[rstest]
            #[case(HashSet::from(['a', 'b', 'c']), "a")]
            #[case(HashSet::from(['a', 'b', 'c']), "b")]
            #[case(HashSet::from(['d', 'e', 'f']), "f")]
            #[case(HashSet::from(['*', '+', '?']), "*")]
            fn should_not_match_any_char_from_the_set(
                #[case] set: HashSet<char>,
                #[case] input: &str,
            ) {
                // given
                let nfa = Nfa::from_char_set(true, &set);
                // then
                assert!(!nfa.is_exact_match(input));
            }

            #[rstest]
            #[case(HashSet::from(['a']), "xy")]
            #[case(HashSet::from(['b']), "zz")]
            #[case(HashSet::from(['c']), "")]
            fn should_not_match_multi_char_or_empty_strings(
                #[case] set: HashSet<char>,
                #[case] input: &str,
            ) {
                // given
                let nfa = Nfa::from_char_set(true, &set);
                // then
                assert!(!nfa.is_exact_match(input));
            }

            #[test]
            fn should_work_with_concatenation() {
                // given: an NFA for `[^a]c`
                let set = HashSet::from(['a']);
                let mut nfa = Nfa::from_char_set(true, &set);
                nfa.concatenate(&Nfa::from_char('c'));

                // then
                assert!(nfa.is_exact_match("bc"));
                assert!(nfa.is_exact_match("xc"));
                assert!(nfa.is_exact_match(".c"));
                assert!(!nfa.is_exact_match("ac"));
                assert!(!nfa.is_exact_match("c"));
                assert!(!nfa.is_exact_match("bbc"));
            }

            #[test]
            fn should_work_with_kleene_star() {
                // given: an NFA for `[^ab]*` (zero or more chars that are not 'a' or 'b')
                let set = HashSet::from(['a', 'b']);
                let mut nfa = Nfa::from_char_set(true, &set);
                nfa.kleene_star();

                // then
                assert!(nfa.is_exact_match(""));
                assert!(nfa.is_exact_match("c"));
                assert!(nfa.is_exact_match("xyz"));
                assert!(nfa.is_exact_match("123_@#$"));
                assert!(!nfa.is_exact_match("a"));
                assert!(!nfa.is_exact_match("b"));
                assert!(!nfa.is_exact_match("ca"));
                assert!(!nfa.is_exact_match("ab"));
            }
        }
    }
}
