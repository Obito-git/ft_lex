use std::collections::HashSet;

struct NfaTransition {
    pub start: usize,
    pub end: usize,
    pub symbol: Option<char>, // None == epsilon, TODO: in future enum?
}

struct Nfa {
    pub states_count: usize,             // for now only simple counter
    pub transitions: Vec<NfaTransition>, // TODO: not vec, probably map or ?
    pub start_state: usize,
    pub accept_state: usize,
}

impl Nfa {
    fn add_transition(&mut self, start: usize, end: usize, symbol: Option<char>) {
        self.transitions.push(NfaTransition { start, end, symbol });
    }

    fn add_state(&mut self) -> usize {
        self.states_count += 1;
        self.states_count
    }

    pub fn from_char(c: char) -> Self {
        let mut nfa = Nfa {
            transitions: vec![],
            start_state: 0,
            accept_state: 1,
            states_count: 2,
        };
        nfa.add_transition(0, 1, Some(c));
        nfa
    }

    pub fn concatenate(&mut self, other: &Self) {
        let offset = self.states_count;
        let old_accept = self.accept_state;

        // merge states count
        self.states_count += other.states_count;

        // merge transitions but update including offsets
        for t in &other.transitions {
            self.add_transition(t.start + offset, t.end + offset, t.symbol);
        }

        // connect two machines using the epsilon
        self.add_transition(old_accept, other.start_state + offset, None);

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
            self.add_transition(t.start + offset, t.end + offset, t.symbol);
        }

        // add new state and make it new start state
        let new_start_state = self.add_state();
        self.start_state = new_start_state;

        // add 2 epsilon transitions to the start of both NFAs
        self.add_transition(new_start_state, old_start, None);
        self.add_transition(new_start_state, other.start_state + offset, None);

        // add new state and make it new accept_state
        let new_accept_state = self.add_state();
        self.accept_state = new_accept_state;

        // add 2 epsilon transitions from old accepts to the new one
        self.add_transition(old_accept, new_accept_state, None);
        self.add_transition(other.accept_state + offset, new_accept_state, None);
    }

    pub fn kleene_star(&mut self) {
        let old_start = self.start_state;
        let old_accept = self.accept_state;

        let new_start = self.add_state();
        let new_accept = self.add_state();

        self.start_state = new_start;
        self.accept_state = new_accept;

        // epsilon from old accept to old start
        self.add_transition(old_accept, old_start, None);

        // epsilons from new start to old start and new accept
        self.add_transition(new_start, old_start, None);
        self.add_transition(new_start, new_accept, None);

        // epsilon from old accept to new accept
        self.add_transition(old_accept, new_accept, None);
    }

    fn simulate_matched(&self, s: &str) -> bool {
        let mut cur_states = follow_epsilons(self, self.start_state);
        for c in s.chars() {
            let mut next_states = HashSet::new();

            for matched_transition in self
                .transitions
                .iter()
                .filter(|t| t.symbol == Some(c) && cur_states.contains(&t.start))
            {
                next_states.extend(follow_epsilons(self, matched_transition.end));
            }
            cur_states = next_states;
        }

        //TODO: redesign to not allocate hashset with only 1 elem each time
        fn follow_epsilons(nfa: &Nfa, state: usize) -> HashSet<usize> {
            let mut res = HashSet::from([state]);

            for t in &nfa.transitions {
                if t.symbol.is_none() && t.start == state {
                    res.extend(follow_epsilons(nfa, t.end));
                }
            }
            res
        }

        cur_states.contains(&self.accept_state)
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
                assert!(nfa.simulate_matched(&pattern_char.to_string()));

                // shouldn't match any other one
                for x in 32..=126 {
                    let not_pattern_char = x as u8 as char;
                    if i != x {
                        assert!(!nfa.simulate_matched(&not_pattern_char.to_string()));
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
            let matched = nfa.simulate_matched(s);

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
            let matched = nfa1.simulate_matched(expected_str);

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
            let matched = nfa1.simulate_matched(expected_str);

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
            let matched = nfa1.simulate_matched(input_str);

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
            let matched = nfa1.simulate_matched(input_str);

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
            let matched = base_nfa.simulate_matched(input_str);

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
            let matched = base_nfa.simulate_matched(input_str);

            //then
            assert!(!matched);
        }
    }
}
