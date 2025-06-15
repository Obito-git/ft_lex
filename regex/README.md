# Regex

Educatinal purpose `The extended regular expression (ERE)` implementation. The  that exposes some internals that we can reuse for our pattern matching engine

## REGEX:
AST is built using Recursive Descent Parsing.
## Resources:

- https://www.regular-expressions.info/refflavors.html - reference
- https://www.youtube.com/watch?v=DiXMoBMWMmA -  Regular expressions as finite automata
- https://www.youtube.com/watch?v=0DQyPKD8RVU - Thompson construction explained
- https://danielbv.github.io/RegexEngine/ - regex visualizer with NFA
- https://www.abstractsyntaxseed.com/blog/regex-engine/introduction - at first sight good tutorial
- https://en.wikipedia.org/wiki/Recursive_descent_parser
- https://craftinginterpreters.com/parsing-expressions.html#recursive-descent-parsing

## Scope

| Feature Category | Syntax | Description | Example |
| :--- | :--- | :--- | :--- |
| **Atoms** | `c` | **Literal Character:** Any character that is not a metacharacter matches itself. | `cat` matches the string "cat". |
| | `.` | **Wildcard (Dot):** Matches any single character, with the exception of a newline character.[1, 2] | `c.t` matches "cat", "cot", and "c@t". |
| | `\` | **Escape Character:** Treats the subsequent character as a literal, removing any special meaning it might have. | `\.` matches a literal period (`.`) instead of any character. |
| **Anchors** | `^` | **Start of Line:** Matches the beginning of a line or string.[3, 2] | `^The` matches "The" only when it appears at the start of a line. |
| | `$` | **End of Line:** Matches the end of a line or string.[3, 2] | `end$` matches "end" only when it appears at the end of a line. |
| **Quantifiers** | `*` | **Zero or More:** Matches the preceding element zero or more times. It is "greedy," meaning it matches as many characters as possible.[1, 2] | `ca*t` matches "ct", "cat", and "caaat". |
| | `+` | **One or More:** Matches the preceding element one or more times. This is an ERE-specific feature.[4, 1, 2] | `ca+t` matches "cat" and "caaat", but not "ct". |
| | `?` | **Zero or One:** Matches the preceding element zero or one time, making it optional. This is an ERE-specific feature.[4, 1, 2] | `colou?r` matches both "color" and "colour". |
| | `{n}` | **Exact Count:** Matches the preceding element exactly `n` times.[5, 2] | `[0-9]{3}` matches exactly three digits, like "123". |
| | `{n,}` | **At Least `n` Times:** Matches the preceding element at least `n` times.[5, 2] | `[0-9]{2,}` matches "12", "123", and "1234". |
| | `{n,m}` | **Range of Times:** Matches the preceding element at least `n` times but no more than `m` times.[5, 2] | `[0-9]{2,4}` matches "12", "123", and "1234", but not "1" or "12345". |
| **Bracket Expressions** | `[...]` | **Character Set:** Matches any single character from the set defined inside the brackets.[5, 2] | `[aeiou]` matches any lowercase vowel. |
| | `[^...]` | **Negated Set:** Matches any single character that is *not* in the set defined inside the brackets. The `^` must be the first character.[5] | `[^0-9]` matches any character that is not a digit. |
| | `[a-z]` | **Range:** Matches any single character within the specified ASCII range.[5, 2] | `[a-f]` matches "a", "b", "c", "d", "e", or "f". |
| **POSIX Classes** | `[[:alnum:]]` | Matches any alphanumeric character (`[A-Za-z0-9]`).[2, 6] | `[[:alnum:]]` matches "a", "B", or "7". |
| | `[[:alpha:]]` | Matches any alphabetic character (`[A-Za-z]`).[2, 6] | `[[:alpha:]]` matches "a" or "B", but not "7". |
| | `[[:blank:]]` | Matches a space or a tab character.[7] | ` ` or `\t` |
| | `[[:cntrl:]]` | Matches control characters.[7] | e.g., backspace, escape. |
| | `[[:digit:]]` | Matches any digit (`[0-9]`).[2, 6] | `[[:digit:]]` matches "7", but not "a". |
| | `[[:graph:]]` | Matches visible characters (alphanumeric and punctuation).[7] | `[[:graph:]]` matches "a", "7", or "!". |
| | `[[:lower:]]` | Matches any lowercase letter (`[a-z]`).[2] | `[[:lower:]]` matches "a", but not "A". |
| | `[[:print:]]` | Matches printable characters, including space.[7] | `[[:print:]]` matches "a", "7", "!", or " ". |
| | `[[:punct:]]` | Matches punctuation characters.[2, 8] | `[[:punct:]]` matches ".", "!", or ";". |
| | `[[:space:]]` | Matches whitespace characters (space, tab, newline, etc.).[7] | `[[:space:]]` matches " ", `\t`, or `\n`. |
| | `[[:upper:]]` | Matches any uppercase letter (`[A-Z]`).[2] | `[[:upper:]]` matches "A", but not "a". |
| | `[[:xdigit:]]` | Matches hexadecimal digits (`[0-9a-fA-F]`).[7] | `[[:xdigit:]]` matches "9", "a", or "F". |
| **Grouping & Alternation** | `(...)` | **Grouping:** Groups a sub-expression to apply quantifiers to the entire group. It also "captures" the matched text for potential backreferences.[1, 2] | `(ha)+` matches "ha" and "hahaha". |
| | `|` | **Alternation (OR):** Matches either the complete expression on its left or the complete expression on its right. This is an ERE-specific feature.[4, 1, 2] | `cat|dog` matches "cat" or "dog". |
| **Backreferences** | `\n` | **Backreference:** Matches the exact string that was previously captured by the `n`-th capturing group `(...)`, where `n` is a digit from 1 to 9.[2] | `(a)b\1` matches "aba". `(c.t) and \1` matches "cat and cat". |

## Parser is LL(1)
- L (First L): Input is scanned from Left to right.
- L (Second L): The parser produces a Leftmost derivation of the string according to the grammar.

- (k): The parser uses k tokens of lookahead to make its parsing decisions (i.e., to predict which grammar rule to apply next).
  - Crucially, our parser functions use .peek() to look at the next single token (k=1) to decide what to do:
  - parse_factor peeks ahead one token to see if it's a Quantifier.
  - parse_term peeks ahead one token to see if it can start another factor (for concatenation).
  - parse_expression peeks ahead one token to see if it's a Pipe.

REGEX + EXPRESSIONS: tokens to AST:
https://craftinginterpreters.com/contents.html

## General TODO:

## Global TODO:

- [ ] write doc and define the scope
- [ ] implement is_match and find fn
- [x] basic regex first (concatenation, alteration and Kleene star)
- [ ] pattern is parsed to AST
- [ ] AST transformed to the NFA
- [ ] NFA transformed to the DTA
- [ ] extended regex
- [ ] the regex is usable for regular needs
- [ ] the regex instance should store NFA
- [ ] is able to combine NFA later into "super" NFA
- [ ] can transform "super" NFA to the DFA
- [ ] fully tested
- [ ] can be "compiled" to a transition table to be used in generated files
