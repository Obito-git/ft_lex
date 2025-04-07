Lex/flex

flex docs: https://perso.esiee.fr/~coupriem/Flex/flex_2.html
lex docs: https://pubs.opengroup.org/onlinepubs/9799919799/utilities/lex.html

section definition lexical analyse (tokenization):
- Scope: until %%
- If start with whitespace - should be copied to the scanner
- %{}% block - should be copied as well
- definition is anything that from the beginning of the line
- definition name and pattern is separated by whitespace

section definition parsing (syntax and correct logic check):


insta:
- https://insta.rs/docs/quickstart/
- `curl -LsSf https://insta.rs/install.sh | sh`

parametrized test:
- https://github.com/la10736/rstest






https://www.youtube.com/watch?v=54bo1qaHAfk

## REGEX:
AST is built using Recursive Descent Parsing.
### Resources:

- https://en.wikipedia.org/wiki/Recursive_descent_parser
- https://craftinginterpreters.com/parsing-expressions.html#recursive-descent-parsing

### Notes

#### Parser is LL(1)
- L (First L): Input is scanned from Left to right.
- L (Second L): The parser produces a Leftmost derivation of the string according to the grammar.

- (k): The parser uses k tokens of lookahead to make its parsing decisions (i.e., to predict which grammar rule to apply next).
  - Crucially, our parser functions use .peek() to look at the next single token (k=1) to decide what to do:
  - parse_factor peeks ahead one token to see if it's a Quantifier.
  - parse_term peeks ahead one token to see if it can start another factor (for concatenation).
  - parse_expression peeks ahead one token to see if it's a Pipe.
- 
REGEX + EXPRESSIONS: tokens to AST:
https://craftinginterpreters.com/contents.html