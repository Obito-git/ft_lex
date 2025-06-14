# Regex

basic `extended regex` implementation, that exposes some internals that we can reuse for our pattern matching engine

### REGEX:
AST is built using Recursive Descent Parsing.
### Resources:

- https://www.regular-expressions.info/refflavors.html - reference
- https://www.youtube.com/watch?v=DiXMoBMWMmA -  Regular expressions as finite automata
- https://www.youtube.com/watch?v=0DQyPKD8RVU - Thompson construction explained
- https://danielbv.github.io/RegexEngine/ - regex visualizer with NFA
- https://www.abstractsyntaxseed.com/blog/regex-engine/introduction - at first sight good tutorial
- https://en.wikipedia.org/wiki/Recursive_descent_parser
- https://craftinginterpreters.com/parsing-expressions.html#recursive-descent-parsing

### Notes

### Parser is LL(1)
- L (First L): Input is scanned from Left to right.
- L (Second L): The parser produces a Leftmost derivation of the string according to the grammar.

- (k): The parser uses k tokens of lookahead to make its parsing decisions (i.e., to predict which grammar rule to apply next).
  - Crucially, our parser functions use .peek() to look at the next single token (k=1) to decide what to do:
  - parse_factor peeks ahead one token to see if it's a Quantifier.
  - parse_term peeks ahead one token to see if it can start another factor (for concatenation).
  - parse_expression peeks ahead one token to see if it's a Pipe.

REGEX + EXPRESSIONS: tokens to AST:
https://craftinginterpreters.com/contents.html
