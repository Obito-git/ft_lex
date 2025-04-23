%%
" "         { /* Matches a single space (quoted) */ }
"a b c"     { /* Matches 'a', space, 'b', space, 'c' */ }
  " "       /* Matches a single space (leading whitespace ignored) */
[ \t]+      { /* Matches one or more spaces or tabs (common idiom) */ }
a\ b        { /* Matches 'a', space, 'b' (space escaped, less common) */ }
abc" "xyz   { /* Matches 'abc xyz' - unquoted space IS part of pattern */ }
"ends "     { /* Pattern ends with space */ }
" starts"   { /* Pattern starts with space */ }
\       { /* Matches a single space (Flex extension: escaped space) */ }
%%