%%
"a \" b \\ c\t d "  { /* Matches 'a', space, quote, space, 'b', space, backslash, space, 'c', tab, space, 'd', space */ }
"Quote: \" "        { /* Pattern containing quote and ending with space */ }
[ \t\"\\.]+       { /* Matches whitespace, quotes, backslashes, or dots */ }
"  \\\"  "         { /* Matches space, space, backslash, quote, space, space */ }
%%