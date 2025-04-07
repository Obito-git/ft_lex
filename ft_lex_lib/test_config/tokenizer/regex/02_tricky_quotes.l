%%
"\""        { /* Matches a single double quote character (escaped within string) */ }
'"'         { /* Matches a single double quote character (single quotes are Flex only) */ }
["]         { /* Matches a single double quote (inside character class) */ }
\"          { /* Matches a single double quote (escaped outside string) */ }
"a\"b"      { /* Matches 'a', double quote, 'b' */ }
""          { /* Matches an empty string (often causes issues/warnings in Flex) */ }
"\"\""      { /* Matches two double quotes */ }
%%