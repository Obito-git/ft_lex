%%
"\\"        { /* Matches a single backslash (escaped in string) */ }
\\          { /* Matches a single backslash (escaped outside string) */ }
\.\*\+      { /* Matches literal '.', '*', '+' */ }
"a\\b"      { /* Matches 'a', backslash, 'b' */ }
"\n"        { /* Matches a newline character */ }
\t          { /* Matches a tab character */ }
\{          { /* Matches a literal opening brace */ }
\}          { /* Matches a literal closing brace */ }
\[          { /* Matches a literal opening bracket */ }
\]          { /* Matches a literal closing bracket */ }
\^          { /* Matches a literal caret */ }
\$          { /* Matches a literal dollar sign */ }
%%