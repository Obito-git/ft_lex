%%
[a-z]+                //kek
[A-Z]+                //kek
[0-9]+                //kek
[a-zA-Z]+             //kek
[a-zA-Z0-9_]+         /* Common identifier */
[ \t]+                /* Whitespace (space or tab) */
" "                   /* Single literal space */
"  "                  /* Two literal spaces */
\                     /* Escaped single space */
\s+                   /* One or more whitespace characters (flex extension) */
\d+                   /* Digits (flex extension) */
\w+                   /* Word characters (flex extension) */
\.                    /* Literal dot */
\*                    /* Literal asterisk */
\+                    /* Literal plus */
\?                    /* Literal question mark */
\\                    /* Literal backslash */
\(                    /* Literal opening parenthesis */
\)                    /* Literal closing parenthesis */
\[                    /* Literal opening bracket */
\]                    /* Literal closing bracket */
\{                    /* Literal opening brace */
\}                    /* Literal closing brace */
\"                    /* Literal double quote */
\'                    /* Literal single quote */
\^                    /* Literal caret */
\$                    /* Literal dollar */
\|                    /* Literal pipe */
abc                   /* Simple literal sequence */
a*                    /* Zero or more 'a's */
b+                    /* One or more 'b's */
c?                    /* Zero or one 'c' */
d{3}                  /* Exactly three 'd's */
e{2,}                 /* Two or more 'e's */
f{1,5}                /* Between one and five 'f's */
(abc)                 /* Grouping */
(a|b)                 /* Alternation */
(a b c)               /* Grouping with spaces (needs quotes or escapes) */
"a b c"               /* Quoted grouping with spaces */
a\ b\ c               /* Escaped spaces */
(a|b)+                /* One or more 'a' or 'b' */
(x y z)?              /* Optional group with spaces (needs quotes/escapes) */
"(x y z)?"            /* Quoted optional group with spaces */
(a(b|c)d)             /* Nested groups */
[aeiou]               /* Character set (vowels) */
[^aeiou]              /* Negated character set (consonants etc.) */
[a-z0-9]              /* Range in set */
[+\-*/=]              /* Set with operators */
[-a-z]                /* Set including literal hyphen (at start) */
[a-z-]                /* Set including literal hyphen (at end) */
[a\-z]                /* Set including literal hyphen (escaped) */
[()]                  /* Set including literal parentheses */
[{}]                  /* Set including literal braces */
[\[\]]                /* Set including literal brackets */
"."                   /* Any character except newline */
^start                /* 'start' at beginning of line */
end$                  /* 'end' at end of line */
\/\/[^\n]*            /* C/C++/Java style single-line comment */
^hello\ world$        /* Exact line with space */
"foo bar"             /* Literal string with space */
"foo | bar"           /* Literal string with space and pipe */
"foo ( bar )"         /* Literal string with spaces and parens */
"foo [ bar ]"         /* Literal string with spaces and brackets */
"foo { bar }"         /* Literal string with spaces and braces */
[ ]                   /* Character set containing only a space */
"()"                  /* Literal empty parentheses */
"[]"                  /* Literal empty brackets */
"{}"                  /* Literal empty braces */
"({[]})"              /* Literal nested brackets */
[[:space:]]+          /* POSIX character class for whitespace */
[[:digit:]]+          /* POSIX character class for digits */
[[:alpha:]]+          /* POSIX character class for alphabetic chars */
[[:alnum:]]+          /* POSIX character class for alphanumeric chars */
[[:punct:]]+          /* POSIX character class for punctuation */
[+-]?[0-9]+           /* Optional sign followed by digits (integer) */
[0-9]+\.[0-9]*        /* Simple float (digits dot optional-digits) */
[0-9]*\.[0-9]+        /* Simple float (optional-digits dot digits) */
\"[^"]*\"             /* Double-quoted string literal (simple) */
'[^']*'               /* Single-quoted string literal (simple) */
<INITIAL>{TOKEN}      /* Using start conditions */
a{0}                  /* Zero 'a's (matches empty string - use carefully!) */
(a{1,2}){3}           /* Three repetitions of 'a' or 'aa' */
(a | b | c)           /* Alternation with spaces (needs quotes/escapes) */
"(a | b | c)"         /* Quoted alternation with spaces */
(a | "b c")           /* Alternation with a multi-word literal */
<string>\"            /* Closing quote in a specific start condition */
<string>\\["]         /* Escaped quote within a string start condition */
<<EOF>>               /* End-of-file condition */
a\ b*c+d?e{1}f{2,}g{3,4}        /* Combination of escaped spaces and quantifiers */
"a b* c+ d? e{1} f{2,} g{3,4}"  /* Quoted version of the above */
([0-9]+\.[0-9]*)|([0-9]*\.[0-9]+)                   /* More complete simple float */
[+-]?(([0-9]+\.[0-9]*)|([0-9]*\.[0-9]+)|([0-9]+))   /* Integer or float */
\/\*([^*]|(\*+[^*/]))*\*+\/                         /* C-style multi-line comment (complex!) */
%%

/* Optional user code section */