%option noyywrap

%x COMMENT

%%
<INITIAL>"/*"      { BEGIN(COMMENT); /* Enter COMMENT state */ }
<COMMENT>"*/"      { BEGIN(INITIAL); /* Return to INITIAL state */ }
<COMMENT>[^*\n]+   { /* Eat comment characters */ }
<COMMENT>"*"       { /* Eat the star, remain in COMMENT */ }
<COMMENT>\n        { yylineno++; /* Keep track of lines within comment */ }

<INITIAL> {
  // TODO: should be handled carefully by parser (flex feature)
  /* Rules active only in the INITIAL state */
  "int"            { return INT_KEYWORD; }
  "float"          { return FLOAT_KEYWORD; }
  [a-zA-Z_]+       { return ID; }
  [0-9]+           { return NUMBER; }
  [ \t]+           { /* Ignore whitespace */ }
  \n               { yylineno++; }
  .                { return UNKNOWN; }
}

%%
/* User code section */
int main() { yylex(); return 0; }
