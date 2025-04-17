%{
/* C code block included directly */
#include <stdio.h>
#include "parser.tab.h" // Example include for Bison/Yacc tokens

// Helper function declaration
static void handle_string(char *s);
%}

ID       [a-zA-Z_][a-zA-Z0-9_]*
DIGIT    [0-9]
NUMBER   {DIGIT}+

%%

"if"     { return IF_TOKEN; }
"else"   { return ELSE_TOKEN; }
"while"  { return WHILE_TOKEN; }
"return" { return RETURN_TOKEN; }

{ID}     { /* yylval.sval = strdup(yytext); return IDENTIFIER; */ }
{NUMBER} { /* yylval.ival = atoi(yytext); return INTEGER_LITERAL; */ }

"=="     { return EQ_OP; }
"="      { return ASSIGN_OP; }
";"      { return SEMICOLON; }
"{"      { return LBRACE; }
"}"      { return RBRACE; }


\"(\\.|[^\\"])*\"   { /* handle_string(yytext); return STRING_LITERAL; */ }

"/*"([^*]|\*+[^*/])*\*+"/" { /* Ignore C-style block comment */ }
"//".* { /* Ignore C++ style comment (Flex extension) */ }


[ \t\n]+ { /* Ignore whitespace and newlines, or handle lines: if (yytext[0] == '\n') yylineno++; */ }
.        { fprintf(stderr, "Lexical error: '%s'\n", yytext); }

%%
/* User Code Section */

static void handle_string(char *s) {
    // Example: Remove quotes and handle escapes
    printf("Found string: %s\n", s);
}

// Required by flex if not using %option noyywrap
int yywrap() {
    return 1; // Indicate no more files to process
}

/* main is often here for standalone lexers */
int main(int argc, char **argv) {
    /* Initialization if needed */
    yylex(); /* Start lexing */
    return 0;
}
