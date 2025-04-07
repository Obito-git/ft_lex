DIGIT [0-9]
%%
{DIGIT}+ { printf("Number: %s\n", yytext);
// Missing closing brace for the action
.        { printf("Other\n"); }
%%
int main() { return 0; }
