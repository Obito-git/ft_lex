%{
unsigned char_count = 0, word_count = 0, line_count = 0;
// This block is missing its closing delimiter

DIGIT [0-9]

%%
{DIGIT}+ { /* action */ }
.        { /* default */ }
%%
int main() { return 0; }
