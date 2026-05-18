    unsigned char_count = 0, word_count = 0, line_count = 0;

DIGIT [0-9]

%%
%{
// This block is missing its closing delimiter
{DIGIT}+ { /* action */ }
.        { /* default */ }
%%
int main() { return 0; }
