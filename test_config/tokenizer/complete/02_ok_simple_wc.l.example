%{
unsigned char_count = 0, word_count = 0, line_count = 0;
%}
word    [^ \t\n]+
eol     \n
%%
{word}  {
        word_count++;
        char_count += yyleng;
}
{eol}   { char_count++; line_count++; }
.       { char_count++; }
%%
int main()
{
    yylex();
    printf("%d %d %d\n", line_count, word_count, char_count);
}