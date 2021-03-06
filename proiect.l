%{
#include <stdio.h>
#include <stdbool.h>
#include "y.tab.h"
#include "header.h"
%}
%option noyywrap
%%
"scope" { return SCOPE; }
"Eval" { return EVAL; }
"Eval_fun" {return EVAL_FUN;}
"return" { return RETURN; }
"const" { return CONST; }
"void" {yylval.tip_val=strdup(yytext); return VOID; }
"int" { yylval.tip_val=strdup(yytext); return INTEGER; }
"float" {yylval.tip_val=strdup(yytext); return FLOAT; }
"char" { yylval.tip_val=strdup(yytext); return CHAR; }
"string" {yylval.tip_val=strdup(yytext); return STRING; }
"bool" { yylval.tip_val=strdup(yytext); return BOOL; }
"if" { return MYIF; }
"while" { return MYWHILE; }
"for" { return MYFOR; }
"false" { yylval.bool_val=false;return BOOL_CONST; }
"true" { yylval.bool_val=true;return BOOL_CONST; }
"struct" { return STRUCT; }
"strcat" { return CONCAT; }
"begin" { return BEGIN_p; }
"end"   { return END_p; }
[a-zA-Z][a-zA-Z0-9]* {yylval.id_val=strdup(yytext); return ID;}
\'[a-zA-Z]\' {yylval.char_val=yytext[1]; return CHAR_CONST; }
[~][a-zA-Z]*[~] {yylval.id_val=strdup(yytext); return STR_CONST; }
"<" {return LT; }
"<=" {return LTE; }
">=" {return GTE; }
">" { return GT; }
"==" {return EQ; }
"!" {return NOT; }
"&&" {return SI;}
"||" {return SAU;}
"+" {return PLUS; }
"-" {return MINUS;}
"/" {return DIV;}
"%" { return MOD;}
"*" { return MUL;}
":=" {return ASSIGN;}
[0-9]+ {yylval.int_val=atoi(yytext);return INT_CONST;}
[0-9]+[.][0-9]+ {yylval.float_val=atof(yytext); return FLOAT_CONST;}
[ \t] ;
\n {yylineno++;}
. {return yytext[0];}