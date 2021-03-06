%{
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include "header.h"
#define max_capacity 60
    extern FILE* yyin;
    extern char* yytext;
    extern int yylineno;
    int yylex();
    void yyerror(char *s);

//declaratii functii
    char scope[100] = "GLOBAL";
    char prev_scope[100];
    int depth = 0;
    int program = 1; // 1 - GLOBAL 2 - MAIN FUNC
    int countVars = 0;
    int countFuncs = 0;
    int countParams=0;
    int func_type_check = 0;
    bool ok;
    Var x[max_capacity];
    Func y[max_capacity];
    char sir_var[100];
    char sir_cifra[100];
    void declareVar(char* nume, char* tip, char* scope,  bool const_state, bool decl_state, bool init_state,bool array_state);
    void declareFunc(char* nume, char* tip, int nr, bool decl_state);
    void initializeVar(char* nume, Expr* expr);
    void initializeFunc(char* nume, Expr* expr);
    void updateTable();
    bool isVarDecl(char* nume);
    bool isFuncDecl(char* nume);
    int getType(char* nume);
    char* getType_char(char * nume);
    char* getScope(char*nume);
    int getFuncID(char* nume);
    Expr* create_custom_expr_2(char* val);
    Expr* create_custom_expr(char* val);
    Expr* create_invalid_expr();
    Expr* create_int_expr(int val);
    Expr* create_string_expr(char* val1,char* val2);
    Expr* create_bool_expr(bool val);
    Expr* create_float_expr(float val);
    Expr* create_char_expr(char val);
    void print_expr(Expr* expr);
    void free_expr(Expr* expr);
    void printFuncResult(int IDfunc);
    /*  -1 - invalid
        1 - int
        2 - string
        3 - bool
        4 - char
        5 - float
        6 - void */
%}

%union
{
    int int_val;
    char char_val;
    float float_val;
    bool bool_val;
    char* str_val;
    char* id_val ;
    char* tip_val;
    struct Expr* expr_ptr;
}

%token ASSIGN EVAL EVAL_FUN
%token <id_val> ID
%token <tip_val> INTEGER CHAR STRING FLOAT BOOL VOID
%token MYIF MYWHILE MYFOR RETURN SCOPE
%token STRUCT CONST
%token <int_val> INT_CONST
%token <bool_val> BOOL_CONST
%token <str_val> STR_CONST
%token <char_val> CHAR_CONST
%token <float_val> FLOAT_CONST
%token PLUS MINUS DIV MOD MUL
%token SI SAU NOT LT GT LTE GTE EQ CONCAT
%token BEGIN_p END_p

%type <tip_val> tip
%type <expr_ptr> EXP RESULT

%left ','
%left ';'
%right ASSIGN
%nonassoc EQ
%left SI SAU
%nonassoc LT GT GTE LTE
%left PLUS MINUS
%left MUL DIV MOD CONCAT
%nonassoc NOT
%nonassoc RETURN
%left '{' '}' '[' ']' '(' ')'

%start program
%%
program : | DECL_BLOC BEGIN_p {bzero(scope,sizeof(scope));strcpy(scope,"MAIN FUNCTION");program = 2;depth = 0;} instructiuni END_p {printf("Program corect din punct de vedere sintactic!\n");}
        ;
instructiuni : instructiune ';'
| instructiuni instructiune ';'
;

instructiune : ID ASSIGN RESULT {if(isVarDecl($1)==true && getType($1) == $3 ->type_int && strcmp(getType_char(getScope($1)),"struct") != 0  ) {initializeVar($1,$3);} else {printf("Eroare la nivel semantic la linia [%d]: variabila %s nu este declarata sau incercare de asignare gresita.\n",yylineno,$1);}free_expr($3);free($1);}
| Declaratii
| RETURN RESULT {if($2->type_int == -1) printf("RETURN cu o valoare invalida la linia [%d]...\n",yylineno);else initializeFunc(scope,$2);free_expr($2);}
| ID'.'ID ASSIGN RESULT {if(isVarDecl($3)==true && (getType($3)==$5->type_int) &&  strcmp($1,getScope($3))==0) {initializeVar($3,$5);} else{printf("Eroare la nivel semantic la linia [%d]: variabila %s nu este declarata sau incercare de asignare gresita.\n",yylineno,$3);}free_expr($5);free($1);free($3);}
| ID'.'ID '[' INT_CONST ']' ASSIGN RESULT {bzero(sir_var,sizeof(sir_var));bzero(sir_cifra,sizeof(sir_cifra));strcat(sir_var,$3);strcat(sir_var,"_");sprintf(sir_cifra,"%d",$5);strcat(sir_var,sir_cifra);if(isVarDecl(sir_var)==true && getType(sir_var)==$8->type_int && strcmp($1,getScope($3))==0){initializeVar(sir_var,$8);} else{printf("Eroare la nivel semantic la linia [%d]: variabila %s nu este declarata sau incercare de asignare gresita.\n",yylineno,$3);} free_expr($8);free($1);free($3);}
| ID '[' INT_CONST ']' ASSIGN RESULT {bzero(sir_var,sizeof(sir_var));bzero(sir_cifra,sizeof(sir_cifra));strcat(sir_var,$1);strcat(sir_var,"_");sprintf(sir_cifra,"%d",$3);strcat(sir_var,sir_cifra);if(isVarDecl(sir_var)==true && getType(sir_var) == $6->type_int){initializeVar(sir_var,$6);}else{printf("Eroare la nivel semantic la linia [%d]: variabila %s nu este declarata.\n",yylineno,sir_var);}free_expr($6);free($1);}
| MYIF '(' EXP ')' '{' instructiuni '}' {if($3->type_int != 3) printf("If-ul de la linia [%d] nu poate fi rulat din cauza conditiei incorecte semantic (nu este expresie booleana)...\n",yylineno);free_expr($3);}
| MYWHILE '(' EXP ')' '{' instructiuni '}' {if($3->type_int != 3) printf("While-ul de la linia [%d] nu poate fi rulat din cauza conditiei incorecte semantic (nu este expresie booleana)...\n",yylineno);free_expr($3);}
| MYFOR '(' instructiune ';' EXP ';' instructiune ')' '{' instructiuni '}' {free_expr($5);}
| EVAL '(' RESULT ')' {if($3->type_int == 1){printf("Rezultatul functiei Eval de la linia [%d] este : ",yylineno);print_expr($3);}else{printf("Eval, linia [%d] - Expresia inserata nu este de tipul INT...\n",yylineno);}free_expr($3);}
| EVAL_FUN ID '(' {func_type_check = y[getFuncID($2)].type_ret_int;ok=true;}lista_apel ')' {if(ok==true && countParams==y[getFuncID($2)].paramsNum) printFuncResult(getFuncID($2)); else printf("Eroare semantica la apelul Eval_fun la linia [%d]...\n",yylineno);free($2);countParams = 0;}
;
DECL_BLOC : Declaratii ';'
| DECL_BLOC Declaratii ';'
;
Declaratii : | tip ID { if(isVarDecl($2)==false) {declareVar($2,$1,scope,false,true,false,false);} else{printf("Eroare la nivel semantic la linia [%d]: a mai fost declarata o variabila cu numele : %s...\n",yylineno,$2);}free($2);free($1);}
| tip ID '[' INT_CONST ']' {if(isVarDecl($2)==false){
                        declareVar($2,"array",scope,false,true,false,true);
                        bzero(sir_var,sizeof(sir_var));
                        for(int i = 0 ; i<$4 ; i++ ){
                            bzero(sir_cifra,sizeof(sir_cifra));
                            strcat(sir_var,$2);
                            strcat(sir_var,"_");
                            sprintf(sir_cifra,"%d",i);
                            strcat (sir_var,sir_cifra);
                            declareVar(sir_var,$1,scope,false,true,false,false);
                           bzero(sir_var,sizeof(sir_var));
                        }

                    }
                    else{
                        printf("Eroare la nivel semantic la linia [%d]: a mai fost declarata o variabila cu numele %s...\n",yylineno,$2);
                    }
                    free($2);free($1);}
| CONST tip ID ASSIGN RESULT {if(isVarDecl($3)==false){declareVar($3,$2,scope,true,true,false,false);initializeVar($3,$5);} else {printf("Eroare la nivel semantic la linia [%d]: a mai fost declarata o variabila cu numele %s...\n",yylineno,$3);}free_expr($5);free($3);free($2);}
| STRUCT ID '{' {if(isVarDecl($2)==false){declareVar($2,"struct",scope,false,true,false,false);} else {printf("Eroare la nivel semantic la linia [%d]: a mai fost declarata o variabila cu numele : %s...\n",yylineno,$2);}bzero(prev_scope,sizeof(prev_scope));strcpy(prev_scope,scope);bzero(scope,sizeof(scope)); strcpy(scope,$2);depth++;} DECL_BLOC {bzero(scope,sizeof(scope)); if(depth == 1){if(program == 1) strcpy(scope,"GLOBAL"); else strcpy(scope,"MAIN FUNCTION");}else{strcpy(scope,prev_scope);}depth--;} '}' {free($2);}

| Decl_fun '{' instructiuni {bzero(scope,sizeof(scope)); if(depth == 1){if(program == 1) strcpy(scope,"GLOBAL"); else strcpy(scope,"MAIN FUNCTION");}else{strcpy(scope,prev_scope);}depth--;bzero(prev_scope,sizeof(prev_scope));} '}'
;
Decl_fun : tip ID '(' param_list ')' {bzero(prev_scope,sizeof(prev_scope));strcpy(prev_scope,scope);bzero(scope,sizeof(scope));strcpy(scope,$2);depth++;if(isFuncDecl($2)==false) {declareFunc($2,$1,countParams,true);} else{printf("Eroare la nivel semantic la linia [%d]: a mai fost declarata o functie cu numele : %s...\n",yylineno,$2);}free($2);free($1);countParams=0;}
| VOID ID '(' param_list')' {bzero(prev_scope,sizeof(prev_scope));strcpy(prev_scope,scope);bzero(scope,sizeof(scope)); strcpy(scope,$2);depth++;if(isFuncDecl($2)==false){declareFunc($2,$1,countParams,true);} else{printf("Eroare la nivel semantic la linia [%d]: a mai fost declarata o functie cu numele : %s... \n",yylineno,$2);}free($2);free($1);countParams=0;}
;
param_list :
| tip ID {countParams++;free($2);free($1);}
| param_list ',' tip ID {countParams++;free($4);free($3);}
;

tip : INTEGER
| CHAR
| STRING
| BOOL
| FLOAT
;
RESULT : CHAR_CONST {$$ = create_char_expr($1);}
| FLOAT_CONST {$$ = create_float_expr($1);}
| EXP {$$ = $1;}
| ID '(' {func_type_check = y[getFuncID($1)].type_ret_int;ok=true;} lista_apel ')' {if(!(ok==true && countParams==y[getFuncID($1)].paramsNum)) {$$ = create_invalid_expr();printf("Eroare la nivel semantic la linia [%d] : apel invalid a functiei %s...\n",yylineno,$1);} else $$=create_custom_expr_2($1);free($1);countParams=0;}
;
EXP : ID {$$ = create_custom_expr($1); free($1);}
| INT_CONST {$$ = create_int_expr($1);}
| BOOL_CONST {$$ = create_bool_expr($1);}
| STR_CONST {$$ = create_string_expr($1,NULL);free($1);}
| EXP PLUS EXP {if($1->type_int==1 && $3->type_int==1)$$ = create_int_expr($1->intVal + $3->intVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| EXP MINUS EXP {if($1->type_int==1 && $3->type_int==1)$$ = create_int_expr($1->intVal - $3->intVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| EXP MUL EXP {if($1->type_int==1 && $3->type_int==1)$$ = create_int_expr($1->intVal * $3->intVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| EXP DIV EXP {if($1->type_int==1 && $3->type_int==1)$$ = create_int_expr($1->intVal / $3->intVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| EXP MOD EXP {if($1->type_int==1 && $3->type_int==1)$$ = create_int_expr($1->intVal % $3->intVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| NOT EXP {if($2->type_int==3)$$ = create_bool_expr($2->boolVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($2);}
| EXP SAU EXP {if($1->type_int==3 && $3->type_int==3)$$ = create_bool_expr($1->boolVal || $3->boolVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| EXP SI EXP {if($1->type_int==3 && $3->type_int==3)$$ = create_bool_expr($1->boolVal && $3->boolVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| EXP LT EXP {if($1->type_int==1 && $3->type_int==1)$$ = create_bool_expr($1->intVal < $3->intVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| EXP GT EXP {if($1->type_int==1 && $3->type_int==1)$$ = create_bool_expr($1->intVal > $3->intVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| EXP LTE EXP {if($1->type_int==1 && $3->type_int==1)$$ = create_bool_expr($1->intVal <= $3->intVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| EXP GTE EXP {if($1->type_int==1 && $3->type_int==1)$$ = create_bool_expr($1->intVal >= $3->intVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| EXP EQ EXP {if($1->type_int==1 && $3->type_int==1)$$ = create_bool_expr($1->intVal == $3->intVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| EXP CONCAT EXP {if($1->type_int==2 && $3->type_int==2)$$ = create_string_expr($1->strVal,$3->strVal); else{printf("Expresie invalida la linia : [%d]...\n",yylineno);$$=create_invalid_expr();} free_expr($1); free_expr($3);}
| '(' EXP ')' {$$ = $2;}
;
lista_apel : | RESULT {if(func_type_check!=$1->type_int) ok = false;countParams++;free_expr($1);}
| lista_apel ',' RESULT {if(func_type_check!=$3->type_int) ok = false;countParams++;free_expr($3);}
;


%%
Expr* create_custom_expr_2(char* fun){
    for(int i=1; i<=countFuncs; i++)
    {
        if(strcmp(y[i].name,fun)==0)
        {
            if(y[i].isDecl==true){
                switch(y[i].type_ret_int)
                {
                case -1 : return create_invalid_expr(); break;
                case 1 : return create_int_expr(y[i].intVal); break;
                case 2 : return create_string_expr(y[i].strVal,NULL); break;
                case 3 : return create_bool_expr(y[i].boolVal); break;
                case 4 : return create_char_expr(y[i].charVal); break;
                case 5 : return create_float_expr(y[i].floatVal); break;
                case 6 : return create_invalid_expr(); break;
                }
            }
        }
    }
    return create_invalid_expr();
}
Expr* create_custom_expr(char* var){
    for(int i=1; i<=countVars; i++)
    {
        if(strcmp(x[i].name,var)==0)
        {
            if(x[i].isInit==true){
                switch(x[i].type_int)
                {
                case -1 : return create_invalid_expr(); break;
                case 1 : return create_int_expr(x[i].intVal); break;
                case 2 : return create_string_expr(x[i].strVal,NULL); break;
                case 3 : return create_bool_expr(x[i].boolVal); break;
                case 4 : return create_char_expr(x[i].charVal); break;
                case 5 : return create_float_expr(x[i].floatVal); break;
                }
            }
        }
    }
    return create_invalid_expr();
}
Expr* create_invalid_expr(){
    Expr* expr = (Expr*)malloc(sizeof(Expr));
    expr->type_int = -1;
    return expr;
}
Expr* create_int_expr(int val){
    Expr* expr = (Expr*)malloc(sizeof(Expr));
    expr->intVal = val;
    expr->type_int = 1;
    return expr;
}
Expr* create_bool_expr(bool val){
    Expr* expr = (Expr*)malloc(sizeof(Expr));
    expr->boolVal = val;
    expr->type_int = 3;
    return expr;
}
Expr* create_string_expr(char* val1, char* val2){
    Expr* expr = (Expr*)malloc(sizeof(Expr));
    int len2 = val2 ? strlen(val2) : 0;
    expr->strVal = (char*)malloc(sizeof(char)*(strlen(val1)+len2+1));
    strcpy(expr->strVal,val1);
    if(val2){
        strcat(expr->strVal,val2);
    }
    expr->type_int = 2;
    return expr;
}
Expr* create_float_expr(float val){
    Expr* expr = (Expr*)malloc(sizeof(Expr));
    expr->floatVal = val;
    expr->type_int = 5;
    return expr;
}
Expr* create_char_expr(char val){
    Expr* expr = (Expr*)malloc(sizeof(Expr));
    expr->charVal = val;
    expr->type_int = 4;
    return expr;
}
void free_expr(Expr* expr){
    if(expr->type_int == 2)
        free(expr->strVal);
    free(expr);
}
void printFuncResult(int IDfunc){
    if(y[IDfunc].isDecl==true){
        switch(y[IDfunc].type_ret_int)
                {
                case -1 : printf("Eval_func de la linia [%d] este invalida...\n",yylineno); break;
                case 1 : printf("Eval_func de la linia [%d] are valoarea %d...\n",yylineno,y[IDfunc].intVal); break;
                case 2 : printf("Eval_func de la linia [%d] are valoarea %s...\n",yylineno,y[IDfunc].strVal); break;
                case 3 : printf("Eval_func de la linia [%d] are valoarea %s...\n",yylineno,y[IDfunc].boolVal? "true" : "false"); break;
                case 4 : printf("Eval_func de la linia [%d] are valoarea %c...\n",yylineno,y[IDfunc].charVal); break;
                case 5 : printf("Eval_func de la linia [%d] are valoarea %f...\n",yylineno,y[IDfunc].floatVal); break;
                case 6 : printf("Eval_func de la linia [%d] are valoarea NULL...\n",yylineno); break;
                }
    }
    else
        printf("Eval_fun de la linia [%d] - Functia nu a fost declarata!...",yylineno);
}
int getFuncID(char* nume){
for(int i=0; i<=countFuncs; i++)
    {
        if(strcmp(y[i].name,nume)==0)
        {
            return i;
        }
    }
    return 0;
}
void print_expr(Expr* expr){
    if(expr->type_int==1){
        printf("INT expression with value: %d\n",expr->intVal);
    }
    else{
            if(expr->type_int==2){
                printf("STRING expression with value: %s\n",expr->strVal);
            }
            else{
                if(expr->type_int==3){
                    printf("BOOL expression with value: %s\n",expr->boolVal? "true" : "false");
                }
                else{
                    if(expr->type_int==4){
                        printf("CHAR expr with value: %c\n",expr->charVal);
                    }
                    else{
                        printf("FLOAT expr with value: %f\n",expr->floatVal);
                    }
                }
            }
        }
}
void initializeFunc(char* nume, Expr* expr){
    for(int i=1; i<=countFuncs; i++)
    {
        if(strcmp(y[i].name,nume)==0)
        {
            if(y[i].isDecl == true && y[i].type_ret_int == expr->type_int && y[i].isInit == false)
            {
                    y[i].isInit = true;
                    if(strcmp(y[i].type_ret,"int")==0)
                    {
                        y[i].intVal=expr->intVal;
                    }
                    else
                    {
                        if(strcmp(y[i].type_ret,"bool")==0)
                        {
                            y[i].boolVal=expr->boolVal;
                        }
                        else
                        {
                            if(strcmp(y[i].type_ret,"char")==0)
                            {
                                y[i].charVal=expr->charVal;
                            }
                            else
                            {
                                if(strcmp(y[i].type_ret,"string")==0)
                                {
                                    bzero(y[i].strVal,sizeof(y[i].strVal));
                                    strcat(y[i].strVal,expr->strVal);
                                }
                                else
                                {
                                    if(strcmp(y[i].type_ret,"float")==0)
                                    {
                                        x[i].floatVal=expr->floatVal;
                                    }
                                }
                            }
                        }
                updateTable();
                }
            }
            else
                printf("Eroare la nivel semantic la linia [%d] : tipul result-ului nu se potriveste cu tipul functiei...\n",yylineno);
        break;
        }
    }
}
bool isVarDecl(char* nume)
{
    for(int i=0; i<=countVars; i++)
    {
        if(strcmp(x[i].name,nume)==0)
        {
            return true;
        }
    }
    return false;
}
int getType(char * nume)
{
for(int i=1 ; i<=countVars ; i++)
{
if(strcmp(x[i].name,nume)==0)
return x[i].type_int;
}
return -1;
}
char* getType_char(char * nume)
{
for(int i=1 ; i<=countVars ; i++)
{
if(strcmp(x[i].name,nume)==0)
return x[i].type;
}
return "invalid";
}
char* getScope(char * nume)
{
for(int i=1 ; i<=countVars ; i++)
{
if(strcmp(x[i].name,nume)==0)
return x[i].scope_lvl;
}



}
bool isFuncDecl(char* nume)
{
    for(int i=0; i<=countFuncs; i++)
    {
        if(strcmp(y[i].name,nume)==0)
        {
            return true;
        }
    }
    return false;
}
void declareVar(char* nume, char* tip, char* scope, bool const_state, bool decl_state, bool init_state,bool array_state)
{
    if(isVarDecl(nume)==false)
    {
        countVars++;
        strcat(x[countVars].name,nume);
        strcat(x[countVars].type,tip);
        if(strcmp(tip,"int")==0)
        {
            x[countVars].type_int=1;
        }
        else
        {
            if(strcmp(tip,"bool")==0)
            {
                x[countVars].type_int=3;
            }
            else
            {
                if(strcmp(tip,"char")==0)
                {
                    x[countVars].type_int=4;
                }
                else
                {
                    if(strcmp(tip,"string")==0)
                    {
                        x[countVars].type_int=2;
                    }
                    else
                    {
                        if(strcmp(tip,"float")==0)
                        {
                            x[countVars].type_int=5;
                        }
                    }
                }
            }
        }
        strcat(x[countVars].scope_lvl,scope);
        x[countVars].isConst = const_state;
        x[countVars].isDecl = decl_state;
        x[countVars].isInit = init_state;
        x[countVars].isArray = array_state;
        updateTable();
    }
}
void declareFunc(char* nume, char* tip,int nr, bool decl_state)
{
    if(isFuncDecl(nume)==false)
    {
        countFuncs++;
        y[countFuncs].paramsNum=nr;
        strcat(y[countFuncs].name,nume);
        strcat(y[countFuncs].type_ret,tip);
        if(strcmp(tip,"int")==0)
        {
            y[countFuncs].type_ret_int=1;
        }
        else
        {
            if(strcmp(tip,"bool")==0)
            {
                y[countFuncs].type_ret_int=3;
            }
            else
            {
                if(strcmp(tip,"char")==0)
                {
                    y[countFuncs].type_ret_int=4;
                }
                else
                {
                    if(strcmp(tip,"string")==0)
                    {
                        y[countFuncs].type_ret_int=2;
                    }
                    else
                    {
                        if(strcmp(tip,"float")==0)
                        {
                            y[countFuncs].type_ret_int=5;
                        }
                        else{
                            y[countFuncs].type_ret_int=6;
                        }
                    }
                }
            }
        }
        y[countFuncs].isDecl = decl_state;
        y[countFuncs].isInit = false;
        updateTable();
    }
}
void initializeVar(char* nume, Expr* expr)
{
    for(int i=1; i<=countVars; i++)
    {
        if(strcmp(x[i].name,nume)==0)
        {
            if(x[i].isDecl == true && x[i].type_int == expr->type_int)
            {
                if(x[i].isInit == false || (x[i].isInit==true && x[i].isConst==false))
                {
                    x[i].isInit = true;
                    if(strcmp(x[i].type,"int")==0)
                    {
                        x[i].intVal=expr->intVal;
                    }
                    else
                    {
                        if(strcmp(x[i].type,"bool")==0)
                        {
                            x[i].boolVal=expr->boolVal;
                        }
                        else
                        {
                            if(strcmp(x[i].type,"char")==0)
                            {
                                x[i].charVal=expr->charVal;
                            }
                            else
                            {
                                if(strcmp(x[i].type,"string")==0)
                                {
                                    bzero(x[i].strVal,sizeof(x[i].strVal));
                                    strcat(x[i].strVal,expr->strVal);
                                }
                                else
                                {
                                    if(strcmp(x[i].type,"float")==0)
                                    {
                                        x[i].floatVal=expr->floatVal;
                                    }
                                }
                            }
                        }
                    }
                updateTable();
                }
            }
        break;
        }
    }
}
void updateTable()
{
    remove("symbol_table.txt");
    FILE* fd = fopen("symbol_table.txt","a");
    if (fd == NULL)
    {
        perror("Eroare la deschiderea bazei de date");
        exit(1);
    }
    fprintf(fd,"~~~~~~~~~~ VARIABILE ~~~~~~~~~~\n\n");
    for(int i=1; i<=countVars; i++)
    {
        if(x[i].isInit==true)
        {
            if(strcmp(x[i].type,"int")==0)
            {
                fprintf(fd,"Numele variabilei : [%s] - Tipul variabilei : [%s%s] - Scope-ul variabilei : [%s] - Valoarea variabilei : [%d]\n", x[i].name, x[i].isConst? "const " : "",x[i].type, x[i].scope_lvl,x[i].intVal);
            }
            else
            {
                if(strcmp(x[i].type,"bool")==0)
                {
                    fprintf(fd,"Numele variabilei : [%s] - Tipul variabilei : [%s%s] - Scope-ul variabilei : [%s] - Valoarea variabilei : [%s]\n", x[i].name,x[i].isConst? "const " : "", x[i].type, x[i].scope_lvl,x[i].boolVal? "true" : "false");
                }
                else
                {
                    if(strcmp(x[i].type,"char")==0)
                    {
                        fprintf(fd,"Numele variabilei : [%s] - Tipul variabilei : [%s%s] - Scope-ul variabilei : [%s] - Valoarea variabilei : [%c]\n", x[i].name,x[i].isConst? "const " : "", x[i].type, x[i].scope_lvl,x[i].charVal);
                    }
                    else
                    {
                        if(strcmp(x[i].type,"string")==0)
                        {
                            fprintf(fd,"Numele variabilei : [%s] - Tipul variabilei : [%s%s] - Scope-ul variabilei : [%s] - Valoarea variabilei : [%s]\n", x[i].name,x[i].isConst? "const " : "", x[i].type, x[i].scope_lvl, x[i].strVal);
                        }
                        else
                        {
                            if(strcmp(x[i].type,"float")==0)
                            {
                                fprintf(fd,"Numele variabilei : [%s] - Tipul variabilei : [%s%s] - Scope-ul variabilei : [%s] - Valoarea variabilei : [%f]\n", x[i].name,x[i].isConst? "const " : "", x[i].type, x[i].scope_lvl,x[i].floatVal);
                            }
                        }
                    }
                }
            }
        }
        else
        {
            fprintf(fd,"Numele variabilei : [%s] - Tipul variabilei : [%s%s] - Scope-ul variabilei : [%s] - Valoarea variabilei : [NULL]\n", x[i].name,x[i].isConst? "const " : "", x[i].type, x[i].scope_lvl);
        }
    }
    fprintf(fd,"\n~~~~~~~~~~ FUNCTII ~~~~~~~~~~\n\n");
    for(int i=1; i<=countFuncs; i++)
    {
        if(y[i].isDecl==true)
        {
            if(y[i].isInit==true)
            {
            if(strcmp(y[i].type_ret,"int")==0)
            {
                fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [%s] - Valoarea return-ului : [%d]\n", y[i].name,y[i].paramsNum,y[i].type_ret,y[i].intVal);
            }
            else
            {
                if(strcmp(y[i].type_ret,"bool")==0)
                {
                    fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [%s] - Valoarea return-ului : [%d]\n", y[i].name,y[i].paramsNum,y[i].type_ret,y[i].boolVal);
                }
                else
                {
                    if(strcmp(y[i].type_ret,"char")==0)
                    {
                        fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [%s] - Valoarea return-ului : [%c]\n", y[i].name,y[i].paramsNum,y[i].type_ret,y[i].charVal);
                    }
                    else
                    {
                        if(strcmp(y[i].type_ret,"string")==0)
                        {
                            fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [%s] - Valoarea return-ului : [%s]\n", y[i].name,y[i].paramsNum,y[i].type_ret,y[i].strVal);
                        }
                        else
                        {
                            if(strcmp(y[i].type_ret,"float")==0)
                            {
                                fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [%s] - Valoarea return-ului : [%f]\n", y[i].name,y[i].paramsNum,y[i].type_ret,y[i].floatVal);
                            }
                            else
                                fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [VOID] - Valoarea return-ului : [NULL]\n", y[i].name,y[i].paramsNum);
                        }
                    }
                }
            }
            }
            else{
                if(strcmp(y[i].type_ret,"int")==0)
            {
                fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [%s] - Valoarea return-ului : [NULL]\n", y[i].name,y[i].paramsNum,y[i].type_ret);
            }
            else
            {
                if(strcmp(y[i].type_ret,"bool")==0)
                {
                    fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [%s] - Valoarea return-ului : [NULL]\n", y[i].name,y[i].paramsNum,y[i].type_ret);
                }
                else
                {
                    if(strcmp(y[i].type_ret,"char")==0)
                    {
                        fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [%s] - Valoarea return-ului : [NULL]\n", y[i].name,y[i].paramsNum,y[i].type_ret);
                    }
                    else
                    {
                        if(strcmp(y[i].type_ret,"string")==0)
                        {
                            fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [%s] - Valoarea return-ului : [NULL]\n", y[i].name,y[i].paramsNum,y[i].type_ret);
                        }
                        else
                        {
                            if(strcmp(y[i].type_ret,"float")==0)
                            {
                                fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [%s] - Valoarea return-ului : [NULL]\n", y[i].name,y[i].paramsNum,y[i].type_ret);
                            }
                            else
                                fprintf(fd,"Numele functiei : [%s] - Numarul de parametri : [%d] - Tipul de return : [VOID] - Valoarea return-ului : [NULL]\n", y[i].name,y[i].paramsNum);
                        }
                    }
                }
            }
            }
        }
    }
    fclose(fd);
}

void yyerror(char* s)
{
    printf("error:%s la linia:%d si cuvantul %s\n",s,yylineno,yytext);
}
int main(int argc, char** argv)
{
    yyin=fopen(argv[1],"r");
    yyparse();
}