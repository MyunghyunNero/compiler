digit [0-9]
letter [a-zA-Z_]
delim [ \t]
line [\n]
ws {delim}+

%{
#include "y.tab.h"

#define YYSTYPE_IS_DECLARED 1

typedef long YYSTYPE;

extern int yylval;
extern int line_no;
extern A_ID *current_id;

int check_identifier();
%}

%%
{ws}      { }
{line}    { line_no++;}
auto      { return AUTO_SYM; }
break     { return BREAK_SYM; }
case      { return CASE_SYM; }
continue  { return CONTINUE_SYM; }
default   { return DEFAULT_SYM; }
do        { return DO_SYM; }
else      { return ELSE_SYM; }
enum      { return ENUM_SYM; }
for       { return FOR_SYM; }
if        { return IF_SYM; }
return    { return RETURN_SYM; }
sizeof    { return SIZEOF_SYM; }
static    { return STATIC_SYM; }
struct    { return STRUCT_SYM; }
switch    { return SWITCH_SYM; }
typedef   { return TYPEDEF_SYM; }
union     { return UNION_SYM; }
while     { return WHILE_SYM; }

"\+\+"    { return PLUSPLUS; }
"\-\-"    { return MINUSMINUS; }
"\->"     { return ARROW; }
"<"       { return LSS; }
">"       { return GTR; }
"<="      { return LEQ; }
">="      { return GEQ; }
"=="      { return EQL; }
"!="      { return NEQ; }
"&&"      { return AMPAMP; }
"||"      { return BARBAR; }
"\.\.\."  { return DOTDOTDOT; }
"\("      { return LP; }
"\)"      { return RP; }
"\["      { return LB; }
"\]"      { return RB; }
"\{"      { return LR; }
"\}"      { return RR; }
"\:"      { return COLON; }
"\."      { return PERIOD; }
"\,"      { return COMMA; }
"\!"      { return EXCL; }
"\*"      { return STAR; }
"\/"      { return SLASH; }
"\%"      { return PERCENT; }
"\&"      { return AMP; }
"\;"      { return SEMICOLON; }
"\+"      { return PLUS; }
"\-"      { return MINUS; }
"\="      { return ASSIGN; }

{digit}+ {
    yylval = atoi(yytext);    //정수 값 가져오기 위함
    return INTEGER_CONSTANT;
}
{digit}+\.{digit}+ {
    yylval = strdup(yytext);   //토큰의 스트링 주소 가져오기 위함
    return FLOAT_CONSTANT;
}
{letter}({letter}|{digit})* {
    return check_identifier(yytext);
}
\"([^"\n]|\\["\n])*\" {
    yylval = strdup(yytext);   //토큰의 스트링 주소 가져오기 위함
    return(STRING_LITERAL);
}
\'([^'\n]|\'\')\' {
    yylval = *(yytext + 1);    //그 문자 값을 갖기 위함
    return CHARACTER_CONSTANT;
}
"//"[^\n]* { }
%%

int check_identifier(char *s) {
    A_ID *id; 
    char *t; 
    id = current_id; 
    while (id) {
        if (strcmp(id->name, s) == 0) 
            break;
        id = id->prev; 
    } 
    
    if (id == 0) {
        yylval = strdup(s);
        return IDENTIFIER;       
    } 
    else if (id->kind == ID_TYPE) {
        yylval = id->type;
        return TYPE_IDENTIFIER;   
    } 
    else {
        yylval = id->name; 
        return IDENTIFIER;
    }
}

int yywrap() {
    return 1;
}