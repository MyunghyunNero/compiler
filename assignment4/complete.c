/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>  // 이거 없으면 strlen 같은 함수 인식 못해서 에러-> 추가해줘서 해결
#include "type.h"
#include "y.tab.h"

char *node_name[] = {
    "N_NULL", "N_PROGRAM", "N_EXP_IDENT", "N_EXP_INT_CONST", "N_EXP_FLOAT_CONST", "N_EXP_CHAR_CONST", "N_EXP_STRING_LITERAL",
    "N_EXP_ARRAY", "N_EXP_FUNCTION_CALL", "N_EXP_STRUCT", "N_EXP_ARROW", "N_EXP_POST_INC", "N_EXP_POST_DEC", "N_EXP_PRE_INC", "N_EXP_PRE_DEC", "N_EXP_AMP", "N_EXP_STAR", "N_EXP_NOT", "N_EXP_PLUS", "N_EXP_MINUS", "N_EXP_SIZE_EXP", "N_EXP_SIZE_TYPE", "N_EXP_CAST", "N_EXP_MUL", "N_EXP_DIV", "N_EXP_MOD", "N_EXP_ADD", "N_EXP_SUB",
    "N_EXP_LSS",
    "N_EXP_GTR",
    "N_EXP_LEQ", "N_EXP_GEQ", "N_EXP_NEQ", "N_EXP_EQL", "N_EXP_AND",
    "N_EXP_OR", "N_EXP_ASSIGN", "N_ARG_LIST", "N_ARG_LIST_NIL", "N_STMT_LABEL_CASE", "N_STMT_LABEL_DEFAULT",
    "N_STMT_COMPOUND", "N_STMT_EMPTY", "N_STMT_EXPRESSION", "N_STMT_IF", "N_STMT_IF_ELSE", "N_STMT_SWITCH", "N_STMT_WHILE", "N_STMT_DO", "N_STMT_FOR", "N_STMT_RETURN", "N_STMT_CONTINUE", "N_STMT_BREAK", "N_FOR_EXP", "N_STMT_LIST", "N_STMT_LIST_NIL", "N_INIT_LIST", "N_INIT_LIST_ONE", "N_INIT_LIST_NIL"};

void print_ast(A_NODE *);
void prt_program(A_NODE *, int);
void prt_initializer(A_NODE *, int);
void prt_arg_expr_list(A_NODE *, int);
void prt_statement(A_NODE *, int);
void prt_statement_list(A_NODE *, int);
void prt_for_expression(A_NODE *, int);
void prt_expression(A_NODE *, int);
void prt_A_TYPE(A_TYPE *, int);
void prt_A_ID_LIST(A_ID *, int);
void prt_A_ID(A_ID *, int);
void prt_A_ID_NAME(A_ID *, int);
void prt_STRING(char *, int);
void prt_integer(int, int);
void print_node(A_NODE *, int);
void print_space(int);

void print_sem_ast(A_NODE *);
void prt_sem_program(A_NODE *, int);

void prt_sem_initializer(A_NODE *, int);
void prt_sem_arg_expr_list(A_NODE *, int);
void prt_sem_statement(A_NODE *, int);
void prt_sem_statement_list(A_NODE *, int);
void prt_sem_for_expression(A_NODE *, int);
void prt_sem_expression(A_NODE *, int);
void prt_sem_A_TYPE(A_TYPE *, int);
void prt_sem_A_ID_LIST(A_ID *, int);
void prt_sem_A_ID(A_ID *, int);
void prt_sem_A_ID_NAME(A_ID *, int);
void prt_sem_LITERAL(int, int);
void prt_sem_integer(int, int);


#define LIT_MAX 100

int global_address = 12;
int semantic_err = 0;
A_LITERAL literal_table[LIT_MAX];
int literal_no = 0;
int literal_size = 0;
//float atof(); // in main, already stdlib.h was called
void semantic_analysis(A_NODE *);
void set_literal_address(A_NODE *);
int put_literal(A_LITERAL, int);
void sem_program(A_NODE *);
A_TYPE*sem_expression(A_NODE *);
int sem_statement(A_NODE *, int, A_TYPE *, BOOLEAN, BOOLEAN, BOOLEAN);
int sem_statement_list(A_NODE *, int, A_TYPE *, BOOLEAN, BOOLEAN, BOOLEAN);
void sem_for_expression(A_NODE *);
int sem_A_TYPE(A_TYPE *) ;
int sem_declaration_list(A_ID *id, int addr);
int sem_declaration(A_ID *,int);
void sem_arg_expr_list(A_NODE *, A_ID *);
A_ID *getStructFieldIdentifier(A_TYPE *, char *);
A_ID *getPointerFieldIdentifier(A_TYPE *, char *);
A_NODE *convertScalarToInteger(A_NODE *);
A_NODE *convertUsualAssignmentConversion(A_TYPE *, A_NODE *);
A_NODE *convertUsualUnaryConversion(A_NODE *);
A_TYPE *convertUsualBinaryConversion(A_NODE *);
A_NODE *convertCastingConversion(A_NODE *,A_TYPE *);
BOOLEAN isAllowableAssignmentConversion(A_TYPE *, A_TYPE *, A_NODE *); 
BOOLEAN isAllowableCastingConversion(A_TYPE *, A_TYPE *);
BOOLEAN isModifiableLvalue(A_NODE *);
BOOLEAN isConstantZeroExp(A_NODE *);
BOOLEAN isSameParameterType(A_ID *, A_ID *);
BOOLEAN isNotSameType(A_TYPE *, A_TYPE *);
BOOLEAN isCompatibleType(A_TYPE *, A_TYPE *);
BOOLEAN isCompatiblePointerType(A_TYPE *, A_TYPE *);
BOOLEAN isIntType(A_TYPE *);
BOOLEAN isFloatType(A_TYPE *);
BOOLEAN isArithmeticType(A_TYPE *);
BOOLEAN isAnyIntegerType(A_TYPE *);
BOOLEAN isIntegralType(A_TYPE *);
BOOLEAN isStructOrUnionType(A_TYPE *);
BOOLEAN isFunctionType(A_TYPE *);
BOOLEAN isScalarType(A_TYPE *);
BOOLEAN isPointerType(A_TYPE *);
BOOLEAN isPointerOrArrayType_sem(A_TYPE *);
BOOLEAN isArrayType(A_TYPE *);
BOOLEAN isStringType(A_TYPE *);
BOOLEAN isVoidType(A_TYPE *);
A_LITERAL checkTypeAndConvertLiteral(A_LITERAL,A_TYPE*, int);
A_LITERAL getTypeAndValueOfExpression(A_NODE *);
A_TYPE *makeType(T_KIND);
void setTypeSize(A_TYPE *, int);
void semantic_warning(int, int);
void semantic_error();

extern char *yytext;

A_TYPE *int_type, *char_type, *void_type, *float_type, *string_type;
A_NODE *root;
A_ID *current_id = NIL;
int syntax_err = 0;
int line_no = 1;
int current_level = 0;
A_NODE *makeNode(NODE_NAME, A_NODE *, A_NODE *, A_NODE *);
A_NODE *makeNodeList(NODE_NAME, A_NODE *, A_NODE *);
A_ID *makeIdentifier(char *);
A_ID *makeDummyIdentifier();
A_TYPE *makeType(T_KIND);
A_SPECIFIER *makeSpecifier(A_TYPE *, S_KIND);
A_ID *searchIdentifier(char *, A_ID *);
A_ID *searchIdentifierAtCurrentLevel(char *, A_ID *);
A_SPECIFIER *updateSpecifier(A_SPECIFIER *, A_TYPE *, S_KIND);
void checkForwardReference();
void setDefaultSpecifier(A_SPECIFIER *);
A_ID *linkDeclaratorList(A_ID *, A_ID *);
A_ID *getIdentifierDeclared(char *);
A_TYPE *getTypeOfStructOrEnumRefIdentifier(T_KIND, char *, ID_KIND);
A_ID *setDeclaratorInit(A_ID *, A_NODE *);
A_ID *setDeclaratorKind(A_ID *, ID_KIND);
A_ID *setDeclaratorType(A_ID *, A_TYPE *);
A_ID *setDeclaratorElementType(A_ID *, A_TYPE *);
A_ID *setDeclaratorTypeAndKind(A_ID *, A_TYPE *, ID_KIND);
A_ID *setDeclaratorListSpecifier(A_ID *, A_SPECIFIER *);
A_ID *setFunctionDeclaratorSpecifier(A_ID *, A_SPECIFIER *);
A_ID *setFunctionDeclaratorBody(A_ID *, A_NODE *);
A_ID *setParameterDeclaratorSpecifier(A_ID *, A_SPECIFIER *);
A_ID *setStructDeclaratorListSpecifier(A_ID *, A_TYPE *);
A_TYPE *setTypeNameSpecifier(A_TYPE *, A_SPECIFIER *);
A_TYPE *setTypeElementType(A_TYPE *, A_TYPE *);
A_TYPE *setTypeField(A_TYPE *, A_ID *);
A_TYPE *setTypeExpr(A_TYPE *, A_NODE *);
A_TYPE *setTypeAndKindOfDeclarator(A_TYPE *, ID_KIND, A_ID *);
A_TYPE *setTypeStructOrEnumIdentifier(T_KIND, char *, ID_KIND);
BOOLEAN isNotSameFormalParameters(A_ID *, A_ID *);
BOOLEAN isNotSameType(A_TYPE *, A_TYPE *);
BOOLEAN isPointerOrArrayType(A_TYPE *);

void syntax_error(int, char *);
void initialize();

char *id_kind_name[] = {"NULL", "VAR", "FUNC", "PARM", "FIELD", "TYPE", "ENUM", "STRUCT", "ENUM_LITERAL"};
char *spec_name[] = {"NULL", "AUTO", "STATIC", "TYPEDEF"};

typedef enum op {
    OP_NULL,
    LOD,
    LDX,
    LDXB,
    LDA,
    LITI,
    STO,
    STOB,
    STX,
    STXB,
    SUBI,
    SUBF,
    DIVI,
    DIVF,
    ADDI,
    ADDF,
    OFFSET,
    MULI,
    MULF,
    MOD,
    LSSI,
    LSSF,
    GTRI,
    GTRF,
    LEQI,
    LEQF,
    GEQI,
    GEQF,
    NEQI,
    NEQF,
    EQLI,
    EQLF,
    NOT,
    OR,
    AND,
    CVTI,
    CVTF,
    JPC,
    JPCR,
    JMP,
    JPT,
    JPTR,
    INT,
    INCI,
    INCF,
    DECI,
    DECF,
    SUP,
    CAL,
    ADDR,
    RET,
    MINUSI,
    MINUSF,
    CHK,
    LDI,
    LDIB,
    SWITCH,
    SWVALUE,
    SWDEFAULT,
    SWLABEL,
    SWEND,
    POP,
    POPB
} OPCODE;

typedef enum {
    SW_VALUE, SW_DEFAULT
} SW_KIND;

typedef struct sw {
    SW_KIND kind;
    int val;
    int label;
} A_SWITCH;

void code_generation(A_NODE *);

void gen_literal_table();

void gen_program(A_NODE *);

void gen_global_init_list(A_ID *);

void gen_expression(A_NODE *);

void gen_expression_left(A_NODE *);

void gen_arg_expression(A_NODE *);

void gen_statement(A_NODE *, int, int, A_SWITCH [], int *);

void gen_statement_list(A_NODE *, int, int, A_SWITCH [], int *);

void gen_initializer_global(A_NODE *, A_TYPE *, int);

void gen_initializer_local(A_NODE *, A_TYPE *, int);

void gen_declaration_list(A_ID *);

void gen_declaration(A_ID *);

void gen_code_i(OPCODE, int, int);

void gen_code_f(OPCODE, int, float);

void gen_code_s(OPCODE, int, char *);

void gen_code_l(OPCODE, int, int);

void gen_label_number(int);

void gen_label_name(char *);

void gen_error(int i, int ll, char *s);

int get_label();

char *opcode_name[] = {
        "OP_NULL",
        "LOD",
        "LDX",
        "LDXB",
        "LDA",
        "LITI",
        "STO",
        "STOB",
        "STX",
        "STXB",
        "SUBI",
        "SUBF",
        "DIVI",
        "DIVF",
        "ADDI",
        "ADDF",
        "OFFSET",
        "MULI",
        "MULF",
        "MOD",
        "LSSI",
        "LSSF",
        "GTRI",
        "GTRF",
        "LEQI",
        "LEQF",
        "GEQI",
        "GEQF",
        "NEQI",
        "NEQF",
        "EQLI",
        "EQLF",
        "NOT",
        "OR",
        "AND",
        "CVTI",
        "CVTF",
        "JPC",
        "JPCR",
        "JMP",
        "JPT",
        "JPTR",
        "INT",
        "INCI",
        "INCF",
        "DECI",
        "DECF",
        "SUP",
        "CAL",
        "ADDR",
        "RET",
        "MINUSI",
        "MINUSF",
        "CHK",
        "LDI",
        "LDIB",
        "SWITCH",
        "SWVALUE",
        "SWDEFAULT",
        "SWLABEL",
        "SWEND",
        "POP",
        "POPB"
};

int label_no = 0;
int gen_err = 0;
extern FILE *fout;
extern A_TYPE *int_type, *float_type, *char_type, *void_type, *string_type;
extern A_LITERAL literal_table[];
extern int literal_no;

void code_generation(A_NODE *node) {
    gen_program(node);
    gen_literal_table();
}

void gen_literal_table() {
    int i;
    for (i = 1; i <= literal_no; i++) {
        fprintf(fout, ".literal %5d ", literal_table[i].addr);
        if (literal_table[i].type == int_type)
            fprintf(fout, "%d\n", literal_table[i].value.i);
        else if (literal_table[i].type == float_type)
            fprintf(fout, "%f\n", literal_table[i].value.f);
        else if (literal_table[i].type == char_type)
            fprintf(fout, "%d\n", literal_table[i].value.c);
        else if (literal_table[i].type == string_type)
            fprintf(fout, "%s\n", literal_table[i].value.s);
    }
}

void gen_program(A_NODE *node) {
    switch (node->name) {
        case N_PROGRAM :
            gen_code_i(INT, 0, node->value);
            // gen_global_init_list((A_ID *) node->clink);
            gen_code_s(SUP, 0, "main");
            gen_code_i(RET, 0, 0);
            gen_declaration_list((A_ID *) node->clink);
            break;
        default :
            gen_error(100, node->line, "");
            break;
    }
}

void gen_global_init_list(A_ID *id) {
    while (id) {
        if (id->kind == ID_VAR && id->level == 0 && id->init) {
            gen_initializer_global(id->init, id->type, id->address);
        } else if (id->kind == ID_FUNC) {
            A_ID* fields = (A_ID *) id->type->expr->llink;
            gen_global_init_list(fields);
        } else {

        }

        id = id->link;
    }
}

void gen_expression(A_NODE *node) {
    A_ID *id;
    A_TYPE *t;
    int i, ll;
    switch (node->name) {
        case N_EXP_IDENT :
            id = (A_ID *) node->clink;
            t = id->type;
            switch (id->kind) {
                case ID_VAR:
                case ID_PARM:
                    switch (t->kind) {
                        case T_ENUM:
                        case T_POINTER:
                            gen_code_i(LOD, id->level, id->address);
                            break;
                        case T_ARRAY:
                            if (id->kind == ID_VAR)
                                gen_code_i(LDA, id->level, id->address);
                            else
                                gen_code_i(LOD, id->level, id->address);
                            break;
                        case T_STRUCT:
                        case T_UNION:
                            gen_code_i(LDA, id->level, id->address);
                            i = id->type->size;
                            gen_code_i(LDI, 0, i % 4 ? i / 4 + 1 : i / 4);
                            break;
                        default:
                            gen_error(11, id->line, "");
                            break;
                    }
                    break;
                case ID_ENUM_LITERAL:
                    gen_code_i(LITI, 0, (int) id->init);
                    break;
                default:
                    gen_error(11, node->line, "");
                    break;
            }
            break;
        case N_EXP_INT_CONST :
            gen_code_i(LITI, 0, (int) node->clink);
            break;
        case N_EXP_FLOAT_CONST :
            i = (int) node->clink;
            gen_code_i(LOD, 0, literal_table[i].addr);
            break;
        case N_EXP_CHAR_CONST :
            gen_code_i(LITI, 0, (int) node->clink);
            break;
        case N_EXP_STRING_LITERAL :
            i = (int) node->clink;
            gen_code_i(LDA, 0, literal_table[i].addr);
            break;
        case N_EXP_ARRAY :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            gen_code_i(CHK,0,node->llink->type->expr);
            if (node->type->size > 1) {
                gen_code_i(LITI, 0, node->type->size);
                gen_code_i(MULI, 0, 0);
            }
            gen_code_i(OFFSET, 0, 0);
            if (!isArrayType(node->type)) {
                i = node->type->size;
                if (i == 1) gen_code_i(LDIB, 0, 0);
                else
                    gen_code_i(LDI, 0, i % 4 ? i / 4 + 1 : i / 4);
            }
            break;
        case N_EXP_FUNCTION_CALL :
            t = node->llink->type;
            i = t->element_type->element_type->size;
            if (i % 4) i = i / 4 * 4 + 4;
            if (node->rlink) {
                gen_code_i(INT, 0, 12 + i);
                gen_arg_expression(node->rlink);
                gen_code_i(POP, 0, node->rlink->value / 4 + 3);
            }
            else
                gen_code_i(INT, 0, i);
            gen_expression(node->llink);
            gen_code_i(CAL, 0, 0);
            break;
        case N_EXP_STRUCT :
            gen_expression_left(node->llink);
            id = (A_ID *) node->rlink;
            if (id->address > 0) {
                gen_code_i(LITI, 0, id->address);
                gen_code_i(OFFSET, 0, 0);
            }
            if (!isArrayType(node->type)) {
                i = node->type->size;
                if (i == 1) gen_code_i(LDIB, 0, 0);
                else gen_code_i(LDI, 0, i % 4 ? i / 4 + 1 : i / 4);
            }
            break;
        case N_EXP_ARROW:
            gen_expression(node->llink);
            id = (A_ID *) node->rlink;
            if (id->address > 0) {
                gen_code_i(LITI, 0, id->address);
                gen_code_i(OFFSET, 0, 0);
            }
            if (!isArrayType(node->type)) {
                i = node->type->size;
                if (i == 1)
                    gen_code_i(LDIB, 0, 0);
                else
                    gen_code_i(LDI, 0, i % 4 ? i / 4 + 1 : i / 4);
            }
            break;

        case N_EXP_POST_INC :
            gen_expression(node->clink);
            gen_expression_left(node->clink);
            t = node->type;
            if (node->type->size == 1)
                gen_code_i(LDXB, 0, 0);
            else
                gen_code_i(LDX, 0, 1);
            if (isPointerOrArrayType(node->type)) {
                gen_code_i(LITI, 0, node->type->element_type->size);
                gen_code_i(ADDI, 0, 0);
            }
            else if (isFloatType(node->type))
                gen_code_i(INCF, 0, 0);
            else
                gen_code_i(INCI, 0, 0);
            if (node->type->size == 1)
                gen_code_i(STOB, 0, 0);
            else
                gen_code_i(STO, 0, 1);
            break;
        case N_EXP_POST_DEC :
            gen_expression(node->clink);
            gen_expression_left(node->clink);
            t = node->type;
            if (node->type->size == 1) gen_code_i(LDXB, 0, 0);
            else
                gen_code_i(LDX, 0, 1);
            if (isPointerOrArrayType(node->type)) {
                gen_code_i(LITI, 0, node->type->element_type->size);
                gen_code_i(SUBI, 0, 0);
            } else if (isFloatType(node->type))
                gen_code_i(DECF, 0, 0);
            else
                gen_code_i(DECI, 0, 0);
            if (node->type->size == 1)
                gen_code_i(STOB, 0, 0);
            else
                gen_code_i(STO, 0, 1);
            break;

        case N_EXP_PRE_INC :
            gen_expression_left(node->clink);
            t = node->type;
            if (node->type->size == 1)
                gen_code_i(LDXB, 0, 0);
            else
                gen_code_i(LDX, 0, 1);
            if (isPointerOrArrayType(node->type)) {
                gen_code_i(LITI, 0, node->type->element_type->size);
                gen_code_i(ADDI, 0, 0);
            } else if (isFloatType(node->type)) {
                gen_code_i(INCF, 0, 0);
            } else {
                gen_code_i(INCI, 0, 0);
            }

            if (node->type->size == 1) {
                gen_code_i(STXB, 0, 0);
            } else {
                gen_code_i(STX, 0, 1);
            }
            break;

        case N_EXP_PRE_DEC :
            gen_expression_left(node->clink);
            t = node->type;
            if (node->type->size == 1)
                gen_code_i(LDXB, 0, 0);
            else
                gen_code_i(LDX, 0, 1);
            if (isPointerOrArrayType(node->type)) {
                gen_code_i(LITI, 0, node->type->element_type->size);
                gen_code_i(SUBI, 0, 0);
            } else if (isFloatType(node->type))
                gen_code_i(DECF, 0, 0);
            else
                gen_code_i(DECI, 0, 0);
            if (node->type->size == 1)
                gen_code_i(STXB, 0, 0);
            else
                gen_code_i(STX, 0, 1);
            break;

        case N_EXP_NOT :
            gen_expression(node->clink);
            gen_code_i(NOT, 0, 0);
            break;

        case N_EXP_PLUS :
            gen_expression(node->clink);
            break;
        case N_EXP_MINUS :
            gen_expression(node->clink);
            if (isFloatType(node->type))
                gen_code_i(MINUSF, 0, 0);
            else
                gen_code_i(MINUSI, 0, 0);
            break;

        case N_EXP_AMP :
            gen_expression_left(node->clink);
            break;
        case N_EXP_STAR :
            gen_expression(node->clink);
            i = node->type->size;
            if (i == 1)
                gen_code_i(LDIB, 0, 0);
            else
                gen_code_i(LDI, 0, i % 4 ? i / 4 + 1 : i / 4);
            break;

        case N_EXP_SIZE_EXP :
            gen_code_i(LITI, 0, (int) node->clink);
            break;
        case N_EXP_SIZE_TYPE :
            gen_code_i(LITI, 0, (int) node->clink);
            break;
        case N_EXP_CAST :
            gen_expression(node->rlink);
            if (node->type != node->rlink->type) {
                if (isFloatType(node->type)) {
                    gen_code_i(CVTF, 0, 0);
                }
                else if (isFloatType(node->rlink->type)) {
                    gen_code_i(CVTI, 0, 0);
                }
            }
            break;

        case N_EXP_MUL :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            if (isFloatType(node->type))
                gen_code_i(MULF, 0, 0);
            else
                gen_code_i(MULI, 0, 0);
            break;

        case N_EXP_DIV :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            if (isFloatType(node->type))
                gen_code_i(DIVF, 0, 0);
            else
                gen_code_i(DIVI, 0, 0);
            break;
        case N_EXP_MOD :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            gen_code_i(MOD, 0, 0);
            break;

        case N_EXP_ADD :
            gen_expression(node->llink);
            if (isPointerOrArrayType(node->rlink->type)) {
                gen_code_i(LITI, 0, node->rlink->type->element_type->size);
                gen_code_i(MULI, 0, 0);
            }
            gen_expression(node->rlink);
            if (isPointerOrArrayType(node->llink->type)) {
                gen_code_i(LITI, 0, node->llink->type->element_type->size);
                gen_code_i(MULI, 0, 0);
            }
            if (isFloatType(node->type)) gen_code_i(ADDF, 0, 0);
            else
                gen_code_i(ADDI, 0, 0);
            break;

        case N_EXP_SUB :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            if (isPointerOrArrayType(node->llink->type) &&
                !isPointerOrArrayType(node->rlink->type)) {
                gen_code_i(LITI, 0, node->llink->type->element_type->size);
                gen_code_i(MULI, 0, 0);
            }
            if (isFloatType(node->type)) gen_code_i(SUBF, 0, 0);
            else
                gen_code_i(SUBI, 0, 0);
            break;
        case N_EXP_LSS :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            if (isFloatType(node->llink->type))
                gen_code_i(LSSF, 0, 0);
            else
                gen_code_i(LSSI, 0, 0);
            break;
        case N_EXP_GTR :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            if (isFloatType(node->llink->type))
                gen_code_i(GTRF, 0, 0);
            else
                gen_code_i(GTRI, 0, 0);
            break;
        case N_EXP_LEQ :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            if (isFloatType(node->llink->type)) {
                gen_code_i(LEQF, 0, 0);
            }
            else {
                gen_code_i(LEQI, 0, 0);
            }
            break;

        case N_EXP_GEQ :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            if (isFloatType(node->llink->type))
                gen_code_i(GEQF, 0, 0);
            else
                gen_code_i(GEQI, 0, 0);
            break;
        case N_EXP_NEQ :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            if (isFloatType(node->llink->type))
                gen_code_i(NEQF, 0, 0);
            else
                gen_code_i(NEQI, 0, 0);
            break;
        case N_EXP_EQL :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            if (isFloatType(node->llink->type))
                gen_code_i(EQLF, 0, 0);
            else
                gen_code_i(EQLI, 0, 0);
            break;
        case N_EXP_AND :
            gen_expression(node->llink);
            gen_code_l(JPCR, 0, i = get_label());
            gen_expression(node->rlink);
            gen_label_number(i);
            break;
        case N_EXP_OR :
            gen_expression(node->llink);
            gen_code_l(JPTR, 0, i = get_label());
            gen_expression(node->rlink);
            gen_label_number(i);
            break;
        case N_EXP_ASSIGN :
            gen_expression_left(node->llink);
            gen_expression(node->rlink);
            i = node->type->size;
            if (i == 1) gen_code_i(STXB, 0, 0);
            else
                gen_code_i(STX, 0, i % 4 ? i / 4 + 1 : i / 4);
            break;
        default :
            gen_error(100, node->line, "");
            break;
    }
}

void gen_expression_left(A_NODE *node) {
    A_ID *id;
    A_TYPE *t;
    int result;
    switch (node->name) {
        case N_EXP_IDENT :
            id = (A_ID *) node->clink;
            t = id->type;
            switch (id->kind) {
                case ID_VAR:
                case ID_PARM:
                    switch (t->kind) {
                        case T_ENUM:
                        case T_POINTER:
                        case T_STRUCT:
                        case T_UNION:
                            gen_code_i(LDA, id->level, id->address);
                            break;
                        case T_ARRAY:
                            if (id->kind == ID_VAR)
                                gen_code_i(LDA, id->level, id->address);
                            else
                                gen_code_i(LOD, id->level, id->address);
                            break;
                        default:
                            gen_error(13, node->line, id->name);
                            break;
                    }
                    break;
                case ID_FUNC:
                    gen_code_s(ADDR, 0, id->name);
                    break;
                default:
                    gen_error(13, node->line, id->name);
                    break;
            }
            break;
        case N_EXP_ARRAY :
            gen_expression(node->llink);
            gen_expression(node->rlink);
            // gen_code_i(CHK,0,node->llink->type->expr);
            if (node->type->size > 1) {
                gen_code_i(LITI, 0, node->type->size);
                gen_code_i(MULI, 0, 0);
            }
            gen_code_i(OFFSET, 0, 0);
            break;

        case N_EXP_STRUCT :
            gen_expression_left(node->llink);
            id = (A_ID *) node->rlink;
            if (id->address > 0) {
                gen_code_i(LITI, 0, id->address);
                gen_code_i(OFFSET, 0, 0);
            }
            break;
        case N_EXP_ARROW :
            gen_expression(node->llink);
            id = (A_ID *) node->rlink;
            if (id->address > 0) {
                gen_code_i(LITI, 0, id->address);
                gen_code_i(OFFSET, 0, 0);
            }
            break;

        case N_EXP_STAR :
            gen_expression(node->clink);
            break;
        case N_EXP_INT_CONST :
        case N_EXP_FLOAT_CONST :
        case N_EXP_CHAR_CONST :
        case N_EXP_STRING_LITERAL :
        case N_EXP_FUNCTION_CALL :
        case N_EXP_POST_INC :
        case N_EXP_POST_DEC :
        case N_EXP_PRE_INC :
        case N_EXP_PRE_DEC :
        case N_EXP_NOT :
        case N_EXP_MINUS :
        case N_EXP_SIZE_EXP :
        case N_EXP_SIZE_TYPE :
        case N_EXP_CAST :
        case N_EXP_MUL :
        case N_EXP_DIV :
        case N_EXP_MOD :
        case N_EXP_ADD :
        case N_EXP_SUB :
        case N_EXP_LSS :
        case N_EXP_GTR :
        case N_EXP_LEQ :
        case N_EXP_GEQ :
        case N_EXP_NEQ :
        case N_EXP_EQL :
        case N_EXP_AMP :
        case N_EXP_AND :
        case N_EXP_OR :
        case N_EXP_ASSIGN :
            gen_error(12, node->line, "");
            break;
        default :
            gen_error(100, node->line, "");
            break;
    }
}

void gen_arg_expression(A_NODE *node) {
    A_NODE *n;
    switch (node->name) {
        case N_ARG_LIST :
            gen_expression(node->llink);
            gen_arg_expression(node->rlink);
            break;
        case N_ARG_LIST_NIL :
            break;
        default :
            gen_error(100, node->line, "");
            break;
    }
}

int get_label() {
    label_no++;
    return (label_no);
}

void gen_statement(A_NODE *node, int cont_label, int break_label, A_SWITCH sw[], int *sn) {
    A_SWITCH switch_table[100];
    int switch_no = 0;
    A_NODE *n;
    int i, l1, l2, l3;
    switch (node->name) {
        case N_STMT_LABEL_CASE :
            if (sw) {
                *sn = *sn + 1;
                sw[*sn].kind = SW_VALUE;
                sw[*sn].val = (int) node->llink;
                sw[*sn].label = l1 = get_label();
                gen_label_number(l1);
            }
            else
                gen_error(21, node->line, "");
            gen_statement(node->rlink, cont_label, break_label, sw, sn);
            break;
        case N_STMT_LABEL_DEFAULT :
            if (sw) {
                *sn = *sn + 1;
                sw[*sn].kind = SW_DEFAULT;
                sw[*sn].label = l1 = get_label();
                gen_label_number(l1);
            }
            else
                gen_error(20, node->line, "");
            gen_statement(node->clink, cont_label, break_label, sw, sn);
            break;
        case N_STMT_COMPOUND:
            if (node->llink) gen_declaration_list(node->llink);
            gen_statement_list(node->rlink, cont_label, break_label, sw, sn);
            break;
        case N_STMT_EMPTY:
            break;
        case N_STMT_EXPRESSION:
            n = node->clink;
            gen_expression(n);
            i = n->type->size;
            if (i)
                gen_code_i(POP, 0, i % 4 ? i / 4 + 1 : i / 4);
            break;
        case N_STMT_IF:
            gen_expression(node->llink);
            gen_code_l(JPC, 0, l1 = get_label());
            gen_statement(node->rlink, cont_label, break_label, 0, 0);
            gen_label_number(l1);
            break;
        case N_STMT_IF_ELSE:
            gen_expression(node->llink);
            gen_code_l(JPC, 0, l1 = get_label());
            gen_statement(node->clink, cont_label, break_label, 0, 0);
            gen_code_l(JMP, 0, l2 = get_label());
            gen_label_number(l1);
            gen_statement(node->rlink, cont_label, break_label, 0, 0);
            gen_label_number(l2);
            break;
        case N_STMT_SWITCH:
            gen_expression(node->llink);
            gen_code_l(SWITCH, 0, l1 = get_label());
            gen_code_l(JMP, 0, l2 = get_label());
            gen_statement(node->rlink, cont_label, l2, switch_table, &switch_no);
            gen_label_number(l1);
            for (i = 1; i <= switch_no; i++) {
                if (switch_table[i].kind == SW_VALUE)
                    gen_code_i(SWVALUE, 0, switch_table[i].val);
                else
                    gen_code_i(SWDEFAULT, 0, 0);
                gen_code_l(SWLABEL, 0, switch_table[i].label);
            }
            gen_code_i(SWEND, 0, 0);
            gen_label_number(l2);
            break;
        case N_STMT_WHILE:
            l3 = get_label();
            gen_label_number(l1 = get_label());
            gen_expression(node->llink);
            gen_code_l(JPC, 0, l2 = get_label());
            gen_statement(node->rlink, l3, l2, 0, 0);
            gen_label_number(l3);
            gen_code_l(JMP, 0, l1);
            gen_label_number(l2);
            break;
        case N_STMT_DO:
            l3 = get_label();
            l2 = get_label();
            gen_label_number(l1 = get_label());
            gen_statement(node->llink, l2, l3, 0, 0);
            gen_label_number(l2);
            gen_expression(node->rlink);
            gen_code_l(JPT, 0, l1);
            gen_label_number(l3);
            break;
        case N_STMT_FOR:
            n = node->llink;
            l3 = get_label();
            if (n->llink) {
                gen_expression(n->llink);
                i = n->llink->type->size;
                if (i)
                    gen_code_i(POP, 0, i % 4 ? i / 4 + 1 : i / 4);
            }
            gen_label_number(l1 = get_label());
            l2 = get_label();
            if (n->clink) {
                gen_expression(n->clink);
                gen_code_l(JPC, 0, l2);
            }
            gen_statement(node->rlink, l3, l2, 0, 0);
            gen_label_number(l3);
            if (n->rlink) {
                gen_expression(n->rlink);
                i = n->rlink->type->size;
                if (i)
                    gen_code_i(POP, 0, i % 4 ? i / 4 + 1 : i / 4);
            }
            gen_code_l(JMP, 0, l1);
            gen_label_number(l2);
            break;
        case N_STMT_CONTINUE:
            if (cont_label) gen_code_l(JMP, 0, cont_label);
            else
                gen_error(22, node->line, "");
            break;
        case N_STMT_BREAK:
            if (break_label)
                gen_code_l(JMP, 0, break_label);
            else
                gen_error(23, node->line, "");
            break;
        case N_STMT_RETURN:
            n = node->clink;
            if (n) {
                i = n->type->size;
                if (i % 4) i = i / 4 * 4 + 4;
                gen_code_i(LDA, 1, -i);
                gen_expression(n);
                gen_code_i(STO, 0, i / 4);
            }
            gen_code_i(RET, 0, 0);
            break;
        default :
            gen_error(100, node->line, "");
            break;
    }
}

void gen_statement_list(A_NODE *node, int cont_label, int break_label, A_SWITCH sw[], int *sn) {
    switch (node->name) {
        case N_STMT_LIST:
            gen_statement(node->llink, cont_label, break_label, sw, sn);
            gen_statement_list(node->rlink, cont_label, break_label, sw, sn);
            break;
        case N_STMT_LIST_NIL:
            break;
        default :
            gen_error(100, node->line, "");
            break;
    }
}

void gen_initializer_global(A_NODE *node, A_TYPE *t, int addr) {
    gen_code_i(LDA, 0, addr);
    gen_expression(node->clink);
    gen_code_i(STX, 0, 1);
    gen_code_i(POP, 0, 1);
}

void gen_initializer_local(A_NODE *node, A_TYPE *t, int addr) {
    gen_code_i(LDA, 1, addr);
    gen_expression(node->clink);
    gen_code_i(STX, 0, 1);
    gen_code_i(POP, 0, 1);
}

void gen_declaration_list(A_ID *id) {
    while (id) {
        gen_declaration(id);
        id = id->link;
    }
}

void gen_declaration(A_ID *id) {
    int i;
    A_NODE *node;
    switch (id->kind) {
        case ID_VAR:
            if (id->init) {
                if (id->level == 0) {
                    // skip here
                } else {
                    gen_initializer_local(id->init, id->type, id->address);
                }
            }
            break;

        case ID_FUNC:
            if (id->type->expr) {
                gen_label_name(id->name);
                gen_code_i(INT, 0, id->type->local_var_size);
                gen_statement(id->type->expr, 0, 0, 0, 0);
                gen_code_i(RET, 0, 0);
            }
            break;

        case ID_PARM:
        case ID_TYPE:
        case ID_ENUM:
        case ID_STRUCT:
        case ID_FIELD:
        case ID_ENUM_LITERAL:
        case ID_NULL:
            break;
        default:
            gen_error(100, id->line, "");
            break;
    }
}

void gen_error(int i, int ll, char *s) {
    gen_err++;
    printf("*** error at line %d: ", ll);
    switch (i) {
        case 11:
            printf("illegal identifier in expression \n");
            break;
        case 12:
            printf("illegal l-value expression \n");
            break;
        case 13:
            printf("identifier %s not l-value expression \n", s);
            break;
        case 20:
            printf("illegal default label in switch statement \n");
            break;
        case 21:
            printf("illegal case label in switch statement \n");
            break;
        case 22:
            printf("no destination for continue statement \n");
            break;
        case 23:
            printf("no destination for break statement \n");
            break;
        case 100:
            printf("fatal compiler error during code generation\n");
            break;
        default:
            printf("unknown \n");
            break;
    }
}

void gen_code_i(OPCODE op, int l, int a) {
    fprintf(fout, "\t%9s %d, %d\n", opcode_name[op], l, a);
}

void gen_code_f(OPCODE op, int l, float a) {
    fprintf(fout, "\t%9s %d, %f\n", opcode_name[op], l, a);
}

void gen_code_s(OPCODE op, int l, char *a) {
    fprintf(fout, "\t%9s %d, %s\n", opcode_name[op], l, a);
}

void gen_code_l(OPCODE op, int l, int a) {
    fprintf(fout, "\t%9s %d, L%d\n", opcode_name[op], l, a);
}

void gen_label_number(int i) {
    fprintf(fout, "L%d:\n", i);
}

void gen_label_name(char *s) {
    fprintf(fout, "%s:\n", s);
}
// make new node for syntax tree
A_NODE *makeNode(NODE_NAME n, A_NODE *a, A_NODE *b, A_NODE *c)
{
    A_NODE *m;
    m = (A_NODE *)malloc(sizeof(A_NODE));
    m->name = n;
    m->llink = a;
    m->clink = b;
    m->rlink = c;
    m->type = NIL;
    m->line = line_no;
    m->value = 0;
    return m;
}

A_NODE *makeNodeList(NODE_NAME n, A_NODE *a, A_NODE *b)
{
    A_NODE *m, *k;
    k = a;
    while (k->rlink)
        k = k->rlink;

    m = (A_NODE *)malloc(sizeof(A_NODE));
    m->name = k->name;
    m->llink = NIL;
    m->clink = NIL;
    m->rlink = NIL;
    m->type = NIL;
    m->line = line_no;
    m->value = 0;
    k->name = n;
    k->llink = b;
    k->rlink = m;
    return a;
}

// make new declarator for identifier
A_ID *makeIdentifier(char *s)
{
    A_ID *id;
    id = malloc(sizeof(A_ID));
    id->name = s;
    id->kind = 0;
    id->specifier = 0;
    id->level = current_level;
    id->address = 0;
    id->init = NIL;
    id->type = NIL;
    id->link = NIL;
    id->line = line_no;
    id->value = 0;
    id->prev = current_id;
    current_id = id;
    return id;
}

// make new declarator for dummy identifier
A_ID *makeDummyIdentifier()
{
    A_ID *id;
    id = malloc(sizeof(A_ID));
    id->name = "";
    id->kind = 0;
    id->specifier = 0;
    id->level = current_level;
    id->address = 0;
    id->init = NIL;
    id->type = NIL;
    id->link = NIL;
    id->line = line_no;
    id->value = 0;
    id->prev = 0;
    return id;
}

// make new type
A_TYPE *makeType(T_KIND k)
{
    A_TYPE *t;
    t = malloc(sizeof(A_TYPE));
    t->kind = k;
    t->size = 0;
    t->local_var_size = 0;
    t->element_type = NIL;
    t->field = NIL;
    t->expr = NIL;
    t->check = FALSE;
    t->prt = FALSE;
    t->line = line_no;
    return t;
}

// make new specifier
A_SPECIFIER *makeSpecifier(A_TYPE *t, S_KIND s)
{
    A_SPECIFIER *p;
    p = malloc(sizeof(A_SPECIFIER));
    p->type = t;
    p->stor = s;
    p->line = line_no;
    return p;
}

A_ID *searchIdentifier(char *s, A_ID *id)
{
    while (id)
    {
        if (strcmp(id->name, s) == 0)
            break;
        id = id->prev;
    }
    return id;
}

A_ID *searchIdentifierAtCurrentLevel(char *s, A_ID *id)
{
    while (id)
    {
        if (id->level < current_level)
            return NIL;
        if (strcmp(id->name, s) == 0)
            break;
        id = id->prev;
    }
    return id;
}

void checkForwardReference()
{
    A_ID *id;
    A_TYPE *t;
    id = current_id;
    while (id)
    {
        if (id->level < current_level)
            break;
        t = id->type;

        if (id->kind == ID_NULL)
            syntax_error(31, id->name);
        else if ((id->kind == ID_STRUCT || id->kind == ID_ENUM) && t->field == NIL)
            syntax_error(32, id->name);
        id = id->prev;
    }
}

// set default specifier
void setDefaultSpecifier(A_SPECIFIER *p)
{
    A_TYPE *t;
    if (p->type == NIL)
        p->type = int_type;
    if (p->stor == S_NULL)
        p->stor = S_AUTO;
}

// specifier merge & update
A_SPECIFIER *updateSpecifier(A_SPECIFIER *p, A_TYPE *t, S_KIND s)
{
    if (t)
        if (p->type)
            if (p->type == t)
                ;
            else
                syntax_error(24, "");
        else
            p->type = t;

    if (s)
    {
        if (p->stor)
            if (s == p->stor)
                ;
            else
                syntax_error(24, "");
        else
            p->stor = s;
    }
    return p;
}

// link two declaraor list id1 & id2
A_ID *linkDeclaratorList(A_ID *id1, A_ID *id2)
{
    A_ID *m = id1;
    if (id1 == NIL)
        return id2;
    while (m->link)
        m = m->link;
    m->link = id2;
    return id1;
}

// check if identifier is already declared in primary expression
A_ID *getIdentifierDeclared(char *s)
{
    A_ID *id;
    id = searchIdentifier(s, current_id);
    if (id == NIL)
        syntax_error(13, s);
    return id;
}

// get type of struct id
A_TYPE *getTypeOfStructOrEnumRefIdentifier(T_KIND k, char *s, ID_KIND kk)
{
    A_TYPE *t;
    A_ID *id;
    id = searchIdentifier(s, current_id);
    if (id)
        if (id->kind == kk && id->type->kind == k)
            return id->type;
        else
            syntax_error(11, s);
    // make a new struct (or enum) identifier
    t = makeType(k);
    id = makeIdentifier(s);
    id->kind = kk;
    id->type = t;
    return t;
}

// set declarator init (expression tree)
A_ID *setDeclaratorInit(A_ID *id, A_NODE *n)
{
    id->init = n;
    return id;
}

// set declarator kind
A_ID *setDeclaratorKind(A_ID *id, ID_KIND k)
{
    A_ID *a;
    a = searchIdentifierAtCurrentLevel(id->name, id->prev);
    if (a)
        syntax_error(12, id->name);
    id->kind = k;
    return id;
}

// set declarator type
A_ID *setDeclaratorType(A_ID *id, A_TYPE *t)
{
    id->type = t;
    return id;
}

// set declarator type (or element type)
A_ID *setDeclaratorElementType(A_ID *id, A_TYPE *t)
{
    A_TYPE *tt;
    if (id->type == NIL)
        id->type = t;
    else
    {
        tt = id->type;
        while (tt->element_type)
            tt = tt->element_type;
        tt->element_type = t;
    }
    return id;
}

// set declarator element type and kind
A_ID *setDeclaratorTypeAndKind(A_ID *id, A_TYPE *t, ID_KIND k)
{
    id = setDeclaratorElementType(id, t);
    id = setDeclaratorKind(id, k);
    return id;
}

// check function declarator and return type
A_ID *setFunctionDeclaratorSpecifier(A_ID *id, A_SPECIFIER *p)
{
    A_ID *a;
    // check storage class
    if (p->stor)
        syntax_error(25, "");
    setDefaultSpecifier(p);
    // check function identifier immediately before '('
    if (id->type->kind != T_FUNC)
    {
        syntax_error(21, "");
        return id;
    }
    else
    {
        id = setDeclaratorElementType(id, p->type);
        id->kind = ID_FUNC;
    }
    // check redeclaration
    a = searchIdentifierAtCurrentLevel(id->name, id->prev);
    if (a)
        if (a->kind != ID_FUNC || a->type->expr)
            syntax_error(12, id->name);
        else
        { // check prototype: parameters and return type
            if (isNotSameFormalParameters(a->type->field, id->type->field))
                syntax_error(22, id->name);
            if (isNotSameType(a->type->element_type, id->type->element_type))
                syntax_error(26, a->name);
        }
    // change parameter scope and check empty name
    a = id->type->field;
    while (a)
    {
        if (strlen(a->name))
            current_id = a;
        else if (a->type)
            syntax_error(23, "");
        a = a->link;
    }
    return id;
}

A_ID *setFunctionDeclaratorBody(A_ID *id, A_NODE *n)
{
    id->type->expr = n;
    return id;
}

// set declarator_list type and kind based on storage class
A_ID *setDeclaratorListSpecifier(A_ID *id, A_SPECIFIER *p)
{
    A_ID *a;
    setDefaultSpecifier(p);
    a = id;
    while (a)
    {
        if (strlen(a->name) && searchIdentifierAtCurrentLevel(a->name, a->prev))
            syntax_error(12, a->name);
        // ** to be completed ** -> completed
        a = setDeclaratorElementType(a, p->type);
        if (p->stor == S_TYPEDEF)
            a->kind = ID_TYPE;
        else if (a->type->kind == T_FUNC)
            a->kind = ID_FUNC;
        else
            a->kind = ID_VAR;
        a->specifier = p->stor; // id's specifier should be storage class of A_SPECIFIER's stor
        if (a->specifier == S_NULL)
            a->specifier = S_AUTO;
        a = a->link;
    }
    return id;
}

// set declarator_list type and kind
A_ID *setParameterDeclaratorSpecifier(A_ID *id, A_SPECIFIER *p)
{
    // check redeclaration
    if (searchIdentifierAtCurrentLevel(id->name, id->prev))
        syntax_error(12, id->name);
    // check papameter storage class && void type
    if (p->stor || p->type == void_type)
        syntax_error(14, "");
    setDefaultSpecifier(p);
    id = setDeclaratorElementType(id, p->type);
    id->kind = ID_PARM;
    return id;
}

A_ID *setStructDeclaratorListSpecifier(A_ID *id, A_TYPE *t)
{
    A_ID *a;
    a = id;
    while (a)
    {
        // ** to be completed ** -> completed
        if (searchIdentifierAtCurrentLevel(a->name, a->prev))
            syntax_error(12, a->name);
        a = setDeclaratorElementType(a, t);
        a->kind = ID_FIELD;
        a = a->link;
    }
    return id;
}

// set type name specifier
A_TYPE *setTypeNameSpecifier(A_TYPE *t, A_SPECIFIER *p)
{
    // check storage class in type name
    if (p->stor)
        syntax_error(20, "");
    setDefaultSpecifier(p);
    t = setTypeElementType(t, p->type);
    return t;
}

// set type element type
A_TYPE *setTypeElementType(A_TYPE *t, A_TYPE *s)
{
    A_TYPE *q;
    if (t == NIL)
        return s;
    q = t;
    while (q->element_type)
        q = q->element_type;
    q->element_type = s;
    return t;
}

// set type field
A_TYPE *setTypeField(A_TYPE *t, A_ID *n)
{
    t->field = n;
    return t;
}
// set type initial value (expression tree)
A_TYPE *setTypeExpr(A_TYPE *t, A_NODE *n)
{
    t->expr = n;
    return t;
}
// set type of struct iIdentifier
A_TYPE *setTypeStructOrEnumIdentifier(T_KIND k, char *s, ID_KIND kk)
{
    A_TYPE *t;
    A_ID *id, *a;
    // check redeclaration or forward declaration
    a = searchIdentifierAtCurrentLevel(s, current_id);
    if (a)
        if (a->kind == kk && a->type->kind == k)
            if (a->type->field)
                syntax_error(12, s);
            else
                return a->type;
        else
            syntax_error(12, s);
    // make a new struct (or enum) identifier
    id = makeIdentifier(s);
    t = makeType(k);
    id->type = t;
    id->kind = kk;
    return t;
}
// set type and kinf of identifier
A_TYPE *setTypeAndKindOfDeclarator(A_TYPE *t, ID_KIND k, A_ID *id)
{
    if (searchIdentifierAtCurrentLevel(id->name, id->prev))
        syntax_error(12, id->name);
    id->type = t;
    id->kind = k;
    return t;
}
// check function parameters with protype
BOOLEAN isNotSameFormalParameters(A_ID *a, A_ID *b)
{
    if (a == NIL) // no parameters in prototype
        return FALSE;
    while (a)
    {
        if (b == NIL || isNotSameType(a->type, b->type))
            return TRUE;
        a = a->link;
        b = b->link;
    }
    if (b)
        return TRUE;
    else
        return FALSE;
}

BOOLEAN isNotSameType(A_TYPE *t1, A_TYPE *t2)
{
    if (isPointerOrArrayType(t1) || isPointerOrArrayType(t2))
        return isNotSameType(t1->element_type, t2->element_type);
    else
        return t1 != t2;
}

BOOLEAN isPointerOrArrayType(A_TYPE *t)
{
    return t && (t->kind == T_POINTER || t->kind == T_ARRAY);
}

void initialize()
{
    // primitive data types
    int_type = setTypeAndKindOfDeclarator(
        makeType(T_ENUM), ID_TYPE, makeIdentifier("int"));
    float_type = setTypeAndKindOfDeclarator(
        makeType(T_ENUM), ID_TYPE, makeIdentifier("float"));
    char_type = setTypeAndKindOfDeclarator(
        makeType(T_ENUM), ID_TYPE, makeIdentifier("char"));
    void_type = setTypeAndKindOfDeclarator(
        makeType(T_VOID), ID_TYPE, makeIdentifier("void"));
    string_type = setTypeElementType(makeType(T_POINTER), char_type);
    int_type->size = 4;
    int_type->check = TRUE;
    float_type->size = 4;
    float_type->check = TRUE;
    char_type->size = 1;
    char_type->check = TRUE;
    void_type->size = 0;
    void_type->check = TRUE;
    string_type->size = 4;
    string_type->check = TRUE;
    // printf(char *, ...) library function
    setDeclaratorTypeAndKind(
        makeIdentifier("printf"),
        setTypeField(
            setTypeElementType(makeType(T_FUNC), void_type),
            linkDeclaratorList(
                setDeclaratorTypeAndKind(makeDummyIdentifier(), string_type, ID_PARM),
                setDeclaratorKind(makeDummyIdentifier(), ID_PARM))),
        ID_FUNC);
    // scanf(char *, ...) library function
    setDeclaratorTypeAndKind(
        makeIdentifier("scanf"),
        setTypeField(
            setTypeElementType(makeType(T_FUNC), void_type),
            linkDeclaratorList(
                setDeclaratorTypeAndKind(makeDummyIdentifier(), string_type, ID_PARM),
                setDeclaratorKind(makeDummyIdentifier(), ID_PARM))),
        ID_FUNC);
    // malloc(int) library function
    setDeclaratorTypeAndKind(
        makeIdentifier("malloc"),
        setTypeField(
            setTypeElementType(makeType(T_FUNC), string_type),
            setDeclaratorTypeAndKind(makeDummyIdentifier(), int_type, ID_PARM)),
        ID_FUNC);
}
void syntax_error(int i, char *s)
{
    syntax_err++;
    printf("line %d: syntax error: ", line_no);
    switch (i)
    {
    case 11:
        printf("illegal referencing struct or union identifier %s", s);
        break;
    case 12:
        printf("redeclaration of identifier %s", s);
        break;
    case 13:
        printf("undefined identifier %s", s);
        break;
    case 14:
        printf("illegal type specifier in formal parameter");
        break;
    case 20:
        printf("illegal storage class in type specifiers");
        break;
    case 21:
        printf("illegal function declarator");
        break;
    case 22:
        printf("conflicting parm type in prototype function %s", s);
        break;
    case 23:
        printf("empty parameter name");
        break;
    case 24:
        printf("illegal declaration specifiers");
        break;
    case 25:
        printf("illegal function specifiers");
        break;
    case 26:
        printf("illegal or conflicting return type in function %s", s);
        break;
    case 31:
        printf("undefined type for identifier %s", s);
        break;
    case 32:
        printf("incomplete forward reference for identifier %s", s);
        break;
    default:
        printf("unknown");
        break;
    }
    if (strlen(yytext) == 0)
        printf(" at end\n");
    else
        printf(" near %s\n", yytext);
}

void semantic_analysis(A_NODE *node)
{
    sem_program(node);
    set_literal_address(node);
}

void set_literal_address(A_NODE *node)
{
    int i;
    for (i = 1; i <= literal_no; i++)
        literal_table[i].addr += node->value;
    node->value += literal_size;
}

void sem_program(A_NODE *node)
{
    int i;
    switch (node->name)
    {
    case N_PROGRAM:
        i = sem_declaration_list((A_ID *)node->clink, 12);
        node->value = global_address;
        break;
    default:
        semantic_error(90, node->line, "");
        break;
    }
}

int put_literal(A_LITERAL lit, int ll)
{
    float ff;
    if (literal_no >= LIT_MAX)
        semantic_error(93, ll, "");
    else
        literal_no++;
    literal_table[literal_no] = lit;
    literal_table[literal_no].addr = literal_size;
    if (lit.type->kind == T_ENUM)
        literal_size += 4;
    else if (isStringType(lit.type))
        literal_size += strlen(lit.value.s) + 1;
    if (literal_size % 4)
        literal_size = literal_size / 4 * 4 + 4;
    return (literal_no);
}

A_TYPE *sem_expression(A_NODE *node)
{
    A_TYPE *result = NIL, *t, *t1, *t2;
    A_ID *id;
    A_LITERAL lit;
    int i;
    BOOLEAN lvalue = FALSE;
    ;
    switch (node->name)
    {
    case N_EXP_IDENT:
        id = (A_ID *)node->clink;
        switch (id->kind)
        {
        case ID_VAR:
        case ID_PARM:
            result = id->type;
            if (!isArrayType(result))
                lvalue = TRUE;
            break;
        case ID_FUNC:
            result = id->type;
            break;
        case ID_ENUM_LITERAL:
            result = int_type;
            break;
        default:
            semantic_error(38, node->line, id->name);
            break;
        }
        break;
    case N_EXP_INT_CONST:
        result = int_type;
        break;
    case N_EXP_FLOAT_CONST:
        lit.type = float_type;
        lit.value.f = (float)atof((const char *)node->clink);
        node->clink = (struct s_node *)put_literal(lit, node->line); // index of literal table
        result = float_type;
        break;
    case N_EXP_CHAR_CONST:
        result = char_type;
        break;
    case N_EXP_STRING_LITERAL:
        lit.type = string_type;
        lit.value.s = (char *)node->clink;
        node->clink = (struct s_node *)put_literal(lit, node->line);
        result = string_type;
        break;
    case N_EXP_ARRAY:
        t1 = sem_expression(node->llink);
        t2 = sem_expression(node->rlink);
        // usual binary conversion
        t = convertUsualBinaryConversion(node);
        t1 = node->llink->type;
        t2 = node->rlink->type;
        if (isPointerOrArrayType(t1))
            result = t1->element_type;
        else
            semantic_error(32, node->line, "");
        if (!isIntegralType(t2))
            semantic_error(29, node->line, "");
        if (!isArrayType(result))
            lvalue = TRUE;
        break;
    case N_EXP_STRUCT:
        t = sem_expression(node->llink);
        id = getStructFieldIdentifier(t, (char *)node->rlink);
        if (id)
        {
            // index of literal table
            result = id->type;
            if (node->llink->value && !isArrayType(result))
                lvalue = TRUE;
            else
                semantic_error(37, node->line, "");
            node->rlink = (struct s_node *)id;
        }
        break;
    case N_EXP_ARROW:
        t = sem_expression(node->llink);
        id = getPointerFieldIdentifier(t, (char *)node->rlink);
        if (id)
        {
            result = id->type;
            if (!isArrayType(result))
                lvalue = TRUE;
        }
        else
        {
            semantic_error(37, node->line, "");
        }

        node->rlink = (struct s_node *)id;
        break;
    case N_EXP_FUNCTION_CALL:
        t = sem_expression(node->llink);
        // usual unary conversion
        node->llink = convertUsualUnaryConversion(node->llink);
        t = node->llink->type;
        if (isPointerType(t) && isFunctionType(t->element_type))
        {
            sem_arg_expr_list(node->rlink, t->element_type->field);
            result = t->element_type->element_type;
        }
        else
            semantic_error(21, node->line, "");
        break;
    case N_EXP_POST_INC:
    case N_EXP_POST_DEC:
        result = sem_expression(node->clink);
        // usual binary conversion between the expression and 1
        if (!isScalarType(result))
            semantic_error(27, node->line, "");
        // check if modifiable lvalue
        if (!isModifiableLvalue(node->clink))
            semantic_error(60, node->line, "");
        break;
    case N_EXP_CAST:
        result = (A_TYPE *)node->llink;
        i = sem_A_TYPE(result);
        t = sem_expression(node->rlink);
        // check allowable casting conversion
        if (!isAllowableCastingConversion(result, t))
            semantic_error(58, node->line, "");
        break;
    case N_EXP_SIZE_TYPE:
        t = (A_TYPE *)node->clink;
        i = sem_A_TYPE(t);
        // check if incomplete array, function, void
        if (isArrayType(t) && t->size == 0 || isFunctionType(t) ||
            isVoidType(t))
            semantic_error(39, node->line, "");
        else
            node->clink = (struct s_node *)i;
        result = int_type;
        break;
    case N_EXP_SIZE_EXP:
        t = sem_expression(node->clink);
        // check if incomplete array, function
        if ((node->clink->name != N_EXP_IDENT ||
             ((A_ID *)node->clink->clink)->kind != ID_PARM) &&
            (isArrayType(t) && t->size == 0 || isFunctionType(t)))
            semantic_error(39, node->line, "");
        else
            node->clink = (struct s_node *)t->size;
        result = int_type;
        break;
    case N_EXP_PLUS:
    case N_EXP_MINUS:
        t = sem_expression(node->clink);
        if (isArithmeticType(t))
        {
            node->clink = convertUsualUnaryConversion(node->clink);
            result = node->clink->type;
        }
        else
            semantic_error(13, node->line, "");
        break;
    case N_EXP_NOT:
        t = sem_expression(node->clink);
        if (isScalarType(t))
        {
            node->clink = convertUsualUnaryConversion(node->clink);
            result = int_type;
        }
        else
            semantic_error(27, node->line, "");
        break;
    case N_EXP_AMP:
        t = sem_expression(node->clink);
        if (node->clink->value == TRUE || isFunctionType(t))
        {
            result = setTypeElementType(makeType(T_POINTER), t);
            result->size = 4;
        }
        else
            semantic_error(60, node->line, "");
        break;

    case N_EXP_STAR:
        t = sem_expression(node->clink);
        node->clink = convertUsualUnaryConversion(node->clink);
        if (isPointerType(t))
        {
            result = t->element_type;
            // lvalue if points to an object
            if (isStructOrUnionType(result) || isScalarType(result))
                lvalue = TRUE;
        }
        else
            semantic_error(31, node->line, "");
        break;
    case N_EXP_PRE_INC:
    case N_EXP_PRE_DEC:
        result = sem_expression(node->clink);
        // usual binary conversion between the expression and 1
        if (!isScalarType(result))
            semantic_error(27, node->line, "");
        // check if modifiable lvalue
        if (!isModifiableLvalue(node->clink))
            semantic_error(60, node->line, "");
        break;
    case N_EXP_MUL:
    case N_EXP_DIV:
        t1 = sem_expression(node->llink);
        t2 = sem_expression(node->rlink);
        if (isArithmeticType(t1) && isArithmeticType(t2))
            result = convertUsualBinaryConversion(node);
        else
            semantic_error(28, node->line, "");
        break;
    case N_EXP_MOD:
        t1 = sem_expression(node->llink);
        t2 = sem_expression(node->rlink);
        if (isIntegralType(t1) && isIntegralType(t2))
            result = convertUsualBinaryConversion(node);
        else
            semantic_error(29, node->line, "");
        result = int_type;
        break;
    case N_EXP_ADD:
        t1 = sem_expression(node->llink);
        t2 = sem_expression(node->rlink);
        if (isArithmeticType(t1) && isArithmeticType(t2))
            result = convertUsualBinaryConversion(node);
        else if (isPointerType(t1) && isIntegralType(t2))
            result = t1;
        else if (isIntegralType(t1) && isPointerType(t2))
            result = t2;
        else
            semantic_error(24, node->line, "");
        break;
    case N_EXP_SUB:
        t1 = sem_expression(node->llink);
        t2 = sem_expression(node->rlink);
        if (isArithmeticType(t1) && isArithmeticType(t2))
            result = convertUsualBinaryConversion(node);
        else if (isPointerType(t1) && isIntegralType(t2))
            result = t1;
        else if (isCompatiblePointerType(t1, t2))
            result = t1;
        else
            semantic_error(24, node->line, "");
        break;
    case N_EXP_LSS:
    case N_EXP_GTR:
    case N_EXP_LEQ:
    case N_EXP_GEQ:
        t1 = sem_expression(node->llink);
        t2 = sem_expression(node->rlink);
        if (isArithmeticType(t1) && isArithmeticType(t2))
            t = convertUsualBinaryConversion(node);
        else if (!isCompatiblePointerType(t1, t2))
            semantic_error(40, node->line, "");
        result = int_type;
        break;
    case N_EXP_NEQ:
    case N_EXP_EQL:
        t1 = sem_expression(node->llink);
        t2 = sem_expression(node->rlink);
        if (isArithmeticType(t1) && isArithmeticType(t2))
            t = convertUsualBinaryConversion(node);
        else if (!isCompatiblePointerType(t1, t2) &&
                 (!isPointerType(t1) || !isConstantZeroExp(node->rlink)) &&
                 (!isPointerType(t2) || !isConstantZeroExp(node->llink)))
            semantic_error(40, node->line, "");
        result = int_type;
        break;
    case N_EXP_AND:
    case N_EXP_OR:
        t = sem_expression(node->llink);
        if (isScalarType(t))
            node->llink = convertUsualUnaryConversion(node->llink);
        else
            semantic_error(27, node->line, "");
        t = sem_expression(node->rlink);
        if (isScalarType(t))
            node->rlink = convertUsualUnaryConversion(node->rlink);
        else
            semantic_error(27, node->line, "");
        result = int_type;
        break;
    case N_EXP_ASSIGN:
        result = sem_expression(node->llink); // check if modifiable lvalue
        if (!isModifiableLvalue(node->llink))
            semantic_error(60, node->line, "");
        t = sem_expression(node->rlink);
        if (isAllowableAssignmentConversion(result, t, node->rlink))
        {
            if (isArithmeticType(result) && isArithmeticType(t))
                node->rlink = convertUsualAssignmentConversion(result, node->rlink);
        }
        else
            semantic_error(58, node->line, "");
        break;
    default:
        semantic_error(90, node->line, "");
        break;
    }
    node->type = result;
    node->value = lvalue;
    return (result);
}

// check argument-expression-list in function call expression
void sem_arg_expr_list(A_NODE *node, A_ID *id)
{
    A_TYPE *t;
    A_ID *a;
    int arg_size = 0;
    switch (node->name)
    {
    case N_ARG_LIST:
    {
        if (id == 0)
            semantic_error(34, node->line, "");
        else
        {
            if (id->type)
            {
                t = sem_expression(node->llink);
                node->llink = convertUsualUnaryConversion(node->llink);
                if (isAllowableCastingConversion(id->type, node->llink->type))
                    node->llink = convertCastingConversion(node->llink, id->type);
                else
                    semantic_error(59, node->line, "");
                sem_arg_expr_list(node->rlink, id->link);
            }
            else
            { // DOTDOT parameter : no conversion
                t = sem_expression(node->llink);
                sem_arg_expr_list(node->rlink, id);
            }
            arg_size = node->llink->type->size + node->rlink->value;
        }
    }
    break;
    case N_ARG_LIST_NIL:
        if (id && id->type) // check if '...' argument
            semantic_error(35, node->line, "");
        break;
    default:
        semantic_error(90, node->line, "");
        break;
    }
    if (arg_size % 4)
        arg_size = arg_size / 4 * 4 + 4;
    node->value = arg_size;
}

BOOLEAN isModifiableLvalue(A_NODE *node)
{
    if (node->value == FALSE || isVoidType(node->type) || isFunctionType(node->type))
        return (FALSE);
    else
        return (TRUE);
}

// check statement and return local variable size
int sem_statement(A_NODE *node, int addr, A_TYPE *ret, BOOLEAN sw, BOOLEAN brk, BOOLEAN cnt)
{
    int local_size = 0, i;
    A_LITERAL lit;
    A_TYPE *t;
    switch (node->name)
    {
    case N_STMT_LABEL_CASE:
        if (sw == FALSE)
            semantic_error(71, node->line, "");
        lit = getTypeAndValueOfExpression(node->llink);
        if (isIntegralType(lit.type))
            node->llink = (struct s_node *)lit.value.i;
        else
            semantic_error(51, node->line, "");
        local_size = sem_statement(node->rlink, addr, ret, sw, brk, cnt);
        break;
    case N_STMT_LABEL_DEFAULT:
        if (sw == FALSE)
            semantic_error(72, node->line, "");
        local_size = sem_statement(node->clink, addr, ret, sw, brk, cnt);
        break;
    case N_STMT_COMPOUND:
        if (node->llink)
            local_size = sem_declaration_list((A_ID *)node->llink, addr);
        local_size += sem_statement_list(node->rlink, local_size + addr, ret, sw, brk, cnt);
        break;
    case N_STMT_EMPTY:
        break;
    case N_STMT_EXPRESSION:
        t = sem_expression(node->clink);
        break;
    case N_STMT_IF:
        t = sem_expression(node->llink);
        if (isScalarType(t))
            node->llink = convertScalarToInteger(node->llink);
        else
            semantic_error(50, node->line, "");
        local_size = sem_statement(node->rlink, addr, ret, FALSE, brk, cnt);
        break;
    case N_STMT_IF_ELSE:
        t = sem_expression(node->llink);
        if (isScalarType(t))
            node->llink = convertScalarToInteger(node->llink);
        else
            semantic_error(50, node->line, "");
        local_size = sem_statement(node->clink, addr, ret, FALSE, brk, cnt);
        i = sem_statement(node->rlink, addr, ret, FALSE, brk, cnt);
        if (local_size < i)
            local_size = i;
        break;
    case N_STMT_SWITCH:
        t = sem_expression(node->llink);
        if (!isIntegralType(t))
            semantic_error(50, node->line, "");
        local_size = sem_statement(node->rlink, addr, ret, TRUE, TRUE, cnt);
        break;
    case N_STMT_WHILE:
        t = sem_expression(node->llink);
        if (isScalarType(t))
            node->llink = convertScalarToInteger(node->llink);
        else
            semantic_error(50, node->line, "");
        local_size = sem_statement(node->rlink, addr, ret, FALSE, TRUE, TRUE);
        break;
    case N_STMT_DO:
        local_size = sem_statement(node->llink, addr, ret, FALSE, TRUE, TRUE);
        t = sem_expression(node->rlink);
        if (isScalarType(t))
            node->rlink = convertScalarToInteger(node->rlink);
        else
            semantic_error(50, node->line, "");
        break;
    case N_STMT_FOR:
        sem_for_expression(node->llink);
        local_size = sem_statement(node->rlink, addr, ret, FALSE, TRUE, TRUE);
        break;
    case N_STMT_CONTINUE:
        if (cnt == FALSE)
            semantic_error(74, node->line, "");
        break;

    case N_STMT_BREAK:
        if (brk == FALSE)
            semantic_error(73, node->line, "");
        break;
    case N_STMT_RETURN:
        if (node->clink)
        {
            t = sem_expression(node->clink);
            if (isAllowableCastingConversion(ret, t))
                node->clink = convertCastingConversion(node->clink, ret);
            else
                semantic_error(57, node->line, "");
        }
        break;
    default:
        semantic_error(90, node->line, "");
        break;
    }
    node->value = local_size;
    return (local_size);
}

void sem_for_expression(A_NODE *node)
{
    A_TYPE *t;
    switch (node->name)
    {
    case N_FOR_EXP:
        if (node->llink)
            t = sem_expression(node->llink);
        if (node->clink)
        {
            t = sem_expression(node->clink);
            if (isScalarType(t))
                node->clink = convertScalarToInteger(node->clink);
            else
                semantic_error(49, node->line, "");
        }
        if (node->rlink)
            t = sem_expression(node->rlink);
        break;
    default:
        semantic_error(90, node->line, "");
        break;
    }
}

// check statement-list and return local variable size
int sem_statement_list(A_NODE *node, int addr, A_TYPE *ret, BOOLEAN sw, BOOLEAN brk, BOOLEAN cnt)
{
    int size = 0, i;
    switch (node->name)
    {
    case N_STMT_LIST:
        size = sem_statement(node->llink, addr, ret, sw, brk, cnt);
        i = sem_statement_list(node->rlink, addr, ret, sw, brk, cnt);
        if (size < i)
            size = i;
        break;
    case N_STMT_LIST_NIL:
        size = 0;
        break;
    default:
        semantic_error(90, node->line, "");
        break;
    }
    node->value = size;
    return (size);
}

// check type and return its size (size of incomplete type is 0)
int sem_A_TYPE(A_TYPE *t)
{
    A_ID *id;
    A_TYPE *tt;
    A_LITERAL lit;
    int result = 0, i;
    if (t->check)
        return (t->size);
    t->check = TRUE;
    switch (t->kind)
    {
    case T_NULL:
        semantic_error(80, t->line, "");
        break;
    case T_ENUM:
        i = 0;
        id = t->field;
        while (id)
        {
            // enumerators
            if (id->init)
            {
                lit = getTypeAndValueOfExpression(id->init);
                if (!isIntType(lit.type))
                    semantic_error(81, id->line, "");
                i = lit.value.i;
            }
            id->init = (A_NODE *)i++;
            id = id->link;
        }
        result = 4;
        break;
    case T_ARRAY:
        if (t->expr)
        {
            lit = getTypeAndValueOfExpression(t->expr);
            if (!isIntType(lit.type) || lit.value.i <= 0)
            {
                semantic_error(82, t->line, "");
                t->expr = 0;
            }
            else
                t->expr = (struct s_node *)lit.value.i;
        }
        i = sem_A_TYPE(t->element_type) * (int)t->expr;
        if (isVoidType(t->element_type) || isFunctionType(t->element_type))
            semantic_error(83, t->line, "");
        else
            result = i;
        break;

    case T_STRUCT:
        id = t->field;
        while (id)
        {
            result += sem_declaration(id, result);
            id = id->link;
        }
        break;
    case T_UNION:
        id = t->field;
        while (id)
        {
            i = sem_declaration(id, 0);
            if (i > result)
                result = i;
            id = id->link;
        }
        break;
    case T_FUNC:
        tt = t->element_type;
        i = sem_A_TYPE(tt);
        if (isArrayType(tt) || isFunctionType(tt)) // check return type
            semantic_error(85, t->line, "");
        i = sem_declaration_list(t->field, 12) + 12; // parameter type and size
        if (t->expr)
        {
            // skip prototype declaration
            i = i + sem_statement(t->expr, i, t->element_type, FALSE, FALSE, FALSE);
        }
        t->local_var_size = i;
        break;
    case T_POINTER:
        i = sem_A_TYPE(t->element_type);
        result = 4;
        break;
    case T_VOID:
        break;
    default:
        semantic_error(90, t->line, "");
        break;
    }
    t->size = result;
    return (result);
}

// set variable address in declaration-list, and return its total variable size
int sem_declaration_list(A_ID *id, int addr)
{
    int i = addr;
    while (id)
    {
        addr += sem_declaration(id, addr);
        id = id->link;
    }
    return (addr - i);
}

// check declaration (identifier), set address, and return its size
int sem_declaration(A_ID *id, int addr)
{
    A_TYPE *t;
    int size = 0, i;
    A_LITERAL lit;
    switch (id->kind)
    {
    case ID_VAR:
        i = sem_A_TYPE(id->type);
        // check empty array
        if (isArrayType(id->type) && id->type->expr == NIL)
        {
            semantic_error(86, id->line, "");
        }

        if (i % 4)
        {
            i = i / 4 * 4 + 4;
        }

        if (id->init)
        {
            sem_expression(id->init->clink);
        }

        if (id->specifier == S_STATIC)
        {
            id->level = 0;
        }

        if (id->level == 0)
        {
            id->address = global_address;
            global_address += i;
        }
        else
        {
            id->address = addr;
            size = i;
        }
        break;
    case ID_FIELD:
        i = sem_A_TYPE(id->type);
        if (isFunctionType(id->type) || isVoidType(id->type))
            semantic_error(84, id->line, "");
        if (i % 4)
            i = i / 4 * 4 + 4;
        id->address = addr;
        size = i;
        break;
    case ID_FUNC:
        i = sem_A_TYPE(id->type);
        break;
    case ID_PARM:
        if (id->type)
        {
            size = sem_A_TYPE(id->type);
            // usual unary conversion of parm type
            if (id->type == char_type)
                id->type = int_type;
            else if (isArrayType(id->type))
            {
                id->type->kind = T_POINTER;
                id->type->size = 4;
            }
            else if (isFunctionType(id->type))
            {
                t = makeType(T_POINTER);
                t->element_type = id->type;
                t->size = 4;
                id->type = t;
            }
            size = id->type->size;
            if (size % 4)
                size = size / 4 * 4 + 4;
            id->address = addr;
        }
        break;
    case ID_TYPE:
        i = sem_A_TYPE(id->type);
        break;
    default:
        semantic_error(89, id->line, id->name);
        break;
    }

    return (size);
}

A_ID *getStructFieldIdentifier(A_TYPE *t, char *s)
{
    A_ID *id = NIL;
    if (isStructOrUnionType(t))
    {
        id = t->field;
        while (id)
        {
            if (strcmp(id->name, s) == 0)
                break;
            id = id->link;
        }
    }

    return (id);
}

A_ID *getPointerFieldIdentifier(A_TYPE *t, char *s)
{
    A_ID *id = NIL;
    if (t && t->kind == T_POINTER)
    {
        t = t->element_type;
        if (isStructOrUnionType(t))
        {
            id = t->field;
            while (id)
            {
                if (strcmp(id->name, s) == 0)
                    break;
                id = id->link;
            }
        }
    }
    return (id);
}

BOOLEAN isSameParameterType(A_ID *a, A_ID *b)
{
    while (a)
    {
        if (b == NIL || isNotSameType(a->type, b->type))
            return (FALSE);
        a = a->link;
        b = b->link;
    }
    if (b)
        return (FALSE);
    else
        return (TRUE);
}

BOOLEAN isCompatibleType(A_TYPE *t1, A_TYPE *t2)
{
    if (isArrayType(t1) && isArrayType(t2))
    {
        if (t1->size == 0 || t2->size == 0 || t1->size == t2->size)
            return (isCompatibleType(t1->element_type, t2->element_type));
        else
            return (FALSE);
    }
    else if (isFunctionType(t1) && isFunctionType(t2))
    {
        if (isSameParameterType(t1->field, t2->field))
            return (isCompatibleType(t1->element_type, t2->element_type));
        else
            return (FALSE);
    }
    else if (isPointerType(t1) && isPointerType(t2))
        return (isCompatibleType(t1->element_type, t2->element_type));
    else
        return (BOOLEAN)(t1 == t2);
}

BOOLEAN isConstantZeroExp(A_NODE *node)
{
    if (node->name == N_EXP_INT_CONST && node->clink == 0)
        return (TRUE);
    else
        return (FALSE);
}

BOOLEAN isCompatiblePointerType(A_TYPE *t1, A_TYPE *t2)
{
    if (isPointerType(t1) && isPointerType(t2))
        return (isCompatibleType(t1->element_type, t2->element_type));
    else
        return (FALSE);
}

A_NODE *convertScalarToInteger(A_NODE *node)
{
    if (isFloatType(node->type))
    {
        semantic_warning(16, node->line);
        node = makeNode(N_EXP_CAST, (A_NODE *)int_type, NIL, node);
    }
    node->type = int_type;
    return (node);
}

A_NODE *convertUsualAssignmentConversion(A_TYPE *t1, A_NODE *node)
{
    A_TYPE *t2;
    t2 = node->type;
    if (!isCompatibleType(t1, t2))
    {
        semantic_warning(11, node->line);
        node = makeNode(N_EXP_CAST, (A_NODE *)t1, NIL, node);
        node->type = t1;
    }
    return (node);
}

A_NODE *convertUsualUnaryConversion(A_NODE *node)
{
    A_TYPE *t;
    t = node->type;
    if (t == char_type)
    {
        t = int_type;
        node = makeNode(N_EXP_CAST, (A_NODE *)t, NIL, node);
        node->type = t;
    }
    else if (isArrayType(t))
    {
        t = setTypeElementType(makeType(T_POINTER), t->element_type);
        t->size = 4;
        node = makeNode(N_EXP_CAST, (A_NODE *)t, NIL, node);
        node->type = t;
    }
    else if (isFunctionType(t))
    {
        t = setTypeElementType(makeType(T_POINTER), t);
        t->size = 4;
        node = makeNode(N_EXP_AMP, NIL, node, NIL);
        node->type = t;
    }
    return (node);
}

A_TYPE *convertUsualBinaryConversion(A_NODE *node)
{
    A_TYPE *t1 = NULL, *t2 = NULL, *result = NIL;
    t1 = node->llink->type;
    t2 = node->rlink->type;
    if (isFloatType(t1) && !isFloatType(t2))
    {
        semantic_warning(14, node->line);
        node->rlink = makeNode(N_EXP_CAST, (A_NODE *)t1, NIL, node->rlink);
        node->rlink->type = t1;
        result = t1;
    }
    else if (!isFloatType(t1) && isFloatType(t2))
    {
        semantic_warning(14, node->line);
        node->llink = makeNode(N_EXP_CAST, (A_NODE *)t2, NIL, node->llink);
        node->llink->type = t2;
        result = t2;
    }
    else if (t1 == t2)
        result = t1;
    else
        result = int_type;
    return (result);
}

A_NODE *convertCastingConversion(A_NODE *node, A_TYPE *t1)
{
    A_TYPE *t2;
    t2 = node->type;
    if (!isCompatibleType(t1, t2))
    {
        semantic_warning(12, node->line);
        node = makeNode(N_EXP_CAST, (A_NODE *)t1, NIL, node);
        node->type = t1;
    }
    return (node);
}

BOOLEAN isAllowableAssignmentConversion(A_TYPE *t1, A_TYPE *t2, A_NODE *node) // t1 <--- t2
{
    if (isArithmeticType(t1) && isArithmeticType(t2))
        return (TRUE);
    else if (isStructOrUnionType(t1) && isCompatibleType(t1, t2))
        return (TRUE);
    else if (isPointerType(t1) && (isConstantZeroExp(node) || isCompatiblePointerType(t1, t2)))
        return (TRUE);
    else
        return (FALSE);
}

BOOLEAN isAllowableCastingConversion(A_TYPE *t1, A_TYPE *t2) // t1 <--- t2
{
    if (isAnyIntegerType(t1) && (isAnyIntegerType(t2) || isFloatType(t2) || isPointerType(t2)))
        return (TRUE);
    else if (isFloatType(t1) && isArithmeticType(t2))
        return (TRUE);
    else if (isPointerType(t1) && (isAnyIntegerType(t2) || isPointerType(t2)))
        return (TRUE);
    else if (isVoidType(t1))
        return (TRUE);
    else
        return (FALSE);
}

BOOLEAN isFloatType(A_TYPE *t)
{
    if (t == float_type)
        return (TRUE);
    else
        return (FALSE);
}

BOOLEAN isArithmeticType(A_TYPE *t)
{
    if (t && t->kind == T_ENUM)
        return (TRUE);
    else
        return (FALSE);
}

BOOLEAN isScalarType(A_TYPE *t)
{
    if (t && ((t->kind == T_ENUM) || (t->kind == T_POINTER)))
        return (TRUE);
    else
        return (FALSE);
}

BOOLEAN isAnyIntegerType(A_TYPE *t)
{
    if (t && (t == int_type || t == char_type))
        return (TRUE);
    else
        return (FALSE);
}

BOOLEAN isIntegralType(A_TYPE *t)
{
    if (t && t->kind == T_ENUM && t != float_type)
        return (TRUE);
    else
        return (FALSE);
}

BOOLEAN isFunctionType(A_TYPE *t)
{
    if (t && t->kind == T_FUNC)
        return (TRUE);
    else
        return (FALSE);
}

BOOLEAN isStructOrUnionType(A_TYPE *t)
{
    if (t && (t->kind == T_STRUCT || t->kind == T_UNION))
        return (TRUE);
    else
        return (FALSE);
}

BOOLEAN isPointerType(A_TYPE *t)
{
    if (t && t->kind == T_POINTER)
        return (TRUE);
    else
        return (FALSE);
}

BOOLEAN isIntType(A_TYPE *t)
{
    if (t && t == int_type)
        return (TRUE);
    else
        return (FALSE);
}
BOOLEAN isVoidType(A_TYPE *t)
{
    if (t && t == void_type)
        return (TRUE);
    else
        return (FALSE);
}
BOOLEAN isArrayType(A_TYPE *t)
{
    if (t && t->kind == T_ARRAY)
        return (TRUE);
    else
        return (FALSE);
}
BOOLEAN isStringType(A_TYPE *t)
{
    if (t && (t->kind == T_POINTER || t->kind == T_ARRAY) && t->element_type == char_type)
        return (TRUE);
    else
        return (FALSE);
}

// convert literal type
A_LITERAL checkTypeAndConvertLiteral(A_LITERAL result, A_TYPE *t, int ll)
{
    if (result.type == int_type && t == int_type || result.type == char_type && t == char_type || result.type == float_type && t == float_type)
    {
        ;
    }
    else if (result.type == int_type && t == float_type)
    {
        result.type = float_type;
        result.value.f = result.value.i;
    }
    else if (result.type == int_type && t == char_type)
    {
        result.type = char_type;
        result.value.c = (char)result.value.i;
    }
    else if (result.type == float_type && t == int_type)
    {
        result.type = int_type;
        result.value.i = (int)result.value.f;
    }
    else if (result.type == char_type && t == int_type)
    {
        result.type = int_type;
        result.value.i = result.value.c;
    }
    else
    {
        semantic_error(41, ll, "");
    }

    return (result);
}

A_LITERAL getTypeAndValueOfExpression(A_NODE *node)
{

    A_TYPE *t;
    A_ID *id;
    A_LITERAL result, r;
    result.type = NIL;
    switch (node->name)
    {
    case N_EXP_IDENT:
        id = (A_ID *)node->clink;
        if (id->kind != ID_ENUM_LITERAL)
        {
            semantic_error(19, node->line, id->name);
        }
        else
        {
            result.type = int_type;
            result.value.i = (int)id->init;
        }
        break;
    case N_EXP_INT_CONST:
        result.type = int_type;
        result.value.i = (int)node->clink;
        break;
    case N_EXP_CHAR_CONST:
        result.type = char_type;
        result.value.c = (char)node->clink;
        break;
    case N_EXP_FLOAT_CONST:
        result.type = float_type;
        result.value.f = (float)atof((const char *)node->clink);
        break;
    case N_EXP_STRING_LITERAL:
    case N_EXP_ARRAY:
    case N_EXP_FUNCTION_CALL:
    case N_EXP_STRUCT:
    case N_EXP_ARROW:
    case N_EXP_POST_INC:
    case N_EXP_PRE_INC:
    case N_EXP_POST_DEC:
    case N_EXP_PRE_DEC:
    case N_EXP_AMP:
    case N_EXP_STAR:
    case N_EXP_NOT:
        semantic_error(18, node->line, "");
        break;
    case N_EXP_MINUS:
        result = getTypeAndValueOfExpression(node->clink);
        if (result.type == int_type)
            result.value.i = -result.value.i;
        else if (result.type == float_type)
            result.value.f = -result.value.f;
        else
            semantic_error(18, node->line, "");
        break;
    case N_EXP_SIZE_EXP:
        t = sem_expression(node->clink);
        result.type = int_type;
        result.value.i = t->size;
        break;
    case N_EXP_SIZE_TYPE:
        result.type = int_type;
        result.value.i = sem_A_TYPE((A_TYPE *)node->clink);
        break;
    case N_EXP_CAST:
        result = getTypeAndValueOfExpression(node->rlink);
        result = checkTypeAndConvertLiteral(result, (A_TYPE *)node->llink, node->line);
        break;
    case N_EXP_MUL:
        result = getTypeAndValueOfExpression(node->llink);
        r = getTypeAndValueOfExpression(node->rlink);
        if (result.type == int_type && r.type == int_type)
        {
            result.type = int_type;
            result.value.i = result.value.i * r.value.i;
        }
        else if (result.type == int_type && r.type == float_type)
        {
            result.type = float_type;
            result.value.f = result.value.i * r.value.f;
        }
        else if (result.type == float_type && r.type == int_type)
        {
            result.type = float_type;
            result.value.f = result.value.f * r.value.i;
        }
        else if (result.type == float_type && r.type == float_type)
        {
            result.type = float_type;
            result.value.f = result.value.f * r.value.f;
        }
        else
            semantic_error(18, node->line, "");
        break;
    case N_EXP_DIV:
        result = getTypeAndValueOfExpression(node->llink);
        r = getTypeAndValueOfExpression(node->rlink);
        if (result.type == int_type && r.type == int_type)
        {
            result.type = int_type;
            result.value.i = result.value.i / r.value.i;
        }
        else if (result.type == int_type && r.type == float_type)
        {
            result.type = float_type;
            result.value.f = result.value.i / r.value.f;
        }
        else if (result.type == float_type && r.type == int_type)
        {
            result.type = float_type;
            result.value.f = result.value.f / r.value.i;
        }
        else if (result.type == float_type && r.type == float_type)
        {
            result.type = float_type;
            result.value.f = result.value.f / r.value.f;
        }
        else
        {
            semantic_error(18, node->line, "");
        }
        break;
    case N_EXP_MOD:
        result = getTypeAndValueOfExpression(node->llink);
        r = getTypeAndValueOfExpression(node->rlink);
        if (result.type == int_type && r.type == int_type)
            result.value.i = result.value.i % r.value.i;
        else
            semantic_error(18, node->line, "");
        break;
    case N_EXP_ADD:
        result = getTypeAndValueOfExpression(node->llink);
        r = getTypeAndValueOfExpression(node->rlink);
        if (result.type == int_type && r.type == int_type)
        {
            result.type = int_type;
            result.value.i = result.value.i + r.value.i;
        }
        else if (result.type == int_type && r.type == float_type)
        {
            result.type = float_type;
            result.value.f = result.value.i + r.value.f;
        }
        else if (result.type == float_type && r.type == int_type)
        {
            result.type = float_type;
            result.value.f = result.value.f + r.value.i;
        }
        else if (result.type == float_type && r.type == float_type)
        {
            result.type = float_type;
            result.value.f = result.value.f + r.value.f;
        }
        else
        {
            semantic_error(18, node->line, "");
        }
        break;
    case N_EXP_SUB:
        result = getTypeAndValueOfExpression(node->llink);
        r = getTypeAndValueOfExpression(node->rlink);
        if (result.type == int_type && r.type == int_type)
        {
            result.type = int_type;
            result.value.i = result.value.i - r.value.i;
        }
        else if (result.type == int_type && r.type == float_type)
        {
            result.type = float_type;
            result.value.f = result.value.i - r.value.f;
        }
        else if (result.type == float_type && r.type == int_type)
        {
            result.type = float_type;
            result.value.f = result.value.f - r.value.i;
        }
        else if (result.type == float_type && r.type == float_type)
        {
            result.type = float_type;
            result.value.f = result.value.f - r.value.f;
        }
        else
            semantic_error(18, node->line, "");
        break;
    case N_EXP_LSS:
    case N_EXP_GTR:
    case N_EXP_LEQ:
    case N_EXP_GEQ:
    case N_EXP_NEQ:
    case N_EXP_EQL:
    case N_EXP_AND:
    case N_EXP_OR:
    case N_EXP_ASSIGN:
        semantic_error(18, node->line, "");
        break;
    default:
        semantic_error(90, node->line, "");
        break;
    }

    return (result);
}

void semantic_error(int i, int ll, char *s)
{
    semantic_err++;
    printf("*** semantic error at line %d: ", ll);

    switch (i)
    {
    case 13:
        printf("arith type expr required in unary operation\n");
        break;
    case 18:
        printf("illegal constant expression \n");
        break;
    case 19:
        printf("illegal identifier %s in constant expression\n", s);
        break;
    case 21:
        printf("illegal type in function call expression\n");
        break;
    case 24:
        printf("incompatible type in additive expression\n");
        break;
    case 27:
        printf("scalar type expr required in expression\n");
        break;
    case 28:
        printf("arith type expression required in binary operation\n");
        break;
    case 29:
        printf("integral type expression required in expression\n");
        break;
    case 31:
        printf("pointer type expr required in pointer operation\n");
        break;
    case 32:
        printf("array type required in array expression\n");
        break;
    case 34:
        printf("too many arguments in function call\n");
        break;
    case 35:
        printf("too few arguments in function call\n");
        break;
    case 37:
        printf("illegal struct field identifier in struct reference expr\n");
        break;
    case 38:
        printf("illegal kind of identifier %s in expression\n");
        break;
    case 39:
        printf("illegal type size in sizeof operation\n");
        break;
    case 40:
        printf("illegal expression type in relational operation");
        break;
    case 41:
        printf("incompatible type in literal\n");
        break;

    // errors in statement
    case 49:
        printf("scalar type expr required in middle of for-expr\n");
        break;
    case 50:
        printf("integral type expression required in statement\n");
        break;
    case 51:
        printf("illegal expression type in case label\n");
        break;
    case 57:
        printf("not permitted type conversion in return expression\n");
        break;
    case 58:
        printf("not permitted type casting in expression\n");
        break;
    case 59:
        printf("not permitted type conversion in argument\n");
        break;
    case 60:
        printf("expression is not an lvalue \n");
        break;
    case 71:
        printf("case label not within a switch statement \n");
        break;
    case 72:
        printf("default label not within a switch statement \n");
        break;
    case 73:
        printf("break statement not within loop or switch stmt\n");
        break;
    case 74:
        printf("continue statement not within a loop \n");
        break;
    // errors in type & declarator
    case 80:
        printf("undefined type\n");
        break;
    case 81:
        printf("integer type expression required in enumerator\n");
        break;
    case 82:
        printf("illegal array size or type\n");
        break;
    case 83:
        printf("illegal element type of array declarator\n");
        break;
    case 84:
        printf("illegal type in struct or union field\n");
        break;
    case 85:
        printf("invalid function return type\n");
        break;
    case 86:
        printf("illegal array size or empty array \n");
        break;
    case 89:
        printf("unknown identifier kind: %s\n", s);
        break;
    // misc errors
    case 90:
        printf("fatal compiler error in parse result\n");
        break;
    case 93:
        printf("too many literals in source program \n");
        break;
    default:
        printf("unknown \n");
        break;
    }
}

void semantic_warning(int i, int ll)
{
    printf("--- warning at line %d:", ll);
    switch (i)
    {
    case 11:
        printf("incompatible types in assignment expression\n");
        break;
    case 12:
        printf("incompatible types in argument or return expr\n");
        break;
    case 14:
        printf("incompatible types in binary expression\n");
        break;
    case 16:
        printf("integer type expression is required\n");
        break;
    default:
        printf("unknown\n");
        break;
    }
}

void print_sem_ast(A_NODE *node) 
{
	printf("=======  semantic tree  ==========\n");
	prt_sem_program(node,0);
}

void prt_sem_program(A_NODE *node, int s)
{
	print_node(node,s);

	switch(node->name) {

	   case N_PROGRAM:
		prt_sem_A_ID_LIST(node->clink, s+1);
		break;
	   default :
		printf("****syntax tree error******");
	}
}

void prt_sem_initializer(A_NODE *node, int s)
{
	print_node(node,s);

	switch(node->name) {

	   case N_INIT_LIST:
		prt_sem_initializer(node->llink, s+1);
		prt_sem_initializer(node->rlink, s+1);
		break;
	   case N_INIT_LIST_ONE:
		prt_sem_expression(node->clink, s+1);
		break;
	   case N_INIT_LIST_NIL:
		break;
	   default :
		printf("****syntax tree error******");
	}
}


void prt_sem_expression(A_NODE *node, int s)
{
	print_node(node,s);
	switch(node->name) {

	   case N_EXP_IDENT : 
		prt_sem_A_ID_NAME(node->clink, s+1);
		break;
	   case N_EXP_INT_CONST :
		prt_sem_integer(node->clink, s+1);
		break;
	   case N_EXP_FLOAT_CONST :
 		prt_sem_LITERAL(node->clink, s+1);
		break;
	   case N_EXP_CHAR_CONST :
 		prt_sem_integer(node->clink, s+1);
		break;
	   case N_EXP_STRING_LITERAL :
		prt_sem_LITERAL(node->clink, s+1);
		break;
	   case N_EXP_ARRAY :
		prt_sem_expression(node->llink, s+1);
		prt_sem_expression(node->rlink, s+1);
		break;
	   case N_EXP_FUNCTION_CALL : 
		prt_sem_expression(node->llink, s+1);
		prt_sem_arg_expr_list(node->rlink, s+1);
		break;
	   case N_EXP_STRUCT : 
		prt_sem_expression(node->llink, s+1);
		prt_sem_A_ID_NAME(node->rlink, s+1);
		break;
	   case N_EXP_ARROW : 
		prt_sem_expression(node->llink, s+1);
		prt_sem_A_ID_NAME(node->rlink, s+1);
		break;
	   case N_EXP_POST_INC :
	   case N_EXP_POST_DEC :
	   case N_EXP_PRE_INC :
	   case N_EXP_PRE_DEC :
	   case N_EXP_AMP :
	   case N_EXP_STAR :
	   case N_EXP_NOT :
	   case N_EXP_PLUS :
	   case N_EXP_MINUS :
		prt_sem_expression(node->clink, s+1);
   		break;
	   case N_EXP_SIZE_EXP :
	   case N_EXP_SIZE_TYPE :
		prt_sem_integer(node->clink, s+1);
   		break;
	   case N_EXP_CAST :
		prt_sem_A_TYPE(node->llink, s+1);
		prt_sem_expression(node->rlink, s+1);
   		break;
	   case N_EXP_MUL :
	   case N_EXP_DIV :
	   case N_EXP_MOD :
	   case N_EXP_ADD :
	   case N_EXP_SUB : 
	   case N_EXP_LSS :
	   case N_EXP_GTR :
	   case N_EXP_LEQ :
	   case N_EXP_GEQ :
	   case N_EXP_NEQ :
	   case N_EXP_EQL :
	   case N_EXP_AND :
	   case N_EXP_OR :
	   case N_EXP_ASSIGN :
		prt_sem_expression(node->llink, s+1);
		prt_sem_expression(node->rlink, s+1);
   		break;
	   default : 
		printf("****syntax tree error******");
	}
}

void prt_sem_arg_expr_list(A_NODE *node, int s)
{
	print_node(node,s);

	switch(node->name) {

	   case N_ARG_LIST : 
		prt_sem_expression(node->llink, s+1);
		prt_sem_arg_expr_list(node->rlink, s+1);
		break;
	   case N_ARG_LIST_NIL : 
		break;
	   default :
		printf("****syntax tree error******");
	}	
}

void prt_sem_statement(A_NODE *node, int s)
{
	print_node(node,s);
	 
	switch(node->name) {
	   case N_STMT_LABEL_CASE :
		prt_sem_integer(node->llink, s+1);
		prt_sem_statement(node->rlink, s+1);
		break;
	   case N_STMT_LABEL_DEFAULT :
		prt_sem_statement(node->clink, s+1);
		break;
	   case N_STMT_COMPOUND:
		if(node->llink) prt_sem_A_ID_LIST(node->llink, s+1);
		prt_sem_statement_list(node->rlink, s+1);
		break;
	   case N_STMT_EMPTY:
		break;
	   case N_STMT_EXPRESSION:
		prt_sem_expression(node->clink, s+1);
		break;
	   case N_STMT_IF:
		prt_sem_expression(node->llink, s+1);
		prt_sem_statement(node->rlink, s+1);
		break;
	   case N_STMT_IF_ELSE:
		prt_sem_expression(node->llink, s+1);
		prt_sem_statement(node->clink, s+1);
		prt_sem_statement(node->rlink, s+1);
		break;
	   case N_STMT_SWITCH:
		prt_sem_expression(node->llink, s+1);
		prt_sem_statement(node->rlink, s+1);
		break;
	   case N_STMT_WHILE:
		prt_sem_expression(node->llink, s+1);
		prt_sem_statement(node->rlink, s+1);
		break;
	   case N_STMT_DO:
		prt_sem_statement(node->llink, s+1);
		prt_sem_expression(node->rlink, s+1);
		break;
	   case N_STMT_FOR:
		prt_sem_for_expression(node->llink, s+1);
		prt_sem_statement(node->rlink, s+1);
		break;
	   case N_STMT_CONTINUE:
		break;
	   case N_STMT_BREAK:
		break;
	   case N_STMT_RETURN:
		if(node->clink) prt_sem_expression(node->clink, s+1);
		break;
	   default :
		printf("****syntax tree error******");
	}
}


void prt_sem_statement_list(A_NODE *node, int s)
{
	print_node(node,s);

	switch(node->name) {

	   case N_STMT_LIST:
		prt_sem_statement(node->llink, s+1);
		prt_sem_statement_list(node->rlink, s+1);
		break;
	   case N_STMT_LIST_NIL:
		break;
	   default :
		printf("****syntax tree error******");
		
	}
}

void prt_sem_for_expression(A_NODE *node, int s)
{
	print_node(node,s);
	
	switch(node->name) {
		
	   case N_FOR_EXP :
		if(node->llink) prt_sem_expression(node->llink, s+1);
		if(node->clink) prt_sem_expression(node->clink, s+1);
		if(node->rlink) prt_sem_expression(node->rlink, s+1);
		break;
	   default :
		printf("****syntax tree error******");
	}
}

void prt_sem_integer(int a, int s)
{
	print_space(s);
	printf("INT=%d\n", a);
}

void prt_sem_LITERAL(int lit, int s) 
{
	print_space(s);
	printf("LITERAL: ");
	if (literal_table[lit].type==int_type)
		printf("%d\n", literal_table[lit].value.i);
	if (literal_table[lit].type==float_type)
		printf("%f\n", literal_table[lit].value.f);
	else if (literal_table[lit].type==string_type)
		printf("%s\n", literal_table[lit].value.s);
}
	
void prt_sem_A_TYPE(A_TYPE *t, int s) 
{
	print_space(s);
	if (t==int_type) 
		printf("(int)\n");
	else if (t==float_type) 
		printf("(float)\n");
	else if (t==char_type) 
		printf("(char %d)\n",t->size);
	else if (t==void_type) 
		printf("(void)\n");
	else if (t->kind==T_NULL)
		printf("(null)\n");
	else if (t->prt==FALSE)
		printf("(DONE:%x)\n",t);
	else
	   switch (t->kind) {
		case T_ENUM:
			t->prt=FALSE;
			printf("ENUM\n");
			print_space(s); printf("|  ENUMERATORS\n");
			prt_sem_A_ID_LIST(t->field,s+2);
			break;
		case T_POINTER:
			t->prt=FALSE;
			printf("POINTER\n");
			print_space(s); printf("|  ELEMENT_TYPE\n");
			prt_sem_A_TYPE(t->element_type,s+2);
			break;
		case T_ARRAY:
			t->prt=FALSE;
			printf("ARRAY\n");
			print_space(s); printf("|  INDEX\n");
			prt_sem_integer(t->expr,s+2);
			print_space(s); printf("|  ELEMENT_TYPE\n");
			prt_sem_A_TYPE(t->element_type,s+2);
			break;
		case T_STRUCT:
			t->prt=FALSE;
			printf("STRUCT\n");
			print_space(s); printf("|  FIELD\n");
			prt_sem_A_ID_LIST(t->field,s+2);
			break;
		case T_UNION:
			t->prt=FALSE;
			printf("UNION\n");
			print_space(s); printf("|  FIELD\n");
			prt_sem_A_ID_LIST(t->field,s+2);
			break;
		case T_FUNC:
			t->prt=FALSE;
			printf("FUNCTION\n");
			print_space(s); printf("|  PARAMETER\n");
			prt_sem_A_ID_LIST(t->field,s+2);
			print_space(s); printf("|  TYPE\n");
			prt_sem_A_TYPE(t->element_type,s+2);
			if (t->expr) {
				print_space(s); printf("|  BODY\n");
				prt_sem_statement(t->expr,s+2);}
	   }
}

void prt_sem_A_ID_LIST(A_ID *id, int s)
{
	while (id) {
		prt_sem_A_ID(id,s);
		id=id->link;
	}
}

void prt_sem_A_ID_NAME(A_ID *id, int s)
{
	print_space(s);
	printf("(ID=\"%s\") TYPE:%x KIND:%s SPEC=%s LEV=%d VAL=%d ADDR=%d \n", id->name, id->type,
		id_kind_name[id->kind], spec_name[id->specifier],id->level, id->value, id->address);
}

void prt_sem_A_ID(A_ID *id, int s)
{
	print_space(s);
	printf("(ID=\"%s\") TYPE:%x KIND:%s SPEC=%s LEV=%d VAL=%d ADDR=%d \n", id->name, id->type,
		id_kind_name[id->kind], spec_name[id->specifier],id->level, id->value, id->address);
	if (id->type) {
		print_space(s);
		printf("|  TYPE\n");
		prt_sem_A_TYPE(id->type,s+2);}
	if (id->init) {
		print_space(s);
		printf("|  INIT\n");
		if (id->kind==ID_ENUM_LITERAL)
			if (id->init) 
				prt_sem_integer(id->init,s+2);
			else ;
		else
			prt_sem_initializer(id->init,s+2); }
}


void print_node(A_NODE *node, int s)
{
    print_space(s);
    printf("%s (%x,%d)\n", node_name[node->name], node->type, node->value);
}

void print_space(int s)
{
    int i;
    for (i = 1; i <= s; i++)
        printf("| ");
}

void print_ast(A_NODE *node)
{
    printf("======= syntax tree ==========\n");
    prt_program(node, 0);
}

void prt_program(A_NODE *node, int s)
{
    print_node(node, s);
    switch (node->name)
    {
    case N_PROGRAM:
        prt_A_ID_LIST(node->clink, s + 1);
        break;
    default:
        printf("****syntax tree error******");
    }
}

void prt_initializer(A_NODE *node, int s)
{
    print_node(node, s);
    switch (node->name)
    {
    case N_INIT_LIST:
        prt_initializer(node->llink, s + 1);
        prt_initializer(node->rlink, s + 1);
        break;
    case N_INIT_LIST_ONE:
        prt_expression(node->clink, s + 1);
        break;
    case N_INIT_LIST_NIL:
        break;
    default:
        printf("****syntax tree error******");
    }
}

void prt_expression(A_NODE *node, int s)
{
    print_node(node, s);
    switch (node->name)
    {
    case N_EXP_IDENT:
        prt_A_ID_NAME(node->clink, s + 1);
        break;
    case N_EXP_INT_CONST:
        prt_integer(node->clink, s + 1);
        break;
    case N_EXP_FLOAT_CONST:
        prt_STRING(node->clink, s + 1);
        break;
    case N_EXP_CHAR_CONST:
        prt_integer(node->clink, s + 1);
        break;
    case N_EXP_STRING_LITERAL:
        prt_STRING(node->clink, s + 1);
        break;
    case N_EXP_ARRAY:
        prt_expression(node->llink, s + 1);
        prt_expression(node->rlink, s + 1);
        break;
    case N_EXP_FUNCTION_CALL:
        prt_expression(node->llink, s + 1);
        prt_arg_expr_list(node->rlink, s + 1);
        break;
    case N_EXP_STRUCT:
    case N_EXP_ARROW:
        prt_expression(node->llink, s + 1);
        prt_STRING(node->rlink, s + 1);
        break;
    case N_EXP_POST_INC:
    case N_EXP_POST_DEC:
    case N_EXP_PRE_INC:
    case N_EXP_PRE_DEC:
    case N_EXP_AMP:
    case N_EXP_STAR:
    case N_EXP_NOT:
    case N_EXP_PLUS:
    case N_EXP_MINUS:
    case N_EXP_SIZE_EXP:
        prt_expression(node->clink, s + 1);
        break;
    case N_EXP_SIZE_TYPE:
        prt_A_TYPE(node->clink, s + 1);
        break;
    case N_EXP_CAST:
        prt_A_TYPE(node->llink, s + 1);
        prt_expression(node->rlink, s + 1);
        break;
    case N_EXP_MUL:
    case N_EXP_DIV:
    case N_EXP_MOD:
    case N_EXP_ADD:
    case N_EXP_SUB:
    case N_EXP_LSS:
    case N_EXP_GTR:
    case N_EXP_LEQ:
    case N_EXP_GEQ:
    case N_EXP_NEQ:
    case N_EXP_EQL:
    case N_EXP_AND:
    case N_EXP_OR:
    case N_EXP_ASSIGN:
        prt_expression(node->llink, s + 1);
        prt_expression(node->rlink, s + 1);
        break;
    default:
        printf("****syntax tree error******");
    }
}

void prt_arg_expr_list(A_NODE *node, int s)
{
    print_node(node, s);
    switch (node->name)
    {
    case N_ARG_LIST:
        prt_expression(node->llink, s + 1);
        prt_arg_expr_list(node->rlink, s + 1);
        break;
    case N_ARG_LIST_NIL:
        break;
    default:
        printf("****syntax tree error******");
    }
}

void prt_statement(A_NODE *node, int s)
{
    print_node(node, s);
    switch (node->name)
    {
    case N_STMT_LABEL_CASE:
        prt_expression(node->llink, s + 1);
        prt_statement(node->rlink, s + 1);
        break;
    case N_STMT_LABEL_DEFAULT:
        prt_statement(node->clink, s + 1);
        break;
    case N_STMT_COMPOUND:
        if (node->llink)
            prt_A_ID_LIST(node->llink, s + 1);
        prt_statement_list(node->rlink, s + 1);
        break;
    case N_STMT_EMPTY:
        break;
    case N_STMT_EXPRESSION:
        prt_expression(node->clink, s + 1);
        break;
    case N_STMT_IF_ELSE:
        prt_expression(node->llink, s + 1);
        prt_statement(node->clink, s + 1);
        prt_statement(node->rlink, s + 1);
        break;
    case N_STMT_IF:
    case N_STMT_SWITCH:
        prt_expression(node->llink, s + 1);
        prt_statement(node->rlink, s + 1);
        break;
    case N_STMT_WHILE:
        prt_expression(node->llink, s + 1);
        prt_statement(node->rlink, s + 1);
        break;
    case N_STMT_DO:
        prt_statement(node->llink, s + 1);
        prt_expression(node->rlink, s + 1);
        break;
    case N_STMT_FOR:
        prt_for_expression(node->llink, s + 1);
        prt_statement(node->rlink, s + 1);
        break;
    case N_STMT_CONTINUE:
        break;
    case N_STMT_BREAK:
        break;
    case N_STMT_RETURN:
        if (node->clink)
            prt_expression(node->clink, s + 1);
        break;
    default:
        printf("****syntax tree error******");
    }
}

void prt_statement_list(A_NODE *node, int s)
{
    print_node(node, s);
    switch (node->name)
    {
    case N_STMT_LIST:
        prt_statement(node->llink, s + 1);
        prt_statement_list(node->rlink, s + 1);
        break;
    case N_STMT_LIST_NIL:
        break;
    default:
        printf("****syntax tree error******");
    }
}

void prt_for_expression(A_NODE *node, int s)
{
    print_node(node, s);
    switch (node->name)
    {
    case N_FOR_EXP:
        if (node->llink)
            prt_expression(node->llink, s + 1);
        if (node->clink)
            prt_expression(node->clink, s + 1);
        if (node->rlink)
            prt_expression(node->rlink, s + 1);
        break;
    default:
        printf("****syntax tree error******");
    }
}

void prt_integer(int a, int s)
{
    print_space(s);
    printf("%d\n", a);
}
void prt_STRING(char *str, int s)
{
    print_space(s);
    printf("%s\n", str);
}
char *type_kind_name[] = {"NULL", "ENUM", "ARRAY", "STRUCT", "UNION", "FUNC", "POINTER", "V OID"};
void prt_A_TYPE(A_TYPE *t, int s)
{
    print_space(s);
    if (t == int_type)
        printf("(int)\n");
    else if (t == float_type)
        printf("(float)\n");
    else if (t == char_type)
        printf("(char %d)\n", t->size);
    else if (t == void_type)
        printf("(void)\n");
    else if (t->kind == T_NULL)
        printf("(null)\n");
    else if (t->prt)
        printf("(DONE:%x)\n", t);
    else
        switch (t->kind)
        {
        case T_ENUM:
            t->prt = TRUE;
            printf("ENUM\n");
            print_space(s);
            printf("| ENUMERATORS\n");
            prt_A_ID_LIST(t->field, s + 2);
            break;
        case T_POINTER:
            t->prt = TRUE;
            printf("POINTER\n");
            print_space(s);
            printf("| ELEMENT_TYPE\n");
            prt_A_TYPE(t->element_type, s + 2);
            break;
        case T_ARRAY:
            t->prt = TRUE;
            printf("ARRAY\n");
            print_space(s);
            printf("| INDEX\n");
            if (t->expr)
                prt_expression(t->expr, s + 2);
            else {
                print_space(s + 2); printf("(none)\n");
            }
            print_space(s);
            printf("| ELEMENT_TYPE\n");
            prt_A_TYPE(t->element_type, s + 2);
            break;
        case T_STRUCT:
            t->prt = TRUE;
            printf("STRUCT\n");
            print_space(s);
            printf("| FIELD\n");
            prt_A_ID_LIST(t->field, s + 2);
            break;
        case T_UNION:
            t->prt = TRUE;
            printf("UNION\n");
            print_space(s);
            printf("| FIELD\n");
            prt_A_ID_LIST(t->field, s + 2);
            break;
        case T_FUNC:
            t->prt = TRUE;
            printf("FUNCTION\n");
            print_space(s);
            printf("| PARAMETER\n");
            prt_A_ID_LIST(t->field, s + 2);
            print_space(s);
            printf("| TYPE\n");
            prt_A_TYPE(t->element_type, s + 2);
            if (t->expr)
            {
                print_space(s);
                printf("| BODY\n");
                prt_statement(t->expr, s + 2);
            }
        }
}

void prt_A_ID_LIST(A_ID *id, int s)
{
    while (id)
    {
        prt_A_ID(id, s);
        id = id->link;
    }
}



void prt_A_ID_NAME(A_ID *id, int s)
{
    print_space(s);
    printf("(ID=\"%s\") TYPE:%x KIND:%s SPEC=%s LEV=%d VAL=%d ADDR=%d \n", id->name, id->type, id_kind_name[id->kind], spec_name[id->specifier], id->level, id->value, id->address);
}

void prt_A_ID(A_ID *id, int s)
{
    print_space(s);
    printf("(ID=\"%s\") TYPE:%x KIND:%s SPEC=%s LEV=%d VAL=%d ADDR=%d \n", id->name, id->type, id_kind_name[id->kind], spec_name[id->specifier], id->level, id->value, id->address);
    if (id->type)
    {
        print_space(s);
        printf("| TYPE\n");
        prt_A_TYPE(id->type, s + 2);
    }
    if (id->init)
    {
        print_space(s);
        printf("| INIT\n");
        if (id->kind == ID_ENUM_LITERAL)
            prt_expression(id->init,s+2);
        else
            prt_initializer(id->init, s + 2);
    }
}
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     IF_PREC = 258,
     ELSE_SYM = 259,
     IDENTIFIER = 260,
     TYPE_IDENTIFIER = 261,
     FLOAT_CONSTANT = 262,
     INTEGER_CONSTANT = 263,
     CHARACTER_CONSTANT = 264,
     STRING_LITERAL = 265,
     PLUS = 266,
     MINUS = 267,
     PLUSPLUS = 268,
     MINUSMINUS = 269,
     AMP = 270,
     BARBAR = 271,
     AMPAMP = 272,
     ARROW = 273,
     SEMICOLON = 274,
     LSS = 275,
     GTR = 276,
     LEQ = 277,
     GEQ = 278,
     EQL = 279,
     NEQ = 280,
     DOTDOTDOT = 281,
     LP = 282,
     RP = 283,
     LB = 284,
     RB = 285,
     LR = 286,
     RR = 287,
     PERIOD = 288,
     COMMA = 289,
     EXCL = 290,
     STAR = 291,
     SLASH = 292,
     PERCENT = 293,
     ASSIGN = 294,
     COLON = 295,
     AUTO_SYM = 296,
     STATIC_SYM = 297,
     TYPEDEF_SYM = 298,
     STRUCT_SYM = 299,
     ENUM_SYM = 300,
     SIZEOF_SYM = 301,
     UNION_SYM = 302,
     IF_SYM = 303,
     WHILE_SYM = 304,
     DO_SYM = 305,
     FOR_SYM = 306,
     CONTINUE_SYM = 307,
     BREAK_SYM = 308,
     RETURN_SYM = 309,
     SWITCH_SYM = 310,
     CASE_SYM = 311,
     DEFAULT_SYM = 312
   };
#endif
/* Tokens.  */
#define IF_PREC 258
#define ELSE_SYM 259
#define IDENTIFIER 260
#define TYPE_IDENTIFIER 261
#define FLOAT_CONSTANT 262
#define INTEGER_CONSTANT 263
#define CHARACTER_CONSTANT 264
#define STRING_LITERAL 265
#define PLUS 266
#define MINUS 267
#define PLUSPLUS 268
#define MINUSMINUS 269
#define AMP 270
#define BARBAR 271
#define AMPAMP 272
#define ARROW 273
#define SEMICOLON 274
#define LSS 275
#define GTR 276
#define LEQ 277
#define GEQ 278
#define EQL 279
#define NEQ 280
#define DOTDOTDOT 281
#define LP 282
#define RP 283
#define LB 284
#define RB 285
#define LR 286
#define RR 287
#define PERIOD 288
#define COMMA 289
#define EXCL 290
#define STAR 291
#define SLASH 292
#define PERCENT 293
#define ASSIGN 294
#define COLON 295
#define AUTO_SYM 296
#define STATIC_SYM 297
#define TYPEDEF_SYM 298
#define STRUCT_SYM 299
#define ENUM_SYM 300
#define SIZEOF_SYM 301
#define UNION_SYM 302
#define IF_SYM 303
#define WHILE_SYM 304
#define DO_SYM 305
#define FOR_SYM 306
#define CONTINUE_SYM 307
#define BREAK_SYM 308
#define RETURN_SYM 309
#define SWITCH_SYM 310
#define CASE_SYM 311
#define DEFAULT_SYM 312




/* Copy the first part of user declarations.  */
#line 1 "yacc.y"

#include <stdio.h>
// #include <type.h>

#define YYSTYPE_IS_DECLARED 1
typedef long YYSTYPE;
extern int line_no, syntax_err;
extern char *yytext;
extern A_NODE *root;
extern A_ID *current_id;
extern int current_level;
extern A_TYPE *int_type;

void yyerror(char *s);

extern FILE *yyin;
extern int yylex();



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 240 "y.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  29
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   437

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  58
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  80
/* YYNRULES -- Number of rules.  */
#define YYNRULES  176
/* YYNRULES -- Number of states.  */
#define YYNSTATES  293

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   312

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     7,    10,    12,    14,    15,    20,
      21,    25,    26,    28,    30,    33,    37,    39,    41,    44,
      47,    49,    51,    53,    54,    56,    58,    62,    64,    68,
      70,    74,    76,    80,    82,    84,    86,    87,    88,    96,
      97,    98,   105,   108,   110,   112,   114,   117,   121,   123,
     127,   129,   130,   137,   138,   144,   147,   149,   153,   155,
     156,   161,   164,   166,   168,   171,   173,   177,   182,   183,
     189,   190,   192,   194,   198,   200,   204,   207,   210,   211,
     213,   215,   217,   220,   224,   228,   233,   237,   242,   243,
     245,   247,   250,   252,   254,   256,   258,   260,   262,   267,
     271,   272,   278,   280,   283,   289,   297,   303,   309,   317,
     323,   329,   330,   332,   336,   339,   342,   343,   345,   347,
     351,   352,   354,   356,   358,   360,   362,   366,   368,   370,
     374,   376,   380,   382,   384,   386,   388,   392,   396,   398,
     402,   406,   410,   414,   416,   418,   422,   426,   428,   432,
     436,   440,   442,   447,   449,   452,   455,   458,   461,   464,
     467,   470,   473,   478,   480,   485,   490,   494,   498,   501,
     504,   506,   508,   510,   512,   514,   518
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
      59,     0,    -1,    60,    -1,    61,    -1,    60,    61,    -1,
      62,    -1,    67,    -1,    -1,    68,    92,    63,   107,    -1,
      -1,    92,    64,   107,    -1,    -1,    66,    -1,    67,    -1,
      66,    67,    -1,    68,    70,    19,    -1,    75,    -1,    69,
      -1,    75,    68,    -1,    69,    68,    -1,    41,    -1,    42,
      -1,    43,    -1,    -1,    71,    -1,    72,    -1,    71,    34,
      72,    -1,    92,    -1,    92,    39,    73,    -1,   118,    -1,
      31,    74,    32,    -1,    73,    -1,    74,    34,    73,    -1,
      76,    -1,    86,    -1,     6,    -1,    -1,    -1,    81,     5,
      77,    31,    78,    82,    32,    -1,    -1,    -1,    81,    79,
      31,    80,    82,    32,    -1,    81,     5,    -1,    44,    -1,
      47,    -1,    83,    -1,    82,    83,    -1,    75,    84,    19,
      -1,    85,    -1,    84,    34,    85,    -1,    92,    -1,    -1,
      45,     5,    87,    31,    89,    32,    -1,    -1,    45,    88,
      31,    89,    32,    -1,    45,     5,    -1,    90,    -1,    89,
      34,    90,    -1,     5,    -1,    -1,     5,    91,    39,   119,
      -1,    93,    94,    -1,    94,    -1,    36,    -1,    36,    93,
      -1,     5,    -1,    27,    92,    28,    -1,    94,    29,   117,
      30,    -1,    -1,    94,    27,    95,    96,    28,    -1,    -1,
      97,    -1,    98,    -1,    98,    34,    26,    -1,    99,    -1,
      98,    34,    99,    -1,    68,    92,    -1,    68,   100,    -1,
      -1,   101,    -1,   102,    -1,    93,    -1,    93,   102,    -1,
      27,   101,    28,    -1,    29,   117,    30,    -1,   102,    29,
     117,    30,    -1,    27,    96,    28,    -1,   102,    27,    96,
      28,    -1,    -1,   104,    -1,   105,    -1,   104,   105,    -1,
     106,    -1,   107,    -1,   109,    -1,   110,    -1,   111,    -1,
     114,    -1,    56,   118,    40,   105,    -1,    57,    40,   105,
      -1,    -1,    31,   108,    65,   103,    32,    -1,    19,    -1,
     119,    19,    -1,    48,    27,   119,    28,   105,    -1,    48,
      27,   119,    28,   105,     4,   105,    -1,    55,    27,   119,
      28,   105,    -1,    49,    27,   119,    28,   105,    -1,    50,
     105,    49,    27,   119,    28,    19,    -1,    51,    27,   112,
      28,   105,    -1,   113,    19,   113,    19,   113,    -1,    -1,
     119,    -1,    54,   113,    19,    -1,    52,    19,    -1,    53,
      19,    -1,    -1,   116,    -1,   121,    -1,   116,    34,   121,
      -1,    -1,   118,    -1,   119,    -1,   120,    -1,   121,    -1,
     122,    -1,   134,    39,   121,    -1,   123,    -1,   124,    -1,
     123,    16,   124,    -1,   125,    -1,   124,    17,   125,    -1,
     126,    -1,   127,    -1,   128,    -1,   129,    -1,   128,    24,
     129,    -1,   128,    25,   129,    -1,   130,    -1,   129,    20,
     130,    -1,   129,    21,   130,    -1,   129,    22,   130,    -1,
     129,    23,   130,    -1,   131,    -1,   132,    -1,   131,    11,
     132,    -1,   131,    12,   132,    -1,   133,    -1,   132,    36,
     133,    -1,   132,    37,   133,    -1,   132,    38,   133,    -1,
     134,    -1,    27,   137,    28,   133,    -1,   135,    -1,    13,
     134,    -1,    14,   134,    -1,    15,   133,    -1,    36,   133,
      -1,    35,   133,    -1,    12,   133,    -1,    11,   133,    -1,
      46,   134,    -1,    46,    27,   137,    28,    -1,   136,    -1,
     135,    29,   119,    30,    -1,   135,    27,   115,    28,    -1,
     135,    33,     5,    -1,   135,    18,     5,    -1,   135,    13,
      -1,   135,    14,    -1,     5,    -1,     8,    -1,     7,    -1,
       9,    -1,    10,    -1,    27,   119,    28,    -1,    68,   100,
      -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    41,    41,    44,    45,    48,    49,    52,    52,    54,
      54,    58,    59,    62,    63,    66,    69,    70,    71,    72,
      75,    76,    77,    80,    81,    84,    85,    88,    89,    92,
      93,    96,    97,   100,   101,   102,   105,   106,   105,   108,
     108,   108,   110,   113,   114,   117,   118,   121,   124,   125,
     128,   131,   131,   133,   133,   135,   138,   139,   142,   143,
     143,   147,   148,   151,   152,   155,   156,   157,   159,   159,
     164,   165,   168,   169,   172,   173,   176,   177,   180,   181,
     184,   185,   186,   189,   190,   191,   192,   193,   196,   197,
     200,   201,   204,   205,   206,   207,   208,   209,   212,   213,
     216,   216,   221,   222,   225,   226,   227,   230,   231,   232,
     235,   238,   239,   242,   243,   244,   247,   248,   251,   252,
     255,   256,   259,   262,   265,   268,   269,   272,   275,   276,
     279,   280,   283,   286,   289,   292,   293,   294,   297,   298,
     299,   300,   301,   304,   307,   308,   309,   312,   313,   314,
     315,   318,   319,   322,   323,   324,   325,   326,   327,   328,
     329,   330,   331,   334,   335,   336,   337,   338,   339,   340,
     343,   344,   345,   346,   347,   348,   351
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IF_PREC", "ELSE_SYM", "IDENTIFIER",
  "TYPE_IDENTIFIER", "FLOAT_CONSTANT", "INTEGER_CONSTANT",
  "CHARACTER_CONSTANT", "STRING_LITERAL", "PLUS", "MINUS", "PLUSPLUS",
  "MINUSMINUS", "AMP", "BARBAR", "AMPAMP", "ARROW", "SEMICOLON", "LSS",
  "GTR", "LEQ", "GEQ", "EQL", "NEQ", "DOTDOTDOT", "LP", "RP", "LB", "RB",
  "LR", "RR", "PERIOD", "COMMA", "EXCL", "STAR", "SLASH", "PERCENT",
  "ASSIGN", "COLON", "AUTO_SYM", "STATIC_SYM", "TYPEDEF_SYM", "STRUCT_SYM",
  "ENUM_SYM", "SIZEOF_SYM", "UNION_SYM", "IF_SYM", "WHILE_SYM", "DO_SYM",
  "FOR_SYM", "CONTINUE_SYM", "BREAK_SYM", "RETURN_SYM", "SWITCH_SYM",
  "CASE_SYM", "DEFAULT_SYM", "$accept", "program", "translation_unit",
  "external_declaration", "function_definition", "@1", "@2",
  "declaration_list_opt", "declaration_list", "declaration",
  "declaration_specifiers", "storage_class_specifier",
  "init_declarator_list_opt", "init_declarator_list", "init_declarator",
  "initializer", "initializer_list", "type_specifier",
  "struct_type_specifier", "@3", "@4", "@5", "@6", "struct_or_union",
  "struct_declaration_list", "struct_declaration",
  "struct_declarator_list", "struct_declarator", "enum_type_specifier",
  "@7", "@8", "enumerator_list", "enumerator", "@9", "declarator",
  "pointer", "direct_declarator", "@10", "parameter_type_list_opt",
  "parameter_type_list", "parameter_list", "parameter_declaration",
  "abstract_declarator_opt", "abstract_declarator",
  "direct_abstract_declarator", "statement_list_opt", "statement_list",
  "statement", "labeled_statement", "compound_statement", "@11",
  "expression_statement", "selection_statement", "iteration_statement",
  "for_expression", "expression_opt", "jump_statement",
  "arg_expression_list_opt", "arg_expression_list",
  "constant_expression_opt", "constant_expression", "expression",
  "comma_expression", "assignment_expression", "conditional_expression",
  "logical_or_expression", "logical_and_expression",
  "bitwise_or_expression", "bitwise_xor_expression",
  "bitwise_and_expression", "equality_expression", "relational_expression",
  "shift_expression", "additive_expression", "multiplicative_expression",
  "cast_expression", "unary_expression", "postfix_expression",
  "primary_expression", "type_name", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    58,    59,    60,    60,    61,    61,    63,    62,    64,
      62,    65,    65,    66,    66,    67,    68,    68,    68,    68,
      69,    69,    69,    70,    70,    71,    71,    72,    72,    73,
      73,    74,    74,    75,    75,    75,    77,    78,    76,    79,
      80,    76,    76,    81,    81,    82,    82,    83,    84,    84,
      85,    87,    86,    88,    86,    86,    89,    89,    90,    91,
      90,    92,    92,    93,    93,    94,    94,    94,    95,    94,
      96,    96,    97,    97,    98,    98,    99,    99,   100,   100,
     101,   101,   101,   102,   102,   102,   102,   102,   103,   103,
     104,   104,   105,   105,   105,   105,   105,   105,   106,   106,
     108,   107,   109,   109,   110,   110,   110,   111,   111,   111,
     112,   113,   113,   114,   114,   114,   115,   115,   116,   116,
     117,   117,   118,   119,   120,   121,   121,   122,   123,   123,
     124,   124,   125,   126,   127,   128,   128,   128,   129,   129,
     129,   129,   129,   130,   131,   131,   131,   132,   132,   132,
     132,   133,   133,   134,   134,   134,   134,   134,   134,   134,
     134,   134,   134,   135,   135,   135,   135,   135,   135,   135,
     136,   136,   136,   136,   136,   136,   137
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     2,     1,     1,     0,     4,     0,
       3,     0,     1,     1,     2,     3,     1,     1,     2,     2,
       1,     1,     1,     0,     1,     1,     3,     1,     3,     1,
       3,     1,     3,     1,     1,     1,     0,     0,     7,     0,
       0,     6,     2,     1,     1,     1,     2,     3,     1,     3,
       1,     0,     6,     0,     5,     2,     1,     3,     1,     0,
       4,     2,     1,     1,     2,     1,     3,     4,     0,     5,
       0,     1,     1,     3,     1,     3,     2,     2,     0,     1,
       1,     1,     2,     3,     3,     4,     3,     4,     0,     1,
       1,     2,     1,     1,     1,     1,     1,     1,     4,     3,
       0,     5,     1,     2,     5,     7,     5,     5,     7,     5,
       5,     0,     1,     3,     2,     2,     0,     1,     1,     3,
       0,     1,     1,     1,     1,     1,     3,     1,     1,     3,
       1,     3,     1,     1,     1,     1,     3,     3,     1,     3,
       3,     3,     3,     1,     1,     3,     3,     1,     3,     3,
       3,     1,     4,     1,     2,     2,     2,     2,     2,     2,
       2,     2,     4,     1,     4,     4,     3,     3,     2,     2,
       1,     1,     1,     1,     1,     3,     2
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    65,    35,     0,    63,    20,    21,    22,    43,    53,
      44,     0,     2,     3,     5,     6,    23,    17,    16,    33,
      39,    34,     9,     0,    62,     0,    64,    55,     0,     1,
       4,     0,    24,    25,    27,    19,    18,    42,     0,     0,
      61,    68,   120,    66,     0,     0,    15,     0,     0,     0,
       0,    40,   100,    10,    70,   170,   172,   171,   173,   174,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     121,   122,   123,   124,   125,   127,   128,   130,   132,   133,
     134,   135,   138,   143,   144,   147,   151,   153,   163,     0,
      58,     0,    56,    26,    27,     0,    28,    29,     8,    37,
       0,    11,    78,     0,    71,    72,    74,   160,   151,   159,
       0,   154,   155,   156,    78,     0,     0,   158,   157,     0,
     161,    67,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   168,   169,     0,   116,
       0,     0,     0,     0,    54,     0,    31,     0,     0,     0,
       0,    45,    88,    12,    13,    23,    70,   120,    76,    81,
      77,    79,    80,    69,     0,    70,    81,   176,   175,     0,
       0,   129,   131,   136,   137,   139,   140,   141,   142,   145,
     146,   148,   149,   150,   126,   167,     0,   117,   118,     0,
     166,    52,     0,    57,    30,     0,     0,     0,    48,    50,
      41,    46,   102,     0,     0,     0,     0,     0,     0,   111,
       0,     0,     0,     0,    89,    90,    92,    93,    94,    95,
      96,    97,     0,    14,     0,     0,     0,    82,    70,   120,
      73,    75,   152,   162,   165,     0,   164,    60,    32,    38,
      47,     0,     0,     0,     0,   111,   114,   115,     0,   112,
       0,     0,     0,   101,    91,   103,    86,    83,    84,     0,
       0,   119,    49,     0,     0,     0,     0,     0,   113,     0,
       0,    99,    87,    85,     0,     0,     0,     0,   111,     0,
      98,   104,   107,     0,   109,     0,   106,     0,     0,   111,
     105,   108,   110
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    11,    12,    13,    14,    49,    39,   152,   153,    15,
     102,    17,    31,    32,    33,    96,   147,    18,    19,    50,
     148,    38,   100,    20,   150,   151,   197,   198,    21,    44,
      28,    91,    92,   143,    22,    23,    24,    54,   224,   104,
     105,   106,   160,   161,   162,   213,   214,   215,   216,   217,
     101,   218,   219,   220,   266,   248,   221,   186,   187,    69,
      70,   222,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    86,    87,    88,   116
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -220
static const yytype_int16 yypact[] =
{
      44,  -220,  -220,    24,    -5,  -220,  -220,  -220,  -220,    56,
    -220,    65,    44,  -220,  -220,  -220,    24,   237,   237,  -220,
      73,  -220,  -220,    17,     1,    72,  -220,    76,    97,  -220,
    -220,   102,   112,  -220,    23,  -220,  -220,   120,   127,   132,
       1,  -220,    28,  -220,   143,   217,  -220,    24,   311,   132,
     198,  -220,  -220,  -220,   237,  -220,  -220,  -220,  -220,  -220,
      28,    28,   351,   351,    28,   171,    28,    28,   391,   200,
    -220,  -220,  -220,  -220,  -220,   221,   224,  -220,  -220,  -220,
      86,   150,  -220,   113,   173,  -220,   199,   206,  -220,   217,
     203,    47,  -220,  -220,   205,   311,  -220,  -220,  -220,  -220,
     285,   237,    40,   218,  -220,   211,  -220,  -220,  -220,  -220,
      28,  -220,  -220,  -220,   130,   220,   236,  -220,  -220,   171,
    -220,  -220,    28,    28,    28,    28,    28,    28,    28,    28,
      28,    28,    28,    28,    28,    28,  -220,  -220,   269,    28,
      28,   271,    60,   244,  -220,   217,  -220,   106,   285,    24,
     117,  -220,   258,   237,  -220,    24,   100,    28,  -220,    70,
    -220,  -220,   170,  -220,    90,   347,   194,  -220,  -220,    28,
     264,   224,  -220,   150,   150,  -220,  -220,  -220,  -220,   173,
     173,  -220,  -220,  -220,  -220,  -220,   267,   252,  -220,   266,
    -220,  -220,    28,  -220,  -220,   311,   243,    13,  -220,  -220,
    -220,  -220,  -220,   272,   273,   258,   274,   279,   283,    28,
     276,    28,   265,   295,   258,  -220,  -220,  -220,  -220,  -220,
    -220,  -220,   298,  -220,   300,   303,   304,   170,   237,    28,
    -220,  -220,  -220,  -220,  -220,    28,  -220,  -220,  -220,  -220,
    -220,    24,    28,    28,   284,    28,  -220,  -220,   316,  -220,
      28,   296,   258,  -220,  -220,  -220,  -220,  -220,  -220,   309,
     310,  -220,  -220,   313,   315,   312,   317,   325,  -220,   320,
     258,  -220,  -220,  -220,   258,   258,    28,   258,    28,   258,
    -220,   345,  -220,   322,  -220,   332,  -220,   258,   333,    28,
    -220,  -220,  -220
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -220,  -220,  -220,   342,  -220,  -220,  -220,  -220,  -220,   -80,
       7,  -220,  -220,  -220,   308,   -83,  -220,   -92,  -220,  -220,
    -220,  -220,  -220,  -220,   219,  -130,  -220,   129,  -220,  -220,
    -220,   280,   223,  -220,    -1,     0,   -20,  -220,   -53,  -220,
    -220,   207,   259,    37,  -132,  -220,  -220,  -157,  -220,    54,
    -220,  -220,  -220,  -220,  -220,  -219,  -220,  -220,  -220,  -139,
     -43,   -42,  -220,  -126,  -220,  -220,   250,   254,  -220,  -220,
    -220,    80,    99,  -220,   101,   -50,   128,  -220,  -220,   256
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -60
static const yytype_int16 yytable[] =
{
      71,   103,    25,    40,    26,    97,    71,    16,   149,   184,
     107,   109,   146,   188,   113,    34,   117,   118,   226,    16,
     201,   154,     1,   115,    35,    36,   267,   227,    41,     1,
      42,     4,   240,    55,   227,    56,    57,    58,    59,    60,
      61,    62,    63,    64,     3,     1,    94,   241,   244,     1,
       2,     3,    97,    71,    -7,    65,   149,   254,   149,   285,
       4,    27,    48,    66,    67,    29,   201,   156,   115,   157,
     292,     3,   114,   223,    68,     1,     4,   115,    37,   144,
       4,   145,   181,   182,   183,     5,     6,     7,     8,     9,
     260,    10,   191,    53,   145,   271,     2,   156,   189,   157,
      43,   158,   159,    98,   149,     1,     2,   -51,   155,   261,
     124,   125,   238,   280,   166,    71,   230,   281,   282,   232,
     284,    46,   286,     2,   130,   131,   114,   156,    45,   157,
     290,     5,     6,     7,     8,     9,     4,    10,   194,    40,
     195,     5,     6,     7,     8,     9,    47,    10,   199,   200,
     237,   -36,    97,    71,    94,    25,   159,   165,    51,   157,
     155,     8,     9,    52,    10,   166,     4,   249,   251,    71,
     126,   127,   128,   129,    89,   259,    55,     2,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    71,   108,   108,
     111,   112,   108,   225,   108,   108,   120,   228,    65,   229,
     263,   264,   225,   249,   173,   174,    66,    67,   269,   132,
     133,   134,     5,     6,     7,     8,     9,    68,    10,   136,
     137,   165,    90,   157,   138,   175,   176,   177,   178,    99,
     121,   179,   180,   139,   283,   140,   249,   122,   135,   141,
     199,   123,   -59,     2,    48,   164,   163,   249,   168,     2,
     108,   108,   108,   108,   108,   108,   108,   108,   108,   108,
     108,   108,   108,    55,   169,    56,    57,    58,    59,    60,
      61,    62,    63,    64,   185,   239,   190,   202,     5,     6,
       7,     8,     9,   192,    10,    65,   235,     8,     9,    52,
      10,     2,   233,    66,    67,   234,   236,   108,   246,   242,
     243,   245,   247,   250,    68,   252,   203,   204,   205,   206,
     207,   208,   209,   210,   211,   212,    55,   255,    56,    57,
      58,    59,    60,    61,    62,    63,    64,   253,   256,     8,
       9,   257,    10,   265,   258,   268,   270,   272,    65,   276,
     273,   274,    95,   275,   278,   277,    66,    67,   279,   287,
     288,   289,   291,     2,    30,    93,    55,    68,    56,    57,
      58,    59,    60,    61,    62,    63,    64,   196,   193,   142,
     262,   231,   171,   167,   165,   170,   157,   172,   110,     0,
       0,     0,     0,     4,     0,     0,    66,    67,     5,     6,
       7,     8,     9,     0,    10,     0,    55,    68,    56,    57,
      58,    59,    60,    61,    62,    63,    64,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   119,     0,
       0,     0,     0,     0,     0,     0,    66,    67,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    68
};

static const yytype_int16 yycheck[] =
{
      42,    54,     3,    23,     4,    48,    48,     0,   100,   135,
      60,    61,    95,   139,    64,    16,    66,    67,   157,    12,
     150,   101,     5,    65,    17,    18,   245,   159,    27,     5,
      29,    36,    19,     5,   166,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    27,     5,    47,    34,   205,     5,
       6,    27,    95,    95,    31,    27,   148,   214,   150,   278,
      36,     5,    39,    35,    36,     0,   196,    27,   110,    29,
     289,    27,    65,   153,    46,     5,    36,   119,     5,    32,
      36,    34,   132,   133,   134,    41,    42,    43,    44,    45,
     229,    47,    32,    39,    34,   252,     6,    27,   140,    29,
      28,   102,   102,    49,   196,     5,     6,    31,   101,   235,
      24,    25,   195,   270,   114,   157,    26,   274,   275,   169,
     277,    19,   279,     6,    11,    12,   119,    27,    31,    29,
     287,    41,    42,    43,    44,    45,    36,    47,    32,   159,
      34,    41,    42,    43,    44,    45,    34,    47,   149,    32,
     192,    31,   195,   195,   155,   156,   156,    27,    31,    29,
     153,    44,    45,    31,    47,   165,    36,   209,   211,   211,
      20,    21,    22,    23,    31,   228,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,   229,    60,    61,
      62,    63,    64,   156,    66,    67,    68,    27,    27,    29,
     242,   243,   165,   245,   124,   125,    35,    36,   250,    36,
      37,    38,    41,    42,    43,    44,    45,    46,    47,    13,
      14,    27,     5,    29,    18,   126,   127,   128,   129,    31,
      30,   130,   131,    27,   276,    29,   278,    16,    39,    33,
     241,    17,    39,     6,    39,    34,    28,   289,    28,     6,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   134,     5,    28,     7,     8,     9,    10,    11,
      12,    13,    14,    15,     5,    32,     5,    19,    41,    42,
      43,    44,    45,    39,    47,    27,    34,    44,    45,    31,
      47,     6,    28,    35,    36,    28,    30,   169,    19,    27,
      27,    27,    19,    27,    46,    40,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,     5,    19,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    32,    28,    44,
      45,    28,    47,    49,    30,    19,    40,    28,    27,    27,
      30,    28,    31,    28,    19,    28,    35,    36,    28,     4,
      28,    19,    19,     6,    12,    47,     5,    46,     7,     8,
       9,    10,    11,    12,    13,    14,    15,   148,   145,    89,
     241,   164,   122,   114,    27,   119,    29,   123,    27,    -1,
      -1,    -1,    -1,    36,    -1,    -1,    35,    36,    41,    42,
      43,    44,    45,    -1,    47,    -1,     5,    46,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    27,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    35,    36,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    46
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     5,     6,    27,    36,    41,    42,    43,    44,    45,
      47,    59,    60,    61,    62,    67,    68,    69,    75,    76,
      81,    86,    92,    93,    94,    92,    93,     5,    88,     0,
      61,    70,    71,    72,    92,    68,    68,     5,    79,    64,
      94,    27,    29,    28,    87,    31,    19,    34,    39,    63,
      77,    31,    31,   107,    95,     5,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    27,    35,    36,    46,   117,
     118,   119,   120,   121,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   134,   135,   136,    31,
       5,    89,    90,    72,    92,    31,    73,   118,   107,    31,
      80,   108,    68,    96,    97,    98,    99,   133,   134,   133,
      27,   134,   134,   133,    68,   119,   137,   133,   133,    27,
     134,    30,    16,    17,    24,    25,    20,    21,    22,    23,
      11,    12,    36,    37,    38,    39,    13,    14,    18,    27,
      29,    33,    89,    91,    32,    34,    73,    74,    78,    75,
      82,    83,    65,    66,    67,    68,    27,    29,    92,    93,
     100,   101,   102,    28,    34,    27,    93,   100,    28,    28,
     137,   124,   125,   129,   129,   130,   130,   130,   130,   132,
     132,   133,   133,   133,   121,     5,   115,   116,   121,   119,
       5,    32,    39,    90,    32,    34,    82,    84,    85,    92,
      32,    83,    19,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,   103,   104,   105,   106,   107,   109,   110,
     111,   114,   119,    67,    96,   101,   117,   102,    27,    29,
      26,    99,   133,    28,    28,    34,    30,   119,    73,    32,
      19,    34,    27,    27,   105,    27,    19,    19,   113,   119,
      27,   118,    40,    32,   105,    19,    28,    28,    30,    96,
     117,   121,    85,   119,   119,    49,   112,   113,    19,   119,
      40,   105,    28,    30,    28,    28,    27,    28,    19,    28,
     105,   105,   105,   119,   105,   113,   105,     4,    28,    19,
     105,    19,   113
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;


	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 41 "yacc.y"
    {root=makeNode(N_PROGRAM,NIL,(yyvsp[(1) - (1)]),NIL); checkForwardReference();}
    break;

  case 3:
#line 44 "yacc.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 4:
#line 45 "yacc.y"
    {(yyval)=linkDeclaratorList((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]));}
    break;

  case 5:
#line 48 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 6:
#line 49 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 7:
#line 52 "yacc.y"
    {(yyval)=setFunctionDeclaratorSpecifier((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)]));}
    break;

  case 8:
#line 53 "yacc.y"
    {(yyval)=setFunctionDeclaratorBody((yyvsp[(3) - (4)]),(yyvsp[(4) - (4)]));}
    break;

  case 9:
#line 54 "yacc.y"
    {(yyval)=setFunctionDeclaratorSpecifier((yyvsp[(1) - (1)]),makeSpecifier(int_type,0));}
    break;

  case 10:
#line 55 "yacc.y"
    {(yyval)=setFunctionDeclaratorBody((yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]));}
    break;

  case 11:
#line 58 "yacc.y"
    {(yyval)=NIL;}
    break;

  case 12:
#line 59 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 13:
#line 62 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 14:
#line 63 "yacc.y"
    {(yyval)=linkDeclaratorList((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]));}
    break;

  case 15:
#line 66 "yacc.y"
    {(yyval)=setDeclaratorListSpecifier((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]));}
    break;

  case 16:
#line 69 "yacc.y"
    {(yyval)=makeSpecifier((yyvsp[(1) - (1)]),0);}
    break;

  case 17:
#line 70 "yacc.y"
    {(yyval)=makeSpecifier(0,(yyvsp[(1) - (1)]));}
    break;

  case 18:
#line 71 "yacc.y"
    {(yyval)=updateSpecifier((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)]),0);}
    break;

  case 19:
#line 72 "yacc.y"
    {(yyval)=updateSpecifier((yyvsp[(2) - (2)]),0,(yyvsp[(1) - (2)]));}
    break;

  case 20:
#line 75 "yacc.y"
    {(yyval)=S_AUTO;}
    break;

  case 21:
#line 76 "yacc.y"
    {(yyval)=S_STATIC;}
    break;

  case 22:
#line 77 "yacc.y"
    {(yyval)=S_TYPEDEF;}
    break;

  case 23:
#line 80 "yacc.y"
    {(yyval)=makeDummyIdentifier();}
    break;

  case 24:
#line 81 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 25:
#line 84 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 26:
#line 85 "yacc.y"
    {(yyval)=linkDeclaratorList((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));}
    break;

  case 27:
#line 88 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 28:
#line 89 "yacc.y"
    {(yyval)=setDeclaratorInit((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));}
    break;

  case 29:
#line 92 "yacc.y"
    {(yyval)=makeNode(N_INIT_LIST_ONE,NIL,(yyvsp[(1) - (1)]),NIL);}
    break;

  case 30:
#line 93 "yacc.y"
    {(yyval)=(yyvsp[(2) - (3)]);}
    break;

  case 31:
#line 96 "yacc.y"
    {(yyval)=makeNode(N_INIT_LIST,(yyvsp[(1) - (1)]),NIL,makeNode(N_INIT_LIST_NIL,NIL,NIL,NIL));}
    break;

  case 32:
#line 97 "yacc.y"
    {(yyval)=makeNodeList(N_INIT_LIST,(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));}
    break;

  case 33:
#line 100 "yacc.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 34:
#line 101 "yacc.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 35:
#line 102 "yacc.y"
    {(yyval) = (yyvsp[(1) - (1)]);}
    break;

  case 36:
#line 105 "yacc.y"
    {(yyval)=setTypeStructOrEnumIdentifier((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]),ID_STRUCT);}
    break;

  case 37:
#line 106 "yacc.y"
    { (yyval)=current_id;current_level++;}
    break;

  case 38:
#line 107 "yacc.y"
    {checkForwardReference();(yyval)=setTypeField((yyvsp[(3) - (7)]),(yyvsp[(6) - (7)]));current_level--; current_id=(yyvsp[(5) - (7)]);}
    break;

  case 39:
#line 108 "yacc.y"
    {(yyval)=makeType((yyvsp[(1) - (1)]));}
    break;

  case 40:
#line 108 "yacc.y"
    {(yyval)=current_id;current_level++;}
    break;

  case 41:
#line 109 "yacc.y"
    {checkForwardReference();(yyval)=setTypeField((yyvsp[(2) - (6)]),(yyvsp[(5) - (6)])); current_level--;current_id=(yyvsp[(4) - (6)]);}
    break;

  case 42:
#line 110 "yacc.y"
    {(yyval)=getTypeOfStructOrEnumRefIdentifier((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]),ID_STRUCT);}
    break;

  case 43:
#line 113 "yacc.y"
    {(yyval)=T_STRUCT;}
    break;

  case 44:
#line 114 "yacc.y"
    {(yyval)=T_UNION;}
    break;

  case 45:
#line 117 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 46:
#line 118 "yacc.y"
    {(yyval)=linkDeclaratorList((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]));}
    break;

  case 47:
#line 121 "yacc.y"
    {(yyval)=setStructDeclaratorListSpecifier((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]));}
    break;

  case 48:
#line 124 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 49:
#line 125 "yacc.y"
    {(yyval)=linkDeclaratorList((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));}
    break;

  case 50:
#line 128 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 51:
#line 131 "yacc.y"
    {(yyval)=setTypeStructOrEnumIdentifier(T_ENUM,(yyvsp[(2) - (2)]),ID_ENUM);}
    break;

  case 52:
#line 132 "yacc.y"
    {(yyval)=setTypeField((yyvsp[(3) - (6)]),(yyvsp[(5) - (6)]));}
    break;

  case 53:
#line 133 "yacc.y"
    {(yyval)=makeType(T_ENUM);}
    break;

  case 54:
#line 134 "yacc.y"
    {(yyval)=setTypeField((yyvsp[(2) - (5)]),(yyvsp[(4) - (5)]));}
    break;

  case 55:
#line 135 "yacc.y"
    {(yyval)=getTypeOfStructOrEnumRefIdentifier(T_ENUM,(yyvsp[(2) - (2)]),ID_ENUM);}
    break;

  case 56:
#line 138 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 57:
#line 139 "yacc.y"
    {(yyval)=linkDeclaratorList((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));}
    break;

  case 58:
#line 142 "yacc.y"
    {(yyval)=setDeclaratorKind(makeIdentifier((yyvsp[(1) - (1)])),ID_ENUM_LITERAL);}
    break;

  case 59:
#line 143 "yacc.y"
    {(yyval)=setDeclaratorKind(makeIdentifier((yyvsp[(1) - (1)])),ID_ENUM_LITERAL);}
    break;

  case 60:
#line 144 "yacc.y"
    {(yyval)=setDeclaratorInit((yyvsp[(2) - (4)]),(yyvsp[(4) - (4)]));}
    break;

  case 61:
#line 147 "yacc.y"
    {(yyval)=setDeclaratorElementType((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)]));}
    break;

  case 62:
#line 148 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 63:
#line 151 "yacc.y"
    {(yyval)=makeType(T_POINTER);}
    break;

  case 64:
#line 152 "yacc.y"
    {(yyval)=setTypeElementType((yyvsp[(2) - (2)]),makeType(T_POINTER));}
    break;

  case 65:
#line 155 "yacc.y"
    {(yyval)=makeIdentifier((yyvsp[(1) - (1)]));}
    break;

  case 66:
#line 156 "yacc.y"
    {(yyval)=(yyvsp[(2) - (3)]);}
    break;

  case 67:
#line 158 "yacc.y"
    {(yyval)=setDeclaratorElementType((yyvsp[(1) - (4)]),setTypeExpr(makeType(T_ARRAY),(yyvsp[(3) - (4)])));}
    break;

  case 68:
#line 159 "yacc.y"
    {(yyval)=current_id;current_level++;}
    break;

  case 69:
#line 161 "yacc.y"
    {checkForwardReference();current_id=(yyvsp[(3) - (5)]);current_level--; (yyval)=setDeclaratorElementType((yyvsp[(1) - (5)]),setTypeField(makeType(T_FUNC),(yyvsp[(4) - (5)])));}
    break;

  case 70:
#line 164 "yacc.y"
    {(yyval)=NIL;}
    break;

  case 71:
#line 165 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 72:
#line 168 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 73:
#line 169 "yacc.y"
    {(yyval)=linkDeclaratorList((yyvsp[(1) - (3)]),setDeclaratorKind(makeDummyIdentifier(),ID_PARM));}
    break;

  case 74:
#line 172 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 75:
#line 173 "yacc.y"
    {(yyval)=linkDeclaratorList((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));}
    break;

  case 76:
#line 176 "yacc.y"
    {(yyval)=setParameterDeclaratorSpecifier((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)]));}
    break;

  case 77:
#line 177 "yacc.y"
    {(yyval)=setParameterDeclaratorSpecifier(setDeclaratorType(makeDummyIdentifier(),(yyvsp[(2) - (2)])),(yyvsp[(1) - (2)]));}
    break;

  case 78:
#line 180 "yacc.y"
    {(yyval)=NIL;}
    break;

  case 79:
#line 181 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 80:
#line 184 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 81:
#line 185 "yacc.y"
    {(yyval)=makeType(T_POINTER);}
    break;

  case 82:
#line 186 "yacc.y"
    {(yyval)=setTypeElementType((yyvsp[(2) - (2)]),makeType(T_POINTER));}
    break;

  case 83:
#line 189 "yacc.y"
    {(yyval)=(yyvsp[(2) - (3)]);}
    break;

  case 84:
#line 190 "yacc.y"
    {(yyval)=setTypeExpr(makeType(T_ARRAY),(yyvsp[(2) - (3)]));}
    break;

  case 85:
#line 191 "yacc.y"
    {(yyval)=setTypeElementType((yyvsp[(1) - (4)]),setTypeExpr(makeType(T_ARRAY),(yyvsp[(3) - (4)])));}
    break;

  case 86:
#line 192 "yacc.y"
    {(yyval)=setTypeExpr(makeType(T_FUNC),(yyvsp[(2) - (3)]));}
    break;

  case 87:
#line 193 "yacc.y"
    {(yyval)=setTypeElementType((yyvsp[(1) - (4)]),setTypeExpr(makeType(T_FUNC),(yyvsp[(3) - (4)])));}
    break;

  case 88:
#line 196 "yacc.y"
    {(yyval)=makeNode(N_STMT_LIST_NIL,NIL,NIL,NIL);}
    break;

  case 89:
#line 197 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 90:
#line 200 "yacc.y"
    {(yyval)=makeNode(N_STMT_LIST,(yyvsp[(1) - (1)]),NIL,makeNode(N_STMT_LIST_NIL,NIL,NIL,NIL));}
    break;

  case 91:
#line 201 "yacc.y"
    {(yyval)=makeNodeList(N_STMT_LIST,(yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]));}
    break;

  case 92:
#line 204 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 93:
#line 205 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 94:
#line 206 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 95:
#line 207 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 96:
#line 208 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 97:
#line 209 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 98:
#line 212 "yacc.y"
    {(yyval)=makeNode(N_STMT_LABEL_CASE, (yyvsp[(2) - (4)]),NIL,(yyvsp[(4) - (4)]));}
    break;

  case 99:
#line 213 "yacc.y"
    {(yyval)=makeNode(N_STMT_LABEL_DEFAULT,NIL,(yyvsp[(3) - (3)]),NIL);}
    break;

  case 100:
#line 216 "yacc.y"
    {(yyval)=current_id;current_level++;}
    break;

  case 101:
#line 218 "yacc.y"
    {checkForwardReference(); (yyval)=makeNode(N_STMT_COMPOUND,(yyvsp[(3) - (5)]),NIL,(yyvsp[(4) - (5)])); current_id=(yyvsp[(2) - (5)]); current_level--;}
    break;

  case 102:
#line 221 "yacc.y"
    {(yyval)=makeNode(N_STMT_EMPTY,NIL,NIL,NIL);}
    break;

  case 103:
#line 222 "yacc.y"
    {(yyval)=makeNode(N_STMT_EXPRESSION,NIL,(yyvsp[(1) - (2)]),NIL);}
    break;

  case 104:
#line 225 "yacc.y"
    {(yyval)=makeNode(N_STMT_IF,(yyvsp[(3) - (5)]),NIL,(yyvsp[(5) - (5)]));}
    break;

  case 105:
#line 226 "yacc.y"
    {(yyval)=makeNode(N_STMT_IF_ELSE,(yyvsp[(3) - (7)]),(yyvsp[(5) - (7)]),(yyvsp[(7) - (7)]));}
    break;

  case 106:
#line 227 "yacc.y"
    {(yyval)=makeNode(N_STMT_SWITCH,(yyvsp[(3) - (5)]),NIL,(yyvsp[(5) - (5)]));}
    break;

  case 107:
#line 230 "yacc.y"
    {(yyval)=makeNode(N_STMT_WHILE,(yyvsp[(3) - (5)]),NIL,(yyvsp[(5) - (5)]));}
    break;

  case 108:
#line 231 "yacc.y"
    {(yyval)=makeNode(N_STMT_DO,(yyvsp[(2) - (7)]),NIL,(yyvsp[(5) - (7)]));}
    break;

  case 109:
#line 232 "yacc.y"
    {(yyval)=makeNode(N_STMT_FOR,(yyvsp[(3) - (5)]),NIL,(yyvsp[(5) - (5)]));}
    break;

  case 110:
#line 235 "yacc.y"
    {(yyval)=makeNode(N_FOR_EXP,(yyvsp[(1) - (5)]),(yyvsp[(3) - (5)]),(yyvsp[(5) - (5)]));}
    break;

  case 111:
#line 238 "yacc.y"
    {(yyval)=NIL;}
    break;

  case 112:
#line 239 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 113:
#line 242 "yacc.y"
    {(yyval)=makeNode(N_STMT_RETURN,NIL,(yyvsp[(2) - (3)]),NIL);}
    break;

  case 114:
#line 243 "yacc.y"
    {(yyval)=makeNode(N_STMT_CONTINUE,NIL,NIL,NIL);}
    break;

  case 115:
#line 244 "yacc.y"
    {(yyval)=makeNode(N_STMT_BREAK,NIL,NIL,NIL);}
    break;

  case 116:
#line 247 "yacc.y"
    {(yyval)=makeNode(N_ARG_LIST_NIL,NIL,NIL,NIL);}
    break;

  case 117:
#line 248 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 118:
#line 251 "yacc.y"
    {(yyval)=makeNode(N_ARG_LIST,(yyvsp[(1) - (1)]),NIL,makeNode(N_ARG_LIST_NIL,NIL,NIL,NIL));}
    break;

  case 119:
#line 252 "yacc.y"
    {(yyval)=makeNodeList(N_ARG_LIST,(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));}
    break;

  case 120:
#line 255 "yacc.y"
    {(yyval)=NIL;}
    break;

  case 121:
#line 256 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 122:
#line 259 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 123:
#line 262 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 124:
#line 265 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 125:
#line 268 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 126:
#line 269 "yacc.y"
    {(yyval)=makeNode(N_EXP_ASSIGN,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 127:
#line 272 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 128:
#line 275 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 129:
#line 276 "yacc.y"
    {(yyval)=makeNode(N_EXP_OR,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 130:
#line 279 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 131:
#line 280 "yacc.y"
    {(yyval)=makeNode(N_EXP_AND,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 132:
#line 283 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 133:
#line 286 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 134:
#line 289 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 135:
#line 292 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 136:
#line 293 "yacc.y"
    {(yyval)=makeNode(N_EXP_EQL,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 137:
#line 294 "yacc.y"
    {(yyval)=makeNode(N_EXP_NEQ,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 138:
#line 297 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 139:
#line 298 "yacc.y"
    {(yyval)=makeNode(N_EXP_LSS,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 140:
#line 299 "yacc.y"
    {(yyval)=makeNode(N_EXP_GTR,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 141:
#line 300 "yacc.y"
    {(yyval)=makeNode(N_EXP_LEQ,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 142:
#line 301 "yacc.y"
    {(yyval)=makeNode(N_EXP_GEQ,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 143:
#line 304 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 144:
#line 307 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 145:
#line 308 "yacc.y"
    {(yyval)=makeNode(N_EXP_ADD,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 146:
#line 309 "yacc.y"
    {(yyval)=makeNode(N_EXP_SUB,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 147:
#line 312 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 148:
#line 313 "yacc.y"
    {(yyval)=makeNode(N_EXP_MUL,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 149:
#line 314 "yacc.y"
    {(yyval)= makeNode(N_EXP_DIV,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 150:
#line 315 "yacc.y"
    {(yyval)= makeNode(N_EXP_MOD,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 151:
#line 318 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 152:
#line 319 "yacc.y"
    {(yyval)=makeNode(N_EXP_CAST,(yyvsp[(2) - (4)]),NIL,(yyvsp[(4) - (4)]));}
    break;

  case 153:
#line 322 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 154:
#line 323 "yacc.y"
    {(yyval)=makeNode(N_EXP_PRE_INC,NIL,(yyvsp[(2) - (2)]),NIL);}
    break;

  case 155:
#line 324 "yacc.y"
    {(yyval)=makeNode(N_EXP_PRE_DEC,NIL,(yyvsp[(2) - (2)]),NIL);}
    break;

  case 156:
#line 325 "yacc.y"
    {(yyval)=makeNode(N_EXP_AMP,NIL,(yyvsp[(2) - (2)]),NIL);}
    break;

  case 157:
#line 326 "yacc.y"
    {(yyval)=makeNode(N_EXP_STAR,NIL,(yyvsp[(2) - (2)]),NIL);}
    break;

  case 158:
#line 327 "yacc.y"
    {(yyval)=makeNode(N_EXP_NOT,NIL,(yyvsp[(2) - (2)]),NIL);}
    break;

  case 159:
#line 328 "yacc.y"
    {(yyval)=makeNode(N_EXP_MINUS,NIL,(yyvsp[(2) - (2)]),NIL);}
    break;

  case 160:
#line 329 "yacc.y"
    {(yyval)=makeNode(N_EXP_PLUS,NIL,(yyvsp[(2) - (2)]),NIL);}
    break;

  case 161:
#line 330 "yacc.y"
    {(yyval)=makeNode(N_EXP_SIZE_EXP,NIL,(yyvsp[(2) - (2)]),NIL);}
    break;

  case 162:
#line 331 "yacc.y"
    {(yyval)=makeNode(N_EXP_SIZE_TYPE,NIL,(yyvsp[(3) - (4)]),NIL);}
    break;

  case 163:
#line 334 "yacc.y"
    {(yyval)=(yyvsp[(1) - (1)]);}
    break;

  case 164:
#line 335 "yacc.y"
    {(yyval)=makeNode(N_EXP_ARRAY,(yyvsp[(1) - (4)]),NIL,(yyvsp[(3) - (4)]));}
    break;

  case 165:
#line 336 "yacc.y"
    {(yyval)=makeNode(N_EXP_FUNCTION_CALL,(yyvsp[(1) - (4)]),NIL,(yyvsp[(3) - (4)]));}
    break;

  case 166:
#line 337 "yacc.y"
    {(yyval)=makeNode(N_EXP_STRUCT,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 167:
#line 338 "yacc.y"
    {(yyval)=makeNode(N_EXP_ARROW,(yyvsp[(1) - (3)]),NIL,(yyvsp[(3) - (3)]));}
    break;

  case 168:
#line 339 "yacc.y"
    {(yyval)=makeNode(N_EXP_POST_INC,NIL,(yyvsp[(1) - (2)]),NIL);}
    break;

  case 169:
#line 340 "yacc.y"
    {(yyval)=makeNode(N_EXP_POST_DEC,NIL,(yyvsp[(1) - (2)]),NIL);}
    break;

  case 170:
#line 343 "yacc.y"
    {(yyval)=makeNode(N_EXP_IDENT,NIL,getIdentifierDeclared((yyvsp[(1) - (1)])),NIL);}
    break;

  case 171:
#line 344 "yacc.y"
    {(yyval)=makeNode(N_EXP_INT_CONST,NIL,(yyvsp[(1) - (1)]),NIL);}
    break;

  case 172:
#line 345 "yacc.y"
    {(yyval)=makeNode(N_EXP_FLOAT_CONST,NIL,(yyvsp[(1) - (1)]),NIL);}
    break;

  case 173:
#line 346 "yacc.y"
    {(yyval)=makeNode(N_EXP_CHAR_CONST,NIL,(yyvsp[(1) - (1)]),NIL);}
    break;

  case 174:
#line 347 "yacc.y"
    {(yyval)=makeNode(N_EXP_STRING_LITERAL,NIL,(yyvsp[(1) - (1)]),NIL);}
    break;

  case 175:
#line 348 "yacc.y"
    {(yyval)=(yyvsp[(2) - (3)]);}
    break;

  case 176:
#line 351 "yacc.y"
    {(yyval)=setTypeNameSpecifier((yyvsp[(2) - (2)]),(yyvsp[(1) - (2)]));}
    break;


/* Line 1267 of yacc.c.  */
#line 2645 "y.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 354 "yacc.y"


void yyerror(char *s) {
    syntax_err++;
    printf("line %d: %s near %s \n", line_no, s, yytext);
}

int main(int argc, char *argv[]) {
	if (argc<2) {
		printf("source file not given\n");
		exit(1);
	}
	if ((yyin=fopen(argv[argc-1],"r"))==NULL) {
		printf("cannot open input file: %s\n",argv[argc-1]);
		exit(1);
	}
	
	initialize();
	yyparse();
	if (syntax_err) exit(1);
	print_ast(root);
    semantic_analysis(root);

    if (semantic_err) exit(1);

    print_sem_ast(root);
	printf("\n");
	return 0;
}
