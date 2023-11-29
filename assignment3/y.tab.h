/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

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




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef long YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

