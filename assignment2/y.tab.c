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
     IDENTIFIER = 258,
     TYPE_IDENTIFIER = 259,
     CHARACTER_CONSTANT = 260,
     STRING_LITERAL = 261,
     FLOAT_CONSTANT = 262,
     INTEGER_CONSTANT = 263,
     ASSIGN = 264,
     MINUS = 265,
     PLUS = 266,
     SEMICOLON = 267,
     AMP = 268,
     PERCENT = 269,
     SLASH = 270,
     STAR = 271,
     EXCL = 272,
     COMMA = 273,
     PERIOD = 274,
     COLON = 275,
     RR = 276,
     LR = 277,
     RB = 278,
     LB = 279,
     RP = 280,
     LP = 281,
     DOTDOTDOT = 282,
     BARBAR = 283,
     AMPAMP = 284,
     NEQ = 285,
     EQL = 286,
     GEQ = 287,
     LEQ = 288,
     GTR = 289,
     LSS = 290,
     ARROW = 291,
     MINUSMINUS = 292,
     PLUSPLUS = 293,
     WHILE_SYM = 294,
     UNION_SYM = 295,
     TYPEDEF_SYM = 296,
     SWITCH_SYM = 297,
     STRUCT_SYM = 298,
     STATIC_SYM = 299,
     SIZEOF_SYM = 300,
     RETURN_SYM = 301,
     IF_SYM = 302,
     FOR_SYM = 303,
     ENUM_SYM = 304,
     ELSE_SYM = 305,
     DO_SYM = 306,
     DEFAULT_SYM = 307,
     CONTINUE_SYM = 308,
     CASE_SYM = 309,
     BREAK_SYM = 310,
     AUTO_SYM = 311
   };
#endif
/* Tokens.  */
#define IDENTIFIER 258
#define TYPE_IDENTIFIER 259
#define CHARACTER_CONSTANT 260
#define STRING_LITERAL 261
#define FLOAT_CONSTANT 262
#define INTEGER_CONSTANT 263
#define ASSIGN 264
#define MINUS 265
#define PLUS 266
#define SEMICOLON 267
#define AMP 268
#define PERCENT 269
#define SLASH 270
#define STAR 271
#define EXCL 272
#define COMMA 273
#define PERIOD 274
#define COLON 275
#define RR 276
#define LR 277
#define RB 278
#define LB 279
#define RP 280
#define LP 281
#define DOTDOTDOT 282
#define BARBAR 283
#define AMPAMP 284
#define NEQ 285
#define EQL 286
#define GEQ 287
#define LEQ 288
#define GTR 289
#define LSS 290
#define ARROW 291
#define MINUSMINUS 292
#define PLUSPLUS 293
#define WHILE_SYM 294
#define UNION_SYM 295
#define TYPEDEF_SYM 296
#define SWITCH_SYM 297
#define STRUCT_SYM 298
#define STATIC_SYM 299
#define SIZEOF_SYM 300
#define RETURN_SYM 301
#define IF_SYM 302
#define FOR_SYM 303
#define ENUM_SYM 304
#define ELSE_SYM 305
#define DO_SYM 306
#define DEFAULT_SYM 307
#define CONTINUE_SYM 308
#define CASE_SYM 309
#define BREAK_SYM 310
#define AUTO_SYM 311




/* Copy the first part of user declarations.  */
#line 1 "yacc.y"

     #include <stdio.h>
     int line_no = 1;
     int yyerror(char *s);
     int yylex();


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
#line 225 "y.tab.c"

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
#define YYLAST   557

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  57
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  58
/* YYNRULES -- Number of rules.  */
#define YYNRULES  155
/* YYNRULES -- Number of states.  */
#define YYNSTATES  276

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   311

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
      55,    56
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     7,    10,    12,    14,    18,    21,
      24,    28,    30,    32,    35,    38,    40,    42,    44,    46,
      50,    52,    56,    58,    60,    62,    68,    73,    76,    78,
      80,    82,    85,    89,    91,    95,    97,   103,   108,   111,
     113,   117,   119,   124,   127,   129,   131,   134,   136,   140,
     145,   150,   151,   153,   154,   156,   158,   162,   164,   168,
     171,   174,   176,   178,   180,   183,   187,   191,   195,   200,
     205,   207,   211,   213,   217,   219,   221,   223,   225,   227,
     229,   234,   238,   242,   247,   248,   251,   252,   255,   257,
     260,   266,   274,   280,   286,   294,   304,   305,   307,   311,
     314,   317,   319,   321,   323,   325,   327,   331,   333,   338,
     343,   347,   351,   354,   357,   358,   360,   362,   366,   368,
     371,   374,   377,   380,   383,   386,   389,   392,   397,   399,
     404,   406,   409,   411,   415,   419,   423,   425,   429,   433,
     435,   439,   443,   447,   451,   453,   457,   461,   463,   467,
     469,   473,   475,   477,   481,   483
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      58,     0,    -1,    59,    -1,    60,    -1,    59,    60,    -1,
      61,    -1,    62,    -1,    63,    77,    91,    -1,    77,    91,
      -1,    63,    12,    -1,    63,    65,    12,    -1,    67,    -1,
      64,    -1,    67,    63,    -1,    64,    63,    -1,    56,    -1,
      44,    -1,    41,    -1,    66,    -1,    65,    18,    66,    -1,
      77,    -1,    77,     9,    87,    -1,    68,    -1,    74,    -1,
       4,    -1,    69,     3,    22,    70,    21,    -1,    69,    22,
      70,    21,    -1,    69,     3,    -1,    43,    -1,    40,    -1,
      71,    -1,    70,    71,    -1,    67,    72,    12,    -1,    73,
      -1,    72,    18,    73,    -1,    77,    -1,    49,     3,    22,
      75,    21,    -1,    49,    22,    75,    21,    -1,    49,     3,
      -1,    76,    -1,    75,    18,    76,    -1,     3,    -1,     3,
       9,   112,    12,    -1,    78,    79,    -1,    79,    -1,    16,
      -1,    16,    78,    -1,     3,    -1,    26,    77,    25,    -1,
      79,    24,    80,    23,    -1,    79,    26,    81,    25,    -1,
      -1,   112,    -1,    -1,    82,    -1,    83,    -1,    83,    18,
      27,    -1,    84,    -1,    83,    18,    84,    -1,    63,    77,
      -1,    63,    85,    -1,    63,    -1,    78,    -1,    86,    -1,
      78,    86,    -1,    26,    85,    25,    -1,    24,    80,    23,
      -1,    26,    81,    25,    -1,    86,    24,    80,    23,    -1,
      86,    26,    81,    25,    -1,   112,    -1,    22,    88,    21,
      -1,    87,    -1,    88,    18,    87,    -1,    90,    -1,    91,
      -1,    94,    -1,    95,    -1,    96,    -1,    98,    -1,    54,
     112,    20,    89,    -1,    52,    20,    89,    -1,     3,    20,
      89,    -1,    22,    92,    93,    21,    -1,    -1,    92,    62,
      -1,    -1,    93,    89,    -1,    12,    -1,   113,    12,    -1,
      47,    26,   113,    25,    89,    -1,    47,    26,   113,    25,
      89,    50,    89,    -1,    42,    26,   113,    25,    89,    -1,
      39,    26,   113,    25,    89,    -1,    51,    89,    39,    26,
     113,    25,    12,    -1,    48,    26,    97,    12,    97,    12,
      97,    25,    89,    -1,    -1,   113,    -1,    46,    97,    12,
      -1,    53,    12,    -1,    55,    12,    -1,     3,    -1,     8,
      -1,     7,    -1,     5,    -1,     6,    -1,    26,   113,    25,
      -1,    99,    -1,   100,    24,   113,    23,    -1,   100,    26,
     101,    25,    -1,   100,    19,     3,    -1,   100,    36,     3,
      -1,   100,    38,    -1,   100,    37,    -1,    -1,   102,    -1,
     114,    -1,   102,    18,   114,    -1,   100,    -1,    38,   103,
      -1,    37,   103,    -1,    13,   104,    -1,    16,   104,    -1,
      17,   104,    -1,    10,   104,    -1,    11,   104,    -1,    45,
     103,    -1,    45,    26,   105,    25,    -1,   103,    -1,    26,
     105,    25,   104,    -1,    63,    -1,    63,    85,    -1,   104,
      -1,   106,    16,   104,    -1,   106,    15,   104,    -1,   106,
      14,   104,    -1,   106,    -1,   107,    11,   106,    -1,   107,
      10,   106,    -1,   107,    -1,   108,    35,   107,    -1,   108,
      34,   107,    -1,   108,    33,   107,    -1,   108,    32,   107,
      -1,   108,    -1,   109,    31,   108,    -1,   109,    30,   108,
      -1,   109,    -1,   110,    29,   109,    -1,   110,    -1,   111,
      28,   110,    -1,   113,    -1,   114,    -1,   113,    18,   114,
      -1,   111,    -1,   103,     9,   114,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    17,    17,    21,    22,    26,    27,    31,    32,    36,
      37,    41,    42,    43,    44,    48,    49,    50,    54,    55,
      59,    60,    64,    65,    66,    70,    71,    72,    76,    77,
      81,    82,    86,    90,    91,    95,    99,   100,   101,   105,
     106,   110,   111,   115,   116,   120,   121,   125,   126,   127,
     128,   131,   133,   136,   138,   142,   143,   147,   148,   152,
     153,   154,   158,   159,   160,   164,   165,   166,   167,   168,
     172,   173,   177,   178,   182,   183,   184,   185,   186,   187,
     191,   192,   193,   197,   200,   202,   205,   207,   211,   212,
     216,   217,   218,   222,   223,   224,   227,   229,   233,   234,
     235,   239,   240,   241,   242,   243,   244,   248,   249,   250,
     251,   252,   253,   254,   257,   259,   263,   264,   268,   269,
     270,   271,   272,   273,   274,   275,   276,   277,   281,   282,
     286,   287,   291,   292,   293,   294,   298,   299,   300,   304,
     305,   306,   307,   308,   312,   313,   314,   318,   319,   323,
     324,   328,   332,   333,   337,   338
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENTIFIER", "TYPE_IDENTIFIER",
  "CHARACTER_CONSTANT", "STRING_LITERAL", "FLOAT_CONSTANT",
  "INTEGER_CONSTANT", "ASSIGN", "MINUS", "PLUS", "SEMICOLON", "AMP",
  "PERCENT", "SLASH", "STAR", "EXCL", "COMMA", "PERIOD", "COLON", "RR",
  "LR", "RB", "LB", "RP", "LP", "DOTDOTDOT", "BARBAR", "AMPAMP", "NEQ",
  "EQL", "GEQ", "LEQ", "GTR", "LSS", "ARROW", "MINUSMINUS", "PLUSPLUS",
  "WHILE_SYM", "UNION_SYM", "TYPEDEF_SYM", "SWITCH_SYM", "STRUCT_SYM",
  "STATIC_SYM", "SIZEOF_SYM", "RETURN_SYM", "IF_SYM", "FOR_SYM",
  "ENUM_SYM", "ELSE_SYM", "DO_SYM", "DEFAULT_SYM", "CONTINUE_SYM",
  "CASE_SYM", "BREAK_SYM", "AUTO_SYM", "$accept", "program",
  "translate_unit", "external_declaration", "function_definition",
  "declaration", "declaration_specifiers", "storage_class_specifier",
  "init_declarator_list", "init_declarator", "type_specifier",
  "struct_specifier", "struct_or_union", "struct_declaration_list",
  "struct_declaration", "struct_declarator_list", "struct_declarator",
  "enum_specifier", "enumerator_list", "enumerator", "declarator",
  "pointer", "direct_declarator", "constant_expression_opt",
  "parameter_type_list_opt", "parameter_type_list", "parameter_list",
  "parameter_declaration", "abstract_declarator",
  "direct_abstract_declarator", "initializer", "initializer_list",
  "statement", "labeled_statement", "compound_statement",
  "declaration_list", "statement_list", "expression_statement",
  "selection_statement", "iteration_statement", "expression_opt",
  "jump_statement", "primary_expression", "postfix_expression",
  "arg_expression_list_opt", "arg_expression_list", "unary_expression",
  "cast_expression", "type_name", "multiplicative_expression",
  "additive_expression", "relational_expression", "equality_expression",
  "logical_and_expression", "logical_or_expression", "constant_expression",
  "expression", "assignment_expression", 0
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
     305,   306,   307,   308,   309,   310,   311
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    57,    58,    59,    59,    60,    60,    61,    61,    62,
      62,    63,    63,    63,    63,    64,    64,    64,    65,    65,
      66,    66,    67,    67,    67,    68,    68,    68,    69,    69,
      70,    70,    71,    72,    72,    73,    74,    74,    74,    75,
      75,    76,    76,    77,    77,    78,    78,    79,    79,    79,
      79,    80,    80,    81,    81,    82,    82,    83,    83,    84,
      84,    84,    85,    85,    85,    86,    86,    86,    86,    86,
      87,    87,    88,    88,    89,    89,    89,    89,    89,    89,
      90,    90,    90,    91,    92,    92,    93,    93,    94,    94,
      95,    95,    95,    96,    96,    96,    97,    97,    98,    98,
      98,    99,    99,    99,    99,    99,    99,   100,   100,   100,
     100,   100,   100,   100,   101,   101,   102,   102,   103,   103,
     103,   103,   103,   103,   103,   103,   103,   103,   104,   104,
     105,   105,   106,   106,   106,   106,   107,   107,   107,   108,
     108,   108,   108,   108,   109,   109,   109,   110,   110,   111,
     111,   112,   113,   113,   114,   114
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     2,     1,     1,     3,     2,     2,
       3,     1,     1,     2,     2,     1,     1,     1,     1,     3,
       1,     3,     1,     1,     1,     5,     4,     2,     1,     1,
       1,     2,     3,     1,     3,     1,     5,     4,     2,     1,
       3,     1,     4,     2,     1,     1,     2,     1,     3,     4,
       4,     0,     1,     0,     1,     1,     3,     1,     3,     2,
       2,     1,     1,     1,     2,     3,     3,     3,     4,     4,
       1,     3,     1,     3,     1,     1,     1,     1,     1,     1,
       4,     3,     3,     4,     0,     2,     0,     2,     1,     2,
       5,     7,     5,     5,     7,     9,     0,     1,     3,     2,
       2,     1,     1,     1,     1,     1,     3,     1,     4,     4,
       3,     3,     2,     2,     0,     1,     1,     3,     1,     2,
       2,     2,     2,     2,     2,     2,     2,     4,     1,     4,
       1,     2,     1,     3,     3,     3,     1,     3,     3,     1,
       3,     3,     3,     3,     1,     3,     3,     1,     3,     1,
       3,     1,     1,     3,     1,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    47,    24,    45,     0,    29,    17,    28,    16,     0,
      15,     0,     2,     3,     5,     6,     0,    12,    11,    22,
       0,    23,     0,     0,    44,    46,     0,    38,     0,     1,
       4,     9,     0,    18,    20,    14,    13,    27,     0,    84,
       8,    43,    51,    53,    48,     0,    41,     0,    39,    10,
       0,     0,     7,     0,     0,     0,    30,    86,   101,   104,
     105,   103,   102,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   107,   118,   128,   132,   136,   139,   144,
     147,   149,   154,    52,   151,   152,    61,     0,    54,    55,
      57,     0,     0,     0,    37,    19,    20,     0,    21,    70,
       0,     0,    33,    35,    26,    31,    85,     0,     0,   128,
     124,   125,   121,   122,   123,   130,     0,     0,     0,   120,
     119,     0,   126,    49,     0,     0,   114,     0,   113,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    51,    53,    59,    62,    60,
      63,    50,     0,    36,     0,    40,    72,     0,    25,    32,
       0,   101,    88,    83,     0,     0,    96,     0,     0,     0,
       0,     0,     0,     0,    87,    74,    75,    76,    77,    78,
      79,     0,    53,    62,   131,     0,   106,     0,   110,     0,
       0,   115,   116,   111,   155,   135,   134,   133,   138,   137,
     143,   142,   141,   140,   146,   145,   148,   150,   153,     0,
       0,     0,    64,    51,    53,    56,    58,    42,     0,    71,
      34,     0,     0,     0,     0,    97,     0,    96,     0,     0,
      99,     0,   100,    89,   129,   127,   108,   109,     0,    66,
      67,    65,     0,     0,    73,    82,     0,     0,    98,     0,
       0,     0,    81,     0,   117,    68,    69,     0,     0,     0,
      96,     0,    80,    93,    92,    90,     0,     0,     0,    96,
       0,    91,     0,    94,     0,    95
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    11,    12,    13,    14,    15,    86,    17,    32,    33,
      18,    19,    20,    55,    56,   101,   102,    21,    47,    48,
      22,    23,    24,    72,   210,    88,    89,    90,   211,   150,
      98,   157,   174,   175,   176,    57,   108,   177,   178,   179,
     224,   180,    73,    74,   190,   191,    75,    76,   116,    77,
      78,    79,    80,    81,    82,    83,   181,    85
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -153
static const yytype_int16 yypact[] =
{
     259,  -153,  -153,    -2,    18,  -153,  -153,  -153,  -153,    10,
    -153,    46,   259,  -153,  -153,  -153,    33,    49,    49,  -153,
      36,  -153,    79,    51,   202,  -153,    87,    98,   144,  -153,
    -153,  -153,   113,  -153,    16,  -153,  -153,   130,     8,  -153,
    -153,   202,   459,    49,  -153,   144,   155,   151,  -153,  -153,
      18,   423,  -153,     8,    18,    20,  -153,    49,  -153,  -153,
    -153,  -153,  -153,   459,   459,   459,   459,   459,   204,   476,
     476,   512,   147,  -153,   231,   165,  -153,   221,    74,   168,
     166,   176,   185,  -153,   161,  -153,    78,   191,  -153,   225,
    -153,   157,   459,   144,  -153,  -153,   223,   423,  -153,  -153,
      70,   148,  -153,  -153,  -153,  -153,  -153,    33,   306,  -153,
    -153,  -153,  -153,  -153,  -153,   129,   236,    62,   459,  -153,
    -153,   204,  -153,  -153,   261,   459,   459,   262,  -153,  -153,
     459,   459,   459,   459,   459,   459,   459,   459,   459,   459,
     459,   459,   459,   459,   459,   459,   230,  -153,    97,  -153,
     205,  -153,   150,  -153,   227,  -153,  -153,   174,  -153,  -153,
      18,   246,  -153,  -153,   250,   251,   459,   252,   254,   359,
     263,   260,   459,   269,  -153,  -153,  -153,  -153,  -153,  -153,
    -153,   149,   376,   214,  -153,   459,  -153,   257,  -153,    93,
     264,   266,  -153,  -153,  -153,  -153,  -153,  -153,   221,   221,
      74,    74,    74,    74,   168,   168,   166,   176,  -153,   265,
     267,   268,   205,   459,    49,  -153,  -153,  -153,   423,  -153,
    -153,   359,   459,   459,   275,   161,   459,   459,   255,   359,
    -153,   270,  -153,  -153,  -153,  -153,  -153,  -153,   459,  -153,
    -153,  -153,   272,   271,  -153,  -153,    89,   132,  -153,   138,
     279,   278,  -153,   359,  -153,  -153,  -153,   359,   359,   359,
     459,   459,  -153,  -153,  -153,   247,   286,   140,   359,   459,
     289,  -153,   280,  -153,   359,  -153
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -153,  -153,  -153,   294,  -153,   253,     5,  -153,  -153,   274,
     -18,  -153,  -153,   273,   -39,  -153,   160,  -153,   276,   232,
       2,     0,   -19,  -135,   -41,  -153,  -153,   177,   -75,  -140,
     -90,  -153,   -70,  -153,    84,  -153,  -153,  -153,  -153,  -153,
    -152,  -153,  -153,  -153,  -153,  -153,     1,   -36,   186,   117,
      86,   118,   188,   190,  -153,   -50,   -42,  -111
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_uint16 yytable[] =
{
      84,    99,    87,    25,    41,    16,    26,   156,   212,    84,
     209,   149,     2,    27,     3,   192,   105,    16,    34,   194,
      54,     1,    35,    36,     2,    51,   117,   110,   111,   112,
     113,   114,    28,   208,     3,    54,     1,    54,    39,    37,
     184,   104,   154,   212,     4,    31,    29,    99,     5,     3,
      84,     7,    96,     2,     1,    84,   103,     9,    38,     4,
       5,   105,   107,     7,   109,   109,   109,   109,   109,     9,
     119,   120,   122,   115,     2,   250,   117,     4,   242,   117,
     144,     1,    54,   189,   134,   135,   148,   186,   147,     5,
       6,   158,     7,     8,     3,   195,   196,   197,     9,   228,
       1,    39,   145,    84,   146,    10,    40,   144,   266,    96,
       5,   144,    44,     7,   257,   183,   236,   272,    52,     9,
      45,   145,   231,   146,   225,    49,   115,   254,   244,    41,
      84,    50,   109,   109,   109,   109,   109,   109,   109,   109,
     109,   109,   109,   109,   109,     3,   148,    46,    26,   234,
     144,   245,    53,   145,     2,   182,   144,   258,   144,   252,
     159,   233,   103,   259,    92,   270,   160,   144,    99,    93,
     123,    84,    94,   243,   130,    93,    84,   215,   153,   144,
     246,   247,   183,   262,   249,   225,   109,   263,   264,   265,
       5,     6,   218,     7,     8,   219,   140,   141,   271,     9,
     136,   137,   138,   139,   275,   142,    10,    58,     2,    59,
      60,    61,    62,   143,    63,    64,   151,    65,   225,   267,
      66,    67,   200,   201,   202,   203,    42,   225,    43,   213,
      68,   214,    51,     1,     2,   131,   132,   133,   145,   217,
     182,    69,    70,   152,     5,     6,     3,     7,     8,    71,
     124,   198,   199,     9,   145,   125,   146,   126,   204,   205,
      10,   185,     1,     2,   188,   193,   221,   127,   128,   129,
       5,     6,   230,     7,     8,     3,   222,   223,   226,     9,
     227,   232,   235,   229,   238,     4,    10,   248,   239,   237,
     253,   260,   240,   241,   251,   255,   256,   268,   269,     5,
       6,   273,     7,     8,   261,   274,    30,   187,     9,   161,
     106,    59,    60,    61,    62,    10,    63,    64,   162,    65,
     220,    91,    66,    67,    95,   155,   100,   163,    39,   216,
     206,     0,    68,   207,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,   164,     0,     0,   165,     0,
       0,    71,   166,   167,   168,     0,     0,   169,   170,   171,
     172,   173,   161,     0,    59,    60,    61,    62,     0,    63,
      64,   162,    65,     0,     0,    66,    67,     0,     0,     0,
       2,    39,     0,     0,     0,    68,     0,     0,     0,     0,
       0,     0,     3,     0,     0,     0,    69,    70,   164,     0,
     145,   165,   182,     0,    71,   166,   167,   168,     0,     0,
     169,   170,   171,   172,   173,     0,     5,     6,     0,     7,
       8,     0,     0,     0,     0,     9,    58,     0,    59,    60,
      61,    62,    10,    63,    64,     0,    65,     0,     0,    66,
      67,     0,     0,     0,     0,    97,     0,     0,     0,    68,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      69,    70,    58,     0,    59,    60,    61,    62,    71,    63,
      64,     0,    65,     0,     0,    66,    67,     0,     0,    58,
       0,    59,    60,    61,    62,    68,    63,    64,     0,    65,
       0,     0,    66,    67,     0,     0,    69,    70,     0,     0,
       0,     0,   118,     0,    71,     0,     0,     0,     0,     0,
       0,     0,     0,    69,    70,    58,     0,    59,    60,    61,
      62,    71,    63,    64,     0,    65,     0,     0,    66,    67,
       0,     0,     0,     0,     0,     0,     0,     0,   121,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    69,
      70,     0,     0,     0,     0,     0,     0,    71
};

static const yytype_int16 yycheck[] =
{
      42,    51,    43,     3,    23,     0,     4,    97,   148,    51,
     145,    86,     4,     3,    16,   126,    55,    12,    16,   130,
      38,     3,    17,    18,     4,     9,    68,    63,    64,    65,
      66,    67,    22,   144,    16,    53,     3,    55,    22,     3,
     115,    21,    92,   183,    26,    12,     0,    97,    40,    16,
      92,    43,    50,     4,     3,    97,    54,    49,    22,    26,
      40,   100,    57,    43,    63,    64,    65,    66,    67,    49,
      69,    70,    71,    68,     4,   227,   118,    26,   213,   121,
      18,     3,   100,   125,    10,    11,    86,    25,    86,    40,
      41,    21,    43,    44,    16,   131,   132,   133,    49,   169,
       3,    22,    24,   145,    26,    56,    22,    18,   260,   107,
      40,    18,    25,    43,    25,   115,    23,   269,    34,    49,
      22,    24,   172,    26,   166,    12,   121,   238,   218,   148,
     172,    18,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,    16,   146,     3,   146,   185,
      18,   221,    22,    24,     4,    26,    18,    25,    18,   229,
      12,    12,   160,    25,     9,    25,    18,    18,   218,    18,
      23,   213,    21,   214,     9,    18,   218,    27,    21,    18,
     222,   223,   182,   253,   226,   227,   185,   257,   258,   259,
      40,    41,    18,    43,    44,    21,    30,    31,   268,    49,
      32,    33,    34,    35,   274,    29,    56,     3,     4,     5,
       6,     7,     8,    28,    10,    11,    25,    13,   260,   261,
      16,    17,   136,   137,   138,   139,    24,   269,    26,    24,
      26,    26,     9,     3,     4,    14,    15,    16,    24,    12,
      26,    37,    38,    18,    40,    41,    16,    43,    44,    45,
      19,   134,   135,    49,    24,    24,    26,    26,   140,   141,
      56,    25,     3,     4,     3,     3,    20,    36,    37,    38,
      40,    41,    12,    43,    44,    16,    26,    26,    26,    49,
      26,    12,    25,    20,    18,    26,    56,    12,    23,    25,
      20,    12,    25,    25,    39,    23,    25,    50,    12,    40,
      41,    12,    43,    44,    26,    25,    12,   121,    49,     3,
      57,     5,     6,     7,     8,    56,    10,    11,    12,    13,
     160,    45,    16,    17,    50,    93,    53,    21,    22,   152,
     142,    -1,    26,   143,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    39,    -1,    -1,    42,    -1,
      -1,    45,    46,    47,    48,    -1,    -1,    51,    52,    53,
      54,    55,     3,    -1,     5,     6,     7,     8,    -1,    10,
      11,    12,    13,    -1,    -1,    16,    17,    -1,    -1,    -1,
       4,    22,    -1,    -1,    -1,    26,    -1,    -1,    -1,    -1,
      -1,    -1,    16,    -1,    -1,    -1,    37,    38,    39,    -1,
      24,    42,    26,    -1,    45,    46,    47,    48,    -1,    -1,
      51,    52,    53,    54,    55,    -1,    40,    41,    -1,    43,
      44,    -1,    -1,    -1,    -1,    49,     3,    -1,     5,     6,
       7,     8,    56,    10,    11,    -1,    13,    -1,    -1,    16,
      17,    -1,    -1,    -1,    -1,    22,    -1,    -1,    -1,    26,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,     3,    -1,     5,     6,     7,     8,    45,    10,
      11,    -1,    13,    -1,    -1,    16,    17,    -1,    -1,     3,
      -1,     5,     6,     7,     8,    26,    10,    11,    -1,    13,
      -1,    -1,    16,    17,    -1,    -1,    37,    38,    -1,    -1,
      -1,    -1,    26,    -1,    45,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,     3,    -1,     5,     6,     7,
       8,    45,    10,    11,    -1,    13,    -1,    -1,    16,    17,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    26,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    -1,    -1,    -1,    -1,    -1,    45
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,    16,    26,    40,    41,    43,    44,    49,
      56,    58,    59,    60,    61,    62,    63,    64,    67,    68,
      69,    74,    77,    78,    79,    78,    77,     3,    22,     0,
      60,    12,    65,    66,    77,    63,    63,     3,    22,    22,
      91,    79,    24,    26,    25,    22,     3,    75,    76,    12,
      18,     9,    91,    22,    67,    70,    71,    92,     3,     5,
       6,     7,     8,    10,    11,    13,    16,    17,    26,    37,
      38,    45,    80,    99,   100,   103,   104,   106,   107,   108,
     109,   110,   111,   112,   113,   114,    63,    81,    82,    83,
      84,    75,     9,    18,    21,    66,    77,    22,    87,   112,
      70,    72,    73,    77,    21,    71,    62,    63,    93,   103,
     104,   104,   104,   104,   104,    63,   105,   113,    26,   103,
     103,    26,   103,    23,    19,    24,    26,    36,    37,    38,
       9,    14,    15,    16,    10,    11,    32,    33,    34,    35,
      30,    31,    29,    28,    18,    24,    26,    77,    78,    85,
      86,    25,    18,    21,   112,    76,    87,    88,    21,    12,
      18,     3,    12,    21,    39,    42,    46,    47,    48,    51,
      52,    53,    54,    55,    89,    90,    91,    94,    95,    96,
      98,   113,    26,    78,    85,    25,    25,   105,     3,   113,
     101,   102,   114,     3,   114,   104,   104,   104,   106,   106,
     107,   107,   107,   107,   108,   108,   109,   110,   114,    80,
      81,    85,    86,    24,    26,    27,    84,    12,    18,    21,
      73,    20,    26,    26,    97,   113,    26,    26,    89,    20,
      12,   112,    12,    12,   104,    25,    23,    25,    18,    23,
      25,    25,    80,    81,    87,    89,   113,   113,    12,   113,
      97,    39,    89,    20,   114,    23,    25,    25,    25,    25,
      12,    26,    89,    89,    89,    89,    97,   113,    50,    12,
      25,    89,    97,    12,    25,    89
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
      
/* Line 1267 of yacc.c.  */
#line 1751 "y.tab.c"
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


#line 340 "yacc.y"

extern char *yytext;

int yyerror(char *s) 
{
     printf("line %d: %s near %s \n", line_no, s, yytext);
}

int main()
{
     yyparse();
     printf("프로그램 종료\n");
}

int yywrap()
{
     return(1);
}
