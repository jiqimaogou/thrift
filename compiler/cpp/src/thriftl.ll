/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

/**
 * Thrift scanner.
 *
 * Tokenizes a thrift definition file.
 */

%{

/* This is redundant with some of the flags in Makefile.am, but it works
 * when people override CXXFLAGS without being careful. The pragmas are
 * the 'right' way to do it, but don't work on old-enough GCC (in particular
 * the GCC that ship on Mac OS X 10.6.5, *counter* to what the GNU docs say)
 *
 * We should revert the Makefile.am changes once Apple ships a reasonable
 * GCC.
 */
#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wunused-label"
#endif

#ifdef _MSC_VER
//warning C4102: 'find_rule' : unreferenced label
#pragma warning(disable:4102)
//avoid isatty redefinition
#define YY_NEVER_INTERACTIVE 1

#define YY_NO_UNISTD_H 1
#endif

#include <cassert>
#include <string>
#include <errno.h>
#include <stdlib.h>

#ifdef _MSC_VER
#include "windows/config.h"
#endif
#include "main.h"
#include "globals.h"
#include "parse/t_program.h"

/**
 * Must be included AFTER parse/t_program.h, but I can't remember why anymore
 * because I wrote this a while ago.
 */
#if defined(BISON_USE_PARSER_H_EXTENSION)
#include "thrifty.h"
#else
#include "thrifty.hh"
#endif

int g_processing_struct = 0;

void thrift_reserved_keyword(char* keyword) {
  yyerror("Cannot use reserved language keyword: \"%s\"\n", keyword);
  exit(1);
}

void integer_overflow(char* text) {
  yyerror("This integer is too big: \"%s\"\n", text);
  exit(1);
}

void unexpected_token(char* text) {
  yyerror("Unexpected token in input: \"%s\"\n", text);
  exit(1);
}

%}

/**
 * Provides the yylineno global, useful for debugging output
 */
%option lex-compat

/**
 * Our inputs are all single files, so no need for yywrap
 */
%option noyywrap

/**
 * We don't use it, and it fires up warnings at -Wall
 */
%option nounput

/**
 * Helper definitions, comments, constants, and whatnot
 */

intconstant   ([+-]?[0-9]+)
hexconstant   ([+-]?"0x"[0-9A-Fa-f]+)
dubconstant   ([+-]?[0-9]*(\.[0-9]+)?([eE][+-]?[0-9]+)?)
identifier    ([a-zA-Z_](\.[a-zA-Z_0-9]|[a-zA-Z_0-9])*)
whitespace    ([ \t\r\n]*)
sillycomm     ("/*""*"*"*/")
multicomm     ("/*"[^*]"/"*([^*/]|[^*]"/"|"*"[^/])*"*"*"*/")
doctext       ("/**"([^*/]|[^*]"/"|"*"[^/])*"*"*"*/")
comment       ("//"[^\n]*)
unixcomment   ("#"[^\n]*)
symbol        ([:;\,\{\}\(\)\=<>\[\]])
st_identifier ([a-zA-Z-](\.[a-zA-Z_0-9-]|[a-zA-Z_0-9-])*)
literal_begin (['\"])

%%

{whitespace}         { /* do nothing */                 }
{sillycomm}          { /* do nothing */                 }
{multicomm}          { /* do nothing */                 }
{comment}            { /* do nothing */                 }
{unixcomment}        { /* do nothing */                 }

{symbol}             {
  if (g_processing_struct) {
    if (yytext[0] == '{') {
      g_processing_struct++;
    } else if (yytext[0] == '}') {
      g_processing_struct--;
      if (g_processing_struct == 1) g_processing_struct = 0;
    }
  }
  return yytext[0];
}
"*"                  { return yytext[0];                }

"false"              { yylval.iconst=0; return tok_int_constant; }
"true"               { yylval.iconst=1; return tok_int_constant; }

"namespace"          { return tok_namespace;            }
"cpp_namespace"      { return tok_cpp_namespace;        }
"cpp_include"        { return tok_cpp_include;          }
"cpp_type"           { return tok_cpp_type;             }
"java_package"       { return tok_java_package;         }
"cocoa_prefix"       { return tok_cocoa_prefix;         }
"csharp_namespace"   { return tok_csharp_namespace;     }
"delphi_namespace"   { return tok_delphi_namespace;     }
"php_namespace"      { return tok_php_namespace;        }
"py_module"          { return tok_py_module;            }
"perl_package"       { return tok_perl_package;         }
"ruby_namespace"     { return tok_ruby_namespace;       }
"smalltalk_category" { return tok_smalltalk_category;   }
"smalltalk_prefix"   { return tok_smalltalk_prefix;     }
"xsd_all"            { return tok_xsd_all;              }
"xsd_optional"       { return tok_xsd_optional;         }
"xsd_nillable"       { return tok_xsd_nillable;         }
"xsd_namespace"      { return tok_xsd_namespace;        }
"xsd_attrs"          { return tok_xsd_attrs;            }
"include"            { return tok_include;              }
"void"               { return tok_void;                 }
"bool"               { return tok_bool;                 }
"byte"               { return tok_byte;                 }
"i16"                { return tok_i16;                  }
"i32"                { return tok_i32;                  }
"i64"                { return tok_i64;                  }
"double"             { return tok_double;               }
"string"             { return tok_string;               }
"binary"             { return tok_binary;               }
"slist" {
  pwarning(0, "\"slist\" is deprecated and will be removed in a future compiler version.  This type should be replaced with \"string\".\n");
  return tok_slist;
}
"senum" {
  pwarning(0, "\"senum\" is deprecated and will be removed in a future compiler version.  This type should be replaced with \"string\".\n");
  return tok_senum;
}
"map"                { return tok_map;                  }
"list"               { return tok_list;                 }
"set"                { return tok_set;                  }
"oneway"             { return tok_oneway;               }
"typedef"            { return tok_typedef;              }
"struct"             { g_processing_struct = 1; return tok_struct; }
"union"              { return tok_union;                }
"exception"          { return tok_xception;             }
"extends"            { return tok_extends;              }
"throws"             { return tok_throws;               }
"service"            { if (g_processing_struct) { REJECT; } else { return tok_service; } }
"enum"               { return tok_enum;                 }
"const"              { return tok_const;                }
"required"           { return tok_required;             }
"optional"           { return tok_optional;             }
"async" {
  pwarning(0, "\"async\" is deprecated.  It is called \"oneway\" now.\n");
  return tok_oneway;
}
"&"                  { return tok_reference;            }


"BEGIN"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"END"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"__CLASS__"          { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"__DIR__"            { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"__FILE__"           { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"__FUNCTION__"       { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"__LINE__"           { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"__METHOD__"         { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"__NAMESPACE__"      { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"abstract"           { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"alias"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"and"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"args"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"as"                 { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"assert"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"begin"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"break"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"case"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"catch"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"class"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"clone"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"continue"           { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"declare"            { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"def"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"default"            { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"del"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"delete"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"do"                 { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"dynamic"            { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"elif"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"else"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"elseif"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"elsif"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"end"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"enddeclare"         { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"endfor"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"endforeach"         { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"endif"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"endswitch"          { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"endwhile"           { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"ensure"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"except"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"exec"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"finally"            { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"float"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"for"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"foreach"            { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"from"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"function"           { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"global"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"goto"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"if"                 { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"implements"         { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"import"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"in"                 { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"inline"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"instanceof"         { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"interface"          { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"is"                 { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"lambda"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"module"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"native"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"new"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"next"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"nil"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"not"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"or"                 { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"package"            { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"pass"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"public"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"print"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"private"            { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"protected"          { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"public"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"raise"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"redo"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"rescue"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"retry"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"register"           { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"return"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"self"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"sizeof"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"static"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"super"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"switch"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"synchronized"       { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"then"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"this"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"throw"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"transient"          { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"try"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"undef"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"union"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"unless"             { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"unsigned"           { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"until"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"use"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"var"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"virtual"            { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"volatile"           { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"when"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"while"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"with"               { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"xor"                { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }
"yield"              { if (g_processing_struct) { REJECT; } else { thrift_reserved_keyword(yytext); } }

{intconstant} {
  errno = 0;
  yylval.iconst = strtoll(yytext, NULL, 10);
  if (errno == ERANGE) {
    integer_overflow(yytext);
  }
  return tok_int_constant;
}

{hexconstant} {
  errno = 0;
  char sign = yytext[0];
  int shift = sign == '0' ? 2 : 3;
  yylval.iconst = strtoll(yytext+shift, NULL, 16);
  if (sign == '-') {
    yylval.iconst = -yylval.iconst;
  }
  if (errno == ERANGE) {
    integer_overflow(yytext);
  }
  return tok_int_constant;
}

{dubconstant} {
  yylval.dconst = atof(yytext);
  return tok_dub_constant;
}

{identifier} {
  yylval.id = strdup(yytext);
  return tok_identifier;
}

{st_identifier} {
  yylval.id = strdup(yytext);
  return tok_st_identifier;
}

{literal_begin} {
  char mark = yytext[0];
  std::string result;
  for(;;)
  {
    int ch = yyinput();
    switch (ch) {
      case EOF:
        yyerror("End of file while read string at %d\n", yylineno);
        exit(1);
      case '\n':
        yyerror("End of line while read string at %d\n", yylineno - 1);
        exit(1);
      case '\\':
        ch = yyinput();
        switch (ch) {
          case 'r':
            result.push_back('\r');
            continue;
          case 'n':
            result.push_back('\n');
            continue;
          case 't':
            result.push_back('\t');
            continue;
          case '"':
            result.push_back('"');
            continue;
          case '\'':
            result.push_back('\'');
            continue;
          case '\\':
            result.push_back('\\');
            continue;
          default:
            yyerror("Bad escape character\n");
            return -1;
        }
        break;
      default:
        if (ch == mark) {
          yylval.id = strdup(result.c_str());
          return tok_literal;
        } else {
          result.push_back(ch);
        }
    }
  }
}


{doctext} {
 /* This does not show up in the parse tree. */
 /* Rather, the parser will grab it out of the global. */
  if (g_parse_mode == PROGRAM) {
    clear_doctext();
    g_doctext = strdup(yytext + 3);
    assert(strlen(g_doctext) >= 2);
    g_doctext[strlen(g_doctext) - 2] = ' ';
    g_doctext[strlen(g_doctext) - 1] = '\0';
    g_doctext = clean_up_doctext(g_doctext);
    g_doctext_lineno = yylineno;
    if( (g_program_doctext_candidate == NULL) && (g_program_doctext_status == INVALID)){
      g_program_doctext_candidate = strdup(g_doctext);
      g_program_doctext_lineno = g_doctext_lineno;
      g_program_doctext_status = STILL_CANDIDATE;
      pdebug("%s","program doctext set to STILL_CANDIDATE");
    }
  }
}

. {
  unexpected_token(yytext);
}


. {
  /* Catch-all to let us catch "*" in the parser. */
  return (int) yytext[0];
}

%%

/* vim: filetype=lex
*/
