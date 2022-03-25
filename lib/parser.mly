(* Copyright 2019-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License.
 *)

%{
open Core
open Ast
%}

%token END
%token<string> TEXT
%token<string> STRING
%token<int * bool * string> INCLUDE
%token<string * string> DEFINE
%token<string> UNDEF
%token<int * string> IFDEF IFNDEF
%token<int> IF
%token<int> ELSE ENDIF
%token<Int64.t> INT
%token<string> IDENT
%token DEFINED LPAREN RPAREN
%token AND OR NOT
%token ADD SUB MULT DIV
%token EQ NEQ GT LT GE LE
%token BOR BAND BNOT BSHL BSHR BXOR

%left OR
%left AND
%left EQ NEQ
%left LT GT
%left LE GE
%left BOR
%left BXOR
%left BAND
%left BSHL BSHR
%left ADD SUB
%left MULT DIV
%nonassoc NOT
%nonassoc BNOT

%start <Ast.term list> program

%%

program:
  term* END
  { $1 }
;

term:
  | STRING
      { String($1) }
  | TEXT
      { Text($1) }
  | INCLUDE
      { let line, search, filename = $1 in
        Include(line, search, filename) }
  | DEFINE
      { let macro,body = $1 in 
        Define(macro,body) }
  | UNDEF
      { Undef($1) }
  | IFDEF term* ENDIF
      { let line1, macro = $1 in
        let line2 = $3 in
        IfDef(macro, line1, $2, line2, [], line2) }
  | IFDEF term* ELSE term* ENDIF
      { let line1, macro = $1 in
        let line2 = $3 in
        let line3 = $5 in
        IfDef(macro, line1, $2, line2, $4, line3) }
  | IFNDEF term* ENDIF
      { let line1, macro = $1 in
        let line2 = $3 in
        IfNDef(macro, line1, $2, line2, [], line2) }
  | IFNDEF term* ELSE term* ENDIF
      { let line1, macro = $1 in
        let line2 = $3 in
        let line3 = $5 in
        IfNDef(macro, line1, $2, line2, $4, line3) }
  | IF test term* ENDIF
      { let line1 = $1 in
        let line2 = $4 in
        If($2, line1, $3, line2, [], line2) }
  | IF test term* ELSE term* ENDIF
      { let line1 = $1 in
        let line2 = $4 in
        let line3 = $6 in
        If($2, line1, $3, line2, $5, line3) }

test:
  | INT
    { Int($1) }
  | IDENT
    { Ident($1) }
  | DEFINED LPAREN IDENT RPAREN
    { Defined($3) }
  | test ADD test
    { BinOp($1, Add, $3) }
  | test SUB test
    { BinOp($1, Sub, $3) }
  | test MULT test
    { BinOp($1, Mult, $3) }
  | test DIV test
    { BinOp($1, Div, $3) }
  | test EQ test
    { BinOp($1, Eq, $3) }
  | test NEQ test
    { BinOp($1, Neq, $3) }
  | test GT test
    { BinOp($1, Gt, $3) }
  | test LT test
    { BinOp($1, Lt, $3) }
  | test LE test
    { BinOp($1, Le, $3) }
  | test GE test
    { BinOp($1, Ge, $3) }
  | test AND test
    { BinOp($1, And, $3) }
  | test OR test
    { BinOp($1, Or, $3) }
  | NOT test
    { UnOp(Not, $2) }
  | test BAND test
    { BinOp($1, BAnd, $3) }
  | test BOR test
    { BinOp($1, BOr, $3) }
  | BNOT test
    { UnOp(BNot, $2) }
  | test BSHL test
    { BinOp($1, BShl, $3) }
  | test BSHR test
    { BinOp($1, BShr, $3) }
  | test BXOR test
    { BinOp($1, BXor, $3) }
  | LPAREN test RPAREN
    { $2 }
