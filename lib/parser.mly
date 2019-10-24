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
%token<string> STRING
%token<int * bool * string> INCLUDE
%token<string> DEFINE
%token<string> UNDEF
%token<string> IFDEF
%token<string> IFNDEF
%token ELSE
%token ENDIF
%token NEWLINE

%start <Ast.term list> program

%%

program:
  term* END
  { $1 }
;

term:
  | STRING
      { String($1) }
  | INCLUDE 
      { let line, search, filename = $1 in 
        Include(line, search, filename) }
  | DEFINE 
      { Define($1) }
  | UNDEF 
      { Undef($1) }
  | NEWLINE 
      { String("\n") }
  | IFDEF term* ENDIF
      { If(true, $1, $2, []) } 
  | IFDEF term* ELSE term* ENDIF
      { If(true, $1, $2, []) } 
  | IFNDEF term* ENDIF
      { If(false, $1, $2, []) } 
  | IFNDEF term* ELSE term* ENDIF
      { If(false, $1, $2, []) } 
