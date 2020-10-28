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
{
open Lexing
open Parser

exception Error of string

let current_line  = ref 1 
    
type lexer = Line | Test
let lexer = ref Line

let reset () =
  lexer := Line;
  current_line := 1

let incr_line () =
  incr current_line
}

let white = [' ' '\t' '\r']
let whitespace = white+
let opt_whitespace = white*
let non_whitespace = [^' ' '\t' '\r' '\n']+
let identifier = ['_' 'A'-'Z' 'a'-'z']['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let newline = ('\r'?'\n')
let eod = opt_whitespace (newline | eof)

let q_chars = [^ '"' '\n']+
let h_chars = [^ '>' '\n']+
let digits = ['0'-'9']+
let macro_body = [^ '\n']+

rule token = parse
  | "" 
    { match !lexer with 
      | Line -> line lexbuf
      | Test -> test lexbuf }

and line = parse
  | newline
      { incr_line (); TEXT("\n") }
  | "\""
      { STRING(string lexbuf) }
  | "#include" opt_whitespace '"' (q_chars as filename) '"' eod
      { incr_line (); INCLUDE(!current_line, false, filename) }
  | "#include" opt_whitespace '<' (h_chars as filename) '>' eod
      { incr_line (); INCLUDE(!current_line, true, filename) }
  | "#define" opt_whitespace (identifier as macro) whitespace (macro_body as body) eod
      { incr_line (); DEFINE(macro,body) }
  | "#define" opt_whitespace (identifier as macro) eod
      { incr_line (); DEFINE(macro,"") }
  | "#undef" opt_whitespace (identifier as macro) eod
      { incr_line (); UNDEF(macro) }
  | "#ifdef" whitespace (identifier as macro) eod
      { incr_line (); IFDEF(!current_line, macro) }
  | "#ifndef" whitespace (identifier as macro) eod
      { incr_line (); IFNDEF(!current_line, macro) }
  | "#if" 
      { lexer := Test; 
        IF(!current_line) }
  | "#else" eod
      { incr_line (); ELSE(!current_line) }
  | "#endif" eod
      { incr_line (); ENDIF(!current_line) }
  | "#line" opt_whitespace (digits as number) eod
      { current_line := int_of_string number;
        TEXT(lexeme lexbuf)}
  | eof
      { END }
  | whitespace 
      { TEXT(lexeme lexbuf) }
  | non_whitespace
      { TEXT(lexeme lexbuf) }

and string = parse
  | eof
    { raise (Error "File ended while reading a string literal" ) }
  | "\\\""
    { let rest = string lexbuf in
      "\"" ^ rest }
  | '\\' 'n'
    { let rest = string lexbuf in 
      "\n" ^ rest }
  | '\\' '\\'
    { let rest = string lexbuf in
      "\\" ^ rest }
  | '\\' _ as c
    { raise (Error ("Escape sequences not yet supported: \\" ^ c)) }
  | '"'
    { "" }
  | _ as chr
    { let rest = string lexbuf in
      (String.make 1 chr) ^ rest }

and test = parse
  | "defined"
    { DEFINED }
  | digits 
    { INT(Int64.of_string (lexeme lexbuf)) }
  | identifier 
    { IDENT(lexeme lexbuf) }
  | "("
    { LPAREN }
  | ")" 
    { RPAREN}
  | "!"
    { NOT }
  | "&&" 
    { AND }
  | "||"
    { OR }
  | "+" 
    { ADD }
  | "-" 
    { SUB }
  | "*"
    { MULT }
  | "/"
    { DIV }
  | "=="
    { EQ }
  | "!=" 
    { NEQ }
  | "<"
    { LT }
  | ">" 
    { GT }
  | "<="
    { LE }
  | ">=" 
    { GE }
  | "&"
    { BAND }
  | "|" 
    { BOR }
  | "~"
    { BNOT }
  | "<<"
    { BSHL }
  | ">>" 
    { BSHR }
  | "^"
    { BXOR }
  | "\n"
    { lexer := Line; incr_line (); TEXT(lexeme lexbuf) }
  | whitespace
    { test lexbuf }
  | eof 
    { END }
  
{

}
