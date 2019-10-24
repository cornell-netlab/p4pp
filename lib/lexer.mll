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
    
let reset () =
  current_line := 1

let newline lexbuf =
  incr current_line
}

let whitespace = [' ' '\t' '\r']*

let identifier = ['_' 'A'-'Z' 'a'-'z']['_' 'A'-'Z' 'a'-'z' '0'-'9']

let q_chars = [^ '"' '\n']+
let h_chars = [^ '>' '\n']+

rule tokenize = parse
  | '\n'
      { newline lexbuf; NEWLINE }
  | "#include" whitespace '"' (q_chars as filename) '"' whitespace '\n' 
      { newline lexbuf; INCLUDE(!current_line, false, filename) }
  | "#include" whitespace '<' (h_chars as filename) '>' whitespace '\n' 
      { newline lexbuf; INCLUDE(!current_line, true, filename) }
  | "#define" whitespace '"' (identifier as macro) '"' whitespace '\n' 
      { newline lexbuf; DEFINE(macro) }
  | "#undef" whitespace (identifier as macro) whitespace '\n'
      { newline lexbuf; UNDEF(macro) }
  | "#ifdef" whitespace (identifier as macro) whitespace '\n'
      { newline lexbuf; IFDEF(macro) }
  | "#ifndef" whitespace (identifier as macro) whitespace '\n'
      { newline lexbuf; IFNDEF(macro) }
  | "#else" whitespace '\n'
      { newline lexbuf; ELSE }
  | "#endif" whitespace '\n'
      { newline lexbuf; ENDIF }
  | eof
      { END }
  | _
      { STRING(lexeme lexbuf) }
                 
{

}
