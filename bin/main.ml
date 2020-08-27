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

open Core
open P4pp

exception ParsingError of string

let preprocess verbose include_dirs defines p4_file () =
  let env = Eval.empty p4_file include_dirs defines in
  let str,_ = Eval.FileSystem.(preprocess env p4_file (load p4_file)) in 
  Printf.printf "%s" str

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-verbose" no_arg ~doc:"Verbose mode"
    +> flag "-I" (listed string) ~doc:"<dir> Add directory to include search path"
    +> flag "-D" (listed string) ~doc:"<macro> Define macro"
    +> anon ("p4file" %:string) in
  Command.basic_spec
    ~summary:"p4pp: P4 preprocessor"
    spec
    (fun verbose includes defines file ->
      let defines = List.map defines ~f:(fun d -> (d,Int64.zero)) in
      preprocess verbose includes defines file)

let () =
  Format.printf "@[";
  Command.run ~version:"0.1.1" command;
  Format.printf "@]"
