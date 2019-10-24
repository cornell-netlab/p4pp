open Core
open Ast

type env = 
  { file : string;
    defines : string list }
let empty file =
   { file; defines = [] }
let is_defined env m = 
  List.mem env.defines m ~equal:(=)
let define env m = 
  if is_defined env m then env 
  else { env with defines = m::env.defines }
let undefine env m = 
  { env with 
    defines = List.filter env.defines ~f:(fun n -> n <> m) }
let get_file env = env.file
let set_file env file = { env with file }

let rec eval (includes:string list) (env:env) (buf:Buffer.t) (term:term) : env = 
  match term with 
  | String(s) -> 
     Buffer.add_string buf s;
     env
  | Include(line,search,file) -> 
     let current = get_file env in 
     Buffer.add_string buf (Printf.sprintf "#line \"%s\" %d\n" file 1);
     let env = set_file env file in
     let env = eval_file includes env buf file in
     let env = set_file env current in 
     Buffer.add_string buf "\n";
     Buffer.add_string buf (Printf.sprintf "#line \"%s\" %d\n" current line);
     env
  | Define(m) -> 
     let env = define env m in 
     Buffer.add_string buf "\n";
     env
  | Undef(m) -> 
     let env = undefine env m in 
     Buffer.add_string buf "\n";
     env
  | If(b,m,tru,fls) ->
     let defined = is_defined env m in
     if b && defined || not (b || defined) then
       List.fold_left ~init:env ~f:(fun env term -> eval includes env buf term) tru
     else
       List.fold_left ~init:env ~f:(fun env term -> eval includes env buf term) fls

and eval_file (includes:string list) (env:env) (buf:Buffer.t) (file:string) : env = 
  let in_chan = In_channel.create file in
  let lexbuf = Lexing.from_channel in_chan in
  let terms = 
    try Parser.program Lexer.tokenize lexbuf 
    with _ -> failwith ("Error parsing " ^ file) in 
  List.fold_left ~init:env ~f:(fun env term -> eval includes env buf term) terms


