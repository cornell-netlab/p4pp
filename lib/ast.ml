type term = 
  | String of string
  | Include of int * bool * string
  | Define of string
  | Undef of string
  | If of bool * string * term list * term list
