
type env
val empty : string -> (string * Int64.t) list -> env
val preprocess_string : string list -> env -> Buffer.t -> string -> string -> env
val preprocess_file : string list -> env -> Buffer.t -> string -> env
