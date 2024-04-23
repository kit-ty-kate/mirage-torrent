type t

exception Parse_error

val parse : string -> t

val print : t -> unit
