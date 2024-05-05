type t

val create : Torrent_file.t -> t

val iter_subpieces : (int -> int -> int -> unit) -> t -> unit

val set_subpiece : t -> index:int -> offset:int -> string -> unit

val test : t -> unit
