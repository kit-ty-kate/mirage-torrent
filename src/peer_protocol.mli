type t

val connect : Torrent_file.t -> Torrent_output.t -> Trackers.peer -> t

val unchoke : t -> unit
val interested : t -> unit
val request : t -> int -> int -> int -> unit

val wait : t -> unit

val close : t -> unit
