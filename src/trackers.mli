type t

val request : Torrent_file.t -> t
(* NOTE: raises effects *)

val print : t -> unit
