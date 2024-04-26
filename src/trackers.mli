type t

val request : resolver:Happy.stack -> Torrent_file.t -> t
(* NOTE: raises effects *)

val print : t -> unit
