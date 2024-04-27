type peer = {
  peer_id : string option;
  ip : Ipaddr.V4.t;
  port : int;
}

type t = {
  interval : int64;
  peers : peer list;
}

exception Request_error

val request : Torrent_file.t -> (t, [`Msg of string]) result
(** NOTE: raises effects *)

val print : t -> unit
