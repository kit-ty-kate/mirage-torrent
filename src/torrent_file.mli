type file = {
  length : int64;
  path : string list;
}

type length_or_files =
  | Length of int64
  | Files of file list

type t = {
  announce : string;
  peer_id : string;
  info_hash : Digestif.SHA1.t;
  name : string;
  piece_length : int64;
  pieces : Digestif.SHA1.t list;
  length_or_files : length_or_files;
}

exception Parse_error

val parse : string -> t

val print : t -> unit
