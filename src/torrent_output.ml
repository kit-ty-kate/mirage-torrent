type subpiece = {
  off : int;
  len : int;
  mutable initialized : bool;
}

type piece = {
  digest : Digestif.SHA1.t;
  data : bytes;
  subpieces : subpiece list; (* TODO: find a better datatype *)
}

type file = {
  filename : string list;
  pieces : piece list;
}

type t = {
  files : file list;
  pieces : piece array;
}

let chunk_length = int_of_float (2.**14.)

let chunk_piece remaining =
  let rec aux acc off remaining =
    if remaining = 0 then
      acc
    else if remaining < 0 then
      failwith "something went wrong"
    else
      let next_remaining = remaining - chunk_length in
      let initialized = false in
      if next_remaining < 0 then
        {off; len = remaining; initialized} :: acc
      else
        aux ({off; len = chunk_length; initialized} :: acc) (off + chunk_length) next_remaining
  in
  aux [] 0 remaining

let rec chunk_file ~piece_length pieces length_remaining = function
  | [] ->
      (* TODO: do the check in Torrent_file *)
      if Int64.compare length_remaining 0L <> 0 then
        failwith "torrent file broken";
      (pieces, [])
  | digest::xs ->
      let left = Int64.sub length_remaining piece_length in
      let length =
        if Int64.compare left 0L < 0 then
          Int64.to_int length_remaining
        else
          Int64.to_int piece_length
      in
      let subpieces = chunk_piece length in
      let data = Bytes.create length in
      let pieces = {digest; data; subpieces} :: pieces in
      if Int64.compare left 0L < 0 then
        (pieces, xs)
      else
        chunk_file ~piece_length pieces left xs

let create {Torrent_file.piece_length; pieces; length_or_files; name; _} =
  match length_or_files with
  | Length length ->
      let file_pieces, remaining_pieces = chunk_file ~piece_length [] length pieces in
      if remaining_pieces <> [] then
        failwith "something is broken";
      let file_pieces = List.rev file_pieces in
      (* TODO: Find a better conversion *)
      {files = [{filename = [name]; pieces = file_pieces}]; pieces = Array.of_list file_pieces}
  | Files files ->
      let rec aux files pieces remaining_pieces = function
        | [] ->
            if remaining_pieces <> [] then
              failwith "something is wrong";
            {files; pieces = Array.of_list (List.rev pieces)}
        | {Torrent_file.length; path}::xs ->
            let file_pieces, remaining_pieces = chunk_file ~piece_length [] length remaining_pieces in
            aux ({filename = path; pieces} :: files) (file_pieces @ pieces) remaining_pieces xs
      in
      aux [] [] pieces files

let iter_subpieces f {pieces; _} =
  Array.iteri (fun i {subpieces; _} ->
    List.iter (fun {off; len; _} ->
      f i off len
    ) subpieces
  ) pieces

let set_subpiece {pieces; _} ~index ~offset str =
  let piece = pieces.(index) in
  let length = String.length str in
  let subpiece =
    List.find (fun {off; len; _} ->
      Int.equal off offset &&
      Int.equal len length
    ) piece.subpieces
  in
  if subpiece.initialized then
    (* TODO: this should just be ignored *)
    print_endline "trying to overwrite already recieved data";
  Bytes.blit_string str 0 piece.data offset length;
  subpiece.initialized <- true (* TODO: if it is ever made multicore, this should be a mutex *)

let test {pieces; _} =
  if
    Array.for_all (fun {subpieces; _} ->
      List.for_all (fun {initialized; _} ->
        initialized
      ) subpieces
    ) pieces
  then
    print_endline "file has been downloaded!"
  else
    print_endline "downloading..."
