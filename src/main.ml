type ('a, 'b) length_or_files =
  | Length of 'a
  | Files of 'b

type torrent = {
  announce : string;
  name : string;
  piece_length : int64;
  pieces : Sha1.t list;
  length_or_files : (int64, unit) length_or_files;
}

let parse_torrent_file file =
  let bt = Bencode.decode (`File_path file) in
  match bt with
  | Integer _ -> assert false
  | String _ -> assert false
  | List _ -> assert false
  | Dict bt ->
      let announce = List.assoc_opt "announce" bt in
      let info = List.assoc_opt "info" bt in
      match announce, info with
      | None, _ -> assert false
      | _, None -> assert false
      | Some (String announce), Some (Dict info) ->
          let name = List.assoc_opt "name" info in
          let piece_length = List.assoc_opt "piece length" info in
          let pieces = List.assoc_opt "pieces" info in
          let length_or_files =
            match List.assoc_opt "length" info, List.assoc_opt "files" info with
            | None, None -> assert false
            | Some _, Some _ -> assert false
            | Some length, None -> Length length
            | None, Some files -> Files files
          in
          begin match name, piece_length, pieces with
          | None, _, _ -> assert false
          | _, None, _ -> assert false
          | _, _, None -> assert false
          | Some (String name), Some (Integer piece_length), Some (String pieces) ->
              let length_or_files = match length_or_files with
                | Length (Integer length) -> Length length
                | Files (List _) -> failwith "TODO"
                | Length _ -> assert false
                | Files _ -> assert false
              in
              let pieces =
                List.init (String.length pieces / 20) (fun i ->
                  let n = i * 20 in
                  let sha1_bin = String.sub pieces n 20 in
                  Sha1.of_bin (String.to_bytes sha1_bin)
                )
              in
              {
                announce;
                name;
                piece_length;
                pieces;
                length_or_files;
              }
          | Some _, Some _, Some _ -> assert false
          end
      | Some _, Some _ -> assert false

let print_torrent {announce; name; piece_length; pieces; length_or_files} =
  print_endline ("announce: " ^ announce);
  print_endline ("name: " ^ name);
  print_endline ("piece_length: " ^ Int64.to_string piece_length);
  print_endline "pieces:";
  List.iter (fun hash -> print_endline ("- " ^ Sha1.to_hex hash)) pieces;
  match length_or_files with
  | Length length -> print_endline ("length: " ^ Int64.to_string length)
  | Files _ -> failwith "TODO"

let () =
  print_torrent (parse_torrent_file Sys.argv.(1))
