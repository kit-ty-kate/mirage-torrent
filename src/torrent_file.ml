type file = {
  length : int64;
  path : string list;
}

type length_or_files =
  | Length of int64
  | Files of file list

type t = {
  announce : string;
  info_hash : Sha1.t;
  name : string;
  piece_length : int64;
  pieces : Sha1.t list;
  length_or_files : length_or_files;
}

exception Parse_error

let parse file =
  let bt = Bencode.decode (`File_path file) in
  match bt with
  | Integer _ -> raise Parse_error
  | String _ -> raise Parse_error
  | List _ -> raise Parse_error
  | Dict bt ->
      let announce = List.assoc_opt "announce" bt in
      let info = List.assoc_opt "info" bt in
      match announce, info with
      | None, _ -> raise Parse_error
      | _, None -> raise Parse_error
      | Some (String announce), Some (Dict info as orig) ->
          let info_hash =
            (* TODO: we could be more efficient if the bencode library kept around the original *)
            (* TODO: this is also discouraged by the spec *)
            let buf = Buffer.create 1024 in
            Bencode.encode (`Buffer buf) orig;
            Sha1.string (Buffer.contents buf)
          in
          let name = List.assoc_opt "name" info in
          let piece_length = List.assoc_opt "piece length" info in
          let pieces = List.assoc_opt "pieces" info in
          let length_or_files =
            match List.assoc_opt "length" info, List.assoc_opt "files" info with
            | None, None -> raise Parse_error
            | Some _, Some _ -> raise Parse_error
            | Some (Integer length), None -> Length length
            | None, Some (List files) ->
                let files =
                  List.map (function
                    | Bencode.Integer _ -> raise Parse_error
                    | Bencode.String _ -> raise Parse_error
                    | Bencode.List _ -> raise Parse_error
                    | Bencode.Dict file ->
                        let length = List.assoc_opt "length" file in
                        let path = List.assoc_opt "path" file in
                        match length, path with
                        | None, _ -> raise Parse_error
                        | _, None -> raise Parse_error
                        | Some (Integer length), Some (List path) ->
                            let path =
                              List.map (function
                                | Bencode.Integer _ -> raise Parse_error
                                | Bencode.String s -> s
                                | Bencode.List _ -> raise Parse_error
                                | Bencode.Dict _ -> raise Parse_error
                              ) path
                            in
                            {length; path}
                        | Some _, Some _ -> raise Parse_error
                  ) files
                in
                Files files
            | Some _, None -> raise Parse_error
            | None, Some _ -> raise Parse_error
          in
          begin match name, piece_length, pieces with
          | None, _, _ -> raise Parse_error
          | _, None, _ -> raise Parse_error
          | _, _, None -> raise Parse_error
          | Some (String name), Some (Integer piece_length), Some (String pieces) ->
              let pieces =
                List.init (String.length pieces / 20) (fun i ->
                  let n = i * 20 in
                  let sha1_bin = String.sub pieces n 20 in
                  Sha1.of_bin (String.to_bytes sha1_bin)
                )
              in
              {
                announce;
                info_hash;
                name;
                piece_length;
                pieces;
                length_or_files;
              }
          | Some _, Some _, Some _ -> raise Parse_error
          end
      | Some _, Some _ -> raise Parse_error

let print {announce; info_hash; name; piece_length; pieces; length_or_files} =
  print_endline ("announce: " ^ announce);
  print_endline ("info_hash: " ^ Sha1.to_hex info_hash);
  print_endline ("name: " ^ name);
  print_endline ("piece length: " ^ Int64.to_string piece_length);
  print_endline "pieces:";
  List.iter (fun hash -> print_endline ("- " ^ Sha1.to_hex hash)) pieces;
  match length_or_files with
  | Length length ->
      print_endline ("length: " ^ Int64.to_string length);
  | Files files ->
      List.iter (fun {length; path} ->
        print_endline (String.concat "/" path ^ ": " ^ Int64.to_string length);
      ) files
