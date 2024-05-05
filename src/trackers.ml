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

let calculate_length = function
  | Torrent_file.Length len -> len
  | Torrent_file.Files files ->
      List.fold_left (fun acc {Torrent_file.length; _} ->
        Int64.add acc length
      ) 0L files

let request {Torrent_file.announce; peer_id; info_hash; length_or_files; _} =
(*  Logs.set_level (Some Logs.Debug);
    Logs.set_reporter (Logs_fmt.reporter ()); *)
  let url =
    Uri.add_query_params' (Uri.of_string announce) [
      "info_hash", Digestif.SHA1.to_raw_string info_hash;
      "peer_id", peer_id;
      "port", "6881"; (* TODO: change this *)
      "uploaded", "0";
      "downloaded", "0";
      "left", Int64.to_string (calculate_length length_or_files);
      "compact", "1"; (* NOTE: see http://www.bittorrent.org/beps/bep_0023.html *)
    ]
  in
  match
    Httpcats.request ~meth:`GET ~follow_redirect:true ~uri:(Uri.to_string url) ~f:(fun _resp acc body ->
      acc ^ body
    ) ""
  with
  | Ok (_resp, body) ->
      (* TODO: use resp *)
      begin match Bencode.decode (`String body) with
      | Dict body ->
          begin match List.assoc_opt "failure reason" body with
          | Some (String failure_reason) ->
              Error (`Msg failure_reason)
          | Some _ -> raise Request_error
          | None ->
              let interval = List.assoc_opt "interval" body in
              let peers = List.assoc_opt "peers" body in
              match interval, peers with
              | Some (Integer interval), Some (List peers) ->
                  let peers =
                    List.map (function
                      | Bencode.Integer _ -> raise Request_error
                      | Bencode.String _ -> raise Request_error
                      | Bencode.List _ -> raise Request_error
                      | Bencode.Dict peer ->
                          let peer_id = List.assoc_opt "peer id" peer in
                          let ip = List.assoc_opt "ip" peer in
                          let port = List.assoc_opt "port" peer in
                          match ip, port with
                          | Some (String ip), Some (Integer port) ->
                              (* NOTE: in practice "peer id" is optional even in
                                 non-compact representation.
                                 Examples include the debian tracker *)
                              (* see http://www.bittorrent.org/beps/bep_0023.html *)
                              let peer_id = match peer_id with
                                | Some (String peer_id) -> Some peer_id
                                | Some _ -> raise Request_error
                                | None -> None
                              in
                              let ip = Ipaddr.V4.of_string_exn ip in
                              let port = Int64.to_int port in
                              {peer_id; ip; port}
                          | None, _ -> raise Request_error
                          | _, None -> raise Request_error
                          | Some _, Some _ -> raise Request_error
                    ) peers
                  in
                  Ok {interval; peers}
              | Some (Integer interval), Some (String peers) ->
                  let peers =
                    List.init (String.length peers / 6) (fun i ->
                      let off = i * 6 in
                      let ip = Ipaddr.V4.of_octets_exn ~off peers in
                      let port = String.get_uint16_be peers (off + 4) in
                      {peer_id = None; ip; port}
                    )
                  in
                  Ok {interval; peers}
              | Some _, Some _ -> raise Request_error
              | None, _ -> raise Request_error
              | _, None -> raise Request_error
          end
      | Integer _ -> raise Request_error
      | String _ -> raise Request_error
      | List _ -> raise Request_error
      end
  | Error e -> Error (`Msg (Format.asprintf "%a" Httpcats.pp_error e))

let print {interval; peers} =
  print_endline ("interval: " ^ Int64.to_string interval);
  print_endline "peers: ";
  List.iter (fun {peer_id; ip; port} ->
    Option.iter (fun peer_id -> print_endline ("- peer id: " ^ peer_id)) peer_id;
    print_endline ("- ip: " ^ Ipaddr.V4.to_string ip);
    print_endline ("- port: " ^ Int.to_string port);
  ) peers
