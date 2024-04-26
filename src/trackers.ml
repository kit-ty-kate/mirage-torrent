open Lwt.Infix

let () = Random.self_init ()

type peer = {
  peer_id : string option;
  ip : Ipaddr.V4.t;
  port : int;
}

type t = {
  interval : int64;
  peers : peer list;
}

let request {Torrent_file.announce; info_hash; _} =
(*  Logs.set_level (Some Logs.Debug);
    Logs.set_reporter (Logs_fmt.reporter ()); *)
  let url =
    Uri.add_query_params' (Uri.of_string announce) [
      "info_hash", Digestif.SHA1.to_raw_string info_hash;
      "peer_id", String.init 20 (fun _ -> Char.chr (Random.int 128));
      "port", "6881";
      "uploaded", "0";
      "downloaded", "0";
      "left", "0";
      "compact", "1"; (* NOTE: see http://www.bittorrent.org/beps/bep_0023.html *)
    ]
  in
  Http_lwt_client.request ~meth:`GET ~follow_redirect:true (Uri.to_string url) (fun _resp acc str ->
    Lwt.return (acc ^ str)
  ) "" >>= function
  | Ok (_resp, body) ->
      begin match Bencode.decode (`String body) with
      | Dict body ->
          begin match List.assoc_opt "failure reason" body with
          | Some (String failure_reason) ->
              failwith failure_reason
          | Some _ -> failwith "wrong failure reason type"
          | None ->
              let interval = List.assoc_opt "interval" body in
              let peers = List.assoc_opt "peers" body in
              match interval, peers with
              | Some (Integer interval), Some (List peers) ->
                  let peers =
                    List.map (function
                      | Bencode.Integer _ -> failwith "a"
                      | Bencode.String _ -> failwith "b"
                      | Bencode.List _ -> failwith "c"
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
                                | Some _ -> failwith "d"
                                | None -> None
                              in
                              let ip = Ipaddr.V4.of_string_exn ip in
                              let port = Int64.to_int port in
                              {peer_id; ip; port}
                          | None, _ -> failwith "e"
                          | _, None -> failwith "f"
                          | Some _, Some _ -> failwith "g"
                    ) peers
                  in
                  Lwt.return {interval; peers}
              | Some (Integer interval), Some (String peers) ->
                  let peers =
                    List.init (String.length peers / 6) (fun i ->
                      let n = i * 6 in
                      let ip = Ipaddr.V4.of_octets_exn (String.sub peers n 4) in
                      let port = String.get_uint16_be peers 4 in
                      {peer_id = None; ip; port}
                    )
                  in
                  Lwt.return {interval; peers}
              | Some _, Some _ -> failwith "h"
              | None, _ -> failwith "i"
              | _, None -> failwith "j"
          end
      | Integer _ -> failwith "integer"
      | String _ -> failwith "string"
      | List _ -> failwith "list"
      end
  | Error `Msg msg -> failwith msg

let print {interval; peers} =
  print_endline ("interval: " ^ Int64.to_string interval);
  print_endline "peers: ";
  List.iter (fun {peer_id; ip; port} ->
    Option.iter (fun peer_id -> print_endline ("- peer id: " ^ peer_id)) peer_id;
    print_endline ("- ip: " ^ Ipaddr.V4.to_string ip);
    print_endline ("- port: " ^ Int.to_string port);
  ) peers
