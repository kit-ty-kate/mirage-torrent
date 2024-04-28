type index = {
  index : int32;
  offset : int32;
  length : int32;
}

type message =
  | Keepalive
  | Choke
  | Unchoke
  | Interested
  | Not_interested
  | Have of int32
  | Bitfield of string
  | Request of index
  | Piece of {index : int32; offset : int32; piece : string}
  | Cancel of index

let bt_header = "\019BitTorrent protocol"
let bt_reserved_bytes = String.make 8 '\000'

let bt_fixed_handshake info_hash =
  bt_header ^
  bt_reserved_bytes ^
  Digestif.SHA1.to_raw_string info_hash

let write_handshake fd fixed_handshake peer_id =
  print_endline "writing...";
  Miou_unix.write_string fd fixed_handshake;
  Miou_unix.write_string fd peer_id

let read_handshake fd fixed_handshake =
  let len = 68 in
  let buf = Bytes.create len in
  Miou_unix.really_read fd buf 0 len;
  if String.equal (Bytes.sub_string buf 0 20) (String.sub fixed_handshake 0 20) &&
     String.equal (Bytes.sub_string buf 28 20) (String.sub fixed_handshake 28 20) then
    (* TODO: we ignore peer_id because it seems unused (compact form) *)
    Bytes.sub_string buf 48 20
  else
    failwith ("?? " ^ Bytes.to_string buf)

let send_keepalive fd =
  Miou_unix.write_string fd "\000\000\000\000"

let read_message fd =
  let buf = Bytes.create 4 in
  Miou_unix.really_read fd buf 0 4;
  let len = Bytes.get_int32_be buf 0 in
  if len = 0l then
    Keepalive
  else
    let len = Int32.to_int len in
    let buf = Bytes.create len in
    Miou_unix.really_read fd buf 0 len;
    let message = Bytes.unsafe_to_string buf in
    match String.unsafe_get message 0 with
    | '\000' -> Choke
    | '\001' -> Unchoke
    | '\002' -> Interested
    | '\003' -> Not_interested
    | '\004' ->
        if len = (1 + 4) then
          Have (String.get_int32_be message 1)
        else
          failwith "unsupported length for Have message"
    | '\005' -> Bitfield (String.sub message 1 (len - 1))
    | '\006' ->
        if len = (1 + 4 + 4 + 4) then
          Request {
            index = String.get_int32_be message 1;
            offset = String.get_int32_be message 5;
            length = String.get_int32_be message 9;
          }
        else
          failwith "unsupported length for Request message"
    | '\007' ->
        if len > (1 + 4 + 4) then
          Piece {
            index = String.get_int32_be message 1;
            offset = String.get_int32_be message 5;
            piece = String.sub message 9 (len - 9);
          }
        else
          failwith "unsupported length for Cancel message"
    | '\008' ->
        if len = (1 + 4 + 4 + 4) then
          Cancel {
            index = String.get_int32_be message 1;
            offset = String.get_int32_be message 5;
            length = String.get_int32_be message 9;
          }
        else
          failwith "unsupported length for Cancel message"
    | _ -> failwith "unknown message"

let talk torrent_file {Trackers.ip; port; _} =
  let socket = Miou_unix.tcpv4 () in
  Fun.protect ~finally:(fun () -> Miou_unix.close socket) @@ fun () ->
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string (Ipaddr.V4.to_string ip), port) in
  Miou_unix.connect socket sockaddr;
  (* NOTE: see http://www.bittorrent.org/beps/bep_0003.html *)
  let fixed_handshake = bt_fixed_handshake torrent_file.Torrent_file.info_hash in
  let write_handshake =
    Miou.call_cc (fun () -> write_handshake socket fixed_handshake torrent_file.Torrent_file.peer_id)
  in
  let read_handshake = Miou.call_cc (fun () -> read_handshake socket fixed_handshake) in
  Miou.await_exn write_handshake;
  let _client_peer_id = Miou.await_exn read_handshake in
  (* TODO: use client_peer_id ? *)
  let send_keepalives =
    Miou.call_cc (fun () ->
      while true do (* TODO: Have a way to stop this program *)
        Miou_unix.sleep 120.0;
        send_keepalive socket;
      done
    )
  in
  let read_messages =
    Miou.call_cc (fun () ->
      while true do
        let _message = read_message socket in
        ignore _message
        (* TODO *)
      done
    )
  in
  Miou.await_exn read_messages;
  Miou.await_exn send_keepalives
(* TODO: support uploads *)
