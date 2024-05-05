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
  Miou_unix.write fd fixed_handshake;
  Miou_unix.write fd peer_id

let read_handshake fd fixed_handshake =
  let len = 68 in
  let buf = Bytes.create len in
  Miou_unix.really_read fd buf ~len;
  if String.equal (Bytes.sub_string buf 0 20) (String.sub fixed_handshake 0 20) &&
     String.equal (Bytes.sub_string buf 28 20) (String.sub fixed_handshake 28 20) then
    (* TODO: we ignore peer_id because it seems unused (compact form) *)
    Bytes.sub_string buf 48 20
  else
    failwith ("?? " ^ Bytes.to_string buf)

let send_keepalive fd =
  print_endline "send keepalive";
  Miou_unix.write fd "\000\000\000\000"

let read_message fd =
  let buf = Bytes.create 4 in
  Miou_unix.really_read fd buf ~len:4;
  let len = Bytes.get_int32_be buf 0 in
  if len = 0l then
    Keepalive
  else
    let len = Int32.to_int len in
    let buf = Bytes.create len in
    Miou_unix.really_read fd buf ~len;
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

let request_the_entire_torrent torrent_output socket =
  Torrent_output.iter_subpieces (fun i off len ->
    (*    Miou_unix.sleep 1.0; *)
    Printf.printf "Request one more...(%d, %d, %d)\n" i off len;
    Miou_unix.write socket "\000\000\000\013\006"; (* NOTE: request *)
    let buf = Bytes.create 12 in
    Bytes.set_int32_be buf 0 (Int32.of_int i);
    Bytes.set_int32_be buf 4 (Int32.of_int off);
    Bytes.set_int32_be buf 8 (Int32.of_int len);
    Miou_unix.write socket (Bytes.unsafe_to_string buf);
    print_endline "request one sent";
  ) torrent_output

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
  let torrent_output = Torrent_output.create torrent_file in
  let send_keepalives =
    Miou.call_cc (fun () ->
      Miou_unix.sleep 1.0;
      Miou_unix.write socket "\000\000\000\001\001"; (* NOTE: unchoke *)
      Miou_unix.sleep 1.0;
      Miou_unix.write socket "\000\000\000\001\002"; (* NOTE: interested *)
      while true do (* TODO: Have a way to stop this program *)
        send_keepalive socket;
        Miou_unix.sleep 120.0;
      done
    )
  in
  let read_messages =
    Miou.call_cc (fun () ->
      while true do
        print_endline "reading message..";
        match read_message socket with
        | Keepalive ->
            print_endline "keepalive";
        | Choke ->
            print_endline "choke";
        | Unchoke ->
            print_endline "unchoke";
        | Interested ->
            print_endline "interested";
        | Not_interested ->
            print_endline "not interested";
        | Have _ ->
            print_endline "have";
        | Bitfield _ ->
            print_endline "bitfield"
        | Request _ ->
            print_endline "request";
        | Piece {index; offset; piece} ->
            print_endline "piece";
            let index = Int32.to_int index in (* TODO: do we need int32 and int64 datatypes? *)
            let offset = Int32.to_int offset in (* TODO: do we need int32 and int64 datatypes? *)
            Torrent_output.set_subpiece torrent_output ~index ~offset piece;
            print_endline "successfully set piece";
        | Cancel _ ->
            print_endline "cancel"
      done
    )
  in
  let check_received =
    Miou.call_cc (fun () ->
      while true do
        Miou_unix.sleep 120.0;
        Torrent_output.test torrent_output;
      done
    )
  in
  let test_requesting =
    Miou.call_cc (fun () ->
      Miou_unix.sleep 5.0;
      print_endline "requesting...";
      request_the_entire_torrent torrent_output socket;
      print_endline "requested...";
    )
  in
  Miou.await_exn read_messages;
  Miou.await_exn send_keepalives;
  Miou.await_exn check_received;
  Miou.await_exn test_requesting
(* TODO: support uploads *)
