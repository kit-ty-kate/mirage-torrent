let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let test_ip =
  let fd = Stdlib.open_in "test_ip" in
  let res = Stdlib.input_line fd in
  Stdlib.close_in fd;
  res

let () =
  Miou_unix.run (fun () ->
    let torrent = Torrent_file.parse Sys.argv.(1) in
    let tracker_resp = Trackers.request torrent in
    match tracker_resp with
    | Ok tracker_resp ->
        (* TODO: use more peers *)
        (* TODO: use interval *)
        let peer = List.find (fun peer -> String.equal (Ipaddr.V4.to_string peer.Trackers.ip) test_ip) tracker_resp.Trackers.peers in
        Peer_protocol.talk torrent peer
    | Error (`Msg msg) -> prerr_endline ("Error: " ^ msg)
  )
