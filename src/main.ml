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
        let torrent_output = Torrent_output.create torrent in
        let proto = Peer_protocol.connect torrent torrent_output peer in
        Peer_protocol.unchoke proto;
        Peer_protocol.interested proto;
        Torrent_output.iter_subpieces (Peer_protocol.request proto) torrent_output;
        let test =
          Miou.async (fun () ->
            let finished = ref false in
            while not !finished do
              Miou_unix.sleep 5.0;
              finished := Torrent_output.checksum torrent_output;
            done
          )
        in
        Miou.await_exn test;
        print_endline "Finished downloading.";
        Peer_protocol.close proto
    | Error (`Msg msg) -> prerr_endline ("Error: " ^ msg)
  )
