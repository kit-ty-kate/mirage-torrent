let () =
  let torrent = Torrent_file.parse Sys.argv.(1) in
  let tracker_resp = Miou.run ~domains:1 (fun () -> Trackers.request torrent) in
  Trackers.print tracker_resp
