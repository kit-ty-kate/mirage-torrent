let () =
  let torrent = Torrent_file.parse Sys.argv.(1) in
  let tracker_resp = Lwt_main.run (Trackers.request torrent) in
  Trackers.print tracker_resp
