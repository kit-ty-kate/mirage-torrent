let () =
  let torrent = Torrent_file.parse Sys.argv.(1) in
  Torrent_file.print torrent;
  Lwt_main.run (Trackers.request torrent)
