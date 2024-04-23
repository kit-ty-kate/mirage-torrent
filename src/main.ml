let () =
  Torrent_file.print (Torrent_file.parse Sys.argv.(1))
