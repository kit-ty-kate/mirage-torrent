let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let () =
  Miou_unix.run (fun () ->
    let torrent = Torrent_file.parse Sys.argv.(1) in
    let tracker_resp = Trackers.request torrent in
    match tracker_resp with
    | Ok tracker_resp -> Trackers.print tracker_resp
    | Error (`Msg msg) -> prerr_endline ("Error: " ^ msg)
  )
