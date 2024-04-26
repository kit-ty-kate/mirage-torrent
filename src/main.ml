let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let getaddrinfo dns =
  {
    Happy.getaddrinfo= (fun record host -> Dns_miou.getaddrinfo dns record host)
  }

let () = Miou_unix.run @@ fun () ->
  let happy, resolver = Happy.stack () in
  let nameservers =
    (`Tcp, [ `Plaintext (Ipaddr.of_string_exn "8.8.8.8", 53) ])
  in
  let dns = Dns_miou.create ~nameservers resolver in
  Happy.inject_resolver ~getaddrinfo:(getaddrinfo dns) resolver;
  let torrent = Torrent_file.parse Sys.argv.(1) in
  let tracker_resp = Trackers.request ~resolver torrent in
  Trackers.print tracker_resp;
  Happy.kill happy
