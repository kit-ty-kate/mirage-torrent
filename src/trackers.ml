open Lwt.Infix

let () = Random.self_init ()

let request {Torrent_file.announce; info_hash; _} =
(*  Logs.set_level (Some Logs.Debug);
    Logs.set_reporter (Logs_fmt.reporter ()); *)
  let url =
    Uri.add_query_params' (Uri.of_string announce) [
      "info_hash", Sha1.to_bin info_hash;
      "peer_id", String.init 20 (fun _ -> Char.chr (Random.int 128));
      "port", "6881";
      "uploaded", "0";
      "downloaded", "0";
      "left", "0";
    ]
  in
  print_endline (Uri.to_string url);
  Http_lwt_client.request ~meth:`GET ~follow_redirect:true (Uri.to_string url) (fun _ _ _ ->
    Lwt.return ()
  ) () >>= function
  | Ok _ -> failwith "TODO"
  | Error `Msg msg -> failwith msg
