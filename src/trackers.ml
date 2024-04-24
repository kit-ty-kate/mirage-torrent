open Lwt.Infix

let () = Random.self_init ()

(* urlencode function from ocaml-bt *)
(* Copyright (c) 2016-2022 Nicolas Ojeda Bar <n.oje.bar@gmail.com> *)
(* SPDX-License-Identifier: MIT *)
let urlencode s =
  let len = String.length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
    | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '~' | '-' | '.' | '_') as c ->
        Buffer.add_char buf c
    | '+' -> Buffer.add_string buf "%2B"
    | ' ' -> Buffer.add_char buf '+'
    | c -> Printf.bprintf buf "%%%02X" (Char.code c)
  done;
  Buffer.contents buf

let request {Torrent_file.announce; info_hash; _} =
(*  Logs.set_level (Some Logs.Debug);
    Logs.set_reporter (Logs_fmt.reporter ()); *)
  let url =
    announce ^ "?" ^
    String.concat "&" [
      "info_hash=" ^ urlencode (Sha1.to_bin info_hash);
      "peer_id=" ^ urlencode (String.init 20 (fun _ -> Char.chr (Random.int 128)));
      "port=6881";
      "uploaded=0";
      "downloaded=0";
      "left=0";
    ]
  in
  Http_lwt_client.request ~meth:`GET ~follow_redirect:true url (fun _ _ _ ->
    Lwt.return ()
  ) () >>= function
  | Ok _ -> failwith "TODO"
  | Error `Msg msg -> failwith msg
