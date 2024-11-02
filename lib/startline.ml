open Shaded
open Angstrom
open Angstrom.Let_syntax

type t =
  | Requestline of Requestline.t
  | Statusline of Statusline.t

(*
   start-line = request-line / status-line
*)
let parser =
  let request_line_parser =
    let%bind rl = Requestline.parser in
    Requestline rl |> return
  in
  let status_line_parser =
    let%bind sl = Statusline.parser in
    Statusline sl |> return
  in
  request_line_parser <|> status_line_parser
;;
