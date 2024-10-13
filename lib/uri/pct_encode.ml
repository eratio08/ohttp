module Angstrom = Shaded.Angstrom
module String = Shaded.String
module Int = Shaded.Int
open Angstrom
open Angstrom.Let_syntax

(*
   https://datatracker.ietf.org/doc/html/rfc3986#section-2.1
*)
let percent_encode = function
  | ( ':'
    | '|'
    | '?'
    | '#'
    | '['
    | ']'
    | '@'
    | '!'
    | '$'
    | '&'
    | '\''
    | '('
    | ')'
    | '*'
    | '+'
    | ','
    | ';'
    | '=' ) as c ->
    let code = int_of_char c in
    (match Int.to_hex code with
     | Some hex_diggits -> "%" ^ hex_diggits
     | None -> Char.escaped c)
  | c -> Char.escaped c
;;

let%test_unit "percent_encode: should encode" =
  [%test_result: String.t] ~expect:"%21" (percent_encode '!');
  [%test_result: String.t] ~expect:"p" (percent_encode 'p')
;;

let percent_decode s =
  if String.length s <> 3 && String.sub s 0 1 <> "%"
  then None
  else (
    let hex_diggits = String.sub s 1 2 in
    Int.of_hex hex_diggits |> Option.map char_of_int)
;;

let is_uri_unreserved = function
  | '-' | '.' | '_' | '~' -> true
  | c when is_alpha c || is_num c -> true
  | _ -> false
;;

let uri_unreserved = satisfy is_uri_unreserved

(*
   pct-encoded = "%" HEXDIG HEXDIG
   HEXDIG      =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
*)
let uri_pct_encoded =
  let%bind first = char '%' *> hexdig in
  let%bind second = hexdig in
  let endoded_str = List.to_seq [ first; second ] |> String.of_seq in
  let decoded = percent_decode endoded_str in
  match decoded with
  | None -> fail ("Invalid percentage encoded string " ^ endoded_str)
  | Some c -> return c
;;
