open Shaded
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

let is_pct_encoded = function
  (* gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@" *)
  | "%3a" | "%2f" | "%3f" | "%23" | "%5b" | "%5d" | "%40" -> true
  (* sub-delims  = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "=" *)
  | "%21" | "%24" | "%26" | "%27" | "%28" | "%29" | "%2a" | "%2b" | "%2c" | "%3b" | "%3d"
    -> true
  | _ -> false
;;

let%test_unit "is_pct_encoded: should return true for percentage encoded characters" =
  (* ' *)
  [%test_result: Bool.t] ~expect:true (is_pct_encoded "%27");
  (* : *)
  [%test_result: Bool.t] ~expect:true (is_pct_encoded "%3a");
  (* p *)
  [%test_result: Bool.t] ~expect:false (is_pct_encoded "%70")
;;

let parser =
  let parser =
    let%bind next = take 3 <?> "next" in
    match is_pct_encoded next with
    | true ->
      (match percent_decode next with
       | None -> fail "not pct encoded"
       | Some c -> return c)
    | false -> fail "not pct encoded"
  in
  parser <?> "Pct_encode.parser"
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: Char.t] (parse "%23") ~expect:'#'
;;
