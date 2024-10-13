include Angstrom
open Angstrom.Let_syntax

let ( <? ) p fmt =
  p
  >>= fun m ->
  Format.printf fmt m;
  return m
;;

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let whitespace = many (satisfy is_whitespace)

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let alpha = satisfy is_alpha

let is_num = function
  | '0' .. '9' -> true
  | _ -> false
;;

let digit = satisfy is_num
let sp = char '\032'
let htab = char '\009'

let begining_with_alpha p =
  let%bind first_char = alpha in
  let%bind rest = p in
  return (first_char :: rest |> List.to_seq |> String.of_seq)
;;

let quoted p = char '\x22' *> p <* char '\x22' <?> "quoted"

let%test "should parse quoted" =
  let parser =
    quoted
      (many1 (satisfy (fun c -> is_whitespace c || is_alpha c))
       >>| fun cs -> List.to_seq cs |> String.of_seq)
  in
  let parse s =
    Angstrom.parse_string ~consume:Angstrom.Consume.All parser s |> Result.get_ok
  in
  String.equal (parse "\"test test\"") "test test"
;;

let zero_or_one p = p >>| (fun r -> Some r) <|> return None

(*
   HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
*)
let is_hexdig = function
  | '0' .. '9' -> true
  | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false
;;

let hexdig = satisfy is_hexdig

(*
   sub-delims  = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
*)
let is_uri_subdelim = function
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' -> true
  | _ -> false
;;

let uri_subdelim = satisfy is_uri_subdelim

(*
   unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
*)
let is_uri_unreserved = function
  | '-' | '.' | '_' | '~' -> true
  | c -> is_alpha c || is_num c
;;

let uri_unreserved = satisfy is_uri_unreserved

let range f t p =
  let rec loop = function
    | n when n = f -> count f p
    | n -> count n p <|> loop (n - 1)
  in
  loop t
;;
