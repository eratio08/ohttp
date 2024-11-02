module Result = Result_sh
module String = String_sh
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

let word =
  let%bind fst = alpha in
  let%bind rest = many_till (alpha <|> digit) sp in
  fst :: rest |> String_sh.of_list |> return
;;

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
  let parse s = parse_string ~consume:Consume.All parser s |> Result.get_ok in
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

let uri_subdelim = satisfy is_uri_subdelim <?> "uri_subdelim"

(*
   unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
*)
let is_uri_unreserved = function
  | '-' | '.' | '_' | '~' -> true
  | c -> is_alpha c || is_num c
;;

let uri_unreserved = satisfy is_uri_unreserved <?> "uri_unreserved"

let is_http_obs_text = function
  | '\x80' .. '\xFF' -> true
  | _ -> false
;;

(*
   obs-text = %x80-FF
*)
let http_obs_text = satisfy is_http_obs_text

let is_vchar = function
  | '\x21' .. '\x7E' -> true
  | _ -> false
;;

(*
   VCHAR = %x21-7E ; according to RFC5234
*)
let vchar = satisfy is_vchar

let range f t p =
  match f, t with
  | f, t when f > t -> failwith "from can not be smaller than to"
  | f, t when f < 0 || t < 0 -> failwith "Range can not be nagative"
  | _ ->
    let rec loop = function
      | n when n = f -> count f p
      | n -> count n p <|> loop (n - 1)
    in
    loop t <?> Format.sprintf "range %d %d" f t
;;

let maybe p =
  let%bind m = range 0 1 p in
  match m with
  | [] -> return None
  | a :: _ -> return (Some a)
;;

let parse_res p s = Angstrom.parse_string ~consume:Angstrom.Consume.All p s
let parse_res_prefix p s = Angstrom.parse_string ~consume:Angstrom.Consume.Prefix p s

let parse p s =
  match parse_res p s with
  | Ok r -> r
  | Error e -> failwith e
;;

let%test_unit "range" =
  let module Char = Char_sh in
  let module List = List_sh in
  let parser = range 0 3 (char 'a') in
  [%test_result: (Char.t List.t, String.t) Result.t]
    (parse_res parser "aaa")
    ~expect:(Ok [ 'a'; 'a'; 'a' ]);
  [%test_result: Char.t List.t] (parse parser "aa") ~expect:[ 'a'; 'a' ];
  [%test_result: Char.t List.t] (parse parser "a") ~expect:[ 'a' ];
  [%test_result: Char.t List.t] (parse parser "") ~expect:[];
  let parser = range 0 1 (char 'a') in
  [%test_result: Char.t List.t] (parse parser "a") ~expect:[ 'a' ];
  [%test_result: Char.t List.t] (parse parser "") ~expect:[]
;;

let%test_unit "uri_unreserved" =
  let module Char = Char_sh in
  let module String = String_sh in
  let module Result = Result_sh in
  let parse = parse uri_unreserved in
  [%test_result: Char.t] (parse "a") ~expect:'a';
  [%test_result: Char.t] (parse "_") ~expect:'_';
  let parse = parse_res uri_unreserved in
  [%test_result: (Char.t, String.t) Result.t]
    (parse ":")
    ~expect:(Error "uri_unreserved: satisfy: ':'");
  [%test_result: (Char.t, String.t) Result.t]
    (parse "")
    ~expect:(Error "uri_unreserved: not enough input")
;;

let%test_unit "uri_subdelim" =
  let module Char = Char_sh in
  let module String = String_sh in
  let module Result = Result_sh in
  let parse = parse uri_subdelim in
  [%test_result: Char.t] (parse "(") ~expect:'(';
  let parse = parse_res uri_subdelim in
  [%test_result: (Char.t, String.t) Result.t]
    (parse ":")
    ~expect:(Error "uri_subdelim: satisfy: ':'");
  [%test_result: (Char.t, String.t) Result.t]
    (parse "")
    ~expect:(Error "uri_subdelim: not enough input")
;;
