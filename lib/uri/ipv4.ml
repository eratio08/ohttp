module Angstrom = Shaded.Angstrom
module Int = Shaded.Int
module Char = Shaded.Char
module String = Shaded.String
open Angstrom
open Angstrom.Let_syntax

type t = Int.t * Int.t * Int.t * Int.t

let digit_int_parser =
  let%bind d = digit in
  Int.of_char d |> return
;;

let%test_unit "digit_int_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All digit_int_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  let parse_fail s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All digit_int_parser s with
    | Ok _ -> failwith "shoudl fail"
    | Error e -> e
  in
  [%test_result: Int.t] (parse "1") ~expect:1;
  [%test_result: Int.t] (parse "9") ~expect:9;
  [%test_result: String.t] (parse_fail "10") ~expect:": end_of_input"
;;

let one_to_9_parser =
  let is_one_to_9 = function
    | '1' .. '9' -> true
    | _ -> false
  in
  satisfy is_one_to_9
;;

let%test_unit "one_to_9_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All one_to_9_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: Char.t] (parse "1") ~expect:'1';
  [%test_result: Char.t] (parse "9") ~expect:'9'
;;

let zero_to_4_parser =
  let is_zero_to_4 = function
    | '0' .. '4' -> true
    | _ -> false
  in
  satisfy is_zero_to_4
;;

let%test_unit "zero_to_4_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All zero_to_4_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: Char.t] (parse "0") ~expect:'0';
  [%test_result: Char.t] (parse "4") ~expect:'4'
;;

let zero_to_5_parse =
  let is_zero_to_5 = function
    | '0' .. '5' -> true
    | _ -> false
  in
  satisfy is_zero_to_5
;;

let%test_unit "zero_to_5_parse" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All zero_to_5_parse s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: Char.t] (parse "0") ~expect:'0';
  [%test_result: Char.t] (parse "5") ~expect:'5'
;;

let ten_to_99_parser =
  let%bind d1 = one_to_9_parser in
  let%bind d2 = digit_int_parser in
  let i = (Int.of_char d1 * 10) + d2 in
  return i
;;

let%test_unit "ten_to_99_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All ten_to_99_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: Int.t] (parse "10") ~expect:10;
  [%test_result: Int.t] (parse "99") ~expect:99
;;

let hundret_to_199_parser =
  let%bind _ = char '1' in
  let%bind d2 = digit_int_parser in
  let%bind d3 = digit_int_parser in
  let i = 100 + (d2 * 10) + d3 in
  return i
;;

let%test_unit "hundret_to_199_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All hundret_to_199_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: Int.t] (parse "100") ~expect:100;
  [%test_result: Int.t] (parse "199") ~expect:199
;;

let twohundret_to_249_parser =
  let%bind _ = char '2' in
  let%bind d2 = zero_to_4_parser in
  let%bind d3 = digit_int_parser in
  let i = 200 + (Int.of_char d2 * 10) + d3 in
  return i
;;

let%test_unit "twohundret_to_249_parser" =
  let parse s =
    match
      Angstrom.parse_string ~consume:Angstrom.Consume.All twohundret_to_249_parser s
    with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: Int.t] (parse "200") ~expect:200;
  [%test_result: Int.t] (parse "249") ~expect:249
;;

let twohundretfiftee_to_255_parser =
  let%bind _ = char '2' in
  let%bind _ = char '5' in
  let%bind d3 = zero_to_5_parse in
  let i = 250 + Int.of_char d3 in
  return i
;;

let%test_unit "twohundretfiftee_to_255_parser" =
  let parse s =
    match
      Angstrom.parse_string ~consume:Angstrom.Consume.All twohundretfiftee_to_255_parser s
    with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: Int.t] (parse "250") ~expect:250;
  [%test_result: Int.t] (parse "255") ~expect:255
;;

(*
   dec-octet = DIGIT   ; 0-9
   / %x31-39 DIGIT     ; 10-99
   / "1" 2DIGIT        ; 100-199
   / "2" %x30-34 DIGIT ; 200-249
   / "25" %x30-35      ; 250-255
*)
let dec_octet_parser =
  twohundretfiftee_to_255_parser
  <|> twohundret_to_249_parser
  <|> hundret_to_199_parser
  <|> ten_to_99_parser
  <|> digit_int_parser
;;

let%test_unit "dec_octet_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All dec_octet_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: Int.t] (parse "7") ~expect:7;
  [%test_result: Int.t] (parse "66") ~expect:66
;;

(*
   IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
*)
let parser =
  let%bind s1 = dec_octet_parser in
  let%bind s2 = char '.' *> dec_octet_parser in
  let%bind s3 = char '.' *> dec_octet_parser in
  let%bind s4 = char '.' *> dec_octet_parser in
  (s1, s2, s3, s4) |> return
;;

let sexp_of_t (s1, s2, s3, s4) =
  let a s = Sexplib0.Sexp.Atom s
  and l s = Sexplib0.Sexp.List s in
  l
    [ a (Int.to_string s1)
    ; a (Int.to_string s2)
    ; a (Int.to_string s3)
    ; a (Int.to_string s4)
    ]
;;

let%test_unit "parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: t] (parse "0.0.0.0") ~expect:(0, 0, 0, 0);
  [%test_result: t] (parse "1.1.1.1") ~expect:(1, 1, 1, 1);
  [%test_result: t] (parse "9.9.9.9") ~expect:(9, 9, 9, 9);
  [%test_result: t] (parse "99.99.99.99") ~expect:(99, 99, 99, 99);
  [%test_result: t] (parse "255.255.255.255") ~expect:(255, 255, 255, 255)
;;
