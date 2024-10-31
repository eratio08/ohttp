open Shaded
open Angstrom
open Angstrom.Let_syntax

type h16 = Int.t

(*
   h16 = 1*4HEXDIG ; 16 bits of address represented in hexadecimal
*)
let h16_parser =
  let%bind digits = range 1 4 hexdig in
  match Int.of_hex_char_list digits with
  | Some i -> return i
  | None ->
    fail ("unable to convert '" ^ (digits |> List.to_seq |> String.of_seq) ^ "' into int")
;;

let%test_unit "h16_parser" =
  let parse = parse h16_parser in
  [%test_result: Int.t] (parse "F") ~expect:15;
  [%test_result: Int.t] (parse "ff") ~expect:255
;;

type ls32 =
  | LS32 of (Int.t * Int.t)
  | IPv4 of (Int.t * Int.t * Int.t * Int.t)

let sexp_of_ls32 =
  let open Sexp in
  function
  | LS32 (s1, s2) -> l [ a "LS32"; a (Int.to_string s1); a (Int.to_string s2) ]
  | IPv4 (s1, s2, s3, s4) ->
    l
      [ a "IPv4"
      ; a (Int.to_string s1)
      ; a (Int.to_string s2)
      ; a (Int.to_string s3)
      ; a (Int.to_string s4)
      ]
;;

let compare_ls32 t1 t2 =
  match t1, t2 with
  | LS32 (s11, s12), LS32 (s21, s22) -> Int.compare s11 s21 + Int.compare s12 s22
  | IPv4 (s11, s12, s13, s14), IPv4 (s21, s22, s23, s24) ->
    Int.compare s11 s21 + Int.compare s12 s22 + Int.compare s13 s23 + Int.compare s14 s24
  | _ -> Int.min_int
;;

(*
   ls32 = ( h16 ":" h16 ) / IPv4address ; least-significant 32 bits of address
*)
let ls32_parser =
  let h16_parser =
    let%bind h16_1 = h16_parser in
    let%bind h16_2 = char ':' *> h16_parser in
    LS32 (h16_1, h16_2) |> return
  in
  let ipv4_parser =
    let%bind ipv4 = Ipv4.parser in
    IPv4 ipv4 |> return
  in
  h16_parser <|> ipv4_parser
;;

let%test_unit "ls32_parser" =
  let parse = parse ls32_parser in
  [%test_result: ls32] (parse "1.1.1.1") ~expect:(IPv4 (1, 1, 1, 1));
  [%test_result: ls32] (parse "255.255.255.255") ~expect:(IPv4 (255, 255, 255, 255));
  [%test_result: ls32] (parse "ffff:ffff") ~expect:(LS32 (65535, 65535))
;;

type t =
  | IPv6 of (int * int * int * int * int * int * int * int)
  | IPv6v4 of (int * int * int * int * int * int * (int * int * int * int))

let sexp_of_t =
  let open Sexp in
  let ia = Int.sexp_of_t in
  function
  | IPv6 (s1, s2, s3, s4, s5, s6, s7, s8) ->
    l [ ia s1; ia s2; ia s3; ia s4; ia s5; ia s6; ia s7; ia s8 ]
  | IPv6v4 (s1, s2, s3, s4, s5, s6, (ip1, ip2, ip3, ip4)) ->
    l [ ia s1; ia s2; ia s3; ia s4; ia s5; ia s6; l [ ia ip1; ia ip2; ia ip3; ia ip4 ] ]
;;

(*
   IPv6address = 6( h16 ":" ) ls32
*)
let ipv6_128_parser =
  let parser =
    let%bind segments = count 6 (h16_parser <* char ':') in
    let%bind ls32 = ls32_parser in
    let ipv6 =
      match segments, ls32 with
      | [ s1; s2; s3; s4; s5; s6 ], LS32 (s7, s8) -> IPv6 (s1, s2, s3, s4, s5, s6, s7, s8)
      | [ s1; s2; s3; s4; s5; s6 ], IPv4 ipv4 -> IPv6v4 (s1, s2, s3, s4, s5, s6, ipv4)
      | _ -> failwith "Angstrom.count has a bug"
    in
    return ipv6
  in
  parser <?> "ipv6_128_parser"
;;

let%test_unit "ipv6_128_parser" =
  let parse = parse ipv6_128_parser in
  [%test_result: t] (parse "1:1:1:1:1:1:1:1") ~expect:(IPv6 (1, 1, 1, 1, 1, 1, 1, 1));
  [%test_result: t]
    (parse "f:f:f:f:f:f:f:f")
    ~expect:(IPv6 (15, 15, 15, 15, 15, 15, 15, 15));
  [%test_result: t]
    (parse "1:1:1:1:1:1:3.3.3.3")
    ~expect:(IPv6v4 (1, 1, 1, 1, 1, 1, (3, 3, 3, 3)));
  [%test_result: t]
    (parse "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")
    ~expect:(IPv6 (65535, 65535, 65535, 65535, 65535, 65535, 65535, 65535))
;;

(*
   "::" 5( h16 ":" ) ls32
*)
let ipv6_112_parser =
  let parser =
    let%bind segments = string "::" *> count 5 (h16_parser <* char ':') in
    let%bind ls32 = ls32_parser in
    let ipv6 =
      match segments, ls32 with
      | [ s1; s2; s3; s4; s5 ], LS32 (s6, s7) -> IPv6 (s1, s2, s3, s4, s5, 0, s6, s7)
      | [ s1; s2; s3; s4; s5 ], IPv4 ipv4 -> IPv6v4 (s1, s2, s3, s4, s5, 0, ipv4)
      | _ -> failwith "Angstrom.count has a bug"
    in
    return ipv6
  in
  parser <?> "ipv6_112_parser"
;;

let%test_unit "ipv6_112_parser" =
  let parse = parse ipv6_112_parser in
  [%test_result: t] (parse "::1:1:1:1:1:1:1") ~expect:(IPv6 (1, 1, 1, 1, 1, 0, 1, 1));
  [%test_result: t]
    (parse "::1:1:1:1:1:3.3.3.3")
    ~expect:(IPv6v4 (1, 1, 1, 1, 1, 0, (3, 3, 3, 3)));
  [%test_result: t]
    (parse "::ffff:ffff:ffff:ffff:ffff:ffff:ffff")
    ~expect:(IPv6 (65535, 65535, 65535, 65535, 65535, 0, 65535, 65535))
;;

(*
   [ h16 ] "::" 4( h16 ":" ) ls32
*)
let ipv6_96_pre_16_parser =
  let parser =
    let%bind prefix = maybe h16_parser in
    let%bind segments = string "::" *> count 4 (h16_parser <* char ':') in
    let%bind ls32 = ls32_parser in
    let ipv6 =
      match prefix, segments, ls32 with
      | None, [ s1; s2; s3; s4 ], LS32 (s5, s6) -> IPv6 (0, 0, s1, s2, s3, s4, s5, s6)
      | Some p1, [ s1; s2; s3; s4 ], LS32 (s5, s6) -> IPv6 (p1, 0, s1, s2, s3, s4, s5, s6)
      | None, [ s1; s2; s3; s4 ], IPv4 ipv4 -> IPv6v4 (0, 0, s1, s2, s3, s4, ipv4)
      | Some p1, [ s1; s2; s3; s4 ], IPv4 ipv4 -> IPv6v4 (p1, 0, s1, s2, s3, s4, ipv4)
      | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
    in
    return ipv6
  in
  parser <?> "ipv6_96_pre_16_parser"
;;

let%test_unit "ipv6_96_pre_16_parser" =
  let parse = parse ipv6_96_pre_16_parser in
  [%test_result: t] (parse "1::1:1:1:1:1:1") ~expect:(IPv6 (1, 0, 1, 1, 1, 1, 1, 1));
  [%test_result: t]
    (parse "::1:1:1:1:3.3.3.3")
    ~expect:(IPv6v4 (0, 0, 1, 1, 1, 1, (3, 3, 3, 3)));
  [%test_result: t]
    (parse "ffff::ffff:ffff:ffff:ffff:ffff:ffff")
    ~expect:(IPv6 (65535, 0, 65535, 65535, 65535, 65535, 65535, 65535))
;;

(*
   h16 ":" ; fails if next char is ':'
*)
let h16_pre_parser =
  let parser =
    let%bind h16 = h16_parser <* char ':' in
    let%bind next = peek_char_fail in
    if next = ':' then fail "" else return h16
  in
  parser <?> "h16_pre_parser"
;;

(*
   [ *<n>( h16 ":" ) h16 ]
*)
let prefix_parser n =
  let parser =
    let%bind preprefix = range 0 n h16_pre_parser in
    let%bind prefix = h16_parser in
    preprefix @ [ prefix ] |> return
  in
  parser <?> "prefix_parser"
;;

(*
   [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
*)
let ipv6_80_pre_32_parser =
  let parser =
    let%bind prefix = range 0 1 (prefix_parser 1) >>| List.flatten in
    let%bind segments = string "::" *> count 3 (h16_parser <* char ':') in
    let%bind ls32 = ls32_parser in
    let ipv6 =
      match prefix, segments, ls32 with
      | [], [ s1; s2; s3 ], LS32 (s4, s5) -> IPv6 (0, 0, 0, s1, s2, s3, s4, s5)
      | [ p1 ], [ s1; s2; s3 ], LS32 (s4, s5) -> IPv6 (0, p1, 0, s1, s2, s3, s4, s5)
      | [ pp1; p1 ], [ s1; s2; s3 ], LS32 (s4, s5) -> IPv6 (pp1, p1, 0, s1, s2, s3, s4, s5)
      | [], [ s1; s2; s3 ], IPv4 ipv4 -> IPv6v4 (0, 0, 0, s1, s2, s3, ipv4)
      | [ p1 ], [ s1; s2; s3 ], IPv4 ipv4 -> IPv6v4 (0, p1, 0, s1, s2, s3, ipv4)
      | [ pp1; p1 ], [ s1; s2; s3 ], IPv4 ipv4 -> IPv6v4 (pp1, p1, 0, s1, s2, s3, ipv4)
      | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
    in
    return ipv6
  in
  parser <?> "ipv6_80_pre_32_parser"
;;

let%test_unit "ipv6_80_pre_32_parser" =
  let parse = parse ipv6_80_pre_32_parser in
  [%test_result: t] (parse "::1:1:1:1:1") ~expect:(IPv6 (0, 0, 0, 1, 1, 1, 1, 1));
  [%test_result: t] (parse "1::1:1:1:1:1") ~expect:(IPv6 (0, 1, 0, 1, 1, 1, 1, 1));
  [%test_result: t] (parse "1:1::1:1:1:1:1") ~expect:(IPv6 (1, 1, 0, 1, 1, 1, 1, 1));
  [%test_result: t]
    (parse "::1:1:1:3.3.3.3")
    ~expect:(IPv6v4 (0, 0, 0, 1, 1, 1, (3, 3, 3, 3)));
  [%test_result: t]
    (parse "ffff::ffff:ffff:ffff:ffff:ffff")
    ~expect:(IPv6 (0, 65535, 0, 65535, 65535, 65535, 65535, 65535))
;;

(*
   [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
*)
let ipv6_64_pre_48_parser =
  let parser =
    let%bind prefix = range 0 1 (prefix_parser 2) >>| List.flatten in
    let%bind segments = string "::" *> count 2 (h16_parser <* char ':') in
    let%bind ls32 = ls32_parser in
    let ipv6 =
      match prefix, segments, ls32 with
      | [], [ s1; s2 ], LS32 (s3, s4) -> IPv6 (0, 0, 0, 0, s1, s2, s3, s4)
      | [ p1 ], [ s1; s2 ], LS32 (s3, s4) -> IPv6 (0, 0, p1, 0, s1, s2, s3, s4)
      | [ pp1; p1 ], [ s1; s2 ], LS32 (s3, s4) -> IPv6 (0, pp1, p1, 0, s1, s2, s3, s4)
      | [ pp1; pp2; p1 ], [ s1; s2 ], LS32 (s3, s4) ->
        IPv6 (pp1, pp2, p1, 0, s1, s2, s3, s4)
      | [], [ s1; s2 ], IPv4 ipv4 -> IPv6v4 (0, 0, 0, 0, s1, s2, ipv4)
      | [ p1 ], [ s1; s2 ], IPv4 ipv4 -> IPv6v4 (0, 0, p1, 0, s1, s2, ipv4)
      | [ pp1; p1 ], [ s1; s2 ], IPv4 ipv4 -> IPv6v4 (0, pp1, p1, 0, s1, s2, ipv4)
      | [ pp1; pp2; p1 ], [ s1; s2 ], IPv4 ipv4 -> IPv6v4 (pp1, pp2, p1, 0, s1, s2, ipv4)
      | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
    in
    return ipv6
  in
  parser <?> "ipv6_64_pre_48_parser"
;;

let%test_unit "ipv6_64_pre_48_parser" =
  let parse = parse ipv6_64_pre_48_parser in
  [%test_result: t] (parse "::1:1:1:1") ~expect:(IPv6 (0, 0, 0, 0, 1, 1, 1, 1));
  [%test_result: t] (parse "1::1:1:1:1") ~expect:(IPv6 (0, 0, 1, 0, 1, 1, 1, 1));
  [%test_result: t] (parse "1:1::1:1:1:1") ~expect:(IPv6 (0, 1, 1, 0, 1, 1, 1, 1));
  [%test_result: t] (parse "1:1:1::1:1:1:1") ~expect:(IPv6 (1, 1, 1, 0, 1, 1, 1, 1));
  [%test_result: t]
    (parse "::1:1:3.3.3.3")
    ~expect:(IPv6v4 (0, 0, 0, 0, 1, 1, (3, 3, 3, 3)));
  [%test_result: t]
    (parse "ffff::ffff:ffff:ffff:ffff")
    ~expect:(IPv6 (0, 0, 65535, 0, 65535, 65535, 65535, 65535))
;;

(*
   [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
*)
let ipv6_48_pre_64_parser =
  let parser =
    let%bind prefix = range 0 1 (prefix_parser 3) >>| List.flatten in
    let%bind segments = string "::" *> h16_parser <* char ':' in
    let%bind ls32 = ls32_parser in
    let ipv6 =
      match prefix, segments, ls32 with
      | [], s1, LS32 (s2, s3) -> IPv6 (0, 0, 0, 0, 0, s1, s2, s3)
      | [ p1 ], s1, LS32 (s2, s3) -> IPv6 (0, 0, 0, p1, 0, s1, s2, s3)
      | [ pp1; p1 ], s1, LS32 (s2, s3) -> IPv6 (0, 0, pp1, p1, 0, s1, s2, s3)
      | [ pp1; pp2; p1 ], s1, LS32 (s2, s3) -> IPv6 (0, pp1, pp2, p1, 0, s1, s2, s3)
      | [ pp1; pp2; pp3; p1 ], s1, LS32 (s2, s3) -> IPv6 (pp1, pp2, pp3, p1, 0, s1, s2, s3)
      | [], s1, IPv4 ipv4 -> IPv6v4 (0, 0, 0, 0, 0, s1, ipv4)
      | [ p1 ], s1, IPv4 ipv4 -> IPv6v4 (0, 0, 0, p1, 0, s1, ipv4)
      | [ pp1; p1 ], s1, IPv4 ipv4 -> IPv6v4 (0, 0, pp1, p1, 0, s1, ipv4)
      | [ pp1; pp2; p1 ], s1, IPv4 ipv4 -> IPv6v4 (0, pp1, pp2, p1, 0, s1, ipv4)
      | [ pp1; pp2; pp3; p1 ], s1, IPv4 ipv4 -> IPv6v4 (pp1, pp2, pp3, p1, 0, s1, ipv4)
      | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
    in
    return ipv6
  in
  parser <?> "ipv6_48_pre_64_parser"
;;

let%test_unit "ipv6_48_pre_64_parser" =
  let parse = parse ipv6_48_pre_64_parser in
  [%test_result: t] (parse "::1:1:1") ~expect:(IPv6 (0, 0, 0, 0, 0, 1, 1, 1));
  [%test_result: t] (parse "1::1:1:1") ~expect:(IPv6 (0, 0, 0, 1, 0, 1, 1, 1));
  [%test_result: t] (parse "1:1::1:1:1") ~expect:(IPv6 (0, 0, 1, 1, 0, 1, 1, 1));
  [%test_result: t] (parse "1:1:1::1:1:1") ~expect:(IPv6 (0, 1, 1, 1, 0, 1, 1, 1));
  [%test_result: t] (parse "1:1:1:1::1:1:1") ~expect:(IPv6 (1, 1, 1, 1, 0, 1, 1, 1))
;;

(*
   [ *4( h16 ":" ) h16 ] "::" ls32
*)
let ipv6_32_pre_80_parser =
  let parser =
    let%bind prefix = range 0 1 (prefix_parser 4) >>| List.flatten in
    let%bind ls32 = string "::" *> ls32_parser in
    let ipv6 =
      match prefix, ls32 with
      | [], LS32 (s1, s2) -> IPv6 (0, 0, 0, 0, 0, 0, s1, s2)
      | [ p1 ], LS32 (s1, s2) -> IPv6 (0, 0, 0, 0, p1, 0, s1, s2)
      | [ pp1; p1 ], LS32 (s1, s2) -> IPv6 (0, 0, 0, pp1, p1, 0, s1, s2)
      | [ pp1; pp2; p1 ], LS32 (s1, s2) -> IPv6 (0, 0, pp1, pp2, p1, 0, s1, s2)
      | [ pp1; pp2; pp3; p1 ], LS32 (s1, s2) -> IPv6 (0, pp1, pp2, pp3, p1, 0, s1, s2)
      | [ pp1; pp2; pp3; pp4; p1 ], LS32 (s1, s2) ->
        IPv6 (pp1, pp2, pp3, pp4, p1, 0, s1, s2)
      | [], IPv4 ipv4 -> IPv6v4 (0, 0, 0, 0, 0, 0, ipv4)
      | [ p1 ], IPv4 ipv4 -> IPv6v4 (0, 0, 0, 0, p1, 0, ipv4)
      | [ pp1; p1 ], IPv4 ipv4 -> IPv6v4 (0, 0, 0, pp1, p1, 0, ipv4)
      | [ pp1; pp2; p1 ], IPv4 ipv4 -> IPv6v4 (0, 0, pp1, pp2, p1, 0, ipv4)
      | [ pp1; pp2; pp3; p1 ], IPv4 ipv4 -> IPv6v4 (0, pp1, pp2, pp3, p1, 0, ipv4)
      | [ pp1; pp2; pp3; pp4; p1 ], IPv4 ipv4 -> IPv6v4 (pp1, pp2, pp3, pp4, p1, 0, ipv4)
      | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
    in
    return ipv6
  in
  parser <?> "ipv6_32_pre_80_parser"
;;

let%test_unit "ipv6_32_pre_80_parser" =
  let parse = parse ipv6_32_pre_80_parser in
  [%test_result: t] (parse "::1:1") ~expect:(IPv6 (0, 0, 0, 0, 0, 0, 1, 1));
  [%test_result: t] (parse "1::1:1") ~expect:(IPv6 (0, 0, 0, 0, 1, 0, 1, 1));
  [%test_result: t] (parse "1:1::1:1") ~expect:(IPv6 (0, 0, 0, 1, 1, 0, 1, 1));
  [%test_result: t] (parse "1:1:1::1:1") ~expect:(IPv6 (0, 0, 1, 1, 1, 0, 1, 1));
  [%test_result: t] (parse "1:1:1:1::1:1") ~expect:(IPv6 (0, 1, 1, 1, 1, 0, 1, 1));
  [%test_result: t] (parse "1:1:1:1:1::1:1") ~expect:(IPv6 (1, 1, 1, 1, 1, 0, 1, 1))
;;

(*
   [ *5( h16 ":" ) h16 ] "::" h16
*)
let ipv6_16_pre_96_parser =
  let parser =
    let%bind prefix = range 0 1 (prefix_parser 5) >>| List.flatten in
    let%bind h16 = string "::" *> h16_parser in
    let ipv6 =
      match prefix, h16 with
      | [], s1 -> IPv6 (0, 0, 0, 0, 0, 0, 0, s1)
      | [ p1 ], s1 -> IPv6 (0, 0, 0, 0, 0, p1, 0, s1)
      | [ pp1; p1 ], s1 -> IPv6 (0, 0, 0, 0, pp1, p1, 0, s1)
      | [ pp1; pp2; p1 ], s1 -> IPv6 (0, 0, 0, pp1, pp2, p1, 0, s1)
      | [ pp1; pp2; pp3; p1 ], s1 -> IPv6 (0, 0, pp1, pp2, pp3, p1, 0, s1)
      | [ pp1; pp2; pp3; pp4; p1 ], s1 -> IPv6 (0, pp1, pp2, pp3, pp4, p1, 0, s1)
      | [ pp1; pp2; pp3; pp4; pp5; p1 ], s1 -> IPv6 (pp1, pp2, pp3, pp4, pp5, p1, 0, s1)
      | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
    in
    return ipv6
  in
  parser <?> "ipv6_16_pre_96_parser"
;;

let%test_unit "ipv6_16_pre_96_parser" =
  let parse = parse ipv6_16_pre_96_parser in
  [%test_result: t] (parse "::1") ~expect:(IPv6 (0, 0, 0, 0, 0, 0, 0, 1));
  [%test_result: t] (parse "1::1") ~expect:(IPv6 (0, 0, 0, 0, 0, 1, 0, 1));
  [%test_result: t] (parse "1:1::1") ~expect:(IPv6 (0, 0, 0, 0, 1, 1, 0, 1));
  [%test_result: t] (parse "1:1:1::1") ~expect:(IPv6 (0, 0, 0, 1, 1, 1, 0, 1));
  [%test_result: t] (parse "1:1:1:1::1") ~expect:(IPv6 (0, 0, 1, 1, 1, 1, 0, 1));
  [%test_result: t] (parse "1:1:1:1:1::1") ~expect:(IPv6 (0, 1, 1, 1, 1, 1, 0, 1));
  [%test_result: t] (parse "1:1:1:1:1:1::1") ~expect:(IPv6 (1, 1, 1, 1, 1, 1, 0, 1))
;;

(*
   [ *6( h16 ":" ) h16 ] "::"
*)
let ipv6_pre_112_parser =
  let parser =
    let%bind prefix = range 0 1 (prefix_parser 6) <* string "::" >>| List.flatten in
    let ipv6 =
      match prefix with
      | [] -> IPv6 (0, 0, 0, 0, 0, 0, 0, 0)
      | [ p1 ] -> IPv6 (0, 0, 0, 0, 0, 0, p1, 0)
      | [ pp1; p1 ] -> IPv6 (0, 0, 0, 0, 0, pp1, p1, 0)
      | [ pp1; pp2; p1 ] -> IPv6 (0, 0, 0, 0, pp1, pp2, p1, 0)
      | [ pp1; pp2; pp3; p1 ] -> IPv6 (0, 0, 0, pp1, pp2, pp3, p1, 0)
      | [ pp1; pp2; pp3; pp4; p1 ] -> IPv6 (0, 0, pp1, pp2, pp3, pp4, p1, 0)
      | [ pp1; pp2; pp3; pp4; pp5; p1 ] -> IPv6 (0, pp1, pp2, pp3, pp4, pp5, p1, 0)
      | [ pp1; pp2; pp3; pp4; pp5; pp6; p1 ] -> IPv6 (pp1, pp2, pp3, pp4, pp5, pp6, p1, 0)
      | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
    in
    return ipv6
  in
  parser <?> "ipv4_pre_112_parser"
;;

let%test_unit "ipv6_pre_112_parser" =
  let parse = parse ipv6_pre_112_parser in
  [%test_result: t] (parse "::") ~expect:(IPv6 (0, 0, 0, 0, 0, 0, 0, 0));
  [%test_result: t] (parse "1::") ~expect:(IPv6 (0, 0, 0, 0, 0, 0, 1, 0));
  [%test_result: t] (parse "1:1::") ~expect:(IPv6 (0, 0, 0, 0, 0, 1, 1, 0));
  [%test_result: t] (parse "1:1:1::") ~expect:(IPv6 (0, 0, 0, 0, 1, 1, 1, 0));
  [%test_result: t] (parse "1:1:1:1::") ~expect:(IPv6 (0, 0, 0, 1, 1, 1, 1, 0));
  [%test_result: t] (parse "1:1:1:1:1::") ~expect:(IPv6 (0, 0, 1, 1, 1, 1, 1, 0));
  [%test_result: t] (parse "1:1:1:1:1:1::") ~expect:(IPv6 (0, 1, 1, 1, 1, 1, 1, 0));
  [%test_result: t] (parse "1:1:1:1:1:1:1::") ~expect:(IPv6 (1, 1, 1, 1, 1, 1, 1, 0))
;;

(*
   IPv6address = 6( h16 ":" ) ls32
   /                       "::" 5( h16 ":" ) ls32
   / [               h16 ] "::" 4( h16 ":" ) ls32
   / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
   / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
   / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
   / [ *4( h16 ":" ) h16 ] "::"              ls32
   / [ *5( h16 ":" ) h16 ] "::"              h16
   / [ *6( h16 ":" ) h16 ] "::"
*)
let parser =
  let parser =
    ipv6_128_parser
    <|> ipv6_112_parser
    <|> ipv6_96_pre_16_parser
    <|> ipv6_80_pre_32_parser
    <|> ipv6_64_pre_48_parser
    <|> ipv6_48_pre_64_parser
    <|> ipv6_32_pre_80_parser
    <|> ipv6_16_pre_96_parser
    <|> ipv6_pre_112_parser
  in
  parser <?> "ipv6_parser"
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t]
    (parse "f:f:f:f:f:f:f:f")
    ~expect:(IPv6 (15, 15, 15, 15, 15, 15, 15, 15));
  [%test_result: t]
    (parse "f:f:f:f::f:f:f")
    ~expect:(IPv6 (15, 15, 15, 15, 0, 15, 15, 15));
  [%test_result: t]
    (parse "f:f:f:f:f::f:f")
    ~expect:(IPv6 (15, 15, 15, 15, 15, 0, 15, 15));
  [%test_result: t]
    (parse "f:f:f:f:f:f::f")
    ~expect:(IPv6 (15, 15, 15, 15, 15, 15, 0, 15));
  [%test_result: t] (parse "::") ~expect:(IPv6 (0, 0, 0, 0, 0, 0, 0, 0))
;;
