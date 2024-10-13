module Angstrom = Shaded.Angstrom
module Int = Shaded.Int
module String = Shaded.String
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
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All h16_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: Int.t] (parse "F") ~expect:15;
  [%test_result: Int.t] (parse "ff") ~expect:255
;;

(* [%test_result: Int.t] (parse "210") ~expect:"210"; *)
(* [%test_result: Int.t] (parse "1fA0") ~expect:"1fA0" *)

type ls32 =
  | LS32 of (Int.t * Int.t)
  | IPv4 of (Int.t * Int.t * Int.t * Int.t)

let sexp_of_ls32 =
  let a s = Sexplib0.Sexp.Atom s
  and l ls = Sexplib0.Sexp.List ls in
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
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All ls32_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: ls32] (parse "1.1.1.1") ~expect:(IPv4 (1, 1, 1, 1));
  [%test_result: ls32] (parse "255.255.255.255") ~expect:(IPv4 (255, 255, 255, 255));
  [%test_result: ls32] (parse "ffff:ffff") ~expect:(LS32 (65535, 65535))
;;

type t =
  (*
     6( h16 ":" ) ls32
  *)
  | IPv6_128 of h16 * h16 * h16 * h16 * h16 * h16 * ls32
  (*
     "::" 5( h16 ":" ) ls32
  *)
  | IPv6_112 of h16 * h16 * h16 * h16 * h16 * ls32
  (*
     [ h16 ] "::" 4( h16 ":" ) ls32
  *)
  | IPv6_96_pre_16 of h16 option * h16 * h16 * h16 * h16 * ls32
  (*
     [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
  *)
  | IPv6_80_pre_32 of (h16 option * h16) option * h16 * h16 * h16 * ls32
  (*
     [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
  *)
  | IPv6_64_pre_48 of (h16 option * h16 option * h16) option * h16 * h16 * ls32
  (*
     [ *3( h16 ":" ) h16 ] "::" h16 ":" ls32
  *)
  | IPv6_48_pre_64 of (h16 option * h16 option * h16 option * h16) option * h16 * ls32
  (*
     [ *4( h16 ":" ) h16 ] "::" ls32
  *)
  | IPv6_32_pre_80 of
      (h16 option * h16 option * h16 option * h16 option * h16) option * ls32
  (*
     [ *5( h16 ":" ) h16 ] "::" h16
  *)
  | IPv6_16_pre_96 of
      (h16 option * h16 option * h16 option * h16 option * h16 option * h16) option * h16
  (*
     [ *6( h16 ":" ) h16 ] "::"
  *)
  | IPv6_pre_112 of
      (h16 option * h16 option * h16 option * h16 option * h16 option * h16 option * h16)
        option

let sexp_of_t =
  let a s = Sexplib0.Sexp.Atom s
  and l ss = Sexplib0.Sexp.List ss in
  let ia i = Int.to_string i |> a in
  let oa to_sexp o =
    match o with
    | None -> a "None"
    | Some s -> l [ a "Some"; to_sexp s ]
  in
  let tup2 to_sexp = function
    | o1, v -> l [ oa to_sexp o1; to_sexp v ]
  in
  let tup3 to_sexp = function
    | o1, o2, v -> l [ oa to_sexp o1; oa to_sexp o2; to_sexp v ]
  in
  let tup4 to_sexp = function
    | o1, o2, o3, v -> l [ oa to_sexp o1; oa to_sexp o2; oa to_sexp o3; to_sexp v ]
  in
  let tup5 to_sexp = function
    | o1, o2, o3, o4, v ->
      l [ oa to_sexp o1; oa to_sexp o2; oa to_sexp o3; oa to_sexp o4; to_sexp v ]
  in
  let tup6 to_sexp = function
    | o1, o2, o3, o4, o5, v ->
      l
        [ oa to_sexp o1
        ; oa to_sexp o2
        ; oa to_sexp o3
        ; oa to_sexp o4
        ; oa to_sexp o5
        ; to_sexp v
        ]
  in
  function
  | IPv6_128 (s1, s2, s3, s4, s5, s6, ls32) ->
    l [ ia s1; ia s2; ia s3; ia s4; ia s5; ia s6; sexp_of_ls32 ls32 ]
  | IPv6_112 (s1, s2, s3, s4, s5, ls32) ->
    l [ ia s1; ia s2; ia s3; ia s4; ia s5; sexp_of_ls32 ls32 ]
  | IPv6_96_pre_16 (s1, s2, s3, s4, s5, ls32) ->
    l [ oa ia s1; ia s2; ia s3; ia s4; ia s5; sexp_of_ls32 ls32 ]
  | IPv6_80_pre_32 (s1, s2, s3, s4, ls32) ->
    l [ oa (tup2 ia) s1; ia s2; ia s3; ia s4; sexp_of_ls32 ls32 ]
  | IPv6_64_pre_48 (s1, s2, s3, ls32) ->
    l [ oa (tup3 ia) s1; ia s2; ia s3; sexp_of_ls32 ls32 ]
  | IPv6_48_pre_64 (s1, s2, ls32) -> l [ oa (tup4 ia) s1; ia s2; sexp_of_ls32 ls32 ]
  | IPv6_32_pre_80 (s1, ls32) -> l [ oa (tup5 ia) s1; sexp_of_ls32 ls32 ]
  | IPv6_16_pre_96 (s1, h16) -> l [ oa (tup6 ia) s1; ia h16 ]
  | _ -> failwith "todo"
;;

(*
   IPv6address = 6( h16 ":" ) ls32
*)
let ipv6_128_parser =
  let%bind segments = count 6 (h16_parser <* char ':') in
  let%bind ls32 = ls32_parser in
  let ipv6 =
    match segments with
    | [ s1; s2; s3; s4; s5; s6 ] -> IPv6_128 (s1, s2, s3, s4, s5, s6, ls32)
    | _ -> failwith "Angstrom.count has a bug"
  in
  return ipv6
;;

let%test_unit "ipv6_128_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All ipv6_128_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: t]
    (parse "1:1:1:1:1:1:1:1")
    ~expect:(IPv6_128 (1, 1, 1, 1, 1, 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "1:1:1:1:1:1:3.3.3.3")
    ~expect:(IPv6_128 (1, 1, 1, 1, 1, 1, IPv4 (3, 3, 3, 3)));
  [%test_result: t]
    (parse "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff")
    ~expect:(IPv6_128 (65535, 65535, 65535, 65535, 65535, 65535, LS32 (65535, 65535)))
;;

(*
   "::" 5( h16 ":" ) ls32
*)
let ipv6_112_parser =
  let%bind segments = string "::" *> count 5 (h16_parser <* char ':') in
  let%bind ls32 = ls32_parser in
  let ipv6 =
    match segments with
    | [ s1; s2; s3; s4; s5 ] -> IPv6_112 (s1, s2, s3, s4, s5, ls32)
    | _ -> failwith "Angstrom.count has a bug"
  in
  return ipv6
;;

let%test_unit "ipv6_112_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All ipv6_112_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: t]
    (parse "::1:1:1:1:1:1:1")
    ~expect:(IPv6_112 (1, 1, 1, 1, 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "::1:1:1:1:1:3.3.3.3")
    ~expect:(IPv6_112 (1, 1, 1, 1, 1, IPv4 (3, 3, 3, 3)));
  [%test_result: t]
    (parse "::ffff:ffff:ffff:ffff:ffff:ffff:ffff")
    ~expect:(IPv6_112 (65535, 65535, 65535, 65535, 65535, LS32 (65535, 65535)))
;;

(*
   [ h16 ] "::" 4( h16 ":" ) ls32
*)
let ipv6_96_pre_16_parser =
  let%bind prefix = range 0 1 h16_parser in
  let%bind segments = string "::" *> count 4 (h16_parser <* char ':') in
  let%bind ls32 = ls32_parser in
  let ipv6 =
    match prefix, segments with
    | [], [ s1; s2; s3; s4 ] -> IPv6_96_pre_16 (None, s1, s2, s3, s4, ls32)
    | [ p1 ], [ s1; s2; s3; s4 ] -> IPv6_96_pre_16 (Some p1, s1, s2, s3, s4, ls32)
    | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
  in
  return ipv6
;;

let%test_unit "ipv6_96_pre_16_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All ipv6_96_pre_16_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: t]
    (parse "1::1:1:1:1:1:1")
    ~expect:(IPv6_96_pre_16 (Some 1, 1, 1, 1, 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "::1:1:1:1:3.3.3.3")
    ~expect:(IPv6_96_pre_16 (None, 1, 1, 1, 1, IPv4 (3, 3, 3, 3)));
  [%test_result: t]
    (parse "ffff::ffff:ffff:ffff:ffff:ffff:ffff")
    ~expect:(IPv6_96_pre_16 (Some 65535, 65535, 65535, 65535, 65535, LS32 (65535, 65535)))
;;

(*
   h16 ":" ; fails if next char is ':'
*)
let h16_pre_parser =
  let%bind h16 = h16_parser <* char ':' in
  let%bind next = peek_char_fail in
  if next = ':' then fail "" else return h16
;;

(*
   [ *<n>( h16 ":" ) h16 ]
*)
let prefix_parser n =
  let%bind preprefix = range 0 n h16_pre_parser in
  let%bind prefix = h16_parser in
  preprefix @ [ prefix ] |> return
;;

(*
   [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
*)
let ipv6_80_pre_32_parser =
  let%bind prefix = range 0 1 (prefix_parser 1) >>| List.flatten in
  let%bind segments = string "::" *> count 3 (h16_parser <* char ':') in
  let%bind ls32 = ls32_parser in
  let ipv6 =
    match prefix, segments with
    | [], [ s1; s2; s3 ] -> IPv6_80_pre_32 (None, s1, s2, s3, ls32)
    | [ p1 ], [ s1; s2; s3 ] -> IPv6_80_pre_32 (Some (None, p1), s1, s2, s3, ls32)
    | [ pp1; p1 ], [ s1; s2; s3 ] -> IPv6_80_pre_32 (Some (Some pp1, p1), s1, s2, s3, ls32)
    | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
  in
  return ipv6
;;

let%test_unit "ipv6_80_pre_32_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All ipv6_80_pre_32_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: t]
    (parse "::1:1:1:1:1")
    ~expect:(IPv6_80_pre_32 (None, 1, 1, 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "1::1:1:1:1:1")
    ~expect:(IPv6_80_pre_32 (Some (None, 1), 1, 1, 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "1:1::1:1:1:1:1")
    ~expect:(IPv6_80_pre_32 (Some (Some 1, 1), 1, 1, 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "::1:1:1:3.3.3.3")
    ~expect:(IPv6_80_pre_32 (None, 1, 1, 1, IPv4 (3, 3, 3, 3)));
  [%test_result: t]
    (parse "ffff::ffff:ffff:ffff:ffff:ffff")
    ~expect:
      (IPv6_80_pre_32 (Some (None, 65535), 65535, 65535, 65535, LS32 (65535, 65535)))
;;

(*
   [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
*)
let ipv6_64_pre_48_parser =
  let%bind prefix = range 0 1 (prefix_parser 2) >>| List.flatten in
  let%bind segments = string "::" *> count 2 (h16_parser <* char ':') in
  let%bind ls32 = ls32_parser in
  let ipv6 =
    match prefix, segments with
    | [], [ s1; s2 ] -> IPv6_64_pre_48 (None, s1, s2, ls32)
    | [ p1 ], [ s1; s2 ] -> IPv6_64_pre_48 (Some (None, None, p1), s1, s2, ls32)
    | [ pp1; p1 ], [ s1; s2 ] -> IPv6_64_pre_48 (Some (None, Some pp1, p1), s1, s2, ls32)
    | [ pp1; pp2; p1 ], [ s1; s2 ] ->
      IPv6_64_pre_48 (Some (Some pp1, Some pp2, p1), s1, s2, ls32)
    | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
  in
  return ipv6
;;

let%test_unit "ipv6_64_pre_48_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All ipv6_64_pre_48_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: t] (parse "::1:1:1:1") ~expect:(IPv6_64_pre_48 (None, 1, 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "1::1:1:1:1")
    ~expect:(IPv6_64_pre_48 (Some (None, None, 1), 1, 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "1:1::1:1:1:1")
    ~expect:(IPv6_64_pre_48 (Some (None, Some 1, 1), 1, 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "1:1:1::1:1:1:1")
    ~expect:(IPv6_64_pre_48 (Some (Some 1, Some 1, 1), 1, 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "::1:1:3.3.3.3")
    ~expect:(IPv6_64_pre_48 (None, 1, 1, IPv4 (3, 3, 3, 3)));
  [%test_result: t]
    (parse "ffff::ffff:ffff:ffff:ffff")
    ~expect:(IPv6_64_pre_48 (Some (None, None, 65535), 65535, 65535, LS32 (65535, 65535)))
;;

(*
   [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
*)
let ipv6_48_pre_64_parser =
  let%bind prefix = range 0 1 (prefix_parser 3) >>| List.flatten in
  let%bind segments = string "::" *> h16_parser <* char ':' in
  let%bind ls32 = ls32_parser in
  let ipv6 =
    match prefix, segments with
    | [], s1 -> IPv6_48_pre_64 (None, s1, ls32)
    | [ p1 ], s1 -> IPv6_48_pre_64 (Some (None, None, None, p1), s1, ls32)
    | [ pp1; p1 ], s1 -> IPv6_48_pre_64 (Some (None, None, Some pp1, p1), s1, ls32)
    | [ pp1; pp2; p1 ], s1 ->
      IPv6_48_pre_64 (Some (None, Some pp1, Some pp2, p1), s1, ls32)
    | [ pp1; pp2; pp3; p1 ], s1 ->
      IPv6_48_pre_64 (Some (Some pp1, Some pp2, Some pp3, p1), s1, ls32)
    | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
  in
  return ipv6
;;

let%test_unit "ipv6_48_pre_64_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All ipv6_48_pre_64_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: t] (parse "::1:1:1") ~expect:(IPv6_48_pre_64 (None, 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "1::1:1:1")
    ~expect:(IPv6_48_pre_64 (Some (None, None, None, 1), 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "1:1::1:1:1")
    ~expect:(IPv6_48_pre_64 (Some (None, None, Some 1, 1), 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "1:1:1::1:1:1")
    ~expect:(IPv6_48_pre_64 (Some (None, Some 1, Some 1, 1), 1, LS32 (1, 1)));
  [%test_result: t]
    (parse "1:1:1:1::1:1:1")
    ~expect:(IPv6_48_pre_64 (Some (Some 1, Some 1, Some 1, 1), 1, LS32 (1, 1)))
;;

(*
   [ *4( h16 ":" ) h16 ] "::" ls32
*)
let ipv6_32_pre_80_parser =
  let%bind prefix = range 0 1 (prefix_parser 4) >>| List.flatten in
  let%bind ls32 = string "::" *> ls32_parser in
  let ipv6 =
    match prefix with
    | [] -> IPv6_32_pre_80 (None, ls32)
    | [ p1 ] -> IPv6_32_pre_80 (Some (None, None, None, None, p1), ls32)
    | [ pp1; p1 ] -> IPv6_32_pre_80 (Some (None, None, None, Some pp1, p1), ls32)
    | [ pp1; pp2; p1 ] -> IPv6_32_pre_80 (Some (None, None, Some pp1, Some pp2, p1), ls32)
    | [ pp1; pp2; pp3; p1 ] ->
      IPv6_32_pre_80 (Some (None, Some pp1, Some pp2, Some pp3, p1), ls32)
    | [ pp1; pp2; pp3; pp4; p1 ] ->
      IPv6_32_pre_80 (Some (Some pp1, Some pp2, Some pp3, Some pp4, p1), ls32)
    | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
  in
  return ipv6
;;

let%test_unit "ipv6_32_pre_80_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All ipv6_32_pre_80_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: t] (parse "::1:1") ~expect:(IPv6_32_pre_80 (None, LS32 (1, 1)));
  [%test_result: t]
    (parse "1::1:1")
    ~expect:(IPv6_32_pre_80 (Some (None, None, None, None, 1), LS32 (1, 1)));
  [%test_result: t]
    (parse "1:1::1:1")
    ~expect:(IPv6_32_pre_80 (Some (None, None, None, Some 1, 1), LS32 (1, 1)));
  [%test_result: t]
    (parse "1:1:1::1:1")
    ~expect:(IPv6_32_pre_80 (Some (None, None, Some 1, Some 1, 1), LS32 (1, 1)));
  [%test_result: t]
    (parse "1:1:1:1::1:1")
    ~expect:(IPv6_32_pre_80 (Some (None, Some 1, Some 1, Some 1, 1), LS32 (1, 1)));
  [%test_result: t]
    (parse "1:1:1:1:1::1:1")
    ~expect:(IPv6_32_pre_80 (Some (Some 1, Some 1, Some 1, Some 1, 1), LS32 (1, 1)))
;;

(*
   [ *5( h16 ":" ) h16 ] "::" h16
*)
let ipv6_16_pre_96_parser =
  let%bind prefix = range 0 1 (prefix_parser 5) >>| List.flatten in
  let%bind h16 = string "::" *> h16_parser in
  let ipv6 =
    match prefix with
    | [] -> IPv6_16_pre_96 (None, h16)
    | [ p1 ] -> IPv6_16_pre_96 (Some (None, None, None, None, None, p1), h16)
    | [ pp1; p1 ] -> IPv6_16_pre_96 (Some (None, None, None, None, Some pp1, p1), h16)
    | [ pp1; pp2; p1 ] ->
      IPv6_16_pre_96 (Some (None, None, None, Some pp1, Some pp2, p1), h16)
    | [ pp1; pp2; pp3; p1 ] ->
      IPv6_16_pre_96 (Some (None, None, Some pp1, Some pp2, Some pp3, p1), h16)
    | [ pp1; pp2; pp3; pp4; p1 ] ->
      IPv6_16_pre_96 (Some (None, Some pp1, Some pp2, Some pp3, Some pp4, p1), h16)
    | [ pp1; pp2; pp3; pp4; pp5; p1 ] ->
      IPv6_16_pre_96 (Some (Some pp1, Some pp2, Some pp3, Some pp4, Some pp5, p1), h16)
    | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
  in
  return ipv6
;;

let%test_unit "ipv6_16_pre_96_parser" =
  let parse s =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All ipv6_16_pre_96_parser s with
    | Ok r -> r
    | Error e -> failwith e
  in
  [%test_result: t] (parse "::1") ~expect:(IPv6_16_pre_96 (None, 1));
  [%test_result: t]
    (parse "1::1")
    ~expect:(IPv6_16_pre_96 (Some (None, None, None, None, None, 1), 1));
  [%test_result: t]
    (parse "1:1::1")
    ~expect:(IPv6_16_pre_96 (Some (None, None, None, None, Some 1, 1), 1));
  [%test_result: t]
    (parse "1:1:1::1")
    ~expect:(IPv6_16_pre_96 (Some (None, None, None, Some 1, Some 1, 1), 1));
  [%test_result: t]
    (parse "1:1:1:1::1")
    ~expect:(IPv6_16_pre_96 (Some (None, None, Some 1, Some 1, Some 1, 1), 1));
  [%test_result: t]
    (parse "1:1:1:1:1::1")
    ~expect:(IPv6_16_pre_96 (Some (None, Some 1, Some 1, Some 1, Some 1, 1), 1));
  [%test_result: t]
    (parse "1:1:1:1:1:1::1")
    ~expect:(IPv6_16_pre_96 (Some (Some 1, Some 1, Some 1, Some 1, Some 1, 1), 1))
;;

(*
   [ *6( h16 ":" ) h16 ] "::"
*)
let ipv6_pre_112_parser =
  let%bind prefix = range 0 1 (prefix_parser 6) <* string "::" >>| List.flatten in
  let ipv6 =
    match prefix with
    | [] -> IPv6_pre_112 None
    | [ p1 ] -> IPv6_pre_112 (Some (None, None, None, None, None, None, p1))
    | [ pp1; p1 ] -> IPv6_pre_112 (Some (None, None, None, None, None, Some pp1, p1))
    | [ pp1; pp2; p1 ] ->
      IPv6_pre_112 (Some (None, None, None, None, Some pp1, Some pp2, p1))
    | [ pp1; pp2; pp3; p1 ] ->
      IPv6_pre_112 (Some (None, None, None, Some pp1, Some pp2, Some pp3, p1))
    | [ pp1; pp2; pp3; pp4; p1 ] ->
      IPv6_pre_112 (Some (None, None, Some pp1, Some pp2, Some pp3, Some pp4, p1))
    | [ pp1; pp2; pp3; pp4; pp5; p1 ] ->
      IPv6_pre_112 (Some (None, Some pp1, Some pp2, Some pp3, Some pp4, Some pp5, p1))
    | [ pp1; pp2; pp3; pp4; pp5; pp6; p1 ] ->
      IPv6_pre_112 (Some (Some pp1, Some pp2, Some pp3, Some pp4, Some pp5, Some pp6, p1))
    | _ -> failwith "Angstrom.count has a bug, or prefix is too long"
  in
  return ipv6
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
let parser () =
  ipv6_128_parser
  <|> ipv6_112_parser
  <|> ipv6_96_pre_16_parser
  <|> ipv6_80_pre_32_parser
  <|> ipv6_64_pre_48_parser
  <|> ipv6_48_pre_64_parser
  <|> ipv6_32_pre_80_parser
  <|> ipv6_16_pre_96_parser
  <|> ipv6_pre_112_parser
;;
