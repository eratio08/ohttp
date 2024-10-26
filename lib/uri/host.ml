module Angstrom = Shaded.Angstrom
open Angstrom
open Angstrom.Let_syntax

(*
   host  = IP-literal / IPv4address / reg-name
*)

type t =
  | IPLiteral of Ip_lit.t
  | IPv4 of Ipv4.t
  | RegName of Regname.t

let sexp_of_t =
  let l aa = Sexplib0.Sexp.List aa
  and a a = Sexplib0.Sexp.Atom a in
  function
  | IPLiteral it -> l [ a "IPLiteral"; Ip_lit.sexp_of_t it ]
  | IPv4 ip4 -> l [ a "IPv4"; Ipv4.sexp_of_t ip4 ]
  | RegName rn -> l [ a "RegName"; Regname.sexp_of_t rn ]
;;

let parser =
  let lit_p =
    Ip_lit.parser
    >>| function
    | lit -> IPLiteral lit
  in
  let ipv4_p =
    Ipv4.parser
    >>| function
    | ipv4 -> IPv4 ipv4
  in
  let regname_p =
    Regname.parser
    >>| function
    | regname -> RegName regname
  in
  lit_p <|> ipv4_p <|> regname_p
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t]
    (parse "example.com")
    ~expect:(RegName (Regname.RegName "example.com"));
  [%test_result: t] (parse "1.1.1.1") ~expect:(IPv4 (1, 1, 1, 1));
  [%test_result: t]
    (parse "[v1.0]")
    ~expect:(IPLiteral (Ip_lit.IPvFuture (Ipvfuture.IPvFuture ("1", "0"))));
  [%test_result: t]
    (parse "[f:f:f:f:f:f:f:f]")
    ~expect:
      (IPLiteral
         (Ip_lit.IPv6 (Ipv6.IPv6_128 (15, 15, 15, 15, 15, 15, Ipv6.LS32 (15, 15)))))
;;
