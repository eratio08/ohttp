open Shaded
open Angstrom
open Angstrom.Let_syntax

type t =
  | IPvFuture of (string * string)
  | IPv6 of (int * int * int * int * int * int * int * int)
  | IPv6v4 of (int * int * int * int * int * int * (int * int * int * int))
  | IPv4 of Ipv4.t
  | RegName of Regname.t

let sexp_of_t =
  let open Sexp in
  let ia = Int.sexp_of_t in
  function
  | IPvFuture (ma, mi) -> l [ a "IPvFuture"; a ma; a mi ]
  | IPv6 (s1, s2, s3, s4, s5, s6, s7, s8) ->
    l [ a "IPv6"; ia s1; ia s2; ia s3; ia s4; ia s5; ia s6; ia s7; ia s8 ]
  | IPv6v4 (s1, s2, s3, s4, s5, s6, (ip1, ip2, ip3, ip4)) ->
    l
      [ a "IPv6v4"
      ; ia s1
      ; ia s2
      ; ia s3
      ; ia s4
      ; ia s5
      ; ia s6
      ; l [ ia ip1; ia ip2; ia ip3; ia ip4 ]
      ]
  | IPv4 ip4 -> l [ a "IPv4"; Ipv4.sexp_of_t ip4 ]
  | RegName rn -> l [ a "RegName"; Regname.sexp_of_t rn ]
;;

(*
   host  = IP-literal / IPv4address / reg-name
*)
let parser =
  let lit_p =
    Ip_lit.parser
    >>| function
    | Ip_lit.IPvFuture ip -> IPvFuture ip
    | Ip_lit.IPv6 ip -> IPv6 ip
    | Ip_lit.IPv6v4 ip -> IPv6v4 ip
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
  [%test_result: t] (parse "example.com") ~expect:(RegName "example.com");
  [%test_result: t] (parse "1.1.1.1") ~expect:(IPv4 (1, 1, 1, 1));
  [%test_result: t] (parse "[v1.0]") ~expect:(IPvFuture ("1", "0"));
  [%test_result: t]
    (parse "[f:f:f:f:f:f:f:f]")
    ~expect:(IPv6 (15, 15, 15, 15, 15, 15, 15, 15))
;;
