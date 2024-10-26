module Angstrom = Shaded.Angstrom
module Int = Shaded.Int
module String = Shaded.String
open Angstrom
open Angstrom.Let_syntax

type t =
  | IPvFuture of Ipvfuture.t
  | IPv6 of Ipv6.t

let sexp_of_t =
  let l aa = Sexplib0.Sexp.List aa
  and a a = Sexplib0.Sexp.Atom a in
  function
  | IPvFuture ipf -> l [ a "IPvFuture"; Ipvfuture.sexp_of_t ipf ]
  | IPv6 ip6 -> l [ a "IPv6"; Ipv6.sexp_of_t ip6 ]
;;

(*
   IP-literal  = "[" ( IPv6address / IPvFuture  ) "]"
*)
let parser =
  let ipv6_p =
    let%bind ipv6 = Ipv6.parser in
    IPv6 ipv6 |> return
  in
  let ipvfut_p =
    let%bind ipvfut = Ipvfuture.parser in
    IPvFuture ipvfut |> return
  in
  char '[' *> (ipv6_p <|> ipvfut_p) <* char ']'
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t] (parse "[::]") ~expect:(IPv6 (Ipv6.IPv6_pre_112 None));
  [%test_result: t]
    (parse "[vff.0]")
    ~expect:(IPvFuture (Ipvfuture.IPvFuture ("ff", "0")))
;;
