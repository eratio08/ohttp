open Shaded
open Angstrom
open Angstrom.Let_syntax

type t =
  | IPvFuture of (string * string)
  | IPv6 of (int * int * int * int * int * int * int * int)
  | IPv6v4 of (int * int * int * int * int * int * (int * int * int * int))

let sexp_of_t =
  let l aa = Sexplib0.Sexp.List aa
  and a a = Sexplib0.Sexp.Atom a in
  let ia i = a (Int.to_string i) in
  function
  | IPvFuture (ma, mi) -> l [ a "IPvFuture"; a ma; a mi ]
  | IPv6 (s1, s2, s3, s4, s5, s6, s7, s8) ->
    l [ a "IPv6"; ia s1; ia s2; ia s3; ia s4; ia s5; ia s6; ia s7; ia s8 ]
  | IPv6v4 (s1, s2, s3, s4, s5, s6, (ip1, ip2, ip3, ip4)) ->
    l
      [ a "IPv6"
      ; ia s1
      ; ia s2
      ; ia s3
      ; ia s4
      ; ia s5
      ; ia s6
      ; l [ ia ip1; ia ip2; ia ip3; ia ip4 ]
      ]
;;

(*
   IP-literal  = "[" ( IPv6address / IPvFuture  ) "]"
*)
let parser =
  let ipv6_p =
    let%bind ipv6 = Ipv6.parser in
    match ipv6 with
    | Ipv6.IPv6 ipv6 -> IPv6 ipv6 |> return
    | Ipv6.IPv6v4 ipv6 -> IPv6v4 ipv6 |> return
  in
  let ipvfut_p =
    let%bind (Ipvfuture.IPvFuture (ma, mi)) = Ipvfuture.parser in
    IPvFuture (ma, mi) |> return
  in
  char '[' *> (ipv6_p <|> ipvfut_p) <* char ']'
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t] (parse "[::]") ~expect:(IPv6 (0, 0, 0, 0, 0, 0, 0, 0));
  [%test_result: t] (parse "[vff.0]") ~expect:(IPvFuture ("ff", "0"))
;;
