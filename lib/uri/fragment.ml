module Angstrom = Shaded.Angstrom
open Angstrom
open Angstrom.Let_syntax

type t = string

let sexp_of_t q =
  let open Sexp in
  l [ a "Fragment"; a q ]
;;

(*
   fragment = *( pchar / "/" / "?" )
*)
let parser =
  let inner_p = Path.pchar_parser <|> char '/' <|> char '?' in
  let%bind fragment = many inner_p in
  let fragment = String.of_list fragment in
  return fragment
;;
