open Shaded
open Angstrom
open Angstrom.Let_syntax

type t = string

let sexp_of_t q =
  let open Sexp in
  l [ a "Query"; a q ]
;;

(*
   query = *( pchar / "/" / "?" )
*)
let parser =
  let inner_p = Path.pchar_parser <|> char '/' <|> char '?' in
  let%bind query = many inner_p in
  let query = String.of_list query in
  return query
;;
