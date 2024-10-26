module Angstrom = Shaded.Angstrom
open Angstrom
open Angstrom.Let_syntax

type t = Query of string

let sexp_of_t (Query q) =
  let a s = Sexplib0.Sexp.Atom s
  and l aa = Sexplib0.Sexp.List aa in
  l [ a "Query"; a q ]
;;

(*
   query = *( pchar / "/" / "?" )
*)
let parser =
  let inner_p = Path.pchar_parser <|> char '/' <|> char '?' in
  let%bind query = many inner_p in
  let query = String.of_list query in
  Query query |> return
;;
