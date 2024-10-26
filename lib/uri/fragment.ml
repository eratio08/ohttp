module Angstrom = Shaded.Angstrom
open Angstrom
open Angstrom.Let_syntax

type t = Fragment of string

let sexp_of_t (Fragment q) =
  let a s = Sexplib0.Sexp.Atom s
  and l aa = Sexplib0.Sexp.List aa in
  l [ a "Fragment"; a q ]
;;

(*
   fragment = *( pchar / "/" / "?" )
*)
let parser =
  let inner_p = Path.pchar_parser <|> char '/' <|> char '?' in
  let%bind fragment = many inner_p in
  let fragment = String.of_list fragment in
  Fragment fragment |> return
;;
