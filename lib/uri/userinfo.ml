module Angstrom = Shaded.Angstrom
module String = Shaded.String
open Angstrom
open Angstrom.Let_syntax

type t = Userinfo of string

let sexp_of_t (Userinfo s) =
  let l aa = Sexplib0.Sexp.List aa
  and a a = Sexplib0.Sexp.Atom a in
  l [ a "Userinfo"; a s ]
;;

(*
   userinfo = *( unreserved / pct-encoded / sub-delims / ":" )
*)
let parser =
  let inner_parser = uri_unreserved <|> Pct_encode.parser <|> uri_subdelim <|> char ':' in
  let%bind userinfo = many inner_parser in
  userinfo |> String.of_list |> fun ui -> Userinfo ui |> return
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t] (parse "bla:user:") ~expect:(Userinfo "bla:user:")
;;
