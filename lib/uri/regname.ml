module Angstrom = Shaded.Angstrom
open Angstrom
open Angstrom.Let_syntax

(*
   reg-name = *( unreserved / pct-encoded / sub-delims )
*)
type t = RegName of string

let sexp_of_t =
  let l aa = Sexplib0.Sexp.List aa
  and a a = Sexplib0.Sexp.Atom a in
  function
  | RegName rn -> l [ a "RegNam"; a rn ]
;;

let parser =
  let%bind regname = many (uri_unreserved <|> Pct_encode.parser <|> uri_subdelim) in
  List.to_seq regname |> String.of_seq |> fun s -> RegName s |> return
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t] (parse "example.com") ~expect:(RegName "example.com");
  [%test_result: t] (parse "sub.sub.example.com") ~expect:(RegName "sub.sub.example.com")
;;
