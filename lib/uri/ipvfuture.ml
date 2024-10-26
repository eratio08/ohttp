module Angstrom = Shaded.Angstrom
module Int = Shaded.Int
module String = Shaded.String
open Angstrom
open Angstrom.Let_syntax

type t = IPvFuture of string * string

let sexp_of_t =
  let l aa = Sexplib0.Sexp.List aa
  and a a = Sexplib0.Sexp.Atom a in
  function
  | IPvFuture (pre, pos) -> l [ a "IPvFuture"; a pre; a pos ]
;;

(*
   IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
*)
let parser =
  let inner_parser = uri_unreserved <|> uri_subdelim <|> char ':' in
  let%bind v = char 'v' *> many1 hexdig <* char '.' in
  let%bind tail = many1 inner_parser in
  let v = String.of_list v in
  let tail = String.of_list tail in
  IPvFuture (v, tail) |> return
;;
