open Shaded
open Angstrom
open Angstrom.Let_syntax

type t = Http of int * int

let sexp_of_t (Http (ma, mi)) =
  let open Sexp in
  l [ a "HttpVersion"; Int.sexp_of_t ma; Int.sexp_of_t mi ]
;;

(*
   HTTP-name     = %s"HTTP"
*)
let http_name_parser = string "HTTP"

(*
   HTTP-version  = HTTP-name "/" DIGIT "." DIGIT
*)
let parser =
  let%bind major = http_name_parser *> char '/' *> digit in
  let%bind minor = char '.' *> digit in
  return (Http (Int.of_char major, Int.of_char minor))
;;
