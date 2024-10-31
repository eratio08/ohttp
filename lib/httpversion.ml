open Shaded
open Angstrom
open Angstrom.Let_syntax

type t = int * int

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
  return (Int.of_char major, Int.of_char minor)
;;
