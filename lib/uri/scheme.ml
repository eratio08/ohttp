module Angstrom = Shaded.Angstrom
module String = Shaded.String

type t =
  | HTTP
  | HTTPS
  | URN
  | Other of string

let sexp_of_t =
  let a s = Sexplib0.Sexp.Atom s
  and l a = Sexplib0.Sexp.List a in
  function
  | HTTP -> a "http"
  | HTTPS -> a "https"
  | URN -> a "urn"
  | Other s -> l [ a "Other"; a s ]
;;

(*
   foo://example.com:8042/over/there?name=ferret#nose
   \_/   \______________/\_________/ \_________/ \__/
   |           |            |            |        |
   scheme     authority       path        query   fragment
   |   _____________________|__
   / \ /                        \
   urn:example:animal:ferret:nose

   https://datatracker.ietf.org/doc/html/rfc3986#section-3.1

   scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
   ALPHA  = %x41-5A / %x61-7A  ; A-Z / a-z
   DIGIT  = %x30-39  ; 0-9
*)
let parser =
  let open Angstrom in
  let open Angstrom.Let_syntax in
  let%bind first_char = alpha in
  let inner_parser = alpha <|> digit <|> char '+' <|> char '-' <|> char '.' in
  let%bind rest = many inner_parser in
  let scheme_str = first_char :: rest |> List.to_seq |> String.of_seq in
  let scheme =
    match String.lowercase_ascii scheme_str with
    | "http" -> HTTP
    | "https" -> HTTPS
    | "urn" -> URN
    | s -> Other s
  in
  return scheme
;;

module Tests = struct
  let parse str =
    let field = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str in
    match field with
    | Error e -> failwith e
    | Ok field -> field
  ;;

  let%test_unit "parser: should parse simple field" =
    [%test_result: t] (parse "http") ~expect:HTTP;
    [%test_result: t] (parse "https") ~expect:HTTPS;
    [%test_result: t] (parse "urn") ~expect:URN;
    [%test_result: t] (parse "a3x") ~expect:(Other "a3x")
  ;;
end
