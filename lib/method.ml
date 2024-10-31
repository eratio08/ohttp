open Shaded
open Angstrom
open Angstrom.Let_syntax

type t =
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTION
  | TRACE

let equal t1 t2 =
  match t1, t2 with
  | GET, GET
  | HEAD, HEAD
  | POST, POST
  | PUT, PUT
  | DELETE, DELETE
  | CONNECT, CONNECT
  | OPTION, OPTION
  | TRACE, TRACE -> true
  | _ -> false
;;

let hash t =
  let h =
    match t with
    | GET -> 0
    | HEAD -> 1
    | POST -> 3
    | PUT -> 4
    | DELETE -> 5
    | CONNECT -> 6
    | OPTION -> 7
    | TRACE -> 8
  in
  h + 31
;;

let pp fmt t =
  let s =
    match t with
    | GET -> "GET"
    | HEAD -> "HEAD"
    | POST -> "POST"
    | PUT -> "PUT"
    | DELETE -> "DELETE"
    | CONNECT -> "CONNECT"
    | OPTION -> "OPTION"
    | TRACE -> "TRACE"
  in
  Format.fprintf fmt "%s" s
;;

let sexp_of_t t = Sexplib0.Sexp.Atom (Format.asprintf "%a" pp t)

let parser =
  string "GET" *> return GET
  <|> string "HEAD" *> return HEAD
  <|> string "POST" *> return POST
  <|> string "PUT" *> return PUT
  <|> string "DELETE" *> return DELETE
  <|> string "CONNECT" *> return CONNECT
  <|> string "OPTION" *> return OPTION
  <|> string "TRACE" *> return TRACE
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t] ~expect:GET (parse "GET");
  [%test_result: t] ~expect:HEAD (parse "HEAD");
  [%test_result: t] ~expect:POST (parse "POST");
  [%test_result: t] ~expect:PUT (parse "PUT");
  [%test_result: t] ~expect:DELETE (parse "DELETE");
  [%test_result: t] ~expect:CONNECT (parse "CONNECT");
  [%test_result: t] ~expect:OPTION (parse "OPTION");
  [%test_result: t] ~expect:TRACE (parse "TRACE")
;;
