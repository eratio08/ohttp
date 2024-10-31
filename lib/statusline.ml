open Shaded
open Angstrom
open Angstrom.Let_syntax

type t =
  { version : Httpversion.t
  ; status : int
  ; reason : string option
  }

let sexp_of_t { version; status; reason } =
  let open Sexp in
  l
    [ a "Statusline"
    ; l [ a "version"; Httpversion.sexp_of_t version ]
    ; l [ a "status"; Int.sexp_of_t status ]
    ; l [ a "reason"; Option.sexp_of_t a reason ]
    ]
;;

(*
   status-code = 3DIGIT
*)
let status_parser =
  let%bind status = count 3 digit in
  let status = String.of_list status |> int_of_string in
  return status
;;

(*
   reason-phrase = 1*( HTAB / SP / VCHAR / obs-text )
*)
let reason_phrase_parser =
  let inner_p = htab <|> sp <|> vchar <|> http_obs_text in
  let%bind reason = many1 inner_p in
  let reason = String.of_list reason in
  return reason
;;

(*
   status-line = HTTP-version SP status-code SP [ reason-phrase ]
*)
let parser =
  let%bind version = Httpversion.parser <* sp in
  let%bind status = status_parser <* sp in
  let%bind reason = maybe reason_phrase_parser in
  return { version; status; reason }
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t]
    (parse "HTTP/1.1 200 OK")
    ~expect:{ version = Httpversion.Http (1, 1); status = 200; reason = Some "OK" };
  [%test_result: t]
    (parse "HTTP/1.1 404 NOT FOUND")
    ~expect:{ version = Httpversion.Http (1, 1); status = 404; reason = Some "NOT FOUND" }
;;
