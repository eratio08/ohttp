open Shaded
open Angstrom
open Angstrom.Let_syntax

type t =
  | OriginForm of
      { path : string list
      ; query : string option
      }
  | AbsoluteForm of
      { scheme : Uri.Scheme.t
      ; authority : Uri.Authority.t option
      ; path : string list
      ; query : string option
      }
  | AuthorityFrom of
      { host : Uri.Host.t
      ; port : int
      }
  | AsteriskForm

let sexp_of_t =
  let open Sexp in
  function
  | OriginForm { path; query } ->
    l
      [ a "OriginForm"
      ; l [ a "path"; l (List.map a path) ]
      ; l [ a "query"; Option.sexp_of_t a query ]
      ]
  | AbsoluteForm { scheme; authority; path; query } ->
    l
      [ a "AbsoluteForm"
      ; l [ a "scheme"; Uri.Scheme.sexp_of_t scheme ]
      ; l [ a "authority"; Option.sexp_of_t Uri.Authority.sexp_of_t authority ]
      ; l [ a "path"; l (List.map a path) ]
      ; l [ a "query"; Option.sexp_of_t a query ]
      ]
  | AuthorityFrom { host; port } ->
    l
      [ a "AuthorityFrom"
      ; l [ l [ a "host"; Uri.Host.sexp_of_t host ] ]
      ; l [ a "port"; Int.sexp_of_t port ]
      ]
  | AsteriskForm -> a "AsteriskForm"
;;

let segment_to_string = function
  | Uri.Path.Segment s -> s
  | Uri.Path.Segment_nz s -> s
  | Uri.Path.Segment_nz_nc s -> s
;;

let segment_of_path = function
  | Uri.Path.PathAbsolute s -> s
  | Uri.Path.PathAbEmpty s -> s
  | Uri.Path.PathNoScheme s -> s
  | Uri.Path.PathRootless s -> s
  | Uri.Path.PathEmpty -> []
;;

(*
   origin-form = absolute-path [ "?" query ]
*)
let origin_from_parser =
  let%bind path = Uri.Path.path_absolute_parser in
  let%bind query = maybe (char '?' *> Uri.Query.parser) in
  let segments = segment_of_path path in
  let path = List.map segment_to_string segments in
  let query = Option.map (fun s -> s) query in
  return (OriginForm { path; query })
;;

(*
   absolute-form = absolute-URI
*)
let absolute_form_parser =
  let%bind { scheme; authority; path; query } = Uri.absolute_uri_parser in
  let segments = segment_of_path path in
  let path = List.map segment_to_string segments in
  AbsoluteForm { scheme; authority; path; query } |> return
;;

(*
   authority-form = uri-host ":" port
*)
let authority_form_parser =
  let%bind host = Uri.Host.parser in
  let%bind port = char ':' *> Uri.Authority.port_parser in
  AuthorityFrom { host; port } |> return
;;

(*
   asterisk-form  = "*"
*)
let asterisk_form_parser = char '*' *> return AsteriskForm

(*
   request-target = origin-form
   / absolute-form
   / authority-form
   / asterisk-form
*)
let parser =
  origin_from_parser
  <|> absolute_form_parser
  <|> authority_form_parser
  <|> asterisk_form_parser
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t] (parse "*") ~expect:AsteriskForm
;;
