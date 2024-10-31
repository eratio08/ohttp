module Path = Path
module Query = Query
module Userinfo = Userinfo
module Authority = Authority
module Scheme = Scheme
module Host = Host
module Ipv6 = Ipv6
module Ipv4 = Ipv4
open Shaded
open Angstrom
open Angstrom.Let_syntax

(*
   https://datatracker.ietf.org/doc/html/rfc3986#section-3

   foo://example.com:8042/over/there?name=ferret#nose
   \_/   \______________/\_________/ \_________/ \__/
   |           |            |            |        |
   scheme     authority       path        query   fragment
   |   _____________________|__
   / \ /                        \
   urn:example:animal:ferret:nose
*)
type t =
  { scheme : Scheme.t
  ; authority : Authority.t option
  ; path : Path.t
  ; query : Query.t option
  ; fragment : Fragment.t option
  }

let sexp_of_t { scheme; authority; path; query; fragment } =
  let open Sexp in
  l
    [ a "URI"
    ; l [ a "scheme"; Scheme.sexp_of_t scheme ]
    ; l [ a "authority"; Option.sexp_of_t Authority.sexp_of_t authority ]
    ; l [ a "path"; Path.sexp_of_t path ]
    ; l [ a "query"; Option.sexp_of_t Query.sexp_of_t query ]
    ; l [ a "fragment"; Option.sexp_of_t Fragment.sexp_of_t fragment ]
    ]
;;

(*
   hier-part   = "//" authority path-abempty
   / path-absolute
   / path-rootless
   / path-empty
*)
let hier_part_parser =
  let url_p =
    let%bind authority = count 2 (char '/') *> Authority.parser in
    let%bind path = Path.path_abempty_parser in
    return (Some authority, path)
  in
  let other_p =
    let%bind path =
      Path.path_absolute_parser <|> Path.path_rootless_parser <|> Path.path_empty_parser
    in
    return (None, path)
  in
  url_p <|> other_p
;;

(*
   URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
*)
let parser =
  let%bind scheme = Scheme.parser <* char ':' in
  let%bind authority, path = hier_part_parser in
  let%bind query = maybe (char '?' *> Query.parser) in
  let%bind fragment = maybe (char '#' *> Fragment.parser) in
  return { scheme; authority; path; query; fragment }
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t]
    (parse "foo://example.com:8042/over/there?name=ferret#nose")
    ~expect:
      { scheme = Scheme.Other "foo"
      ; authority =
          Some { userinfo = None; host = Host.RegName "example.com"; port = Some 8042 }
      ; path = Path.PathAbEmpty [ Path.Segment "over"; Path.Segment "there" ]
      ; query = Some "name=ferret"
      ; fragment = Some "nose"
      };
  [%test_result: t]
    (parse "urn:example:animal:ferret:nose")
    ~expect:
      { scheme = Scheme.URN
      ; authority = None
      ; path = Path.PathRootless [ Segment_nz "example:animal:ferret:nose" ]
      ; query = None
      ; fragment = None
      }
;;

type absolute_uri =
  { scheme : Scheme.t
  ; authority : Authority.t option
  ; path : Path.t
  ; query : Query.t option
  }

(*
   absolute-URI = scheme ":" hier-part [ "?" query ]
*)
let absolute_uri_parser =
  let%bind scheme = Scheme.parser <* char ':' in
  let%bind authority, path = hier_part_parser in
  let%bind query = maybe (char '?' *> Query.parser) in
  return { scheme; authority; path; query }
;;
