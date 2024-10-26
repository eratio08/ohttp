module Angstrom = Shaded.Angstrom
module String = Shaded.String
module Option = Shaded.Option
module Int = Shaded.Int
open Angstrom
open Angstrom.Let_syntax

(*
   foo://example.com:8042/over/there?name=ferret#nose
   \_/   \______________/\_________/ \_________/ \__/
   |           |            |            |        |
   scheme     authority       path        query   fragment
   |   _____________________|__
   / \ /                        \
   urn:example:animal:ferret:nose

   https://datatracker.ietf.org/doc/html/rfc3986#section-3.2
*)
type t =
  { userinfo : Userinfo.t option
  ; host : Host.t
  ; port : int option
  }

let sexp_of_t t =
  let l aa = Sexplib0.Sexp.List aa
  and a a = Sexplib0.Sexp.Atom a in
  l
    [ l [ a "userinfo"; Option.sexp_of_t Userinfo.sexp_of_t t.userinfo ]
    ; l [ a "host"; Host.sexp_of_t t.host ]
    ; l [ a "port"; Option.sexp_of_t Int.sexp_of_t t.port ]
    ]
;;

(*
   port = *DIGIT
*)
let port_parser =
  let parser =
    let%bind port = many digit in
    let port =
      match port with
      | [] -> 0
      | port -> String.of_list port |> int_of_string
    in
    return port
  in
  parser <?> "port_parser"
;;

(*
   authority = [ userinfo "@" ] host [ ":" port ]
*)
let parser =
  let%bind userinfo = range 0 1 (Userinfo.parser <* char '@') in
  let userinfo =
    match userinfo with
    | [ userinfo ] -> Some userinfo
    | _ -> None
  in
  let%bind host = Host.parser in
  let%bind port = range 0 1 (char ':' *> port_parser) in
  let port =
    match port with
    | [] -> None
    | [ port ] -> Some port
    | _ -> failwith ""
  in
  return { userinfo; host; port }
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t]
    (parse "user:password@google.com:443")
    ~expect:
      { userinfo = Some (Userinfo.Userinfo "user:password")
      ; host = Host.RegName (Regname.RegName "google.com")
      ; port = Some 443
      };
  [%test_result: t]
    (parse "127.0.0.1:80")
    ~expect:{ userinfo = None; host = Host.IPv4 (127, 0, 0, 1); port = Some 80 };
  [%test_result: t]
    (parse "u:p@[::]:21")
    ~expect:
      { userinfo = Some (Userinfo.Userinfo "u:p")
      ; host = Host.IPLiteral (Ip_lit.IPv6 (Ipv6.IPv6_pre_112 None))
      ; port = Some 21
      }
;;
