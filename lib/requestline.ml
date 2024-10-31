open Shaded
open Angstrom
open Angstrom.Let_syntax

type t =
  { method_ : Method.t
  ; request_target : Requesttarget.t
  ; version : Httpversion.t
  }

let sexp_of_t { method_; request_target; version } =
  let open Sexp in
  l
    [ l [ a "method"; Method.sexp_of_t method_ ]
    ; l [ a "request_target"; Requesttarget.sexp_of_t request_target ]
    ; l [ a "version"; Httpversion.sexp_of_t version ]
    ]
;;

(*
   request-line = method SP request-target SP HTTP-version
*)
let parser =
  let%bind method_ = Method.parser <* sp in
  let%bind request_target = Requesttarget.parser <* sp in
  let%bind version = Httpversion.parser in
  return { method_; request_target; version }
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t]
    (parse "GET / HTTP/1.1")
    ~expect:
      { method_ = Method.GET
      ; request_target = Requesttarget.OriginForm { path = []; query = None }
      ; version = Httpversion.Http (1, 1)
      };
  [%test_result: t]
    (parse "OPTION * HTTP/1.1")
    ~expect:
      { method_ = Method.OPTION
      ; request_target = Requesttarget.AsteriskForm
      ; version = Httpversion.Http (1, 1)
      }
;;
