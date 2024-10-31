open Shaded
open Angstrom
open Angstrom.Let_syntax

type t =
  { method_ : Method.t
  ; request_target : Requesttarget.t
  ; version : Httpversion.t
  }

let sexp_of_t { method_; request_target; version } = fail ""

(*
   request-line = method SP request-target SP HTTP-version
*)
let parser =
  let%bind method_ = Method.parser <* sp in
  let%bind request_target = Requesttarget.parser <* sp in
  let%bind version = Httpversion.parser in
  return { method_; request_target; version }
;;

let%test_unit "parser" = [%test_result: t]
