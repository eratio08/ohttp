include Result

let sexp_of_t sexp_of_r sexp_of_e =
  let open Sexp in
  function
  | Ok r -> l [ a "Ok"; sexp_of_r r ]
  | Error e -> l [ a "Error"; sexp_of_e e ]
;;

let compare ok error r0 r1 =
  match r0, r1 with
  | Ok v0, Ok v1 -> ok v0 v1
  | Error e0, Error e1 -> error e0 e1
  | Ok _, Error _ -> -1
  | Error _, Ok _ -> 1
;;
