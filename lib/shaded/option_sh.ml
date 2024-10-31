include Option

let sexp_of_t sexp_of_a =
  let open Sexp in
  function
  | Some v -> l [ a "Some"; sexp_of_a v ]
  | None -> a "None"
;;
