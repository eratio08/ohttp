include Option

let sexp_of_t sexp_of_a =
  let a s = Sexplib0.Sexp.Atom s
  and l aa = Sexplib0.Sexp.List aa in
  function
  | Some v -> l [ a "Some"; sexp_of_a v ]
  | None -> a "None"
;;
