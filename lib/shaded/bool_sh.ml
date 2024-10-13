include Bool
open Sexplib0

let sexp_of_t t = Sexp.Atom (Bool.to_string t)
