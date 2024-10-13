include Char
open Sexplib0

let sexp_of_t t = Sexp.Atom (Char.escaped t)
