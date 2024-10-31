include Char

let sexp_of_t t = Sexp.a (Char.escaped t)
