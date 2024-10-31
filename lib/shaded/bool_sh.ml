include Bool

let sexp_of_t t = Sexp.a (Bool.to_string t)
