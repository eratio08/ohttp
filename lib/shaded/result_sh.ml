include Result

module ResultSexp (M : sig
    type t

    val sexp_of_t : t -> Sexplib0.Sexp.t
  end) =
struct
  include Result

  let sexp_of_t t =
    let a s = Sexplib0.Sexp.Atom s
    and l aa = Sexplib0.Sexp.List aa in
    match t with
    | Ok x -> l [ a "Ok"; M.sexp_of_t x ]
    | Error e -> l [ a "Error"; a e ]
  ;;
end
