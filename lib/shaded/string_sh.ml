include String

let sexp_of_t t = Sexp.a t

let join ?sep strs =
  let res = ref "" in
  let sep = Option.map Char.escaped sep in
  for i = 0 to List.length strs - 1 do
    let nth = List.nth strs i in
    match sep with
    | Some sep ->
      if i == List.length strs - 1 then res := !res ^ nth else res := !res ^ nth ^ sep
    | None -> res := !res ^ nth
  done;
  !res
;;

let pp fmt t = Stdlib.Format.fprintf fmt "%s" t
let of_list s = List.to_seq s |> String.of_seq
