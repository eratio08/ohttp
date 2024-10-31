module String = Shaded.String

type t = Bits of Bool.t List.t

let of_int n =
  let rec loop n res =
    match n with
    | 0 -> res
    | n ->
      let rem = Int.rem n 2 > 0 in
      loop (n / 2) (rem :: res)
  in
  loop n [] |> fun bs -> Bits bs
;;

let bools_to_str bs = List.map (fun b -> if b then '1' else '0') bs |> String.of_list
let to_str (Bits bs) = bools_to_str bs

let%test_unit "dec_to_int" =
  let bits_str i = of_int i |> to_str in
  [%test_result: String.t] (bits_str 1) ~expect:"1";
  [%test_result: String.t] (bits_str 2) ~expect:"10";
  [%test_result: String.t] (bits_str 5) ~expect:"101";
  [%test_result: String.t] (bits_str 11) ~expect:"1011"
;;

let pp fmt (Bits bs) =
  let rec chunk bs res =
    match bs with
    | [] -> res
    | a :: b :: c :: d :: tail -> chunk tail ([ a; b; c; d ] :: res)
    | a :: b :: c :: tail -> chunk tail ([ false; a; b; c ] :: res)
    | a :: b :: tail -> chunk tail ([ false; false; a; b ] :: res)
    | a :: tail -> chunk tail ([ false; false; false; a ] :: res)
  in
  let chunks = chunk bs [] in
  let str =
    List.fold_left
      (fun acc chunk ->
        if acc = "" then bools_to_str chunk else acc ^ "_" ^ bools_to_str chunk)
      ""
      chunks
  in
  Format.fprintf fmt "%s" str
;;
