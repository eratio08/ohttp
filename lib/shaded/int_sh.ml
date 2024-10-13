module String = String_sh
include Int

let pow base exp =
  let rec loop base exp res =
    match exp with
    | 0 -> res
    | _ -> loop base (exp - 1) (res * base)
  in
  loop base exp 1
;;

let sexp_of_t t = Sexplib0.Sexp.Atom (Int.to_string t)

let of_char c =
  match Char.code c - 48 with
  | n when 0 <= n && n <= 9 -> n
  | _ -> failwith ("'" ^ Char.escaped c ^ "' is not a number")
;;

let of_hex_char = function
  | '0' .. '9' as c -> int_of_char c - 48 |> Option.some
  | 'A' .. 'Z' as c -> int_of_char c - 55 |> Option.some
  | 'a' .. 'f' as c -> int_of_char c - 87 |> Option.some
  | _ -> None
;;

let of_hex_char_seq s =
  let l = Seq.length s in
  s
  |> Seq.mapi (fun i c ->
    of_hex_char c
    |> Option.map (fun p ->
      let i = l - 1 - i in
      let q = pow 16 i in
      p * q))
  |> Seq.fold_left
       (fun acc p -> Option.bind acc (fun acc -> p |> Option.map (fun p -> acc + p)))
       (Some 0)
;;

let of_hex_char_list cs = List.to_seq cs |> of_hex_char_seq
let of_hex h = String.to_seq h |> of_hex_char_seq

let%test_unit "of_hex: should convert a hex string to an int" =
  [%test_result: t] (of_hex "f" |> Option.get) ~expect:15;
  [%test_result: t] (of_hex "10" |> Option.get) ~expect:16;
  [%test_result: t] (of_hex "a" |> Option.get) ~expect:10;
  [%test_result: t] (of_hex "ccc" |> Option.get) ~expect:3276
;;

let of_hex_exn h = of_hex h |> Option.get

let to_hex_char = function
  | i when i >= 0 && i <= 9 -> i + 48 |> char_of_int |> Option.some
  | i when i >= 10 && i <= 15 -> i + 87 |> char_of_int |> Option.some
  | _ -> None
;;

let to_hex i =
  let rec loop i seq =
    if i < 16
    then to_hex_char i |> Option.map (fun x -> Seq.cons x seq)
    else (
      let q = i / 16 in
      let rem = i mod 16 in
      to_hex_char rem
      |> Option.map (fun rem -> Seq.cons rem seq)
      |> fun seq -> Option.bind seq (fun seq -> loop q seq))
    (* loop q h *)
  in
  loop i Seq.empty |> Option.map String.of_seq
;;

let%test_unit "to_hex: should convert an int to a hex string" =
  [%test_result: String.t] (to_hex 15 |> Option.get) ~expect:"f";
  [%test_result: String.t] (to_hex 16 |> Option.get) ~expect:"10";
  [%test_result: String.t] (to_hex 10 |> Option.get) ~expect:"a";
  [%test_result: String.t] (to_hex 3276 |> Option.get) ~expect:"ccc"
;;
