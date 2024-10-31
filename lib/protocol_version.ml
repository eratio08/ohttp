open Shaded

(* Is used in request & response *)
type t =
  | HTTP0_9
  | HTTP1_0
  | HTTP1_1
  | HTTP2_0
  | HTTP3_0

let pp fmt t =
  let s =
    match t with
    | HTTP0_9 -> "HTTP/0.9"
    | HTTP1_0 -> "HTTP/1.0"
    | HTTP1_1 -> "HTTP/1.1"
    | HTTP2_0 -> "HTTP/2.0"
    | HTTP3_0 -> "HTTP/3.0"
  in
  Format.fprintf fmt "%s" s
;;

let sexp_of_t t = Sexplib0.Sexp.Atom (Format.asprintf "%a" pp t)

(*
   HTTP-version  = HTTP-name "/" DIGIT "." DIGIT
   HTTP-name     = %s"HTTP"
*)
let parser =
  let open Angstrom in
  let open Angstrom.Let_syntax in
  let%bind major = string "HTTP/" *> satisfy is_num in
  let%bind minor = char '.' *> satisfy is_num in
  match major, minor with
  | '0', '9' -> return HTTP0_9
  | '1', '0' -> return HTTP1_0
  | '1', '1' -> return HTTP1_1
  | '2', '0' -> return HTTP2_0
  | '3', '0' -> return HTTP3_0
  | _, _ -> fail (Printf.sprintf "Invalid HTTP version HTTP/%c.%c" major minor)
;;

module Tests = struct
  let parse str =
    match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
    | Error e -> failwith e
    | Ok field -> field
  ;;

  let%test_unit "parser: should parse http version" =
    [%test_result: t] ~expect:HTTP0_9 (parse "HTTP/0.9");
    [%test_result: t] ~expect:HTTP1_0 (parse "HTTP/1.0");
    [%test_result: t] ~expect:HTTP1_1 (parse "HTTP/1.1");
    [%test_result: t] ~expect:HTTP2_0 (parse "HTTP/2.0");
    [%test_result: t] ~expect:HTTP3_0 (parse "HTTP/3.0")
  ;;
end
