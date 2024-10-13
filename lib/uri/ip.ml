module Angstrom = Shaded.Angstrom
module Int = Shaded.Int
module String = Shaded.String
open Angstrom
open Angstrom.Let_syntax

type t =
  | IPvFuture of Char.t List.t * Char.t List.t
  | IPv6
  | IPv4 of Int.t * Int.t * Int.t * Int.t

module IPFuture = struct
  (*
     IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
  *)
  let parser =
    let%bind v = char 'v' *> many1 hexdig <* char '.' in
    let inner_parser = uri_unreserved <|> uri_subdelim <|> char ':' in
    let%bind tail = many1 inner_parser in
    IPvFuture (v, tail) |> return
  ;;
end

(*
   IP-literal  = "[" ( IPv6address / IPvFuture  ) "]"
*)
let parser () = failwith "todo"
