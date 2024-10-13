module StringMap = Map.Make (String)

type t = string StringMap.t

(* query = *( pchar / "/" / "?" ) *)
