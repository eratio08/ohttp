module Bool = Shaded.Bool

(*
   https://datatracker.ietf.org/doc/html/rfc3986#section-2.2
*)
let is_unreserved_char = function
  | '-' | '.' | '_' | '~' -> true
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
;;

let is_sub_delim = function
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' -> true
  | _ -> false
;;

let is_pct_encoded = function
  (* gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@" *)
  | "%3a" | "%2f" | "%3f" | "%23" | "%5b" | "%5d" | "%40" -> true
  (* sub-delims  = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "=" *)
  | "%21" | "%24" | "%26" | "%27" | "%28" | "%29" | "%2a" | "%2b" | "%2c" | "%3b" | "%3d"
    -> true
  | _ -> false
;;

let%test_unit "is_pct_encoded: should return true for percentage encoded characters" =
  (* ' *)
  [%test_result: Bool.t] ~expect:true (is_pct_encoded "%27");
  (* : *)
  [%test_result: Bool.t] ~expect:true (is_pct_encoded "%3a");
  (* p *)
  [%test_result: Bool.t] ~expect:false (is_pct_encoded "%70")
;;
