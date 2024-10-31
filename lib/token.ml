open Shaded
open Angstrom

(*
   tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*"
   / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
   / DIGIT / ALPHA
   ; any VCHAR, except delimiters
*)
let tchar_parser =
  let is_allowed = function
    | '!'
    | '#'
    | '$'
    | '%'
    | '&'
    | '\''
    | '*'
    | '+'
    | '-'
    | '.'
    | '^'
    | '_'
    | '`'
    | '|'
    | '~' -> true
    | _ -> false
  in
  satisfy is_allowed <|> digit <|> alpha
;;

(*
   token = 1*tchar
*)
let parser = many1 tchar_parser >>| String.of_list
