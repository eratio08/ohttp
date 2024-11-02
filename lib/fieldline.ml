open Shaded
open Angstrom
open Angstrom.Let_syntax

(*
   OWS = *( SP / HTAB ) ; optional whitespace
*)
let ows = many (sp <|> htab)

(*
   field-vchar = VCHAR / obs-text
*)
let field_vchar_parser = vchar <|> http_obs_text

(*
   field-content = field-vchar [ 1*( SP / HTAB / field-vchar ) field-vchar ]
*)
let field_content_parser =
  let inner_p =
    let%bind fst = many1 (sp <|> htab <|> field_vchar_parser) in
    let%bind rst = field_vchar_parser in
    return (fst @ [ rst ])
  in
  let%bind first = field_vchar_parser in
  let%bind rest = maybe inner_p in
  let rest = Option.value ~default:[] rest in
  first :: rest |> String.of_list |> return
;;

(*
   field-value = *field-content
*)
let field_value_parser = many field_content_parser

(*
   tchar      = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters
*)
let tchar_parser =
  let is_tchar = function
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
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | '0' .. '9' -> true
    | _ -> false
  in
  satisfy is_tchar
;;

(*
   token      = 1*tchar
*)
let token_parser = many1 tchar_parser >>| String.of_list

(*
   field-name = token
*)
let field_name_parser = token_parser

(*
   field-line = field-name ":" OWS field-value OWS
*)
let field_line_parser =
  let%bind name = field_name_parser <* char ':' <* ows in
  let%bind value = field_value_parser <* ows in
  return (name, value)
;;

module FieldValueParser = struct
  module Element = struct
    (*
       https://datatracker.ietf.org/doc/html/rfc9110#section-5.6.1.2
       #element => [ element ] *( OWS "," OWS [ element ] )
    *)
    let element_parser element_parser =
      let%bind head_element = maybe element_parser in
      let%bind tail_elements = many (ows *> char ',' *> ows *> maybe element_parser) in
      let elements = head_element :: tail_elements in
      let elements = List.filter_map Sys.opaque_identity elements in
      return elements
    ;;

    let%test_unit "element_parser" =
      let parse = parse (element_parser token_parser) in
      [%test_result: String.t List.t] (parse "first , ") ~expect:[ "first" ];
      [%test_result: String.t List.t] (parse "first, ") ~expect:[ "first" ];
      [%test_result: String.t List.t] (parse "first ,") ~expect:[ "first" ]
    ;;
  end

  module QuotedString = struct
    (*
       quoted-pair = '\' ( HTAB / SP / VCHAR / obs-text )
    *)
    let quoted_pair_parser =
      let%bind q_pair = char '\\' *> (htab <|> sp <|> vchar <|> http_obs_text) in
      return q_pair
    ;;

    (*
       qdtext = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text
    *)
    let qdtext_parser =
      htab
      <|> sp
      <|> satisfy (function
        | '\x21' -> true
        | _ -> false)
      <|> satisfy (function
        | '\x23' .. '\x5b' -> true
        | _ -> false)
      <|> satisfy (function
        | '\x5d' .. '\x7e' -> true
        | _ -> false)
      <|> http_obs_text
    ;;

    (*
       https://datatracker.ietf.org/doc/html/rfc9110#section-5.6.4
       quoted-string  = DQUOTE *( qdtext / quoted-pair ) DQUOTE
    *)
    let quoted_string_parser =
      let%bind str =
        char '\x22' *> many (qdtext_parser <|> quoted_pair_parser) <* char '\x22'
      in
      let str = String.of_list str in
      return str
    ;;
  end

  module Comment = struct
    (*
       ctext = HTAB / SP / %x21-27 / %x2A-5B / %x5D-7E / obs-text
    *)
    let ctext_parser =
      htab
      <|> sp
      <|> satisfy (function
        | '\x21' .. '\x27' -> true
        | _ -> false)
      <|> satisfy (function
        | '\x2a' .. '\x5b' -> true
        | _ -> false)
      <|> satisfy (function
        | '\x5d' .. '\x7e' -> true
        | _ -> false)
      <|> http_obs_text
    ;;

    type c =
      | CommentC of char
      | Comment of c list

    (*
       https://datatracker.ietf.org/doc/html/rfc9110#section-5.6.5
       comment = "(" *( ctext / quoted-pair / comment ) ")"
    *)
    (* TODO: fix infinite loop *)
    (* let comment_parser = *)
    (*   let ctext_parser = ctext_parser >>| fun c -> CommentC c in *)
    (*   let quoted_pair_parser = QuotedString.quoted_pair_parser >>| fun c -> CommentC c in *)
    (*   let flatten cs = *)
    (*     let rec loop res = function *)
    (*       | CommentC c -> res ^ Char.escaped c *)
    (*       | Comment cs -> List.fold_left (fun acc c -> loop acc c) res cs *)
    (*     in *)
    (*     loop "" cs *)
    (*   in *)
    (*   let rec comment_p () = *)
    (*     char '(' *)
    (*     *> many *)
    (*          (ctext_parser *)
    (*           <|> quoted_pair_parser *)
    (*           <|> (comment_p () >>= fun cs -> return (Comment cs))) *)
    (*     <* char ')' *)
    (*   in *)
    (*   let%bind comment = comment_p () in *)
    (*   let comment = List.fold_left (fun acc c -> acc ^ flatten c) "" comment in *)
    (*   return comment *)
    (* ;; *)
  end

  module Parameter = struct
    (*
       parameter-value = ( token / quoted-string )
    *)
    let parameter_value_parser = token_parser <|> QuotedString.quoted_string_parser

    (*
       parameter-name = token
    *)
    let parameter_name_parser = token_parser

    (*
       parameter = parameter-name "=" parameter-value
    *)
    let parameter_parser =
      let%bind name = parameter_name_parser <* char '=' in
      let%bind value = parameter_value_parser in
      return (name, value)
    ;;

    (*
       https://datatracker.ietf.org/doc/html/rfc9110#section-5.6.6
       parameters      = *( OWS ";" OWS [ parameter ] )
    *)
    let parameters_parser =
      let%bind parameters = many (ows *> char ';' *> ows *> maybe parameter_parser) in
      let parameters = List.filter_map (fun i -> i) parameters in
      return parameters
    ;;
  end

  module ObsoletedLineFolding = struct
    (*
       TODO: Handle OBS https://datatracker.ietf.org/doc/html/rfc9112#name-obsolete-line-folding
    *)
  end
end

(* let parser = *)
(*   let element_field_value : string Angstrom.t = *)
(*     many (field_content vchar_without_comma) *)
(*     >>| fun cs -> List.concat cs |> List.to_seq |> String.of_seq *)
(*   in *)
(*   (* *)
     (*      quoted-string  = DQUOTE *( qdtext / quoted-pair ) DQUOTE *)
     (*      qdtext         = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text *)
     (*      quoted-pair    = '\' ( HTAB / SP / VCHAR / obs-text ) *)
     (*   *) *)
(*   let quoted_string = *)
(*     let qdtext = *)
(*       htab *)
(*       <|> sp *)
(*       <|> satisfy (function *)
(*         | '\x21' -> true *)
(*         | '\x23' .. '\x5b' -> true *)
(*         | '\x5d' .. '\x7e' -> true *)
(*         | _ -> false) *)
(*       <|> obs_text *)
(*       >>| fun c -> Char.escaped c *)
(*     in *)
(*     let quoted_pair = *)
(*       let%bind s = char '\\' in *)
(*       let%map res = htab <|> sp <|> vchar <|> obs_text in *)
(*       Char.escaped s ^ Char.escaped res *)
(*     in *)
(*     let%map parts = quoted (many (qdtext <|> quoted_pair)) in *)
(*     String.join parts *)
(*   in *)
(*   let is_allowsed_special_char = function *)
(*     | '!' *)
(*     | '#' *)
(*     | '$' *)
(*     | '%' *)
(*     | '&' *)
(*     | '\'' *)
(*     | '*' *)
(*     | '+' *)
(*     | '-' *)
(*     | '.' *)
(*     | '^' *)
(*     | '_' *)
(*     | '`' *)
(*     | '|' *)
(*     | '~' -> true *)
(*     | _ -> false *)
(*   in *)
(*   let tchar = satisfy (fun c -> is_allowsed_special_char c || is_num c || is_alpha c) in *)
(*   let token = many1 tchar >>| fun cs -> cs |> List.to_seq |> String.of_seq in *)
(*   (* *)
     (*      #element => *1( element *( OWS "," OWS element ) ) *)
     (*   *) *)
(*   let element field_value = *)
(*     (* element *( OWS "," OWS element ) *) *)
(*     let inner = *)
(*       let%bind el = field_value in *)
(*       let%map rest = many (ows *> char ',' *> ows *> field_value) in *)
(*       el :: rest *)
(*     in *)
(*     option [] inner *)
(*     >>= fun fvs -> *)
(*     if List.filter (fun v -> v <> "") fvs |> List.is_empty *)
(*     then fail "Field value needs at least a single non-empty element" *)
(*     else ( *)
(*       match fvs with *)
(*       | [ fv ] -> return (Value.FieldValue fv) *)
(*       | _ -> return (Value.FieldValues fvs)) *)
(*   in *)
(*   let parameters = *)
(*     let parameter_value = token <|> quoted_string in *)
(*     let parameter_name = token in *)
(*     let parameter = *)
(*       let%bind pname = parameter_name <* char '=' in *)
(*       let%map pvalue = parameter_value in *)
(*       pname, pvalue *)
(*     in *)
(*     let%map ps = many1 (ows *> char ';' *> ows *> zero_or_one parameter) in *)
(*     List.filter Option.is_some ps |> List.map Option.get |> fun ps -> Value.Parameters ps *)
(*   in *)
(*   let quoted_string_t = quoted_string >>| fun fv -> Value.FieldValue fv in *)
(*   let%bind key = token <* char ':' in *)
(*   let%bind value = *)
(*     ows *)
(*     *> (quoted_string_t *)
(*         <|> parameters *)
(*         <|> element element_field_value (* <|> field_value_t *)) *)
(*     <* ows *)
(*   in *)
(*   return { key; value } *)
(* ;; *)
(**)
(* let equal t1 t2 = t1.key = t2.key && t1.value = t2.value *)
(**)
(* (* ((key key) (value value)) *) *)
(* let sexp_of_t t = *)
(*   let open Sexp in *)
(*   l [ l [ a "key"; String.sexp_of_t t.key ]; l [ a "value"; Value.sexp_of_t t.value ] ] *)
(* ;; *)
(**)
(* module Tests = struct *)
(*   let parse str = *)
(*     let field = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str in *)
(*     match field with *)
(*     | Error e -> failwith e *)
(*     | Ok field -> field *)
(*   ;; *)
(**)
(*   let parse_error str = *)
(*     let field = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str in *)
(*     match field with *)
(*     | Error e -> e *)
(*     | Ok field -> failwith (Format.asprintf "%a" pp field) *)
(*   ;; *)
(**)
(*   let%test_unit "parser: should parse simple field" = *)
(*     let field = parse "key: value" in *)
(*     [%test_result: t] ~expect:{ key = "key"; value = FieldValue "value" } field *)
(*   ;; *)
(**)
(*   let%test_unit "parser: should parse field with multi-value" = *)
(*     let field = parse "key: value1, value2" in *)
(*     [%test_result: t] *)
(*       ~expect:{ key = "key"; value = Value.FieldValues [ "value1"; "value2" ] } *)
(*       field *)
(*   ;; *)
(**)
(*   let%test_unit "parser: should parse examples from RFC" = *)
(*     [%test_result: t] *)
(*       ~expect:{ key = "key"; value = Value.FieldValues [ "foo"; "bar" ] } *)
(*       (parse "key: foo,bar"); *)
(*     [%test_result: t] *)
(*       ~expect:{ key = "key"; value = Value.FieldValues [ "foo"; "bar"; "" ] } *)
(*       (parse "key: foo ,bar,"); *)
(*     [%test_result: t] *)
(*       ~expect:{ key = "key"; value = Value.FieldValues [ "foo"; ""; "bar"; "charlie" ] } *)
(*       (parse "key: foo , ,bar,charlie") *)
(*   ;; *)
(**)
(*   let%test_unit "parser: should fail for examples from RFC" = *)
(*     let expect = ": Field value needs at least a single non-empty element" in *)
(*     [%test_result: String.t] ~expect (parse_error "key: "); *)
(*     [%test_result: String.t] ~expect (parse_error "key: ,"); *)
(*     [%test_result: String.t] ~expect (parse_error "key: ,   ,") *)
(*   ;; *)
(**)
(*   let%test_unit "parse: should parse quoted field with multi-value" = *)
(*     let field = parse "key: \"value1, value2\"" in *)
(*     [%test_result: t] *)
(*       ~expect:{ key = "key"; value = Value.FieldValue "value1, value2" } *)
(*       field *)
(*   ;; *)
(**)
(*   let%test_unit "parse: should parse field with invalid multi-value" = *)
(*     let field = parse "key: value1, " in *)
(*     [%test_result: t] *)
(*       ~expect:{ key = "key"; value = Value.FieldValues [ "value1"; "" ] } *)
(*       field *)
(*   ;; *)
(**)
(*   let%test_unit "parse: should fail for faulty key" = *)
(*     let field = parse_error "key : value1, " in *)
(*     [%test_result: String.t] ~expect:": char ':'" field *)
(*   ;; *)
(**)
(*   let%test_unit "parse: should parse single parameter" = *)
(*     let field = parse "key: ;key=value" in *)
(*     [%test_result: t] *)
(*       ~expect:{ key = "key"; value = Value.Parameters [ "key", "value" ] } *)
(*       field *)
(*   ;; *)
(* end *)
(**)
(* let write () = failwith "todo" *)
