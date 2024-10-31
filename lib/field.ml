open Shaded
open Angstrom
open Angstrom.Let_syntax

(* Is used in request & response *)
module Value = struct
  type t =
    | FieldValue of string
    | FieldValues of string list
    | Parameters of (string * string) list

  let sexp_of_t =
    let open Sexp in
    function
    | FieldValue fv -> l [ a "FieldValue"; String.sexp_of_t fv ]
    | FieldValues fvs -> l [ a "FieldValues"; l (List.map String.sexp_of_t fvs) ]
    | Parameters ps ->
      l [ a "Parameters"; l (List.map (fun (key, value) -> l [ a key; a value ]) ps) ]
  ;;

  let pp fmt = function
    | FieldValue fv -> Format.fprintf fmt "FieldValue %s" fv
    | FieldValues fvs ->
      Format.fprintf
        fmt
        "FieldValues [%a]"
        (Format.pp_print_list ~pp_sep:Format.semi Format.pp_print_text)
        fvs
    | Parameters ps ->
      Format.fprintf
        fmt
        "Parameters %a"
        (Format.pp_print_list ~pp_sep:Format.semi (fun fmt (key, value) ->
           Format.fprintf fmt "%s=%s" key value))
        ps
  ;;
end

type t =
  { key : string
  ; value : Value.t
  }

let pp fmt { key; value } =
  Format.fprintf fmt "Field { key=%s; value=%a }" key Value.pp value
;;

(*
   RFC9112
   field-line = field-name ":" OWS field-value OWS
   OWS        = *( SP / HTAB ) ; optional whitespace

   field-name = token
   token      = 1*tchar
   tchar      = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA ; any VCHAR, except delimiters

   RFC9110
   field-value    = *field-content
   field-content  = field-vchar [ 1*( SP / HTAB / field-vchar ) field-vchar ]
   field-vchar    = VCHAR / obs-text
   obs-text       = %x80-FF
   VCHAR          =  %x21-7E ; according to RFC5234

   https://datatracker.ietf.org/doc/html/rfc9110#section-5.6.1.2
   #element => [ element ] *( OWS "," OWS [ element ] )

   https://datatracker.ietf.org/doc/html/rfc9110#section-5.6.4
   quoted-string  = DQUOTE *( qdtext / quoted-pair ) DQUOTE
   qdtext         = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text
   quoted-pair    = '\' ( HTAB / SP / VCHAR / obs-text )

   https://datatracker.ietf.org/doc/html/rfc9110#section-5.6.5
   comment        = "(" *( ctext / quoted-pair / comment ) ")"
   ctext          = HTAB / SP / %x21-27 / %x2A-5B / %x5D-7E / obs-text

   https://datatracker.ietf.org/doc/html/rfc9110#section-5.6.6
   parameters      = *( OWS ";" OWS [ parameter ] )
   parameter       = parameter-name "=" parameter-value
   parameter-name  = token
   parameter-value = ( token / quoted-string )

   TODO: Handle OBS https://datatracker.ietf.org/doc/html/rfc9112#name-obsolete-line-folding
*)
let parser =
  let ows = many (sp <|> htab) in
  let obs_text =
    satisfy (function
      | '\128' .. '\255' -> true
      | _ -> false)
  in
  let vchar =
    satisfy (function
      | '\033' .. '\126' -> true
      | _ -> false)
  in
  let vchar_without_comma =
    satisfy (function
      | '\044' -> false
      | '\033' .. '\126' -> true
      | _ -> false)
  in
  let field_vchar vchar = vchar <|> obs_text in
  let field_content vchar =
    (*
       field-content  = field-vchar *1( inner-value )
       inner-value    = 1*( SP / HTAB / field-vchar ) field-vchar
    *)
    let inner_field_content field_vchar =
      let%bind fst =
        let%bind fst = many1 (sp <|> htab <|> field_vchar) in
        let%map last = field_vchar in
        List.append fst [ last ]
      in
      let%map rest = field_vchar in
      List.append fst [ rest ]
    in
    let%bind first = field_vchar vchar in
    let%map rest = option [] (inner_field_content (field_vchar vchar)) in
    first :: rest
  in
  let element_field_value : string Angstrom.t =
    many (field_content vchar_without_comma)
    >>| fun cs -> List.concat cs |> List.to_seq |> String.of_seq
  in
  (*
     quoted-string  = DQUOTE *( qdtext / quoted-pair ) DQUOTE
     qdtext         = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text
     quoted-pair    = '\' ( HTAB / SP / VCHAR / obs-text )
  *)
  let quoted_string =
    let qdtext =
      htab
      <|> sp
      <|> satisfy (function
        | '\x21' -> true
        | '\x23' .. '\x5b' -> true
        | '\x5d' .. '\x7e' -> true
        | _ -> false)
      <|> obs_text
      >>| fun c -> Char.escaped c
    in
    let quoted_pair =
      let%bind s = char '\\' in
      let%map res = htab <|> sp <|> vchar <|> obs_text in
      Char.escaped s ^ Char.escaped res
    in
    let%map parts = quoted (many (qdtext <|> quoted_pair)) in
    String.join parts
  in
  let is_allowsed_special_char = function
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
  let tchar = satisfy (fun c -> is_allowsed_special_char c || is_num c || is_alpha c) in
  let token = many1 tchar >>| fun cs -> cs |> List.to_seq |> String.of_seq in
  (*
     #element => *1( element *( OWS "," OWS element ) )
  *)
  let element field_value =
    (* element *( OWS "," OWS element ) *)
    let inner =
      let%bind el = field_value in
      let%map rest = many (ows *> char ',' *> ows *> field_value) in
      el :: rest
    in
    option [] inner
    >>= fun fvs ->
    if List.filter (fun v -> v <> "") fvs |> List.is_empty
    then fail "Field value needs at least a single non-empty element"
    else (
      match fvs with
      | [ fv ] -> return (Value.FieldValue fv)
      | _ -> return (Value.FieldValues fvs))
  in
  let parameters =
    let parameter_value = token <|> quoted_string in
    let parameter_name = token in
    let parameter =
      let%bind pname = parameter_name <* char '=' in
      let%map pvalue = parameter_value in
      pname, pvalue
    in
    let%map ps = many1 (ows *> char ';' *> ows *> zero_or_one parameter) in
    List.filter Option.is_some ps |> List.map Option.get |> fun ps -> Value.Parameters ps
  in
  let quoted_string_t = quoted_string >>| fun fv -> Value.FieldValue fv in
  let%bind key = token <* char ':' in
  let%bind value =
    ows
    *> (quoted_string_t
        <|> parameters
        <|> element element_field_value (* <|> field_value_t *))
    <* ows
  in
  return { key; value }
;;

let equal t1 t2 = t1.key = t2.key && t1.value = t2.value

(* ((key key) (value value)) *)
let sexp_of_t t =
  let open Sexp in
  l [ l [ a "key"; String.sexp_of_t t.key ]; l [ a "value"; Value.sexp_of_t t.value ] ]
;;

module Tests = struct
  let parse str =
    let field = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str in
    match field with
    | Error e -> failwith e
    | Ok field -> field
  ;;

  let parse_error str =
    let field = Angstrom.parse_string ~consume:Angstrom.Consume.All parser str in
    match field with
    | Error e -> e
    | Ok field -> failwith (Format.asprintf "%a" pp field)
  ;;

  let%test_unit "parser: should parse simple field" =
    let field = parse "key: value" in
    [%test_result: t] ~expect:{ key = "key"; value = FieldValue "value" } field
  ;;

  let%test_unit "parser: should parse field with multi-value" =
    let field = parse "key: value1, value2" in
    [%test_result: t]
      ~expect:{ key = "key"; value = Value.FieldValues [ "value1"; "value2" ] }
      field
  ;;

  let%test_unit "parser: should parse examples from RFC" =
    [%test_result: t]
      ~expect:{ key = "key"; value = Value.FieldValues [ "foo"; "bar" ] }
      (parse "key: foo,bar");
    [%test_result: t]
      ~expect:{ key = "key"; value = Value.FieldValues [ "foo"; "bar"; "" ] }
      (parse "key: foo ,bar,");
    [%test_result: t]
      ~expect:{ key = "key"; value = Value.FieldValues [ "foo"; ""; "bar"; "charlie" ] }
      (parse "key: foo , ,bar,charlie")
  ;;

  let%test_unit "parser: should fail for examples from RFC" =
    let expect = ": Field value needs at least a single non-empty element" in
    [%test_result: String.t] ~expect (parse_error "key: ");
    [%test_result: String.t] ~expect (parse_error "key: ,");
    [%test_result: String.t] ~expect (parse_error "key: ,   ,")
  ;;

  let%test_unit "parse: should parse quoted field with multi-value" =
    let field = parse "key: \"value1, value2\"" in
    [%test_result: t]
      ~expect:{ key = "key"; value = Value.FieldValue "value1, value2" }
      field
  ;;

  let%test_unit "parse: should parse field with invalid multi-value" =
    let field = parse "key: value1, " in
    [%test_result: t]
      ~expect:{ key = "key"; value = Value.FieldValues [ "value1"; "" ] }
      field
  ;;

  let%test_unit "parse: should fail for faulty key" =
    let field = parse_error "key : value1, " in
    [%test_result: String.t] ~expect:": char ':'" field
  ;;

  let%test_unit "parse: should parse single parameter" =
    let field = parse "key: ;key=value" in
    [%test_result: t]
      ~expect:{ key = "key"; value = Value.Parameters [ "key", "value" ] }
      field
  ;;
end

let write () = failwith "todo"
