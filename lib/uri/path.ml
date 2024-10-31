open Shaded
open Angstrom
open Angstrom.Let_syntax

type segment =
  | Segment of string
  | Segment_nz of string
  | Segment_nz_nc of string

let sexp_of_segment =
  let open Sexp in
  function
  | Segment s -> l [ a "Segment"; a s ]
  | Segment_nz s -> l [ a "Segment_nz"; a s ]
  | Segment_nz_nc s -> l [ a "Segment_nz_nc"; a s ]
;;

let compare_segment s1 s2 =
  match s1, s2 with
  | Segment s1, Segment s2 -> String.compare s1 s2
  | Segment_nz s1, Segment_nz s2 -> String.compare s1 s2
  | Segment_nz_nc s1, Segment_nz_nc s2 -> String.compare s1 s2
  | _ -> Int.min_int
;;

(*
   / path-abempty  ; begins with "/" or is empty
   / path-absolute ; begins with "/" but not "//"
   / path-noscheme ; begins with a non-colon segment
   / path-rootless ; begins with a segment
   / path-empty    ; zero characters
*)
type t =
  | PathAbEmpty of segment list
  | PathAbsolute of segment list
  | PathNoScheme of segment list
  | PathRootless of segment list
  | PathEmpty

let sexp_of_t =
  let open Sexp in
  function
  | PathAbEmpty s -> l [ a "PathAbEmpty"; List.sexp_of_t sexp_of_segment s ]
  | PathAbsolute s -> l [ a "PathAbsolute"; List.sexp_of_t sexp_of_segment s ]
  | PathNoScheme s -> l [ a "PathNoScheme"; List.sexp_of_t sexp_of_segment s ]
  | PathRootless s -> l [ a "PathRootless"; List.sexp_of_t sexp_of_segment s ]
  | PathEmpty -> a "PathEmpty"
;;

(*
   pchar = unreserved / pct-encoded / sub-delims / ":" / "@"
*)
let pchar_parser =
  let parser =
    uri_unreserved <|> Pct_encode.parser <|> uri_subdelim <|> char ':' <|> char '@'
  in
  parser <?> "pchar_parser"
;;

let%test_unit "pchar_parser" =
  let parse = parse pchar_parser in
  [%test_result: Char.t] (parse "-") ~expect:'-';
  [%test_result: Char.t] (parse "!") ~expect:'!';
  [%test_result: Char.t] (parse "%23") ~expect:'#';
  [%test_result: Char.t] (parse ":") ~expect:':';
  [%test_result: Char.t] (parse "@") ~expect:'@'
;;

(*
   segment = *pchar
*)
let segment_parser =
  let parser =
    let%map chars = many pchar_parser in
    List.to_seq chars |> String.of_seq |> fun s -> Segment s
  in
  parser <?> "segment_parser"
;;

let%test_unit "segment_parser" =
  let parse = parse segment_parser in
  [%test_result: segment] (parse "abs&cde%23") ~expect:(Segment "abs&cde#");
  [%test_result: segment] (parse "") ~expect:(Segment "")
;;

(*
   segment-nz = 1*pchar
*)
let segment_nz_parser =
  let parser =
    let%map chars = many1 pchar_parser in
    List.to_seq chars |> String.of_seq |> fun s -> Segment_nz s
  in
  parser <?> "segment_nz_parser"
;;

let%test_unit "segment_nz_parser" =
  let parse = parse segment_nz_parser in
  [%test_result: segment] (parse ":(abcabc)") ~expect:(Segment_nz ":(abcabc)")
;;

(*
   segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" ) ; non-zero-length segment without any colon ":"
*)
let segment_nz_nc_parser =
  let parser =
    let inner_parser =
      uri_unreserved <|> Pct_encode.parser <|> uri_subdelim <|> char '@'
    in
    let%bind segment = many1 inner_parser in
    let%bind next = peek_char in
    match next with
    | Some ':' -> fail "segment ':'"
    | _ ->
      let segment = String.of_list segment in
      Segment_nz_nc segment |> return
  in
  parser <?> "segment_nz_nc_parser"
;;

let%test_unit "segment_nz_nc_parser" =
  let parse = parse segment_nz_nc_parser in
  [%test_result: segment] (parse "(abc%23)") ~expect:(Segment_nz_nc "(abc#)");
  let parse = parse_res segment_nz_nc_parser in
  [%test_result: (segment, String.t) Result.t]
    (parse ":abc")
    ~expect:(Error "segment_nz_nc_parser: char '@'");
  [%test_result: (segment, String.t) Result.t]
    (parse "")
    ~expect:(Error "segment_nz_nc_parser: not enough input");
  [%test_result: (segment, String.t) Result.t]
    (parse ":a")
    ~expect:(Error "segment_nz_nc_parser: char '@'");
  [%test_result: (segment, String.t) Result.t]
    (parse "a:")
    ~expect:(Error "segment_nz_nc_parser: segment ':'")
;;

(*
   path-empty = 0<pchar>
*)
let path_empty_parser =
  let parser =
    let%bind _ = count 0 pchar_parser in
    let%bind _ = end_of_input in
    return PathEmpty
  in
  parser <?> "path_empty_parser"
;;

let%test_unit "path_empty_parser" =
  let parse = parse path_empty_parser in
  [%test_result: t] (parse "") ~expect:PathEmpty
;;

(*
   path-abempty  = *( "/" segment )
*)
let path_abempty_parser =
  let parser =
    let inner_parser = char '/' *> segment_parser in
    let%bind ab_empty = many inner_parser in
    PathAbEmpty ab_empty |> return
  in
  parser <?> "path_abempty_parser"
;;

let%test_unit "path_abempty_parser" =
  let parse = parse path_abempty_parser in
  [%test_result: t] (parse "") ~expect:(PathAbEmpty []);
  [%test_result: t] (parse "/") ~expect:(PathAbEmpty [ Segment "" ]);
  [%test_result: t] (parse "//") ~expect:(PathAbEmpty [ Segment ""; Segment "" ]);
  [%test_result: t]
    (parse "/abc/def")
    ~expect:(PathAbEmpty [ Segment "abc"; Segment "def" ])
;;

(*
   path-rootless = segment-nz *( "/" segment )
*)
let path_rootless_parser =
  let parser =
    let%bind segment_nz = segment_nz_parser in
    let%map segments = many (char '/' *> segment_parser) in
    PathRootless (segment_nz :: segments)
  in
  parser <?> "path_rootless_parser"
;;

let%test_unit "path_rootless_parser" =
  let parse = parse path_rootless_parser in
  [%test_result: t]
    (parse "abc/def")
    ~expect:(PathRootless [ Segment_nz "abc"; Segment "def" ]);
  [%test_result: t]
    (parse "ab:c/def")
    ~expect:(PathRootless [ Segment_nz "ab:c"; Segment "def" ]);
  [%test_result: t] (parse "a:") ~expect:(PathRootless [ Segment_nz "a:" ]);
  [%test_result: t] (parse ":a") ~expect:(PathRootless [ Segment_nz ":a" ]);
  let parse = parse_res path_rootless_parser in
  [%test_result: (t, String.t) Result.t]
    (parse "")
    ~expect:
      (Error "path_rootless_parser > segment_nz_parser > pchar_parser: not enough input")
;;

(*
   path-noscheme = segment-nz-nc *( "/" segment )
*)
let path_noscheme_parser =
  let parser =
    let%bind segment_nz_nc = segment_nz_nc_parser in
    let%map segments = many (char '/' *> segment_parser) <?> "segments" in
    PathNoScheme (segment_nz_nc :: segments)
  in
  parser <?> "path_noscheme_parser"
;;

let%test_unit "path_noscheme_parser" =
  let parse = parse path_noscheme_parser in
  [%test_result: t]
    (parse "abc/(cde)/%23")
    ~expect:(PathNoScheme [ Segment_nz_nc "abc"; Segment "(cde)"; Segment "#" ]);
  let parse = parse_res path_noscheme_parser in
  [%test_result: (t, String.t) Result.t]
    (parse ":abc")
    ~expect:(Error "path_noscheme_parser > segment_nz_nc_parser: char '@'");
  [%test_result: (t, String.t) Result.t]
    (parse "ab:c")
    ~expect:(Error "path_noscheme_parser > segment_nz_nc_parser: segment ':'");
  [%test_result: (t, String.t) Result.t]
    (parse "ab:c/def")
    ~expect:(Error "path_noscheme_parser > segment_nz_nc_parser: segment ':'");
  [%test_result: (t, String.t) Result.t]
    (parse ":abc/(cde)/%23")
    ~expect:(Error "path_noscheme_parser > segment_nz_nc_parser: char '@'")
;;

(*
   path-absolute = "/" [ segment-nz *( "/" segment ) ]
*)
let path_absolute_parser =
  let parser =
    let inner_parser =
      let%bind segment_nz = segment_nz_parser in
      let%map segments = many (char '/' *> segment_parser) in
      segment_nz :: segments
    in
    let%bind _ = char '/' in
    let%bind next = peek_char in
    match next with
    | Some '/' -> fail "absolute path must not stat with //"
    | _ ->
      let%bind segments = range 0 1 inner_parser in
      let segments = List.flatten segments in
      PathAbsolute segments |> return
  in
  parser <?> "path_absolute_parser"
;;

let%test_unit "path_absolute_parser" =
  let parse = parse path_absolute_parser in
  [%test_result: t]
    (parse "/abc/cde/%23")
    ~expect:(PathAbsolute [ Segment_nz "abc"; Segment "cde"; Segment "#" ]);
  [%test_result: t] (parse "/") ~expect:(PathAbsolute []);
  [%test_result: t]
    (parse "/ab:c/def")
    ~expect:(PathAbsolute [ Segment_nz "ab:c"; Segment "def" ])
;;

(*
   https://datatracker.ietf.org/doc/html/rfc3986#section-3.3

   path          = path-abempty    ; begins with "/" or is empty
   / path-absolute   ; begins with "/" but not "//"
   / path-noscheme   ; begins with a non-colon segment
   / path-rootless   ; begins with a segment
   / path-empty      ; zero characters

   path-abempty  = *( "/" segment )
   path-absolute = "/" [ segment-nz *( "/" segment ) ]
   path-noscheme = segment-nz-nc *( "/" segment )
   path-rootless = segment-nz *( "/" segment )
   path-empty    = 0<pchar>
   segment       = *pchar
   segment-nz    = 1*pchar
   segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
   ; non-zero-length segment without any colon ":"

   pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
*)
let parser =
  path_empty_parser
  <|> path_noscheme_parser
  <|> path_rootless_parser
  <|> path_absolute_parser
  <|> path_abempty_parser
;;

let%test_unit "parser" =
  let parse = parse parser in
  [%test_result: t] (parse "") ~expect:PathEmpty;
  [%test_result: t] (parse "/abs") ~expect:(PathAbsolute [ Segment_nz "abs" ]);
  [%test_result: t] (parse "/") ~expect:(PathAbsolute []);
  [%test_result: t] (parse "//abc") ~expect:(PathAbEmpty [ Segment ""; Segment "abc" ]);
  [%test_result: t]
    (parse "abc/")
    ~expect:(PathNoScheme [ Segment_nz_nc "abc"; Segment "" ]);
  [%test_result: t]
    (parse "ab:c/def")
    ~expect:(PathRootless [ Segment_nz "ab:c"; Segment "def" ]);
  [%test_result: t]
    (parse "abc//")
    ~expect:(PathNoScheme [ Segment_nz_nc "abc"; Segment ""; Segment "" ]);
  [%test_result: t] (parse ":abc") ~expect:(PathRootless [ Segment_nz ":abc" ]);
  [%test_result: t] (parse "a:b:c") ~expect:(PathRootless [ Segment_nz "a:b:c" ])
;;
