module Angstrom = Shaded.Angstrom

type t = { segments : string list }

type segment =
  | Segment of string
  | Segment_nz of string
  | Segment_nz_nc of string
  | Empty

(*
   / path-abempty  ; begins with "/" or is empty
   / path-absolute ; begins with "/" but not "//"
   / path-noscheme ; begins with a non-colon segment
   / path-rootless ; begins with a segment
   / path-empty    ; zero characters
*)
type path =
  | PathAbEmpty of segment list
  | PathAbsolute of segment list
  | PathNoScheme of segment list
  | PathRootless of segment list
  | PathEmpty

let add_segment s = function
  | PathAbEmpty seg -> s :: seg
  | PathAbsolute seg -> s :: seg
  | PathNoScheme seg -> s :: seg
  | PathRootless seg -> s :: seg
  | PathEmpty -> []
;;

(* https://datatracker.ietf.org/doc/html/rfc3986#section-3.3 *)
let parser =
  let open Angstrom in
  let open Angstrom.Let_syntax in
  (* pchar = unreserved / pct-encoded / sub-delims / ":" / "@" *)
  let pchar =
    satisfy (fun c ->
      Util.is_unreserved_char c
      || Util.is_sub_delim c
      ||
      match c with
      | ':' | '@' -> true
      | '%' -> true
      | _ -> false)
  in
  (* segment = *pchar *)
  let segment =
    let%map chars = many pchar in
    List.to_seq chars |> String.of_seq |> fun s -> Segment s
  in
  (* segment-nz = 1*pchar *)
  let segment_nz =
    let%map chars = many1 pchar in
    List.to_seq chars |> String.of_seq |> fun s -> Segment_nz s
  in
  (* segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" ) ; non-zero-length segment without any colon ":" *)
  let segment_nz_nc =
    let%map segments =
      many1
        (satisfy (fun c ->
           Util.is_unreserved_char c
           || Util.is_sub_delim c
           ||
           match c with
           | '@' -> true
           | '%' -> true
           | _ -> false))
    in
    segments |> List.to_seq |> String.of_seq |> fun s -> Segment_nz_nc s
  in
  (* path-empty = 0<pchar> *)
  (* let path_empty = *)
  (*   let%map _ = string "" in *)
  (*   PathEmpty *)
  (* in *)
  (* path-abempty  = *( "/" segment ) *)
  let path_abempty = many (char '/' *> segment) >>= fun p -> PathAbEmpty p |> return in
  (* path-rootless = segment-nz *( "/" segment ) *)
  let path_rootless =
    let%bind segment_nz = segment_nz in
    let%map segments = path_abempty in
    PathRootless (add_segment segment_nz segments)
  in
  (* path-noscheme = segment-nz-nc *( "/" segment ) *)
  let path_noscheme =
    let%bind segment_nz_nc = segment_nz_nc in
    let%map segments = path_abempty in
    PathNoScheme (add_segment segment_nz_nc segments)
  in
  (* path-absolute = "/" [ segment-nz *( "/" segment ) ] *)
  let path_absolute =
    let%bind _ = char '/' in
    let%map segments =
      option
        []
        (let%bind segment_nz = segment_nz in
         let%map segments = path_abempty in
         add_segment segment_nz segments)
    in
    PathAbsolute segments
  in
  (*
     path =
     / path-abempty  ; begins with "/" or is empty
     / path-absolute ; begins with "/" but not "//"
     / path-noscheme ; begins with a non-colon segment
     / path-rootless ; begins with a segment
     / path-empty    ; zero characters
  *)
  (* path_empty <|> *)
  path_rootless <|> path_noscheme <|> path_absolute <|> path_abempty
;;
