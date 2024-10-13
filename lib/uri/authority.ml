module Angstrom = Shaded.Angstrom
open Angstrom
open Angstrom.Let_syntax

(*
   foo://example.com:8042/over/there?name=ferret#nose
   \_/   \______________/\_________/ \_________/ \__/
   |           |            |            |        |
   scheme     authority       path        query   fragment
   |   _____________________|__
   / \ /                        \
   urn:example:animal:ferret:nose

   https://datatracker.ietf.org/doc/html/rfc3986#section-3.2

   authority   = [ userinfo "@" ] host [ ":" port ]
   userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
   unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
   pct-encoded = "%" HEXDIG HEXDIG
   HEXDIG      =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
   sub-delims  = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
   host        = IP-literal / IPv4address / reg-name
   IP-literal  = "[" ( IPv6address / IPvFuture  ) "]"
   IPvFuture   = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
   IPv6address = 6( h16 ":" ) ls32
   /                       "::" 5( h16 ":" ) ls32
   / [               h16 ] "::" 4( h16 ":" ) ls32
   / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
   / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
   / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
   / [ *4( h16 ":" ) h16 ] "::"              ls32
   / [ *5( h16 ":" ) h16 ] "::"              h16
   / [ *6( h16 ":" ) h16 ] "::"
   ls32        = ( h16 ":" h16 ) / IPv4address ; least-significant 32 bits of address
   h16         = 1*4HEXDIG ; 16 bits of address represented in hexadecimal
   IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
   dec-octet   = DIGIT                 ; 0-9
   / %x31-39 DIGIT         ; 10-99
   / "1" 2DIGIT            ; 100-199
   / "2" %x30-34 DIGIT     ; 200-249
   / "25" %x30-35          ; 250-255
   reg-name    = *( unreserved / pct-encoded / sub-delims )
   port        = *DIGIT
   DIGIT       = %x30-39 ; 0-9
*)
type t =
  { userinfo : string option
  ; host : string
  ; port : int option
  }

(* let parser () = *)
(*   let open Angstrom in *)
(*   let open Angstrom.Let_syntax in *)
(*   let user_info_parser () = string <* char '@' in *)
(*   let%bind maybe_user_info = option (user_info_parser ()) in *)
(*   many1 any_char *)
(* ;; *)

module UserInfo = struct
  (*
     userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
     unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
     pct-encoded = "%" HEXDIG HEXDIG
     HEXDIG      =  DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
     sub-delims  = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
  *)
  let parser =
    let inner_parser = uri_unreserved <|> Pct_encode.uri_pct_encoded <|> uri_subdelim in
    many inner_parser |> return
  ;;
end

module Host = struct
  (* let parser *)
end
