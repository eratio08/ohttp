module Angstrom = Shaded.Angstrom
module Bool = Shaded.Bool
module String = Shaded.String

(*
   https://datatracker.ietf.org/doc/html/rfc3986#section-3

   foo://example.com:8042/over/there?name=ferret#nose
   \_/   \______________/\_________/ \_________/ \__/
   |           |            |            |        |
   scheme     authority       path        query   fragment
   |   _____________________|__
   / \ /                        \
   urn:example:animal:ferret:nose
*)
type t =
  { scheme : Scheme.t
  ; authority : Authority.t option
  ; path : Path.t
  ; query : string option
  ; fragment : string option
  }
