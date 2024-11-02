module RequestTarget = struct
  type t = Uri.t

  (*
     request-target =
     / origin-form
     / absolute-form
     / authority-form
     / asterisk-form

     origin-form = absolute-path [ "?" query ]
     absolute-form = absolute-URI ;
     authority-form = uri-host ":" port ; only with CONNECT
     asterisk-form  = "*" ; only for OPTION
  *)
  let parser () = failwith "todo"

  (* https://datatracker.ietf.org/doc/html/rfc9112#section-3.3 *)
  let reconstruct_target_uri () = failwith "todo"
end

(*
   request-line = method SP request-target SP HTTP-version
*)
module RequestLine = struct
  type t =
    { method_ : Method.t
    ; target : RequestTarget.t
    ; version : Protocol_version.t
    }
end

type t =
  { line : RequestLine.t (* ; fields : Field.t list *)
  ; body : bytes option
  }
