(*   status-line = HTTP-version SP status-code SP [ reason-phrase ] *)
type t =
  { version : Protocol_version.t
  ; status : Status.t
  ; phase : string option
  }

let write () = failwith "todo"
