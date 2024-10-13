type informational =
  | Continue
  | SwitchProtocols

type success =
  | Ok
  | Created
  | Accepted
  | NonAuthorative
  | NoContent
  | ResetContent
  | PartialContent

type redirection =
  | MultipleChoices
  | MovedPermanetly
  | Found
  | SeeOther
  | NotModified
  | UseProxy
  | TemporaryRedirect
  | PermanentlyRedirect

type client_error =
  | BadRequest
  | Unauthorized
  | PaymentRequired
  | Forbidden
  | NotFound
  | MethodNotFound
  | NotAcceptable
  | ProxyAuthenticationRequire
  | RequestTimeout
  | Conflict
  | Gone
  | LengthRequired
  | PreconditionFailed
  | ContentTooLong
  | UriTooLong
  | UnsupportedMediaType
  | RangeNotSatisfiable
  | ExpectationFailed
  | MisdirectedRequest
  | UnprocessableContent
  | UpgradeRequired

type server_error =
  | InternalServerError
  | NotImplemented
  | BadGateway
  | ServiceUnavailabel
  | GatewayTimeout
  | HttpVersionNotSupported

type t =
  | Informational of informational
  | Success of success
  | Redirection of redirection
  | ClientError of client_error
  | ServerError of server_error

let to_code = function
  | Informational Continue -> 100
  | _ -> failwith "todo"
;;
