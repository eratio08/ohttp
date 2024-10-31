open Shaded
open Angstrom
open Angstrom.Let_syntax

(*
   HTTP Semantics
   https://datatracker.ietf.org/doc/html/rfc9110
   HTTP Caching
   https://datatracker.ietf.org/doc/html/rfc9111

   HTTP 1.1 Syntax
   https://datatracker.ietf.org/doc/html/rfc9112

   Augmented Backus-Naur Form (ABNF)
   https://datatracker.ietf.org/doc/html/rfc5234
   https://datatracker.ietf.org/doc/html/rfc7405
*)
module HttpServer = struct
  open Eio.Net

  let addr = `Tcp (Ipaddr.V4.loopback, 8080)

  (*
     HTTP-message =
     start-line CRLF
     ( field-line CRLF )
     CRLF
     [ message-body ]

     start-line = request-line / status-line
  *)

  module StringMap = Hashtbl.Make (String)

  (* Is used in request & response *)
  module Body = struct
    (*
       Content-Length or Transfer-Encoding signal existence of a body
       If no headers are set read bytes until sender closes the connection.

       depends Content-Location header, if unset expect just bytes

       representation-data := Content-Encoding( Content-Type( data ) )

       TODO: chunked encoding
       https://datatracker.ietf.org/doc/html/rfc9112#section-7.1

       TODO: Transfer-Encoding
       TODO: Transfer-Encoding Negotiation
       https://datatracker.ietf.org/doc/html/rfc9112#section-7.4
    *)
    type t = bytes
  end

  (* module HttpHandlerKey = struct *)
  (*   type t = string * Http_method.t *)
  (**)
  (*   let equal t1 t2 = *)
  (*     match t1, t2 with *)
  (*     | (p1, m1), (p2, m2) when String.equal p1 p2 -> Http_method.equal m1 m2 *)
  (*     | _ -> false *)
  (*   ;; *)
  (**)
  (*   let hash (p, m) = 31 + String.hash p + Http_method.hash m *)
  (* end *)
  (**)
  (* module HandlerMap = Hashtbl.Make (HttpHandlerKey) *)

  module HttpResponse = struct
    module HttpStatus = struct
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
    end

    type 'a t =
      { status : HttpStatus.t
      ; body : 'a
      }
  end

  (* module HttpRequestHandler = struct *)
  (*   type t = HttpRequest.t -> unit *)
  (* end *)

  (* type t = *)
  (*   { handlers : HttpRequestHandler.t HandlerMap.t *)
  (*   ; port : int *)
  (*   ; on_error : exn -> unit *)
  (*   } *)
  (**)
  (* let create ~on_error port = { handlers = HandlerMap.create 20; port; on_error } *)
  (**)
  (* let add_route t ~path ~http_method handler = *)
  (*   HandlerMap.add t.handlers (path, http_method) handler *)
  (* ;; *)
end

let handle_request flow _addr =
  let open Eio in
  traceln "Handling request from %a" Net.Sockaddr.pp _addr;
  let buf = Buf_read.of_flow ~initial_size:100 ~max_size:1_000_000 flow in
  let request = Buf_read.line buf in
  let parts = String.split_on_char ' ' request in
  let http_method = List.nth parts 0 in
  let resource = List.nth parts 1 in
  let http_version = List.nth parts 2 in
  if String.starts_with ~prefix:"HTTP/1" http_version |> not
  then (
    Flow.copy_string "HTTP/1.1 400 Bad Request\r\n\r\nUnsupported HTTP Version\r\n" flow;
    Flow.shutdown flow `Send)
  else (
    let lines = Buf_read.lines buf in
    let headers =
      Seq.take_while (fun l -> String.empty = l |> not) lines
      |> List.of_seq
      |> String.concat "\n"
    in
    Flow.copy_string
      ("HTTP/1.1 200 OK\r\n\r\n"
       ^ request
       ^ "\n"
       ^ headers
       ^ "\n"
       ^ http_method
       ^ "\n"
       ^ resource
       ^ "\r\n")
      flow;
    Flow.shutdown flow `Send)
;;

let listen_to ~net addr =
  let open Eio in
  Switch.run
  @@ fun sw ->
  let socket = Net.listen ~sw ~backlog:2 net addr in
  Net.run_server ~on_error:(traceln "Error %a" Fmt.exn) socket handle_request
;;

(* let () = *)
(*   Eio_main.run *)
(*   @@ fun env -> *)
(*   let net = Stdenv.net env in *)
(*   listen_to ~net addr |> ignore *)
(* ;; *)
