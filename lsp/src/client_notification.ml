open Import
open Protocol

type t =
  | TextDocumentDidOpen of DidOpen.params
  | TextDocumentDidChange of DidChangeTextDocumentParams.t
  | ChangeWorkspaceFolders of DidChangeWorkspaceFolders.Params.t
  | ChangeConfiguration of DidChangeConfiguration.Params.t
  | Initialized
  | Exit
  | Unknown_notification of Jsonrpc.Request.t

let of_jsonrpc (r : Jsonrpc.Request.t) =
  let open Result.Infix in
  match r.method_ with
  | "textDocument/didOpen" ->
    Jsonrpc.Request.params r DidOpen.params_of_yojson >>| fun params ->
    TextDocumentDidOpen params
  | "textDocument/didChange" ->
    Jsonrpc.Request.params r DidChangeTextDocumentParams.t_of_yojson
    >>| fun params -> TextDocumentDidChange params
  | "textDocument/didSave" ->
      begin
        match packet.params with
        | Some (`Assoc ["textDocument", `Assoc["uri", uri]]) ->
            Ok(Client_notification (TextDocumentDidSave (Uri.t_of_yojson uri)))
        | _ -> Error "uh"

      end
  | "exit" -> Ok Exit
  | "initialized" -> Ok Initialized
  | "workspace/didChangeWorkspaceFolders" ->
    Jsonrpc.Request.params r DidChangeWorkspaceFolders.Params.t_of_yojson
    >>| fun params -> ChangeWorkspaceFolders params
  | "workspace/didChangeConfiguration" ->
    Jsonrpc.Request.params r DidChangeConfiguration.Params.t_of_yojson
    >>| fun params -> ChangeConfiguration params
  | _ -> Ok (Unknown_notification r)
