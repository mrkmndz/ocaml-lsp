open Import

(** * This encodes LSP protocol specification as document at * *
    https://microsoft.github.io/language-server-protocol/specification * * Most
    of this was borrowed from facebook/flow repository. * *)

module Only = struct
  type 'a t =
    | All
    | Only of 'a list

  let yojson_of_t f = function
    | All -> `Null
    | Only xs -> `List (List.map ~f xs)

  let t_of_yojson f = function
    | `Null -> All
    | `List xs -> Only (List.map ~f xs)
    | json -> yojson_error "invalid only" json
end

module Or_bool = struct
  type 'a t =
    | Bool of bool
    | Value of 'a

  let t_of_yojson f (y : Ppx_yojson_conv_lib.Yojson.Safe.t) =
    match y with
    | `Bool b -> Bool b
    | x -> Value (f x)

  let yojson_of_t f = function
    | Bool b -> `Bool b
    | Value x -> f x
end

module Or_string = struct
  type 'a t =
    | String of string
    | Value of 'a

  let t_of_yojson f (y : Ppx_yojson_conv_lib.Yojson.Safe.t) =
    match y with
    | `String b -> String b
    | x -> Value (f x)

  let yojson_of_t f = function
    | String b -> `String b
    | Value x -> f x
end

module Void = struct
  type t

  let t_of_yojson = yojson_error "Void.t"

  let yojson_of_t (_ : t) = assert false
end

module DocumentFilter = struct
  type t =
    { language : string option
    ; scheme : string option
    ; pattern : string
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.DocumentFilter.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let language_field = ref None
        and scheme_field = ref None
        and pattern_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "language" -> (
              match Ppx_yojson_conv_lib.( ! ) language_field with
              | None ->
                let fvalue = option_of_yojson string_of_yojson _field_yojson in
                language_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "scheme" -> (
              match Ppx_yojson_conv_lib.( ! ) scheme_field with
              | None ->
                let fvalue = option_of_yojson string_of_yojson _field_yojson in
                scheme_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "pattern" -> (
              match Ppx_yojson_conv_lib.( ! ) pattern_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                pattern_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) language_field
              , Ppx_yojson_conv_lib.( ! ) scheme_field
              , Ppx_yojson_conv_lib.( ! ) pattern_field )
            with
            | Some language_value, Some scheme_value, Some pattern_value ->
              { language = language_value
              ; scheme = scheme_value
              ; pattern = pattern_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) language_field)
                      None
                  , "language" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) scheme_field)
                      None
                  , "scheme" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) pattern_field)
                      None
                  , "pattern" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { language = v_language; scheme = v_scheme; pattern = v_pattern } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_string v_pattern in
          ("pattern", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_option yojson_of_string v_scheme in
          ("scheme", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_option yojson_of_string v_language in
          ("language", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module Registration = struct
  type t =
    { id : string
    ; method_ : string [@key "method"]
    ; registerOptions : json option
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Registration.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let id_field = ref None
        and method__field = ref None
        and registerOptions_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "id" -> (
              match Ppx_yojson_conv_lib.( ! ) id_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                id_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "method" -> (
              match Ppx_yojson_conv_lib.( ! ) method__field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                method__field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "registerOptions" -> (
              match Ppx_yojson_conv_lib.( ! ) registerOptions_field with
              | None ->
                let fvalue = option_of_yojson json_of_yojson _field_yojson in
                registerOptions_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) id_field
              , Ppx_yojson_conv_lib.( ! ) method__field
              , Ppx_yojson_conv_lib.( ! ) registerOptions_field )
            with
            | Some id_value, Some method__value, Some registerOptions_value ->
              { id = id_value
              ; method_ = method__value
              ; registerOptions = registerOptions_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) id_field)
                      None
                  , "id" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) method__field)
                      None
                  , "method_" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) registerOptions_field)
                      None
                  , "registerOptions" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { id = v_id; method_ = v_method_; registerOptions = v_registerOptions }
        ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_option yojson_of_json v_registerOptions in
          ("registerOptions", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_method_ in
          ("method", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_id in
          ("id", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module RegistrationParams = struct
  type t = { registrations : Registration.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.RegistrationParams.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let registrations_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "registrations" -> (
              match Ppx_yojson_conv_lib.( ! ) registrations_field with
              | None ->
                let fvalue =
                  list_of_yojson Registration.t_of_yojson _field_yojson
                in
                registrations_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) registrations_field with
            | Some registrations_value ->
              { registrations = registrations_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) registrations_field)
                      None
                  , "registrations" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { registrations = v_registrations } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_list Registration.yojson_of_t v_registrations in
          ("registrations", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module Unregistration = struct
  type t =
    { id : string
    ; method_ : string [@key "method"]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Unregistration.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let id_field = ref None
        and method__field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "id" -> (
              match Ppx_yojson_conv_lib.( ! ) id_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                id_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "method" -> (
              match Ppx_yojson_conv_lib.( ! ) method__field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                method__field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) id_field
              , Ppx_yojson_conv_lib.( ! ) method__field )
            with
            | Some id_value, Some method__value ->
              { id = id_value; method_ = method__value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) id_field)
                      None
                  , "id" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) method__field)
                      None
                  , "method_" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { id = v_id; method_ = v_method_ } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_string v_method_ in
          ("method", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_id in
          ("id", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module UnregistrationParams = struct
  type t = { unregistrations : Unregistration.t list }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.UnregistrationParams.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let unregistrations_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "unregistrations" -> (
              match Ppx_yojson_conv_lib.( ! ) unregistrations_field with
              | None ->
                let fvalue =
                  list_of_yojson Unregistration.t_of_yojson _field_yojson
                in
                unregistrations_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) unregistrations_field with
            | Some unregistrations_value ->
              { unregistrations = unregistrations_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) unregistrations_field)
                      None
                  , "unregistrations" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { unregistrations = v_unregistrations } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg =
            yojson_of_list Unregistration.yojson_of_t v_unregistrations
          in
          ("unregistrations", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

type documentUri = Uri.t [@@deriving_inline yojson]

let _ = fun (_ : documentUri) -> ()

let documentUri_of_yojson =
  (Uri.t_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> documentUri)

let _ = documentUri_of_yojson

let yojson_of_documentUri =
  (Uri.yojson_of_t : documentUri -> Ppx_yojson_conv_lib.Yojson.Safe.t)

let _ = yojson_of_documentUri

[@@@end]

module Position = struct
  type t =
    { line : int
    ; character : int
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Position.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let line_field = ref None
        and character_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "line" -> (
              match Ppx_yojson_conv_lib.( ! ) line_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                line_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "character" -> (
              match Ppx_yojson_conv_lib.( ! ) character_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                character_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) line_field
              , Ppx_yojson_conv_lib.( ! ) character_field )
            with
            | Some line_value, Some character_value ->
              { line = line_value; character = character_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) line_field)
                      None
                  , "line" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) character_field)
                      None
                  , "character" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { line = v_line; character = v_character } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_int v_character in
          ("character", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_int v_line in
          ("line", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module Range = struct
  type t =
    { start_ : Position.t [@key "start"]
    ; end_ : Position.t [@key "end"]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Range.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let start__field = ref None
        and end__field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "start" -> (
              match Ppx_yojson_conv_lib.( ! ) start__field with
              | None ->
                let fvalue = Position.t_of_yojson _field_yojson in
                start__field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "end" -> (
              match Ppx_yojson_conv_lib.( ! ) end__field with
              | None ->
                let fvalue = Position.t_of_yojson _field_yojson in
                end__field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) start__field
              , Ppx_yojson_conv_lib.( ! ) end__field )
            with
            | Some start__value, Some end__value ->
              { start_ = start__value; end_ = end__value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) start__field)
                      None
                  , "start_" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) end__field)
                      None
                  , "end_" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { start_ = v_start_; end_ = v_end_ } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = Position.yojson_of_t v_end_ in
          ("end", arg) :: bnds
        in
        let bnds =
          let arg = Position.yojson_of_t v_start_ in
          ("start", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module SymbolKind = struct
  type t =
    | File (* 1 *)
    | Module (* 2 *)
    | Namespace (* 3 *)
    | Package (* 4 *)
    | Class (* 5 *)
    | Method (* 6 *)
    | Property (* 7 *)
    | Field (* 8 *)
    | Constructor (* 9 *)
    | Enum (* 10 *)
    | Interface (* 11 *)
    | Function (* 12 *)
    | Variable (* 13 *)
    | Constant (* 14 *)
    | String (* 15 *)
    | Number (* 16 *)
    | Boolean (* 17 *)
    | Array (* 18 *)
    | Object (* 19 *)
    | Key (* 20 *)
    | Null (* 21 *)
    | EnumMember (* 22 *)
    | Struct (* 23 *)
    | Event (* 24 *)
    | Operator (* 25 *)
    | TypeParameter

  (* 26 *)

  let yojson_of_t = function
    | File -> `Int 1
    | Module -> `Int 2
    | Namespace -> `Int 3
    | Package -> `Int 4
    | Class -> `Int 5
    | Method -> `Int 6
    | Property -> `Int 7
    | Field -> `Int 8
    | Constructor -> `Int 9
    | Enum -> `Int 10
    | Interface -> `Int 11
    | Function -> `Int 12
    | Variable -> `Int 13
    | Constant -> `Int 14
    | String -> `Int 15
    | Number -> `Int 16
    | Boolean -> `Int 17
    | Array -> `Int 18
    | Object -> `Int 19
    | Key -> `Int 20
    | Null -> `Int 21
    | EnumMember -> `Int 22
    | Struct -> `Int 23
    | Event -> `Int 24
    | Operator -> `Int 25
    | TypeParameter -> `Int 26

  let t_of_yojson = function
    | `Int 1 -> File
    | `Int 2 -> Module
    | `Int 3 -> Namespace
    | `Int 4 -> Package
    | `Int 5 -> Class
    | `Int 6 -> Method
    | `Int 7 -> Property
    | `Int 8 -> Field
    | `Int 9 -> Constructor
    | `Int 10 -> Enum
    | `Int 11 -> Interface
    | `Int 12 -> Function
    | `Int 13 -> Variable
    | `Int 14 -> Constant
    | `Int 15 -> String
    | `Int 16 -> Number
    | `Int 17 -> Boolean
    | `Int 18 -> Array
    | `Int 19 -> Object
    | `Int 20 -> Key
    | `Int 21 -> Null
    | `Int 22 -> EnumMember
    | `Int 23 -> Struct
    | `Int 24 -> Event
    | `Int 25 -> Operator
    | `Int 26 -> TypeParameter
    | node -> yojson_error "invalid SymbolKind" node
end

module Command = struct
  type t =
    { title : string
    ; command : string
    ; arguments : json list option [@yojson.option]
    }
  [@@yojson.allow_extra_fields] [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Command.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let title_field = ref None
        and command_field = ref None
        and arguments_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "title" -> (
              match Ppx_yojson_conv_lib.( ! ) title_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                title_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "command" -> (
              match Ppx_yojson_conv_lib.( ! ) command_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                command_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "arguments" -> (
              match Ppx_yojson_conv_lib.( ! ) arguments_field with
              | None ->
                let fvalue = list_of_yojson json_of_yojson _field_yojson in
                arguments_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) title_field
              , Ppx_yojson_conv_lib.( ! ) command_field
              , Ppx_yojson_conv_lib.( ! ) arguments_field )
            with
            | Some title_value, Some command_value, arguments_value ->
              { title = title_value
              ; command = command_value
              ; arguments = arguments_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) title_field)
                      None
                  , "title" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) command_field)
                      None
                  , "command" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { title = v_title; command = v_command; arguments = v_arguments } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_arguments with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_list yojson_of_json v in
            let bnd = ("arguments", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_command in
          ("command", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_title in
          ("title", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module MarkupKind = struct
  type t =
    | Plaintext
    | Markdown

  let yojson_of_t = function
    | Plaintext -> `String "plaintext"
    | Markdown -> `String "markdown"

  let t_of_yojson = function
    | `String "plaintext" -> Plaintext
    | `String "markdown" -> Markdown
    | `String _ -> Plaintext
    | node -> yojson_error "invalid contentFormat" node
end

module MarkupContent = struct
  type t =
    { value : string
    ; kind : MarkupKind.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.MarkupContent.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let value_field = ref None
        and kind_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "value" -> (
              match Ppx_yojson_conv_lib.( ! ) value_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                value_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "kind" -> (
              match Ppx_yojson_conv_lib.( ! ) kind_field with
              | None ->
                let fvalue = MarkupKind.t_of_yojson _field_yojson in
                kind_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) value_field
              , Ppx_yojson_conv_lib.( ! ) kind_field )
            with
            | Some value_value, Some kind_value ->
              { value = value_value; kind = kind_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) value_field)
                      None
                  , "value" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) kind_field)
                      None
                  , "kind" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { value = v_value; kind = v_kind } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = MarkupKind.yojson_of_t v_kind in
          ("kind", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_value in
          ("value", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module Location = struct
  type t =
    { uri : Uri.t
    ; range : Range.t
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Location.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let uri_field = ref None
        and range_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "uri" -> (
              match Ppx_yojson_conv_lib.( ! ) uri_field with
              | None ->
                let fvalue = Uri.t_of_yojson _field_yojson in
                uri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "range" -> (
              match Ppx_yojson_conv_lib.( ! ) range_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                range_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) uri_field
              , Ppx_yojson_conv_lib.( ! ) range_field )
            with
            | Some uri_value, Some range_value ->
              { uri = uri_value; range = range_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) uri_field)
                      None
                  , "uri" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) range_field)
                      None
                  , "range" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { uri = v_uri; range = v_range } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = Range.yojson_of_t v_range in
          ("range", arg) :: bnds
        in
        let bnds =
          let arg = Uri.yojson_of_t v_uri in
          ("uri", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module LocationLink = struct
  type t =
    { originSelectionRange : Range.t option [@yojson.option]
    ; targetUri : documentUri
    ; targetrange : Range.t
    ; targetSelectionRange : Range.t
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.LocationLink.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let originSelectionRange_field = ref None
        and targetUri_field = ref None
        and targetrange_field = ref None
        and targetSelectionRange_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "originSelectionRange" -> (
              match Ppx_yojson_conv_lib.( ! ) originSelectionRange_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                originSelectionRange_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "targetUri" -> (
              match Ppx_yojson_conv_lib.( ! ) targetUri_field with
              | None ->
                let fvalue = documentUri_of_yojson _field_yojson in
                targetUri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "targetrange" -> (
              match Ppx_yojson_conv_lib.( ! ) targetrange_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                targetrange_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "targetSelectionRange" -> (
              match Ppx_yojson_conv_lib.( ! ) targetSelectionRange_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                targetSelectionRange_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) originSelectionRange_field
              , Ppx_yojson_conv_lib.( ! ) targetUri_field
              , Ppx_yojson_conv_lib.( ! ) targetrange_field
              , Ppx_yojson_conv_lib.( ! ) targetSelectionRange_field )
            with
            | ( originSelectionRange_value
              , Some targetUri_value
              , Some targetrange_value
              , Some targetSelectionRange_value ) ->
              { originSelectionRange = originSelectionRange_value
              ; targetUri = targetUri_value
              ; targetrange = targetrange_value
              ; targetSelectionRange = targetSelectionRange_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) targetUri_field)
                      None
                  , "targetUri" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) targetrange_field)
                      None
                  , "targetrange" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) targetSelectionRange_field)
                      None
                  , "targetSelectionRange" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { originSelectionRange = v_originSelectionRange
        ; targetUri = v_targetUri
        ; targetrange = v_targetrange
        ; targetSelectionRange = v_targetSelectionRange
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = Range.yojson_of_t v_targetSelectionRange in
          ("targetSelectionRange", arg) :: bnds
        in
        let bnds =
          let arg = Range.yojson_of_t v_targetrange in
          ("targetrange", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_documentUri v_targetUri in
          ("targetUri", arg) :: bnds
        in
        let bnds =
          match v_originSelectionRange with
          | None -> bnds
          | Some v ->
            let arg = Range.yojson_of_t v in
            let bnd = ("originSelectionRange", arg) in
            bnd :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module Locations = struct
  type t =
    | Location of Location.t
    | Locations of Location.t list
    | Location_links of LocationLink.t list

  let yojson_of_t = function
    | Location l -> Location.yojson_of_t l
    | Locations l -> `List (List.map ~f:Location.yojson_of_t l)
    | Location_links l -> `List (List.map ~f:LocationLink.yojson_of_t l)

  let t_of_yojson (json : json) =
    match json with
    | `Assoc _ -> Location (Location.t_of_yojson json)
    | `List [] -> Locations []
    | `List (x :: xs) -> (
      match Location.t_of_yojson x with
      | loc -> Locations (loc :: List.map ~f:Location.t_of_yojson xs)
      | exception Of_yojson_error (_, _) ->
        Location_links (List.map ~f:LocationLink.t_of_yojson (x :: xs)) )
    | _ -> yojson_error "Locations.t" json
end

(* Text documents are identified using a URI. *)
module TextDocumentIdentifier = struct
  type t = { uri : documentUri (* the text document's URI *) }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.TextDocumentIdentifier.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let uri_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "uri" -> (
              match Ppx_yojson_conv_lib.( ! ) uri_field with
              | None ->
                let fvalue = documentUri_of_yojson _field_yojson in
                uri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) uri_field with
            | Some uri_value -> { uri = uri_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) uri_field)
                      None
                  , "uri" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { uri = v_uri } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_documentUri v_uri in
          ("uri", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

(* An identifier to denote a specific version of a text document. *)
module VersionedTextDocumentIdentifier = struct
  type t =
    { uri : documentUri
    ; (* the text document's URI *)
      version : int (* the version number of this document *)
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.VersionedTextDocumentIdentifier.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let uri_field = ref None
        and version_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "uri" -> (
              match Ppx_yojson_conv_lib.( ! ) uri_field with
              | None ->
                let fvalue = documentUri_of_yojson _field_yojson in
                uri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "version" -> (
              match Ppx_yojson_conv_lib.( ! ) version_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                version_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) uri_field
              , Ppx_yojson_conv_lib.( ! ) version_field )
            with
            | Some uri_value, Some version_value ->
              { uri = uri_value; version = version_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) uri_field)
                      None
                  , "uri" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) version_field)
                      None
                  , "version" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { uri = v_uri; version = v_version } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_int v_version in
          ("version", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_documentUri v_uri in
          ("uri", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

(* An item to transfer a text document from the client to the server. The
   version number strictly increases after each change, including undo/redo. *)
module TextDocumentItem = struct
  type t =
    { uri : documentUri
    ; (* the text document's URI *)
      languageId : string
    ; (* the text document's language identifier *)
      version : int
    ; (* the version of the document *)
      text : string (* the content of the opened text document *)
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.TextDocumentItem.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let uri_field = ref None
        and languageId_field = ref None
        and version_field = ref None
        and text_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "uri" -> (
              match Ppx_yojson_conv_lib.( ! ) uri_field with
              | None ->
                let fvalue = documentUri_of_yojson _field_yojson in
                uri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "languageId" -> (
              match Ppx_yojson_conv_lib.( ! ) languageId_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                languageId_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "version" -> (
              match Ppx_yojson_conv_lib.( ! ) version_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                version_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "text" -> (
              match Ppx_yojson_conv_lib.( ! ) text_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                text_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) uri_field
              , Ppx_yojson_conv_lib.( ! ) languageId_field
              , Ppx_yojson_conv_lib.( ! ) version_field
              , Ppx_yojson_conv_lib.( ! ) text_field )
            with
            | ( Some uri_value
              , Some languageId_value
              , Some version_value
              , Some text_value ) ->
              { uri = uri_value
              ; languageId = languageId_value
              ; version = version_value
              ; text = text_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) uri_field)
                      None
                  , "uri" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) languageId_field)
                      None
                  , "languageId" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) version_field)
                      None
                  , "version" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) text_field)
                      None
                  , "text" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { uri = v_uri
        ; languageId = v_languageId
        ; version = v_version
        ; text = v_text
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_string v_text in
          ("text", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_int v_version in
          ("version", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_languageId in
          ("languageId", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_documentUri v_uri in
          ("uri", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

(* DidOpenTextDocument notification, method="textDocument/didOpen" *)
module DidOpen = struct
  type params = didOpenTextDocumentParams

  and didOpenTextDocumentParams =
    { textDocument : TextDocumentItem.t (* the document that was opened *) }
  [@@yojson.allow_extra_fields] [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let _ = fun (_ : didOpenTextDocumentParams) -> ()

  let rec params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.DidOpen.params" in
      fun t -> didOpenTextDocumentParams_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  and didOpenTextDocumentParams_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.DidOpen.didOpenTextDocumentParams" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocument_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "textDocument" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocument_field with
              | None ->
                let fvalue = TextDocumentItem.t_of_yojson _field_yojson in
                textDocument_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) textDocument_field with
            | Some textDocument_value -> { textDocument = textDocument_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                      None
                  , "textDocument" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> didOpenTextDocumentParams )

  let _ = params_of_yojson

  and _ = didOpenTextDocumentParams_of_yojson

  let rec yojson_of_params =
    ( fun v -> yojson_of_didOpenTextDocumentParams v
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_didOpenTextDocumentParams =
    ( function
      | { textDocument = v_textDocument } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = TextDocumentItem.yojson_of_t v_textDocument in
          ("textDocument", arg) :: bnds
        in
        `Assoc bnds
      : didOpenTextDocumentParams -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  and _ = yojson_of_didOpenTextDocumentParams

  [@@@end]
end

module TextDocumentContentChangeEvent = struct
  type t =
    { range : Range.t option [@yojson.option]
    ; (* the range of the document that changed *)
      rangeLength : int option [@yojson.option]
    ; (* the length that got replaced *)
      text : string (* the new text of the range/document *)
    }
  [@@yojson.allow_extra_fields] [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.TextDocumentContentChangeEvent.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let range_field = ref None
        and rangeLength_field = ref None
        and text_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "range" -> (
              match Ppx_yojson_conv_lib.( ! ) range_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                range_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "rangeLength" -> (
              match Ppx_yojson_conv_lib.( ! ) rangeLength_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                rangeLength_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "text" -> (
              match Ppx_yojson_conv_lib.( ! ) text_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                text_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) range_field
              , Ppx_yojson_conv_lib.( ! ) rangeLength_field
              , Ppx_yojson_conv_lib.( ! ) text_field )
            with
            | range_value, rangeLength_value, Some text_value ->
              { range = range_value
              ; rangeLength = rangeLength_value
              ; text = text_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) text_field)
                      None
                  , "text" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { range = v_range; rangeLength = v_rangeLength; text = v_text } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_string v_text in
          ("text", arg) :: bnds
        in
        let bnds =
          match v_rangeLength with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_int v in
            let bnd = ("rangeLength", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_range with
          | None -> bnds
          | Some v ->
            let arg = Range.yojson_of_t v in
            let bnd = ("range", arg) in
            bnd :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module DidChangeTextDocumentParams = struct
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
    ; contentChanges : TextDocumentContentChangeEvent.t list
    }
  [@@yojson.allow_extra_fields] [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.DidChangeTextDocumentParams.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocument_field = ref None
        and contentChanges_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "textDocument" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocument_field with
              | None ->
                let fvalue =
                  VersionedTextDocumentIdentifier.t_of_yojson _field_yojson
                in
                textDocument_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "contentChanges" -> (
              match Ppx_yojson_conv_lib.( ! ) contentChanges_field with
              | None ->
                let fvalue =
                  list_of_yojson TextDocumentContentChangeEvent.t_of_yojson
                    _field_yojson
                in
                contentChanges_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) textDocument_field
              , Ppx_yojson_conv_lib.( ! ) contentChanges_field )
            with
            | Some textDocument_value, Some contentChanges_value ->
              { textDocument = textDocument_value
              ; contentChanges = contentChanges_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                      None
                  , "textDocument" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) contentChanges_field)
                      None
                  , "contentChanges" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { textDocument = v_textDocument; contentChanges = v_contentChanges } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg =
            yojson_of_list TextDocumentContentChangeEvent.yojson_of_t
              v_contentChanges
          in
          ("contentChanges", arg) :: bnds
        in
        let bnds =
          let arg =
            VersionedTextDocumentIdentifier.yojson_of_t v_textDocument
          in
          ("textDocument", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module TextDocumentPositionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; (* the text document *)
      position : Position.t (* the position inside the text document *)
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.TextDocumentPositionParams.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocument_field = ref None
        and position_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "textDocument" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocument_field with
              | None ->
                let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
                textDocument_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "position" -> (
              match Ppx_yojson_conv_lib.( ! ) position_field with
              | None ->
                let fvalue = Position.t_of_yojson _field_yojson in
                position_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) textDocument_field
              , Ppx_yojson_conv_lib.( ! ) position_field )
            with
            | Some textDocument_value, Some position_value ->
              { textDocument = textDocument_value; position = position_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                      None
                  , "textDocument" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) position_field)
                      None
                  , "position" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { textDocument = v_textDocument; position = v_position } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = Position.yojson_of_t v_position in
          ("position", arg) :: bnds
        in
        let bnds =
          let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
          ("textDocument", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

(** A document highlight is a range inside a text document which deserves
    special attention. Usually a document highlight is visualized by changing
    the background color of its range. *)
module DocumentHighlight = struct
  (** The highlight kind, default is DocumentHighlightKind.Text. *)
  type kind =
    | Text  (** 1: A textual occurrence. *)
    | Read  (** 2: Read-access of a symbol, like reading a variable. *)
    | Write  (** 3: Write-access of a symbol, like writing a variable. *)

  let yojson_of_kind = function
    | Text -> `Int 1
    | Read -> `Int 2
    | Write -> `Int 3

  let kind_of_yojson = function
    | `Int 1 -> Text
    | `Int 2 -> Read
    | `Int 3 -> Write
    | node -> yojson_error "kind expected to be an int between 1 and 3" node

  type t =
    { range : Range.t
    ; kind : kind option
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.DocumentHighlight.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let range_field = ref None
        and kind_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "range" -> (
              match Ppx_yojson_conv_lib.( ! ) range_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                range_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "kind" -> (
              match Ppx_yojson_conv_lib.( ! ) kind_field with
              | None ->
                let fvalue = option_of_yojson kind_of_yojson _field_yojson in
                kind_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) range_field
              , Ppx_yojson_conv_lib.( ! ) kind_field )
            with
            | Some range_value, Some kind_value ->
              { range = range_value; kind = kind_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) range_field)
                      None
                  , "range" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) kind_field)
                      None
                  , "kind" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { range = v_range; kind = v_kind } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_option yojson_of_kind v_kind in
          ("kind", arg) :: bnds
        in
        let bnds =
          let arg = Range.yojson_of_t v_range in
          ("range", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

(** Complex text manipulations are described with an array of TextEdit's,
    representing a single change to the document.

    All text edits ranges refer to positions in the original document. Text
    edits ranges must never overlap, that means no part of the original document
    must be manipulated by more than one edit. However, it is possible that
    multiple edits have the same start position: multiple inserts, or any number
    of inserts followed by a single remove or replace edit. If multiple inserts
    have the same position, the order in the array defines the order in which
    the inserted strings appear in the resulting text. *)
module TextEdit = struct
  type t =
    { range : Range.t
          (** The range of the text document to be manipulated. To insert text
              into a document create a range where start === end. *)
    ; newText : string
          (** The string to be inserted. For delete operations use an empty
              string. *)
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.TextEdit.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let range_field = ref None
        and newText_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "range" -> (
              match Ppx_yojson_conv_lib.( ! ) range_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                range_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "newText" -> (
              match Ppx_yojson_conv_lib.( ! ) newText_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                newText_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) range_field
              , Ppx_yojson_conv_lib.( ! ) newText_field )
            with
            | Some range_value, Some newText_value ->
              { range = range_value; newText = newText_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) range_field)
                      None
                  , "range" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) newText_field)
                      None
                  , "newText" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { range = v_range; newText = v_newText } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_string v_newText in
          ("newText", arg) :: bnds
        in
        let bnds =
          let arg = Range.yojson_of_t v_range in
          ("range", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

(** Describes textual changes on a single text document. The text document is
    referred to as a VersionedTextDocumentIdentifier to allow clients to check
    the text document version before an edit is applied. A TextDocumentEdit
    describes all changes on a version Si and after they are applied move the
    document to version Si+1. So the creator of a TextDocumentEdit doesn't need
    to sort the array or do any kind of ordering. However the edits must be non
    overlapping. *)
module TextDocumentEdit = struct
  type t =
    { textDocument : VersionedTextDocumentIdentifier.t
          (** The text document to change. *)
    ; edits : TextEdit.t list  (** The edits to be applied. *)
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.TextDocumentEdit.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocument_field = ref None
        and edits_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "textDocument" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocument_field with
              | None ->
                let fvalue =
                  VersionedTextDocumentIdentifier.t_of_yojson _field_yojson
                in
                textDocument_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "edits" -> (
              match Ppx_yojson_conv_lib.( ! ) edits_field with
              | None ->
                let fvalue =
                  list_of_yojson TextEdit.t_of_yojson _field_yojson
                in
                edits_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) textDocument_field
              , Ppx_yojson_conv_lib.( ! ) edits_field )
            with
            | Some textDocument_value, Some edits_value ->
              { textDocument = textDocument_value; edits = edits_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                      None
                  , "textDocument" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) edits_field)
                      None
                  , "edits" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { textDocument = v_textDocument; edits = v_edits } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_list TextEdit.yojson_of_t v_edits in
          ("edits", arg) :: bnds
        in
        let bnds =
          let arg =
            VersionedTextDocumentIdentifier.yojson_of_t v_textDocument
          in
          ("textDocument", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module CreateFileOptions = struct
  type t =
    { overwrite : bool option
    ; ignoreIfExists : bool option
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.CreateFileOptions.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let overwrite_field = ref None
        and ignoreIfExists_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "overwrite" -> (
              match Ppx_yojson_conv_lib.( ! ) overwrite_field with
              | None ->
                let fvalue = option_of_yojson bool_of_yojson _field_yojson in
                overwrite_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "ignoreIfExists" -> (
              match Ppx_yojson_conv_lib.( ! ) ignoreIfExists_field with
              | None ->
                let fvalue = option_of_yojson bool_of_yojson _field_yojson in
                ignoreIfExists_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) overwrite_field
              , Ppx_yojson_conv_lib.( ! ) ignoreIfExists_field )
            with
            | Some overwrite_value, Some ignoreIfExists_value ->
              { overwrite = overwrite_value
              ; ignoreIfExists = ignoreIfExists_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) overwrite_field)
                      None
                  , "overwrite" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) ignoreIfExists_field)
                      None
                  , "ignoreIfExists" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { overwrite = v_overwrite; ignoreIfExists = v_ignoreIfExists } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_option yojson_of_bool v_ignoreIfExists in
          ("ignoreIfExists", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_option yojson_of_bool v_overwrite in
          ("overwrite", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module CreateFile = struct
  type t =
    { uri : documentUri
    ; options : CreateFileOptions.t option [@yojson.option]
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.CreateFile.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let uri_field = ref None
        and options_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "uri" -> (
              match Ppx_yojson_conv_lib.( ! ) uri_field with
              | None ->
                let fvalue = documentUri_of_yojson _field_yojson in
                uri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "options" -> (
              match Ppx_yojson_conv_lib.( ! ) options_field with
              | None ->
                let fvalue = CreateFileOptions.t_of_yojson _field_yojson in
                options_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) uri_field
              , Ppx_yojson_conv_lib.( ! ) options_field )
            with
            | Some uri_value, options_value ->
              { uri = uri_value; options = options_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) uri_field)
                      None
                  , "uri" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { uri = v_uri; options = v_options } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_options with
          | None -> bnds
          | Some v ->
            let arg = CreateFileOptions.yojson_of_t v in
            let bnd = ("options", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_documentUri v_uri in
          ("uri", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let yojson_of_t t =
    match yojson_of_t t with
    | `Assoc fields -> `Assoc (("kind", `String "create") :: fields)
    | _ -> assert false
end

module RenameFileOptions = struct
  type t =
    { overwrite : bool option
    ; ignoreIfExists : bool option
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.RenameFileOptions.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let overwrite_field = ref None
        and ignoreIfExists_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "overwrite" -> (
              match Ppx_yojson_conv_lib.( ! ) overwrite_field with
              | None ->
                let fvalue = option_of_yojson bool_of_yojson _field_yojson in
                overwrite_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "ignoreIfExists" -> (
              match Ppx_yojson_conv_lib.( ! ) ignoreIfExists_field with
              | None ->
                let fvalue = option_of_yojson bool_of_yojson _field_yojson in
                ignoreIfExists_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) overwrite_field
              , Ppx_yojson_conv_lib.( ! ) ignoreIfExists_field )
            with
            | Some overwrite_value, Some ignoreIfExists_value ->
              { overwrite = overwrite_value
              ; ignoreIfExists = ignoreIfExists_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) overwrite_field)
                      None
                  , "overwrite" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) ignoreIfExists_field)
                      None
                  , "ignoreIfExists" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { overwrite = v_overwrite; ignoreIfExists = v_ignoreIfExists } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_option yojson_of_bool v_ignoreIfExists in
          ("ignoreIfExists", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_option yojson_of_bool v_overwrite in
          ("overwrite", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module RenameFile = struct
  type t =
    { oldUri : documentUri
    ; newUri : documentUri
    ; options : RenameFileOptions.t option
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.RenameFile.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let oldUri_field = ref None
        and newUri_field = ref None
        and options_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "oldUri" -> (
              match Ppx_yojson_conv_lib.( ! ) oldUri_field with
              | None ->
                let fvalue = documentUri_of_yojson _field_yojson in
                oldUri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "newUri" -> (
              match Ppx_yojson_conv_lib.( ! ) newUri_field with
              | None ->
                let fvalue = documentUri_of_yojson _field_yojson in
                newUri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "options" -> (
              match Ppx_yojson_conv_lib.( ! ) options_field with
              | None ->
                let fvalue =
                  option_of_yojson RenameFileOptions.t_of_yojson _field_yojson
                in
                options_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) oldUri_field
              , Ppx_yojson_conv_lib.( ! ) newUri_field
              , Ppx_yojson_conv_lib.( ! ) options_field )
            with
            | Some oldUri_value, Some newUri_value, Some options_value ->
              { oldUri = oldUri_value
              ; newUri = newUri_value
              ; options = options_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) oldUri_field)
                      None
                  , "oldUri" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) newUri_field)
                      None
                  , "newUri" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) options_field)
                      None
                  , "options" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { oldUri = v_oldUri; newUri = v_newUri; options = v_options } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_option RenameFileOptions.yojson_of_t v_options in
          ("options", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_documentUri v_newUri in
          ("newUri", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_documentUri v_oldUri in
          ("oldUri", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let yojson_of_t t =
    match yojson_of_t t with
    | `Assoc fields -> `Assoc (("kind", `String "rename") :: fields)
    | _ -> assert false
end

module DeleteFileOptions = struct
  type t =
    { recursive : bool option
    ; ignoreIfNotExists : bool option
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.DeleteFileOptions.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let recursive_field = ref None
        and ignoreIfNotExists_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "recursive" -> (
              match Ppx_yojson_conv_lib.( ! ) recursive_field with
              | None ->
                let fvalue = option_of_yojson bool_of_yojson _field_yojson in
                recursive_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "ignoreIfNotExists" -> (
              match Ppx_yojson_conv_lib.( ! ) ignoreIfNotExists_field with
              | None ->
                let fvalue = option_of_yojson bool_of_yojson _field_yojson in
                ignoreIfNotExists_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) recursive_field
              , Ppx_yojson_conv_lib.( ! ) ignoreIfNotExists_field )
            with
            | Some recursive_value, Some ignoreIfNotExists_value ->
              { recursive = recursive_value
              ; ignoreIfNotExists = ignoreIfNotExists_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) recursive_field)
                      None
                  , "recursive" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) ignoreIfNotExists_field)
                      None
                  , "ignoreIfNotExists" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { recursive = v_recursive; ignoreIfNotExists = v_ignoreIfNotExists } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_option yojson_of_bool v_ignoreIfNotExists in
          ("ignoreIfNotExists", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_option yojson_of_bool v_recursive in
          ("recursive", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module DeleteFile = struct
  type t =
    { uri : documentUri
    ; options : DeleteFileOptions.t option
    }
  [@@deriving_inline yojson] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.DeleteFile.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let uri_field = ref None
        and options_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "uri" -> (
              match Ppx_yojson_conv_lib.( ! ) uri_field with
              | None ->
                let fvalue = documentUri_of_yojson _field_yojson in
                uri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "options" -> (
              match Ppx_yojson_conv_lib.( ! ) options_field with
              | None ->
                let fvalue =
                  option_of_yojson DeleteFileOptions.t_of_yojson _field_yojson
                in
                options_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) uri_field
              , Ppx_yojson_conv_lib.( ! ) options_field )
            with
            | Some uri_value, Some options_value ->
              { uri = uri_value; options = options_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) uri_field)
                      None
                  , "uri" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) options_field)
                      None
                  , "options" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { uri = v_uri; options = v_options } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_option DeleteFileOptions.yojson_of_t v_options in
          ("options", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_documentUri v_uri in
          ("uri", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let yojson_of_t t =
    match yojson_of_t t with
    | `Assoc fields -> `Assoc (("kind", `String "delete") :: fields)
    | _ -> assert false
end

(** A workspace edit represents changes to many resources managed in the
    workspace. The edit should either provide [changes] or [documentChanges]. If
    the client can handle versioned document edits and if [documentChanges] are
    present, the latter are preferred over [changes]. *)
module WorkspaceEdit = struct
  module DocumentChange = struct
    type t =
      | TextDocumentEdit of TextDocumentEdit.t
      | CreateFile of CreateFile.t
      | RenameFile of RenameFile.t
      | DeleteFile of DeleteFile.t

    let yojson_of_t = function
      | TextDocumentEdit a -> TextDocumentEdit.yojson_of_t a
      | CreateFile a -> CreateFile.yojson_of_t a
      | RenameFile a -> RenameFile.yojson_of_t a
      | DeleteFile a -> DeleteFile.yojson_of_t a
  end

  module Changes = struct
    type t = (Uri.t * TextEdit.t list) list

    let yojson_of_t changes =
      let changes =
        List.map
          ~f:(fun (uri, edits) ->
            let uri = Uri.to_string uri in
            let edits = `List (List.map ~f:TextEdit.yojson_of_t edits) in
            (uri, edits))
          changes
      in
      `Assoc changes
  end

  (** Depending on the client capability
      [workspace.workspaceEdit.resourceOperations] document changes are either
      an array of [TextDocumentEdit]s to express changes to n different text
      documents where each text document edit addresses a specific version of a
      text document. Or it can contain above [TextDocumentEdit]s mixed with
      create, rename and delete file / folder operations.

      Whether a client supports versioned document edits is expressed via
      [workspace.workspaceEdit.documentChanges] client capability.

      If a client neither supports [documentChanges] nor
      [workspace.workspaceEdit.resourceOperations] then only plain [TextEdit]s
      using the [changes] property are supported. *)
  type t =
    { changes : Changes.t [@default []] [@yojson_drop_default ( = )]
    ; documentChanges : DocumentChange.t list
          [@default []] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson_of] [@@yojson.allow_extra_fields]

  let _ = fun (_ : t) -> ()

  let yojson_of_t =
    ( function
      | { changes = v_changes; documentChanges = v_documentChanges } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          if [] = v_documentChanges then
            bnds
          else
            let arg =
              (yojson_of_list DocumentChange.yojson_of_t) v_documentChanges
            in
            let bnd = ("documentChanges", arg) in
            bnd :: bnds
        in
        let bnds =
          if [] = v_changes then
            bnds
          else
            let arg = Changes.yojson_of_t v_changes in
            let bnd = ("changes", arg) in
            bnd :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  let empty = { changes = []; documentChanges = [] }

  (** Create a {!type:t} based on the capabilities of the client. *)
  let make ~documentChanges ~uri ~version ~edits =
    match documentChanges with
    | false ->
      let changes = [ (uri, edits) ] in
      { empty with changes }
    | true ->
      let documentChanges =
        let textDocument = { VersionedTextDocumentIdentifier.uri; version } in
        let edits = { TextDocumentEdit.edits; textDocument } in
        [ DocumentChange.TextDocumentEdit edits ]
      in
      { empty with documentChanges }
end

(* PublishDiagnostics notification, method="textDocument/PublishDiagnostics" *)
module PublishDiagnostics = struct
  type diagnosticCode =
    | IntCode of int
    | StringCode of string
    | NoCode

  let yojson_of_diagnosticCode = function
    | IntCode v -> `Int v
    | StringCode v -> `String v
    | NoCode -> `Null

  let diagnosticCode_of_yojson = function
    | `Int v -> IntCode v
    | `String v -> StringCode v
    | `Null -> NoCode
    | node -> yojson_error "invalid diagnostic.code" node

  type diagnosticSeverity =
    | Error (* 1 *)
    | Warning (* 2 *)
    | Information (* 3 *)
    | Hint

  (* 4 *)

  let yojson_of_diagnosticSeverity = function
    | Error -> `Int 1
    | Warning -> `Int 2
    | Information -> `Int 3
    | Hint -> `Int 4

  let diagnosticSeverity_of_yojson = function
    | `Int 1 -> Error
    | `Int 2 -> Warning
    | `Int 3 -> Information
    | `Int 4 -> Hint
    | node -> yojson_error "diagnostic.severity expected to be int" node

  type params = publishDiagnosticsParams

  and publishDiagnosticsParams =
    { uri : documentUri
    ; diagnostics : diagnostic list
    }
  [@@yojson.allow_extra_fields]

  and diagnostic =
    { range : Range.t
    ; (* the range at which the message applies *)
      severity : diagnosticSeverity option [@yojson.option]
    ; (* if omitted, client decides *)
      code : diagnosticCode [@default NoCode] [@yojson_drop_default ( = )]
    ; (* the diagnostic's code. *)
      source : string option [@yojson.option]
    ; (* human-readable string, eg. typescript/lint *)
      message : string
    ; (* the diagnostic's message *)
      relatedInformation : diagnosticRelatedInformation list
    ; relatedLocations : relatedLocation list (* legacy FB extension *)
    }
  [@@yojson.allow_extra_fields]

  and diagnosticRelatedInformation =
    { relatedLocation : Location.t
    ; (* wire: just "location" *)
      relatedMessage : string (* wire: just "message" *)
    }
  [@@yojson.allow_extra_fields]

  (* legacy FB extension *)
  and relatedLocation = diagnosticRelatedInformation [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let _ = fun (_ : publishDiagnosticsParams) -> ()

  let _ = fun (_ : diagnostic) -> ()

  let _ = fun (_ : diagnosticRelatedInformation) -> ()

  let _ = fun (_ : relatedLocation) -> ()

  let rec params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.PublishDiagnostics.params" in
      fun t -> publishDiagnosticsParams_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  and publishDiagnosticsParams_of_yojson =
    ( let _tp_loc =
        "lsp/src/protocol.ml.PublishDiagnostics.publishDiagnosticsParams"
      in
      function
      | `Assoc field_yojsons as yojson -> (
        let uri_field = ref None
        and diagnostics_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "uri" -> (
              match Ppx_yojson_conv_lib.( ! ) uri_field with
              | None ->
                let fvalue = documentUri_of_yojson _field_yojson in
                uri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "diagnostics" -> (
              match Ppx_yojson_conv_lib.( ! ) diagnostics_field with
              | None ->
                let fvalue =
                  list_of_yojson diagnostic_of_yojson _field_yojson
                in
                diagnostics_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) uri_field
              , Ppx_yojson_conv_lib.( ! ) diagnostics_field )
            with
            | Some uri_value, Some diagnostics_value ->
              { uri = uri_value; diagnostics = diagnostics_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) uri_field)
                      None
                  , "uri" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) diagnostics_field)
                      None
                  , "diagnostics" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> publishDiagnosticsParams )

  and diagnostic_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.PublishDiagnostics.diagnostic" in
      function
      | `Assoc field_yojsons as yojson -> (
        let range_field = ref None
        and severity_field = ref None
        and code_field = ref None
        and source_field = ref None
        and message_field = ref None
        and relatedInformation_field = ref None
        and relatedLocations_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "range" -> (
              match Ppx_yojson_conv_lib.( ! ) range_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                range_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "severity" -> (
              match Ppx_yojson_conv_lib.( ! ) severity_field with
              | None ->
                let fvalue = diagnosticSeverity_of_yojson _field_yojson in
                severity_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "code" -> (
              match Ppx_yojson_conv_lib.( ! ) code_field with
              | None ->
                let fvalue = diagnosticCode_of_yojson _field_yojson in
                code_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "source" -> (
              match Ppx_yojson_conv_lib.( ! ) source_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                source_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "message" -> (
              match Ppx_yojson_conv_lib.( ! ) message_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                message_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "relatedInformation" -> (
              match Ppx_yojson_conv_lib.( ! ) relatedInformation_field with
              | None ->
                let fvalue =
                  list_of_yojson diagnosticRelatedInformation_of_yojson
                    _field_yojson
                in
                relatedInformation_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "relatedLocations" -> (
              match Ppx_yojson_conv_lib.( ! ) relatedLocations_field with
              | None ->
                let fvalue =
                  list_of_yojson relatedLocation_of_yojson _field_yojson
                in
                relatedLocations_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) range_field
              , Ppx_yojson_conv_lib.( ! ) severity_field
              , Ppx_yojson_conv_lib.( ! ) code_field
              , Ppx_yojson_conv_lib.( ! ) source_field
              , Ppx_yojson_conv_lib.( ! ) message_field
              , Ppx_yojson_conv_lib.( ! ) relatedInformation_field
              , Ppx_yojson_conv_lib.( ! ) relatedLocations_field )
            with
            | ( Some range_value
              , severity_value
              , code_value
              , source_value
              , Some message_value
              , Some relatedInformation_value
              , Some relatedLocations_value ) ->
              { range = range_value
              ; severity = severity_value
              ; code =
                  ( match code_value with
                  | None -> NoCode
                  | Some v -> v )
              ; source = source_value
              ; message = message_value
              ; relatedInformation = relatedInformation_value
              ; relatedLocations = relatedLocations_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) range_field)
                      None
                  , "range" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) message_field)
                      None
                  , "message" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) relatedInformation_field)
                      None
                  , "relatedInformation" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) relatedLocations_field)
                      None
                  , "relatedLocations" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> diagnostic )

  and diagnosticRelatedInformation_of_yojson =
    ( let _tp_loc =
        "lsp/src/protocol.ml.PublishDiagnostics.diagnosticRelatedInformation"
      in
      function
      | `Assoc field_yojsons as yojson -> (
        let relatedLocation_field = ref None
        and relatedMessage_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "relatedLocation" -> (
              match Ppx_yojson_conv_lib.( ! ) relatedLocation_field with
              | None ->
                let fvalue = Location.t_of_yojson _field_yojson in
                relatedLocation_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "relatedMessage" -> (
              match Ppx_yojson_conv_lib.( ! ) relatedMessage_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                relatedMessage_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) relatedLocation_field
              , Ppx_yojson_conv_lib.( ! ) relatedMessage_field )
            with
            | Some relatedLocation_value, Some relatedMessage_value ->
              { relatedLocation = relatedLocation_value
              ; relatedMessage = relatedMessage_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) relatedLocation_field)
                      None
                  , "relatedLocation" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) relatedMessage_field)
                      None
                  , "relatedMessage" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> diagnosticRelatedInformation )

  and relatedLocation_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.PublishDiagnostics.relatedLocation" in
      fun t -> diagnosticRelatedInformation_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> relatedLocation )

  let _ = params_of_yojson

  and _ = publishDiagnosticsParams_of_yojson

  and _ = diagnostic_of_yojson

  and _ = diagnosticRelatedInformation_of_yojson

  and _ = relatedLocation_of_yojson

  let rec yojson_of_params =
    ( fun v -> yojson_of_publishDiagnosticsParams v
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_publishDiagnosticsParams =
    ( function
      | { uri = v_uri; diagnostics = v_diagnostics } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_list yojson_of_diagnostic v_diagnostics in
          ("diagnostics", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_documentUri v_uri in
          ("uri", arg) :: bnds
        in
        `Assoc bnds
      : publishDiagnosticsParams -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_diagnostic =
    ( function
      | { range = v_range
        ; severity = v_severity
        ; code = v_code
        ; source = v_source
        ; message = v_message
        ; relatedInformation = v_relatedInformation
        ; relatedLocations = v_relatedLocations
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg =
            yojson_of_list yojson_of_relatedLocation v_relatedLocations
          in
          ("relatedLocations", arg) :: bnds
        in
        let bnds =
          let arg =
            yojson_of_list yojson_of_diagnosticRelatedInformation
              v_relatedInformation
          in
          ("relatedInformation", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_message in
          ("message", arg) :: bnds
        in
        let bnds =
          match v_source with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_string v in
            let bnd = ("source", arg) in
            bnd :: bnds
        in
        let bnds =
          if NoCode = v_code then
            bnds
          else
            let arg = yojson_of_diagnosticCode v_code in
            let bnd = ("code", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_severity with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_diagnosticSeverity v in
            let bnd = ("severity", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = Range.yojson_of_t v_range in
          ("range", arg) :: bnds
        in
        `Assoc bnds
      : diagnostic -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_diagnosticRelatedInformation =
    ( function
      | { relatedLocation = v_relatedLocation
        ; relatedMessage = v_relatedMessage
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_string v_relatedMessage in
          ("relatedMessage", arg) :: bnds
        in
        let bnds =
          let arg = Location.yojson_of_t v_relatedLocation in
          ("relatedLocation", arg) :: bnds
        in
        `Assoc bnds
      : diagnosticRelatedInformation -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_relatedLocation =
    ( fun v -> yojson_of_diagnosticRelatedInformation v
      : relatedLocation -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  and _ = yojson_of_publishDiagnosticsParams

  and _ = yojson_of_diagnostic

  and _ = yojson_of_diagnosticRelatedInformation

  and _ = yojson_of_relatedLocation

  [@@@end]
end

(* Hover request, method="textDocument/hover" *)
module Hover = struct
  type params = TextDocumentPositionParams.t

  and result = (hoverResult option[@yojson.option])

  and hoverResult =
    { contents : MarkupContent.t
    ; range : Range.t option [@yojson.option]
    }
  [@@yojson.allow_extra_fields] [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let _ = fun (_ : result) -> ()

  let _ = fun (_ : hoverResult) -> ()

  let rec params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Hover.params" in
      fun t -> TextDocumentPositionParams.t_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  and result_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Hover.result" in
      fun t -> option_of_yojson hoverResult_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> result )

  and hoverResult_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Hover.hoverResult" in
      function
      | `Assoc field_yojsons as yojson -> (
        let contents_field = ref None
        and range_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "contents" -> (
              match Ppx_yojson_conv_lib.( ! ) contents_field with
              | None ->
                let fvalue = MarkupContent.t_of_yojson _field_yojson in
                contents_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "range" -> (
              match Ppx_yojson_conv_lib.( ! ) range_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                range_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) contents_field
              , Ppx_yojson_conv_lib.( ! ) range_field )
            with
            | Some contents_value, range_value ->
              { contents = contents_value; range = range_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) contents_field)
                      None
                  , "contents" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> hoverResult )

  let _ = params_of_yojson

  and _ = result_of_yojson

  and _ = hoverResult_of_yojson

  let rec yojson_of_params =
    ( fun v -> TextDocumentPositionParams.yojson_of_t v
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_result =
    ( fun v -> yojson_of_option yojson_of_hoverResult v
      : result -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_hoverResult =
    ( function
      | { contents = v_contents; range = v_range } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_range with
          | None -> bnds
          | Some v ->
            let arg = Range.yojson_of_t v in
            let bnd = ("range", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = MarkupContent.yojson_of_t v_contents in
          ("contents", arg) :: bnds
        in
        `Assoc bnds
      : hoverResult -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  and _ = yojson_of_result

  and _ = yojson_of_hoverResult

  [@@@end]
end

module ParameterInformation = struct
  module Label = struct
    type t =
      | Substring of string
      | Range of int * int

    let t_of_yojson = function
      | `String b -> Substring b
      | `List [ `Int s; `Int e ] -> Range (s, e)
      | y -> yojson_error "ParameterInformation.Label.t" y

    let yojson_of_t = function
      | Substring s -> `String s
      | Range (s, e) -> `List [ `Int s; `Int e ]
  end

  type t =
    { label : Label.t
    ; documentation : MarkupContent.t Or_string.t
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.ParameterInformation.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let label_field = ref None
        and documentation_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "label" -> (
              match Ppx_yojson_conv_lib.( ! ) label_field with
              | None ->
                let fvalue = Label.t_of_yojson _field_yojson in
                label_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "documentation" -> (
              match Ppx_yojson_conv_lib.( ! ) documentation_field with
              | None ->
                let fvalue =
                  Or_string.t_of_yojson MarkupContent.t_of_yojson _field_yojson
                in
                documentation_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) label_field
              , Ppx_yojson_conv_lib.( ! ) documentation_field )
            with
            | Some label_value, Some documentation_value ->
              { label = label_value; documentation = documentation_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) label_field)
                      None
                  , "label" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) documentation_field)
                      None
                  , "documentation" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { label = v_label; documentation = v_documentation } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg =
            Or_string.yojson_of_t MarkupContent.yojson_of_t v_documentation
          in
          ("documentation", arg) :: bnds
        in
        let bnds =
          let arg = Label.yojson_of_t v_label in
          ("label", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module SignatureInformation = struct
  type t =
    { label : string
    ; documentation : string option [@yojson.option]
    ; parameters : ParameterInformation.t list [@default []]
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.SignatureInformation.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let label_field = ref None
        and documentation_field = ref None
        and parameters_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "label" -> (
              match Ppx_yojson_conv_lib.( ! ) label_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                label_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "documentation" -> (
              match Ppx_yojson_conv_lib.( ! ) documentation_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                documentation_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "parameters" -> (
              match Ppx_yojson_conv_lib.( ! ) parameters_field with
              | None ->
                let fvalue =
                  list_of_yojson ParameterInformation.t_of_yojson _field_yojson
                in
                parameters_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) label_field
              , Ppx_yojson_conv_lib.( ! ) documentation_field
              , Ppx_yojson_conv_lib.( ! ) parameters_field )
            with
            | Some label_value, documentation_value, parameters_value ->
              { label = label_value
              ; documentation = documentation_value
              ; parameters =
                  ( match parameters_value with
                  | None -> []
                  | Some v -> v )
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) label_field)
                      None
                  , "label" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { label = v_label
        ; documentation = v_documentation
        ; parameters = v_parameters
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg =
            yojson_of_list ParameterInformation.yojson_of_t v_parameters
          in
          ("parameters", arg) :: bnds
        in
        let bnds =
          match v_documentation with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_string v in
            let bnd = ("documentation", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_label in
          ("label", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module SignatureHelp = struct
  type t =
    { signatures : SignatureInformation.t list
    ; activeSignature : int option [@yojson.option]
    ; activeParameter : int option [@yojson.option]
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.SignatureHelp.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let signatures_field = ref None
        and activeSignature_field = ref None
        and activeParameter_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "signatures" -> (
              match Ppx_yojson_conv_lib.( ! ) signatures_field with
              | None ->
                let fvalue =
                  list_of_yojson SignatureInformation.t_of_yojson _field_yojson
                in
                signatures_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "activeSignature" -> (
              match Ppx_yojson_conv_lib.( ! ) activeSignature_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                activeSignature_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "activeParameter" -> (
              match Ppx_yojson_conv_lib.( ! ) activeParameter_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                activeParameter_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) signatures_field
              , Ppx_yojson_conv_lib.( ! ) activeSignature_field
              , Ppx_yojson_conv_lib.( ! ) activeParameter_field )
            with
            | ( Some signatures_value
              , activeSignature_value
              , activeParameter_value ) ->
              { signatures = signatures_value
              ; activeSignature = activeSignature_value
              ; activeParameter = activeParameter_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) signatures_field)
                      None
                  , "signatures" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { signatures = v_signatures
        ; activeSignature = v_activeSignature
        ; activeParameter = v_activeParameter
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_activeParameter with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_int v in
            let bnd = ("activeParameter", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_activeSignature with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_int v in
            let bnd = ("activeSignature", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg =
            yojson_of_list SignatureInformation.yojson_of_t v_signatures
          in
          ("signatures", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module CodeActionKind = struct
  type t =
    | Empty
    | QuickFix
    | Refactor
    | RefactorExtract
    | RefactorInline
    | RefactorRewrite
    | Source
    | SourceOrganizeImports
    | Other of string

  let to_string = function
    | Empty -> ""
    | QuickFix -> "quickfix"
    | Refactor -> "refactor"
    | RefactorExtract -> "refactor.extract"
    | RefactorInline -> "refactor.inline"
    | RefactorRewrite -> "refactor.rewrite"
    | Source -> "source"
    | SourceOrganizeImports -> "source.organizeImports"
    | Other s -> s

  let yojson_of_t t = `String (to_string t)

  let t_of_yojson = function
    | `String s -> (
      match s with
      | "" -> Empty
      | "quickfix" -> QuickFix
      | "refactor" -> Refactor
      | "refactor.extract" -> RefactorExtract
      | "refactor.inline" -> RefactorInline
      | "refactor.rewrite" -> RefactorRewrite
      | "source" -> Source
      | "source.organizeImports" -> SourceOrganizeImports
      | s -> Other s )
    | j -> yojson_error "Invalid code action" j
end

module WorkspaceFolder = struct
  type t =
    { uri : documentUri
    ; name : string
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.WorkspaceFolder.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let uri_field = ref None
        and name_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "uri" -> (
              match Ppx_yojson_conv_lib.( ! ) uri_field with
              | None ->
                let fvalue = documentUri_of_yojson _field_yojson in
                uri_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "name" -> (
              match Ppx_yojson_conv_lib.( ! ) name_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                name_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) uri_field
              , Ppx_yojson_conv_lib.( ! ) name_field )
            with
            | Some uri_value, Some name_value ->
              { uri = uri_value; name = name_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) uri_field)
                      None
                  , "uri" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) name_field)
                      None
                  , "name" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { uri = v_uri; name = v_name } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_string v_name in
          ("name", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_documentUri v_uri in
          ("uri", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

(* Goto Definition request, method="textDocument/definition" *)
module Definition = struct
  [@@@ocaml.warning "-39"]

  type params = TextDocumentPositionParams.t

  and result = Locations.t option
  (* wire: either a single one or an array *)
  [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let _ = fun (_ : result) -> ()

  let rec params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Definition.params" in
      fun t -> TextDocumentPositionParams.t_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  and result_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Definition.result" in
      fun t -> option_of_yojson Locations.t_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> result )

  let _ = params_of_yojson

  and _ = result_of_yojson

  let yojson_of_params =
    ( TextDocumentPositionParams.yojson_of_t
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_result =
    ( fun v -> yojson_of_option Locations.yojson_of_t v
      : result -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  and _ = yojson_of_result

  [@@@end]
end

(* Goto Type Definition request, method="textDocument/typeDefinition" *)
module TypeDefinition = struct
  [@@@ocaml.warning "-39"]

  type params = TextDocumentPositionParams.t

  and result = Location.t list (* wire: either a single one or an array *)
  [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let _ = fun (_ : result) -> ()

  let rec params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.TypeDefinition.params" in
      fun t -> TextDocumentPositionParams.t_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  and result_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.TypeDefinition.result" in
      fun t -> list_of_yojson Location.t_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> result )

  let _ = params_of_yojson

  and _ = result_of_yojson

  let yojson_of_params =
    ( TextDocumentPositionParams.yojson_of_t
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_result =
    ( fun v -> yojson_of_list Location.yojson_of_t v
      : result -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  and _ = yojson_of_result

  [@@@end]
end

(* References request, method="textDocument/references" *)
module References = struct
  type params =
    { textDocument : TextDocumentIdentifier.t
    ; (* the text document *)
      position : Position.t
    ; (* the position inside the text document *)
      context : referenceContext
    }
  [@@yojson.allow_extra_fields]

  and referenceContext = { includeDeclaration : bool }
  [@@yojson.allow_extra_fields]

  and result = Location.t list (* wire: either a single one or an array *)
  [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let _ = fun (_ : referenceContext) -> ()

  let _ = fun (_ : result) -> ()

  let rec params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.References.params" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocument_field = ref None
        and position_field = ref None
        and context_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "textDocument" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocument_field with
              | None ->
                let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
                textDocument_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "position" -> (
              match Ppx_yojson_conv_lib.( ! ) position_field with
              | None ->
                let fvalue = Position.t_of_yojson _field_yojson in
                position_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "context" -> (
              match Ppx_yojson_conv_lib.( ! ) context_field with
              | None ->
                let fvalue = referenceContext_of_yojson _field_yojson in
                context_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) textDocument_field
              , Ppx_yojson_conv_lib.( ! ) position_field
              , Ppx_yojson_conv_lib.( ! ) context_field )
            with
            | Some textDocument_value, Some position_value, Some context_value
              ->
              { textDocument = textDocument_value
              ; position = position_value
              ; context = context_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                      None
                  , "textDocument" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) position_field)
                      None
                  , "position" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) context_field)
                      None
                  , "context" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  and referenceContext_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.References.referenceContext" in
      function
      | `Assoc field_yojsons as yojson -> (
        let includeDeclaration_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "includeDeclaration" -> (
              match Ppx_yojson_conv_lib.( ! ) includeDeclaration_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                includeDeclaration_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) includeDeclaration_field with
            | Some includeDeclaration_value ->
              { includeDeclaration = includeDeclaration_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) includeDeclaration_field)
                      None
                  , "includeDeclaration" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> referenceContext )

  and result_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.References.result" in
      fun t -> list_of_yojson Location.t_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> result )

  let _ = params_of_yojson

  and _ = referenceContext_of_yojson

  and _ = result_of_yojson

  let rec yojson_of_params =
    ( function
      | { textDocument = v_textDocument
        ; position = v_position
        ; context = v_context
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_referenceContext v_context in
          ("context", arg) :: bnds
        in
        let bnds =
          let arg = Position.yojson_of_t v_position in
          ("position", arg) :: bnds
        in
        let bnds =
          let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
          ("textDocument", arg) :: bnds
        in
        `Assoc bnds
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_referenceContext =
    ( function
      | { includeDeclaration = v_includeDeclaration } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_bool v_includeDeclaration in
          ("includeDeclaration", arg) :: bnds
        in
        `Assoc bnds
      : referenceContext -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_result =
    ( fun v -> yojson_of_list Location.yojson_of_t v
      : result -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  and _ = yojson_of_referenceContext

  and _ = yojson_of_result

  [@@@end]
end

(* DocumentHighlight request, method="textDocument/documentHighlight" *)
module TextDocumentHighlight = struct
  [@@@ocaml.warning "-39"]

  type params = TextDocumentPositionParams.t

  and result = DocumentHighlight.t list
  (* wire: either a single one or an array *)
  [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let _ = fun (_ : result) -> ()

  let rec params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.TextDocumentHighlight.params" in
      fun t -> TextDocumentPositionParams.t_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  and result_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.TextDocumentHighlight.result" in
      fun t -> list_of_yojson DocumentHighlight.t_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> result )

  let _ = params_of_yojson

  and _ = result_of_yojson

  let yojson_of_params =
    ( TextDocumentPositionParams.yojson_of_t
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_result =
    ( fun v -> yojson_of_list DocumentHighlight.yojson_of_t v
      : result -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  and _ = yojson_of_result

  [@@@end]
end

module SymbolInformation = struct
  type t =
    { name : string
    ; kind : SymbolKind.t
    ; deprecated : bool option [@yojson.option]
    ; (* the span of the symbol including its contents *)
      location : Location.t
    ; (* the symbol containing this symbol *)
      containerName : string option [@yojson.option]
    }
  [@@yojson.allow_extra_fields] [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.SymbolInformation.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let name_field = ref None
        and kind_field = ref None
        and deprecated_field = ref None
        and location_field = ref None
        and containerName_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "name" -> (
              match Ppx_yojson_conv_lib.( ! ) name_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                name_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "kind" -> (
              match Ppx_yojson_conv_lib.( ! ) kind_field with
              | None ->
                let fvalue = SymbolKind.t_of_yojson _field_yojson in
                kind_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "deprecated" -> (
              match Ppx_yojson_conv_lib.( ! ) deprecated_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                deprecated_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "location" -> (
              match Ppx_yojson_conv_lib.( ! ) location_field with
              | None ->
                let fvalue = Location.t_of_yojson _field_yojson in
                location_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "containerName" -> (
              match Ppx_yojson_conv_lib.( ! ) containerName_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                containerName_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) name_field
              , Ppx_yojson_conv_lib.( ! ) kind_field
              , Ppx_yojson_conv_lib.( ! ) deprecated_field
              , Ppx_yojson_conv_lib.( ! ) location_field
              , Ppx_yojson_conv_lib.( ! ) containerName_field )
            with
            | ( Some name_value
              , Some kind_value
              , deprecated_value
              , Some location_value
              , containerName_value ) ->
              { name = name_value
              ; kind = kind_value
              ; deprecated = deprecated_value
              ; location = location_value
              ; containerName = containerName_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) name_field)
                      None
                  , "name" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) kind_field)
                      None
                  , "kind" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) location_field)
                      None
                  , "location" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { name = v_name
        ; kind = v_kind
        ; deprecated = v_deprecated
        ; location = v_location
        ; containerName = v_containerName
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_containerName with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_string v in
            let bnd = ("containerName", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = Location.yojson_of_t v_location in
          ("location", arg) :: bnds
        in
        let bnds =
          match v_deprecated with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_bool v in
            let bnd = ("deprecated", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = SymbolKind.yojson_of_t v_kind in
          ("kind", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_name in
          ("name", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module DocumentSymbol = struct
  type t =
    { name : string
          (** * The name of this symbol. Will be displayed in the user interface
              and * therefore must not be an empty string or a string only
              consisting of * white spaces. *)
    ; detail : string option
          (** * More detail for this symbol, e.g the signature of a function. *)
    ; kind : SymbolKind.t  (** * The kind of this symbol. *)
    ; deprecated : bool  (** * Indicates if this symbol is deprecated. *)
    ; range : Range.t
          (** * The range enclosing this symbol not including leading/trailing
              whitespace * but everything else like comments. This information
              is typically used to * determine if the clients cursor is inside
              the symbol to reveal in the * symbol in the UI. *)
    ; selectionRange : Range.t
          (** * The range that should be selected and revealed when this symbol
              is being * picked, e.g the name of a function. Must be contained
              by the `range`. *)
    ; children : t list
          (** * Children of this symbol, e.g. properties of a class. *)
    }
  [@@yojson.allow_extra_fields] [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let rec t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.DocumentSymbol.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let name_field = ref None
        and detail_field = ref None
        and kind_field = ref None
        and deprecated_field = ref None
        and range_field = ref None
        and selectionRange_field = ref None
        and children_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "name" -> (
              match Ppx_yojson_conv_lib.( ! ) name_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                name_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "detail" -> (
              match Ppx_yojson_conv_lib.( ! ) detail_field with
              | None ->
                let fvalue = option_of_yojson string_of_yojson _field_yojson in
                detail_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "kind" -> (
              match Ppx_yojson_conv_lib.( ! ) kind_field with
              | None ->
                let fvalue = SymbolKind.t_of_yojson _field_yojson in
                kind_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "deprecated" -> (
              match Ppx_yojson_conv_lib.( ! ) deprecated_field with
              | None ->
                let fvalue = bool_of_yojson _field_yojson in
                deprecated_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "range" -> (
              match Ppx_yojson_conv_lib.( ! ) range_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                range_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "selectionRange" -> (
              match Ppx_yojson_conv_lib.( ! ) selectionRange_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                selectionRange_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "children" -> (
              match Ppx_yojson_conv_lib.( ! ) children_field with
              | None ->
                let fvalue = list_of_yojson t_of_yojson _field_yojson in
                children_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) name_field
              , Ppx_yojson_conv_lib.( ! ) detail_field
              , Ppx_yojson_conv_lib.( ! ) kind_field
              , Ppx_yojson_conv_lib.( ! ) deprecated_field
              , Ppx_yojson_conv_lib.( ! ) range_field
              , Ppx_yojson_conv_lib.( ! ) selectionRange_field
              , Ppx_yojson_conv_lib.( ! ) children_field )
            with
            | ( Some name_value
              , Some detail_value
              , Some kind_value
              , Some deprecated_value
              , Some range_value
              , Some selectionRange_value
              , Some children_value ) ->
              { name = name_value
              ; detail = detail_value
              ; kind = kind_value
              ; deprecated = deprecated_value
              ; range = range_value
              ; selectionRange = selectionRange_value
              ; children = children_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) name_field)
                      None
                  , "name" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) detail_field)
                      None
                  , "detail" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) kind_field)
                      None
                  , "kind" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) deprecated_field)
                      None
                  , "deprecated" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) range_field)
                      None
                  , "range" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) selectionRange_field)
                      None
                  , "selectionRange" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) children_field)
                      None
                  , "children" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let rec yojson_of_t =
    ( function
      | { name = v_name
        ; detail = v_detail
        ; kind = v_kind
        ; deprecated = v_deprecated
        ; range = v_range
        ; selectionRange = v_selectionRange
        ; children = v_children
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_list yojson_of_t v_children in
          ("children", arg) :: bnds
        in
        let bnds =
          let arg = Range.yojson_of_t v_selectionRange in
          ("selectionRange", arg) :: bnds
        in
        let bnds =
          let arg = Range.yojson_of_t v_range in
          ("range", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_bool v_deprecated in
          ("deprecated", arg) :: bnds
        in
        let bnds =
          let arg = SymbolKind.yojson_of_t v_kind in
          ("kind", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_option yojson_of_string v_detail in
          ("detail", arg) :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_name in
          ("name", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

(* Document Symbols request, method="textDocument/documentSymbols" *)
module TextDocumentDocumentSymbol = struct
  type params = { textDocument : TextDocumentIdentifier.t }
  [@@yojson.allow_extra_fields] [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.TextDocumentDocumentSymbol.params" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocument_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "textDocument" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocument_field with
              | None ->
                let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
                textDocument_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) textDocument_field with
            | Some textDocument_value -> { textDocument = textDocument_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                      None
                  , "textDocument" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  let _ = params_of_yojson

  let yojson_of_params =
    ( function
      | { textDocument = v_textDocument } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
          ("textDocument", arg) :: bnds
        in
        `Assoc bnds
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  [@@@end]

  type result =
    | DocumentSymbol of DocumentSymbol.t list
    | SymbolInformation of SymbolInformation.t list

  let yojson_of_result = function
    | DocumentSymbol symbols ->
      `List (List.map symbols ~f:DocumentSymbol.yojson_of_t)
    | SymbolInformation symbols ->
      `List (List.map symbols ~f:SymbolInformation.yojson_of_t)
end

module CodeLens = struct
  type params = { textDocument : TextDocumentIdentifier.t }
  [@@yojson.allow_extra_fields]

  and result = item list

  and item =
    { range : Range.t
    ; command : Command.t option
    }
  [@@yojson.allow_extra_fields] [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let _ = fun (_ : result) -> ()

  let _ = fun (_ : item) -> ()

  let rec params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.CodeLens.params" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocument_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "textDocument" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocument_field with
              | None ->
                let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
                textDocument_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) textDocument_field with
            | Some textDocument_value -> { textDocument = textDocument_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                      None
                  , "textDocument" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  and result_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.CodeLens.result" in
      fun t -> list_of_yojson item_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> result )

  and item_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.CodeLens.item" in
      function
      | `Assoc field_yojsons as yojson -> (
        let range_field = ref None
        and command_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "range" -> (
              match Ppx_yojson_conv_lib.( ! ) range_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                range_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "command" -> (
              match Ppx_yojson_conv_lib.( ! ) command_field with
              | None ->
                let fvalue =
                  option_of_yojson Command.t_of_yojson _field_yojson
                in
                command_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) range_field
              , Ppx_yojson_conv_lib.( ! ) command_field )
            with
            | Some range_value, Some command_value ->
              { range = range_value; command = command_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) range_field)
                      None
                  , "range" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) command_field)
                      None
                  , "command" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> item )

  let _ = params_of_yojson

  and _ = result_of_yojson

  and _ = item_of_yojson

  let rec yojson_of_params =
    ( function
      | { textDocument = v_textDocument } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
          ("textDocument", arg) :: bnds
        in
        `Assoc bnds
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_result =
    ( fun v -> yojson_of_list yojson_of_item v
      : result -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_item =
    ( function
      | { range = v_range; command = v_command } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_option Command.yojson_of_t v_command in
          ("command", arg) :: bnds
        in
        let bnds =
          let arg = Range.yojson_of_t v_range in
          ("range", arg) :: bnds
        in
        `Assoc bnds
      : item -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  and _ = yojson_of_result

  and _ = yojson_of_item

  [@@@end]
end

(** Rename symbol request, metho="textDocument/rename" *)
module Rename = struct
  type params =
    { textDocument : TextDocumentIdentifier.t  (** The document to rename. *)
    ; position : Position.t  (** The position at which this request was sent. *)
    ; newName : string
          (** The new name of the symbol. If the given name is not valid the
              request must return a [ResponseError](#ResponseError) with an
              appropriate message set. *)
    }
  [@@yojson.allow_extra_fields] [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.Rename.params" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocument_field = ref None
        and position_field = ref None
        and newName_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "textDocument" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocument_field with
              | None ->
                let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
                textDocument_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "position" -> (
              match Ppx_yojson_conv_lib.( ! ) position_field with
              | None ->
                let fvalue = Position.t_of_yojson _field_yojson in
                position_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "newName" -> (
              match Ppx_yojson_conv_lib.( ! ) newName_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                newName_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) textDocument_field
              , Ppx_yojson_conv_lib.( ! ) position_field
              , Ppx_yojson_conv_lib.( ! ) newName_field )
            with
            | Some textDocument_value, Some position_value, Some newName_value
              ->
              { textDocument = textDocument_value
              ; position = position_value
              ; newName = newName_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                      None
                  , "textDocument" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) position_field)
                      None
                  , "position" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) newName_field)
                      None
                  , "newName" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  let _ = params_of_yojson

  let yojson_of_params =
    ( function
      | { textDocument = v_textDocument
        ; position = v_position
        ; newName = v_newName
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_string v_newName in
          ("newName", arg) :: bnds
        in
        let bnds =
          let arg = Position.yojson_of_t v_position in
          ("position", arg) :: bnds
        in
        let bnds =
          let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
          ("textDocument", arg) :: bnds
        in
        `Assoc bnds
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  [@@@end]

  type result = WorkspaceEdit.t [@@deriving_inline yojson_of]

  let _ = fun (_ : result) -> ()

  let yojson_of_result =
    (WorkspaceEdit.yojson_of_t : result -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_result

  [@@@end]
end

module DebugEcho = struct
  type params = { message : string } [@@yojson.allow_extra_fields]

  and result = params [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let _ = fun (_ : result) -> ()

  let rec params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.DebugEcho.params" in
      function
      | `Assoc field_yojsons as yojson -> (
        let message_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "message" -> (
              match Ppx_yojson_conv_lib.( ! ) message_field with
              | None ->
                let fvalue = string_of_yojson _field_yojson in
                message_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) message_field with
            | Some message_value -> { message = message_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) message_field)
                      None
                  , "message" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  and result_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.DebugEcho.result" in
      fun t -> params_of_yojson t
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> result )

  let _ = params_of_yojson

  and _ = result_of_yojson

  let rec yojson_of_params =
    ( function
      | { message = v_message } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = yojson_of_string v_message in
          ("message", arg) :: bnds
        in
        `Assoc bnds
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  and yojson_of_result =
    (fun v -> yojson_of_params v : result -> Ppx_yojson_conv_lib.Yojson.Safe.t)

  let _ = yojson_of_params

  and _ = yojson_of_result

  [@@@end]
end

module DebugTextDocumentGet = struct
  type params = TextDocumentPositionParams.t [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let params_of_yojson =
    ( TextDocumentPositionParams.t_of_yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  let _ = params_of_yojson

  let yojson_of_params =
    ( TextDocumentPositionParams.yojson_of_t
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  [@@@end]

  type result = (string option[@default None])

  let yojson_of_result = function
    | Some s -> `String s
    | None -> `Null
end

module FoldingRange = struct
  module Kind = struct
    type t =
      | Comment
      | Imports
      | Region

    let yojson_of_t t =
      `String
        ( match t with
        | Comment -> "comment"
        | Imports -> "imports"
        | Region -> "region" )

    let t_of_yojson node =
      match node with
      | `String s -> (
        match s with
        | "comment" -> Comment
        | "imports" -> Imports
        | "region" -> Region
        | _ -> yojson_error "invalid t_of_yojson" node )
      | _ -> yojson_error "invalid t_of_yojson" node
  end

  type t =
    { startLine : int
    ; startCharacter : int option [@yojson.option]
    ; endLine : int
    ; endCharacter : int option [@yojson.option]
    ; kind : Kind.t option [@yojson.option]
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.FoldingRange.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let startLine_field = ref None
        and startCharacter_field = ref None
        and endLine_field = ref None
        and endCharacter_field = ref None
        and kind_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "startLine" -> (
              match Ppx_yojson_conv_lib.( ! ) startLine_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                startLine_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "startCharacter" -> (
              match Ppx_yojson_conv_lib.( ! ) startCharacter_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                startCharacter_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "endLine" -> (
              match Ppx_yojson_conv_lib.( ! ) endLine_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                endLine_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "endCharacter" -> (
              match Ppx_yojson_conv_lib.( ! ) endCharacter_field with
              | None ->
                let fvalue = int_of_yojson _field_yojson in
                endCharacter_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "kind" -> (
              match Ppx_yojson_conv_lib.( ! ) kind_field with
              | None ->
                let fvalue = Kind.t_of_yojson _field_yojson in
                kind_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) startLine_field
              , Ppx_yojson_conv_lib.( ! ) startCharacter_field
              , Ppx_yojson_conv_lib.( ! ) endLine_field
              , Ppx_yojson_conv_lib.( ! ) endCharacter_field
              , Ppx_yojson_conv_lib.( ! ) kind_field )
            with
            | ( Some startLine_value
              , startCharacter_value
              , Some endLine_value
              , endCharacter_value
              , kind_value ) ->
              { startLine = startLine_value
              ; startCharacter = startCharacter_value
              ; endLine = endLine_value
              ; endCharacter = endCharacter_value
              ; kind = kind_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) startLine_field)
                      None
                  , "startLine" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) endLine_field)
                      None
                  , "endLine" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { startLine = v_startLine
        ; startCharacter = v_startCharacter
        ; endLine = v_endLine
        ; endCharacter = v_endCharacter
        ; kind = v_kind
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_kind with
          | None -> bnds
          | Some v ->
            let arg = Kind.yojson_of_t v in
            let bnd = ("kind", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_endCharacter with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_int v in
            let bnd = ("endCharacter", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_int v_endLine in
          ("endLine", arg) :: bnds
        in
        let bnds =
          match v_startCharacter with
          | None -> bnds
          | Some v ->
            let arg = yojson_of_int v in
            let bnd = ("startCharacter", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_int v_startLine in
          ("startLine", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  type params = { textDocument : TextDocumentIdentifier.t }
  [@@yojson.allow_extra_fields] [@@deriving_inline yojson]

  let _ = fun (_ : params) -> ()

  let params_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.FoldingRange.params" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocument_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "textDocument" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocument_field with
              | None ->
                let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
                textDocument_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ -> () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match Ppx_yojson_conv_lib.( ! ) textDocument_field with
            | Some textDocument_value -> { textDocument = textDocument_value }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                      None
                  , "textDocument" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> params )

  let _ = params_of_yojson

  let yojson_of_params =
    ( function
      | { textDocument = v_textDocument } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
          ("textDocument", arg) :: bnds
        in
        `Assoc bnds
      : params -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_params

  [@@@end]

  type result = t list

  let yojson_of_result r = `List (List.map ~f:yojson_of_t r)
end

module CodeActionContext = struct
  type t =
    { diagnostics : PublishDiagnostics.diagnostic list
    ; only : CodeActionKind.t Only.t
          [@default Only.All] [@yojson_drop_default ( = )]
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.CodeActionContext.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let diagnostics_field = ref None
        and only_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "diagnostics" -> (
              match Ppx_yojson_conv_lib.( ! ) diagnostics_field with
              | None ->
                let fvalue =
                  list_of_yojson PublishDiagnostics.diagnostic_of_yojson
                    _field_yojson
                in
                diagnostics_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "only" -> (
              match Ppx_yojson_conv_lib.( ! ) only_field with
              | None ->
                let fvalue =
                  Only.t_of_yojson CodeActionKind.t_of_yojson _field_yojson
                in
                only_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) diagnostics_field
              , Ppx_yojson_conv_lib.( ! ) only_field )
            with
            | Some diagnostics_value, only_value ->
              { diagnostics = diagnostics_value
              ; only =
                  ( match only_value with
                  | None -> Only.All
                  | Some v -> v )
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) diagnostics_field)
                      None
                  , "diagnostics" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { diagnostics = v_diagnostics; only = v_only } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          if Only.All = v_only then
            bnds
          else
            let arg = (Only.yojson_of_t CodeActionKind.yojson_of_t) v_only in
            let bnd = ("only", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg =
            yojson_of_list PublishDiagnostics.yojson_of_diagnostic v_diagnostics
          in
          ("diagnostics", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module CodeActionParams = struct
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : CodeActionContext.t
    }
  [@@deriving_inline yojson]

  let _ = fun (_ : t) -> ()

  let t_of_yojson =
    ( let _tp_loc = "lsp/src/protocol.ml.CodeActionParams.t" in
      function
      | `Assoc field_yojsons as yojson -> (
        let textDocument_field = ref None
        and range_field = ref None
        and context_field = ref None
        and duplicates = ref []
        and extra = ref [] in
        let rec iter = function
          | (field_name, _field_yojson) :: tail ->
            ( match field_name with
            | "textDocument" -> (
              match Ppx_yojson_conv_lib.( ! ) textDocument_field with
              | None ->
                let fvalue = TextDocumentIdentifier.t_of_yojson _field_yojson in
                textDocument_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "range" -> (
              match Ppx_yojson_conv_lib.( ! ) range_field with
              | None ->
                let fvalue = Range.t_of_yojson _field_yojson in
                range_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | "context" -> (
              match Ppx_yojson_conv_lib.( ! ) context_field with
              | None ->
                let fvalue = CodeActionContext.t_of_yojson _field_yojson in
                context_field := Some fvalue
              | Some _ ->
                duplicates := field_name :: Ppx_yojson_conv_lib.( ! ) duplicates
              )
            | _ ->
              if
                Ppx_yojson_conv_lib.( ! )
                  Ppx_yojson_conv_lib.Yojson_conv.record_check_extra_fields
              then
                extra := field_name :: Ppx_yojson_conv_lib.( ! ) extra
              else
                () );
            iter tail
          | [] -> ()
        in
        iter field_yojsons;
        match Ppx_yojson_conv_lib.( ! ) duplicates with
        | _ :: _ ->
          Ppx_yojson_conv_lib.Yojson_conv_error.record_duplicate_fields _tp_loc
            (Ppx_yojson_conv_lib.( ! ) duplicates)
            yojson
        | [] -> (
          match Ppx_yojson_conv_lib.( ! ) extra with
          | _ :: _ ->
            Ppx_yojson_conv_lib.Yojson_conv_error.record_extra_fields _tp_loc
              (Ppx_yojson_conv_lib.( ! ) extra)
              yojson
          | [] -> (
            match
              ( Ppx_yojson_conv_lib.( ! ) textDocument_field
              , Ppx_yojson_conv_lib.( ! ) range_field
              , Ppx_yojson_conv_lib.( ! ) context_field )
            with
            | Some textDocument_value, Some range_value, Some context_value ->
              { textDocument = textDocument_value
              ; range = range_value
              ; context = context_value
              }
            | _ ->
              Ppx_yojson_conv_lib.Yojson_conv_error.record_undefined_elements
                _tp_loc yojson
                [ ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) textDocument_field)
                      None
                  , "textDocument" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) range_field)
                      None
                  , "range" )
                ; ( Ppx_yojson_conv_lib.poly_equal
                      (Ppx_yojson_conv_lib.( ! ) context_field)
                      None
                  , "context" )
                ] ) ) )
      | _ as yojson ->
        Ppx_yojson_conv_lib.Yojson_conv_error.record_list_instead_atom _tp_loc
          yojson
      : Ppx_yojson_conv_lib.Yojson.Safe.t -> t )

  let _ = t_of_yojson

  let yojson_of_t =
    ( function
      | { textDocument = v_textDocument; range = v_range; context = v_context }
        ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          let arg = CodeActionContext.yojson_of_t v_context in
          ("context", arg) :: bnds
        in
        let bnds =
          let arg = Range.yojson_of_t v_range in
          ("range", arg) :: bnds
        in
        let bnds =
          let arg = TextDocumentIdentifier.yojson_of_t v_textDocument in
          ("textDocument", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]
end

module CodeAction = struct
  type t =
    { title : string
    ; kind : CodeActionKind.t option [@yojson.option]
    ; diagnostics : PublishDiagnostics.diagnostic list
          [@default []] [@yojson_drop_default ( = )]
    ; edit : WorkspaceEdit.t option [@yojson.option]
    ; command : Command.t option [@yojson.option]
    }
  [@@deriving_inline yojson_of]

  let _ = fun (_ : t) -> ()

  let yojson_of_t =
    ( function
      | { title = v_title
        ; kind = v_kind
        ; diagnostics = v_diagnostics
        ; edit = v_edit
        ; command = v_command
        } ->
        let bnds : (string * Ppx_yojson_conv_lib.Yojson.Safe.t) list = [] in
        let bnds =
          match v_command with
          | None -> bnds
          | Some v ->
            let arg = Command.yojson_of_t v in
            let bnd = ("command", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_edit with
          | None -> bnds
          | Some v ->
            let arg = WorkspaceEdit.yojson_of_t v in
            let bnd = ("edit", arg) in
            bnd :: bnds
        in
        let bnds =
          if [] = v_diagnostics then
            bnds
          else
            let arg =
              (yojson_of_list PublishDiagnostics.yojson_of_diagnostic)
                v_diagnostics
            in
            let bnd = ("diagnostics", arg) in
            bnd :: bnds
        in
        let bnds =
          match v_kind with
          | None -> bnds
          | Some v ->
            let arg = CodeActionKind.yojson_of_t v in
            let bnd = ("kind", arg) in
            bnd :: bnds
        in
        let bnds =
          let arg = yojson_of_string v_title in
          ("title", arg) :: bnds
        in
        `Assoc bnds
      : t -> Ppx_yojson_conv_lib.Yojson.Safe.t )

  let _ = yojson_of_t

  [@@@end]

  type result = (Command.t, t) Either.t list

  let yojson_of_result (elems : result) : Yojson.Safe.t =
    `List
      (List.map elems ~f:(function
        | Either.Right r -> yojson_of_t r
        | Left l -> Command.yojson_of_t l))
end
