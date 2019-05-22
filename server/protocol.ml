(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis

open Pyre


module DefinitionRequest = struct
  type t = {
    id: LanguageServer.Types.RequestId.t;
    file: File.t;
    position: Location.position;
  }
  [@@deriving eq, show]
end


type client =
  | FileNotifier
  | Persistent
[@@deriving eq, show]


module TypeQuery = struct
  type serialized_ocaml_value =
    | SerializedValue of {
        serialized_key: string;
        serialized_value: string;
      }
    | SerializedPair of {
        serialized_key: string;
        first_serialized_value: string;
        second_serialized_value: string;
      }
  [@@deriving eq, show, to_yojson]

  type request =
    | Attributes of Reference.t
    | ComputeHashesToKeys
    | CoverageInFile of File.t
    | DecodeOcamlValues of serialized_ocaml_value list
    | DumpDependencies of File.t
    | DumpMemoryToSqlite of Path.t
    | IsCompatibleWith of Expression.t * Expression.t
    | Join of Expression.t * Expression.t
    | LessOrEqual of Expression.t * Expression.t
    | Meet of Expression.t * Expression.t
    | Methods of Reference.t
    | NormalizeType of Expression.t
    | PathOfModule of Reference.t
    | SaveServerState of Path.t
    | Signature of Reference.t
    | Superclasses of Expression.t
    | Type of Expression.t
    | TypeAtPosition of {
        file: File.t;
        position: Location.position;
      }
    | TypesInFile of File.t
  [@@deriving eq, show]

  type coverage_level =
    | Typed
    | Partial
    | Untyped
  [@@deriving eq, show, to_yojson]

  type attribute = {
    name: string;
    annotation: Type.t;
  }
  [@@deriving eq, show, to_yojson]

  type method_representation = {
    name: string;
    parameters: Type.t list;
    return_annotation: Type.t;
  }
  [@@deriving eq, show, to_yojson]

  type found_parameter = {
    parameter_name: string;
    annotation: Type.t option;
  }
  [@@deriving eq, show, to_yojson]

  type found_signature = {
    return_type: Type.t option;
    parameters: found_parameter list;
  }
  [@@deriving eq, show, to_yojson]

  type type_at_location = {
    location: Location.Instantiated.t;
    annotation: Type.t;
  }
  [@@deriving eq, show, to_yojson]

  type coverage_at_location = {
    location: Location.Instantiated.t;
    coverage: coverage_level;
  }
  [@@deriving eq, show, to_yojson]

  type key_mapping = {
    hash: string;
    key: string;
  }
  [@@deriving eq, show, to_yojson]

  type decoded_value =
    | DecodedValue of {
        serialized_key: string;
        kind: string;
        actual_key: string;
        actual_value: string option;
      }
    | DecodedPair of {
        serialized_key: string;
        kind: string;
        actual_key: string;
        first_value: string option;
        second_value: string option;
        equal: bool;
      }
  [@@deriving eq, show, to_yojson]

  type decoded = {
    decoded: decoded_value list;
    undecodable_keys: string list;
  }
  [@@deriving eq, show, to_yojson]

  type compatibility = {
    actual: Type.t;
    expected: Type.t;
    result: bool;
  }
  [@@deriving eq, show]

  let _ = show_compatibility (* unused, but pp is *)

  type base_response =
    | Boolean of bool
    | Compatibility of compatibility
    | CoverageAtLocations of coverage_at_location list
    | Decoded of decoded
    | FoundAttributes of attribute list
    | FoundKeyMapping of key_mapping list
    | FoundMethods of method_representation list
    | FoundPath of string
    | FoundSignature of found_signature list
    | Path of Path.t
    | Success of unit
    | Superclasses of Type.t list
    | Type of Type.t
    | TypeAtLocation of type_at_location
    | TypesAtLocations of type_at_location list
  [@@deriving eq, show]

  let base_response_to_yojson = function
    | Boolean boolean ->
        `Assoc ["boolean", `Bool boolean]
    | Compatibility { actual; expected; result } ->
        `Assoc [
          "actual", Type.to_yojson actual;
          "expected", Type.to_yojson expected;
          "boolean", `Bool result;
        ]
    | CoverageAtLocations annotations ->
        `Assoc ["types", `List (List.map annotations ~f:coverage_at_location_to_yojson)]
    | Decoded { decoded; undecodable_keys } ->
        let to_json decoded =
          match decoded with
          | DecodedValue { serialized_key; kind; actual_key; actual_value } ->
              let value =
                match actual_value with
                | Some actual_value ->
                    ["value", `String actual_value]
                | None ->
                    []
              in
              `Assoc ([
                  "serialized_key", `String serialized_key;
                  "kind", `String kind;
                  "key", `String actual_key;
                ] @ value)
          | DecodedPair {
              serialized_key;
              kind;
              actual_key;
              first_value;
              second_value;
              equal;
            } ->
              let first_value =
                match first_value with
                | Some first_value ->
                    ["value", `String first_value]
                | None ->
                    []
              in
              let second_value =
                match second_value with
                | Some second_value ->
                    ["value", `String second_value]
                | None ->
                    []
              in
              `Assoc ([
                  "serialized_key", `String serialized_key;
                  "kind", `String kind;
                  "key", `String actual_key;
                  "equal", `Bool equal;
                ]
                  @ first_value
                  @ second_value)
        in
        `Assoc [
          "decoded", `List (List.map decoded ~f:to_json);
          "undecodable_keys", `List (List.map undecodable_keys ~f:(fun key -> `String key));
        ]
    | Path path ->
        `Assoc ["path", `String (Path.absolute path)]
    | FoundAttributes attributes ->
        `Assoc ["attributes", `List (List.map attributes ~f:attribute_to_yojson)]
    | FoundKeyMapping associative_list ->
        `Assoc (List.map associative_list ~f:(fun { hash; key } -> (hash, `String key)))
    | FoundMethods methods ->
        `Assoc ["methods", `List (List.map methods ~f:method_representation_to_yojson)]
    | FoundPath path ->
        `Assoc ["path", `String path]
    | FoundSignature signatures ->
        `Assoc ["signature", `List (List.map signatures ~f:found_signature_to_yojson)]
    | Success () ->
        `Assoc []
    | Superclasses classes ->
        `Assoc ["superclasses", `List (List.map classes ~f:Type.to_yojson)]
    | Type annotation ->
        `Assoc ["type", Type.to_yojson annotation]
    | TypeAtLocation annotation ->
        type_at_location_to_yojson annotation
    | TypesAtLocations annotations ->
        `Assoc ["types", `List (List.map annotations ~f:type_at_location_to_yojson)]

  type response =
    | Response of base_response
    | Error of string
  [@@deriving eq, show]

  let response_to_yojson = function
    | Response base_response ->
        `Assoc ["response", base_response_to_yojson base_response]
    | Error message ->
        `Assoc ["error", `String message]
end


module Request = struct
  type t =
    | LanguageServerProtocolRequest of string
    | ClientConnectionRequest of client
    | ClientExitRequest of client
    | RageRequest of LanguageServer.Types.RequestId.t
    | DisplayTypeErrors of File.t list
    | TypeCheckRequest of File.t list
    | TypeQueryRequest of TypeQuery.request
    | StopRequest
    | ClientShutdownRequest of LanguageServer.Types.RequestId.t
    | GetDefinitionRequest of DefinitionRequest.t
    | HoverRequest of DefinitionRequest.t
    | OpenDocument of File.t
    | CloseDocument of File.t
    | SaveDocument of File.t
    | CodeActionRequest of  {
        id: LanguageServer.Types.RequestId.t;
        uri: LanguageServer.Types.DocumentUri.t;
        diagnostics: LanguageServer.Types.Diagnostic.t list;
        file: File.t;
      }
    | ExecuteCommandRequest of {
        id: LanguageServer.Types.RequestId.t;
        arguments: LanguageServer.Types.CommandArguments.t list;
      }
  [@@deriving eq, show]


  type origin =
    | PersistentSocket of Unix.File_descr.t
    | NewConnectionSocket of Unix.File_descr.t
    | FileNotifier
    | Background


  let origin_name = function
    | PersistentSocket _ -> "Persistent client"
    | NewConnectionSocket _ -> "New connection"
    | FileNotifier -> "File notifier"
    | Background -> "Background"


  let name = function
    | LanguageServerProtocolRequest _ -> "LanguageServerProtocol"
    | ClientConnectionRequest _ -> "ClientConnection"
    | ClientExitRequest _ -> "ClientExit"
    | RageRequest _ -> "Rage"
    | DisplayTypeErrors _ -> "DisplayTypeErrors"
    | TypeCheckRequest [] -> "TypeCheck"
    | TypeCheckRequest _ -> "IncrementalCheck"
    | TypeQueryRequest _ -> "TypeQuery"
    | StopRequest -> "Stop"
    | ClientShutdownRequest _ -> "ClientConnection"
    | GetDefinitionRequest _ -> "GetDefinition"
    | HoverRequest _ -> "Hover"
    | OpenDocument _ -> "OpenDocument"
    | CloseDocument _ -> "CloseDocument"
    | SaveDocument _ -> "SaveDocument"
    | CodeActionRequest _ -> "CodeAction"
    | ExecuteCommandRequest _ -> "ExecuteCommandRequest"
end


type response =
  | LanguageServerProtocolResponse of string
  | ClientConnectionResponse of client
  | ClientExitResponse of client
  | TypeCheckResponse of (File.Handle.t * (Error.t list)) list
  | TypeQueryResponse of TypeQuery.response
  | StopResponse
  | GetDefinitionResponse of Location.Instantiated.t option
  | HoverResponse of Location.Instantiated.t option
[@@deriving eq, show]
