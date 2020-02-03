(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Pyre

module CompletionRequest = struct
  type t = {
    id: LanguageServer.Types.RequestId.t;
    path: Path.t;
    position: Location.position;
  }
  [@@deriving eq, show]
end

module DefinitionRequest = struct
  type t = {
    id: LanguageServer.Types.RequestId.t;
    path: Path.t;
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
    | RunCheck of {
        check_name: string;
        paths: Path.t list;
      }
    | Attributes of Reference.t
    | Callees of Reference.t
    | CalleesWithLocation of Reference.t
    | ComputeHashesToKeys
    | CoverageInFile of Path.t
    | DecodeOcamlValues of serialized_ocaml_value list
    | Defines of Reference.t list
    | DumpCallGraph
    | DumpClassHierarchy
    | DumpDependencies of Path.t
    | DumpMemoryToSqlite of Path.t
    | Help of string
    | IsCompatibleWith of Expression.t * Expression.t
    | Join of Expression.t * Expression.t
    | LessOrEqual of Expression.t * Expression.t
    | Meet of Expression.t * Expression.t
    | Methods of Expression.t
    | NormalizeType of Expression.t
    | PathOfModule of Reference.t
    | SaveServerState of Path.t
    | Signature of Reference.t list
    | Superclasses of Expression.t
    | Type of Expression.t
    | TypeAtPosition of {
        path: Path.t;
        position: Location.position;
      }
    | TypesInFiles of Path.t list
    | ValidateTaintModels of Path.t option
  [@@deriving eq, show]

  type coverage_level =
    | Typed
    | Partial
    | Untyped
  [@@deriving eq, show, to_yojson]

  type attribute_kind =
    | Regular
    | Property
  [@@deriving eq, show, to_yojson]

  type attribute = {
    name: string;
    annotation: Type.t;
    kind: attribute_kind;
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
    function_name: string;
    return_type: Type.t option;
    parameters: found_parameter list;
  }
  [@@deriving eq, show, to_yojson]

  type type_at_location = {
    location: Location.t;
    annotation: Type.t;
  }
  [@@deriving eq, show, to_yojson]

  type types_at_file = {
    path: PyrePath.t;
    types: type_at_location list;
  }
  [@@deriving eq, show, to_yojson]

  type coverage_at_location = {
    location: Location.t;
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

  type callee_with_instantiated_locations = {
    callee: Callgraph.callee;
    locations: Location.WithPath.t list;
  }
  [@@deriving eq, show]

  type callees = {
    caller: Reference.t;
    callees: callee_with_instantiated_locations list;
  }
  [@@deriving eq, show]

  type parameter_representation = {
    parameter_name: string;
    parameter_annotation: Expression.t option;
  }
  [@@deriving eq, show]

  type define = {
    define_name: Reference.t;
    parameters: parameter_representation list;
    return_annotation: Expression.t option;
  }
  [@@deriving eq, show]

  let _ = show_compatibility (* unused, but pp is *)

  type base_response =
    | Boolean of bool
    | Callees of Callgraph.callee list
    | CalleesWithLocation of callee_with_instantiated_locations list
    | Callgraph of callees list
    | ClassHierarchy of Yojson.Safe.t
    | Compatibility of compatibility
    | CoverageAtLocations of coverage_at_location list
    | Decoded of decoded
    | Errors of AnalysisError.Instantiated.t list
    | Help of string
    | FoundAttributes of attribute list
    | FoundDefines of define list
    | FoundKeyMapping of key_mapping list
    | FoundMethods of method_representation list
    | FoundPath of string
    | FoundSignature of found_signature list
    | Path of Path.t
    | References of Reference.t list
    | Success of string
    | Superclasses of Type.t list
    | Type of Type.t
    | TypeAtLocation of type_at_location
    | TypesByFile of types_at_file list
  [@@deriving eq, show]

  let base_response_to_yojson = function
    | Boolean boolean -> `Assoc ["boolean", `Bool boolean]
    | Callees callees -> `Assoc ["callees", `List (List.map callees ~f:Callgraph.callee_to_yojson)]
    | CalleesWithLocation callees ->
        let callee_to_yojson { callee; locations } = Callgraph.callee_to_yojson ~locations callee in
        `Assoc ["callees", `List (List.map callees ~f:callee_to_yojson)]
    | Callgraph callees ->
        let callee_to_yojson { callee; locations } = Callgraph.callee_to_yojson ~locations callee in
        `Assoc
          (List.map callees ~f:(fun { caller; callees } ->
               Reference.show caller, `List (List.map callees ~f:callee_to_yojson)))
    | ClassHierarchy hierarchy -> hierarchy
    | Compatibility { actual; expected; result } ->
        `Assoc
          [
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
                | Some actual_value -> ["value", `String actual_value]
                | None -> []
              in
              `Assoc
                ( [
                    "serialized_key", `String serialized_key;
                    "kind", `String kind;
                    "key", `String actual_key;
                  ]
                @ value )
          | DecodedPair { serialized_key; kind; actual_key; first_value; second_value; equal } ->
              let first_value =
                match first_value with
                | Some first_value -> ["first_value", `String first_value]
                | None -> []
              in
              let second_value =
                match second_value with
                | Some second_value -> ["second_value", `String second_value]
                | None -> []
              in
              `Assoc
                ( [
                    "serialized_key", `String serialized_key;
                    "kind", `String kind;
                    "key", `String actual_key;
                    "equal", `Bool equal;
                  ]
                @ first_value
                @ second_value )
        in
        `Assoc
          [
            "decoded", `List (List.map decoded ~f:to_json);
            "undecodable_keys", `List (List.map undecodable_keys ~f:(fun key -> `String key));
          ]
    | Errors errors ->
        `Assoc
          [
            ( "errors",
              `List
                (List.map
                   ~f:(fun error ->
                     AnalysisError.Instantiated.to_json ~show_error_traces:true error)
                   errors) );
          ]
    | Help string -> `Assoc ["help", `String string]
    | Path path -> `Assoc ["path", `String (Path.absolute path)]
    | FoundAttributes attributes ->
        let attribute_to_yojson { name; annotation; kind } =
          let kind =
            match kind with
            | Regular -> "regular"
            | Property -> "property"
          in

          `Assoc
            ["name", `String name; "annotation", Type.to_yojson annotation; "kind", `String kind]
        in
        `Assoc ["attributes", `List (List.map attributes ~f:attribute_to_yojson)]
    | FoundDefines defines ->
        let define_to_yojson { define_name; parameters; return_annotation } =
          let annotation_to_yojson = function
            | None -> `Null
            | Some annotation ->
                Expression.sanitized annotation
                |> Expression.show
                |> fun annotation -> `String annotation
          in
          let parameter_representation_to_yojson { parameter_name; parameter_annotation } =
            `Assoc
              [
                "name", `String parameter_name;
                "annotation", annotation_to_yojson parameter_annotation;
              ]
          in
          `Assoc
            [
              "name", `String (Reference.show define_name);
              "parameters", `List (List.map parameters ~f:parameter_representation_to_yojson);
              "return_annotation", annotation_to_yojson return_annotation;
            ]
        in
        `List (List.map defines ~f:define_to_yojson)
    | FoundKeyMapping associative_list ->
        `Assoc (List.map associative_list ~f:(fun { hash; key } -> hash, `String key))
    | FoundMethods methods ->
        `Assoc ["methods", `List (List.map methods ~f:method_representation_to_yojson)]
    | FoundPath path -> `Assoc ["path", `String path]
    | FoundSignature signatures ->
        `Assoc ["signature", `List (List.map signatures ~f:found_signature_to_yojson)]
    | References references ->
        let json_references =
          List.map references ~f:(fun reference -> `String (Reference.show reference))
        in
        `Assoc ["references", `List json_references]
    | Success message -> `Assoc ["message", `String message]
    | Superclasses classes -> `Assoc ["superclasses", `List (List.map classes ~f:Type.to_yojson)]
    | Type annotation -> `Assoc ["type", Type.to_yojson annotation]
    | TypeAtLocation annotation -> type_at_location_to_yojson annotation
    | TypesByFile paths_to_annotations ->
        `List (List.map paths_to_annotations ~f:types_at_file_to_yojson)


  type response =
    | Response of base_response
    | Error of string
  [@@deriving eq, show]

  let response_to_yojson = function
    | Response base_response -> `Assoc ["response", base_response_to_yojson base_response]
    | Error message -> `Assoc ["error", `String message]


  let create_type_at_location (location, annotation) = { location; annotation }

  let json_socket_response response =
    `Assoc ["jsonrpc", `String "2.0"; "error", `Null; "result", response_to_yojson response]
end

module Request = struct
  type t =
    | ClientConnectionRequest of client
    | ClientExitRequest of client
    | ClientShutdownRequest of LanguageServer.Types.RequestId.t
    | CloseDocument of Path.t
    | CodeActionRequest of {
        id: LanguageServer.Types.RequestId.t;
        uri: LanguageServer.Types.DocumentUri.t;
        diagnostics: LanguageServer.Types.Diagnostic.t list;
        path: Path.t;
      }
    | DisplayTypeErrors of Path.t list
    | ExecuteCommandRequest of {
        id: LanguageServer.Types.RequestId.t;
        arguments: LanguageServer.Types.CommandArguments.t list;
      }
    | GetDefinitionRequest of DefinitionRequest.t
    | CompletionRequest of CompletionRequest.t
    | HoverRequest of DefinitionRequest.t
    | LanguageServerProtocolRequest of string
    | OpenDocument of Path.t
    | RageRequest of LanguageServer.Types.RequestId.t
    | DocumentChange of File.t
    | SaveDocument of Path.t
    | GetServerUuid
    | ShowStatusRequest of LanguageServer.Types.ShowStatusParameters.t
    | StopRequest
    | TypeCheckRequest of Path.t list
    | TypeCoverageRequest of {
        path: Path.t;
        id: LanguageServer.Types.RequestId.t;
      }
    | TypeQueryRequest of TypeQuery.request
  [@@deriving eq, show]

  type origin =
    | PersistentSocket of Unix.File_descr.t
    | NewConnectionSocket of Unix.File_descr.t
    | FileNotifier
    | JSONSocket of Unix.File_descr.t

  let origin_name = function
    | PersistentSocket _ -> "Persistent client"
    | NewConnectionSocket _ -> "New connection"
    | FileNotifier -> "File notifier"
    | JSONSocket _ -> "JSONSocket"


  let name = function
    | LanguageServerProtocolRequest _ -> "LanguageServerProtocol"
    | ClientConnectionRequest _ -> "ClientConnection"
    | ClientExitRequest _ -> "ClientExit"
    | RageRequest _ -> "Rage"
    | DisplayTypeErrors _ -> "DisplayTypeErrors"
    | TypeCheckRequest [] -> "TypeCheck"
    | TypeCheckRequest _ -> "IncrementalCheck"
    | TypeQueryRequest _ -> "TypeQuery"
    | ShowStatusRequest _ -> "ShowStatusRequest"
    | StopRequest -> "Stop"
    | ClientShutdownRequest _ -> "ClientConnection"
    | GetDefinitionRequest _ -> "GetDefinition"
    | CompletionRequest _ -> "Completion"
    | HoverRequest _ -> "Hover"
    | OpenDocument _ -> "OpenDocument"
    | CloseDocument _ -> "CloseDocument"
    | DocumentChange _ -> "DocumentChange"
    | SaveDocument _ -> "SaveDocument"
    | CodeActionRequest _ -> "CodeAction"
    | ExecuteCommandRequest _ -> "ExecuteCommandRequest"
    | TypeCoverageRequest _ -> "TypeCoverageRequest"
    | GetServerUuid -> "GetServerUuid"
end

type response =
  | LanguageServerProtocolResponse of string
  | ClientConnectionResponse of client
  | ClientExitResponse of client
  | TypeCheckResponse of AnalysisError.Instantiated.t list
  | TypeQueryResponse of TypeQuery.response
  | StopResponse
  | GetDefinitionResponse of Location.WithPath.t option
  | HoverResponse of Location.t option
  | ServerUuidResponse of string
[@@deriving eq, show]
