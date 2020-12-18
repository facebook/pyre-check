(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Pyre

module CompletionRequest : sig
  type t = {
    id: LanguageServer.Types.RequestId.t;
    path: Path.t;
    position: Location.position;
  }
  [@@deriving eq, show]
end

module DefinitionRequest : sig
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

module TypeQuery : sig
  type request =
    | Attributes of Reference.t
    | Batch of request list
    | Callees of Reference.t
    | CalleesWithLocation of Reference.t
    | Defines of Reference.t list
    | DumpCallGraph
    | DumpClassHierarchy
    | Help of string
    | IsCompatibleWith of Expression.t * Expression.t
    | LessOrEqual of Expression.t * Expression.t
    | Methods of Expression.t
    | NamesInFiles of Path.t list
    | PathOfModule of Reference.t
    | RunCheck of {
        check_name: string;
        paths: Path.t list;
      }
    | SaveServerState of Path.t
    | Superclasses of Expression.t list
    | Type of Expression.t
    | TypeAtPosition of {
        path: Path.t;
        position: Location.position;
      }
    | TypesInFiles of Path.t list
    | ValidateTaintModels of Path.t option
  [@@deriving eq, show]

  type attribute_kind =
    | Regular
    | Property

  type attribute = {
    name: string;
    annotation: Type.t;
    kind: attribute_kind;
    final: bool;
  }
  [@@deriving eq, show, to_yojson]

  type method_representation = {
    name: string;
    parameters: Type.t list;
    return_annotation: Type.t;
  }
  [@@deriving eq, show, to_yojson]

  type type_at_location = {
    location: Location.t;
    annotation: Type.t;
  }
  [@@deriving eq, show, to_yojson]

  type types_at_path = {
    path: PyrePath.t;
    types: type_at_location list;
  }
  [@@deriving eq, show, to_yojson]

  type qualified_name_at_location = {
    location: Location.t;
    qualified_name: Reference.t;
  }
  [@@deriving eq, show, to_yojson]

  type qualified_names_at_path = {
    path: PyrePath.t;
    qualified_names: qualified_name_at_location list;
  }
  [@@deriving eq, show, to_yojson]

  type compatibility = {
    actual: Type.t;
    expected: Type.t;
    result: bool;
  }
  [@@derving eq, show]

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

  type superclasses_mapping = {
    class_name: Reference.t;
    superclasses: Type.t list;
  }
  [@@deriving eq, show]

  type base_response =
    | Batch of response list
    | Boolean of bool
    | Callees of Callgraph.callee list
    | CalleesWithLocation of callee_with_instantiated_locations list
    | Callgraph of callees list
    | ClassHierarchy of Yojson.Safe.t
    | Compatibility of compatibility
    | Errors of AnalysisError.Instantiated.t list
    | FoundAttributes of attribute list
    | FoundDefines of define list
    | FoundMethods of method_representation list
    | FoundPath of string
    | Help of string
    | ModelVerificationErrors of Taint.Model.ModelVerificationError.t list
    | NamesByPath of qualified_names_at_path list
    | Success of string
    | Superclasses of superclasses_mapping list
    | Type of Type.t
    | TypeAtLocation of type_at_location
    | TypesByPath of types_at_path list
  [@@deriving eq, show, to_yojson]

  and response =
    | Response of base_response
    | Error of string
  [@@deriving eq, show, to_yojson]

  val create_type_at_location : Location.t * Type.t -> type_at_location

  val create_qualified_name_at_location : Location.t * Reference.t -> qualified_name_at_location

  val json_socket_response : response -> Yojson.Safe.t
end

module Request : sig
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
    | CompletionRequest of CompletionRequest.t
    | DisplayTypeErrors of Path.t list
    | DocumentChange of File.t
    | ExecuteCommandRequest of {
        id: LanguageServer.Types.RequestId.t;
        arguments: LanguageServer.Types.CommandArguments.t list;
      }
    | GetDefinitionRequest of DefinitionRequest.t
    | HoverRequest of DefinitionRequest.t
    | InitializeRequest of LanguageServer.Types.RequestId.t
    | InitializedRequest
    | LanguageServerProtocolRequest of string
    | OpenDocument of Path.t
    | RageRequest of LanguageServer.Types.RequestId.t
    | SaveDocument of Path.t
    | ShowStatusRequest of LanguageServer.Types.ShowStatusParameters.t
    | StopRequest
    | TypeCheckRequest of Path.t list
    | TypeQueryRequest of TypeQuery.request
    | UnparsableQuery of {
        query: string;
        reason: string;
      }
  [@@deriving eq, show]

  type origin =
    | FileNotifier
    | JSONSocket of Unix.File_descr.t
    | NewConnectionSocket of Unix.File_descr.t
    | PersistentSocket of Unix.File_descr.t

  val origin_name : origin -> string

  val name : t -> string
end

type response =
  | ClientConnectionResponse of client
  | ClientExitResponse of client
  | GetDefinitionResponse of Location.WithPath.t option
  | HoverResponse of Location.t option
  | LanguageServerProtocolResponse of string
  | ServerUuidResponse of string
  | StopResponse
  | TypeCheckResponse of AnalysisError.Instantiated.t list
  | TypeQueryResponse of TypeQuery.response
[@@deriving eq, show]
