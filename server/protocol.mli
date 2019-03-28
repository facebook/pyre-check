(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Pyre
open Expression

module DefinitionRequest: sig
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


module TypeQuery: sig
  type serialized_ocaml_value = {
    serialized_key: string;
    serialized_value: string;
  }
  [@@deriving eq, show, to_yojson]

  type request =
    | Attributes of Access.t
    | ComputeHashesToKeys
    | DecodeOcamlValues of serialized_ocaml_value list
    | DumpDependencies of File.t
    | DumpMemoryToSqlite of Path.t
    | IsCompatibleWith of Access.t * Access.t
    | Join of Access.t * Access.t
    | LessOrEqual of Access.t * Access.t
    | Meet of Access.t * Access.t
    | Methods of Access.t
    | NormalizeType of Access.t
    | PathOfModule of Access.t
    | SaveServerState of Path.t
    | Signature of Access.t
    | Superclasses of Access.t
    | Type of Expression.t
    | TypeAtPosition of {
        file: File.t;
        position: Location.position;
      }
    | TypesInFile of File.t
  [@@deriving eq, show]

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

  type decoded_value = {
    serialized_key: string;
    kind: string;
    actual_key: string;
    actual_value: string option;
  }
  [@@deriving eq, show, to_yojson]

  type decoded = {
    decoded: decoded_value list;
    undecodable_keys: string list;
  }
  [@@deriving eq, show, to_yojson]

  type key_mapping = {
    hash: string;
    key: string;
  }
  [@@deriving eq, show, to_yojson]

  type base_response =
    | Boolean of bool
    | Decoded of decoded
    | FoundAttributes of attribute list
    | FoundKeyMapping of key_mapping list
    | FoundMethods of method_representation list
    | FoundPath of string
    | FoundSignature of found_signature list
    | Path of Pyre.Path.t
    | Success of unit
    | Superclasses of Type.t list
    | Type of Type.t
    | TypeAtLocation of type_at_location
    | TypesAtLocations of type_at_location list
  [@@deriving eq, show, to_yojson]

  type response =
    | Response of base_response
    | Error of string
  [@@deriving eq, show, to_yojson]
end


module Request : sig
  type t =
    | LanguageServerProtocolRequest of string
    | ClientConnectionRequest of client
    | ClientExitRequest of client
    | RageRequest of LanguageServer.Types.RequestId.t
    | DisplayTypeErrors of { files: File.t list; flush: bool }
    | TypeCheckRequest of File.t list
    | TypeQueryRequest of TypeQuery.request
    | StopRequest
    | ClientShutdownRequest of LanguageServer.Types.RequestId.t
    | GetDefinitionRequest of DefinitionRequest.t
    | HoverRequest of DefinitionRequest.t
    | OpenDocument of File.t
    | CloseDocument of File.t
    | SaveDocument of File.t
  [@@deriving eq, show]

  type origin =
    | PersistentSocket of Unix.File_descr.t
    | NewConnectionSocket of Unix.File_descr.t
    | FileNotifier
    | Background

  val origin_name: origin -> string

  val name: t -> string
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
