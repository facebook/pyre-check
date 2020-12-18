(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Pyre

exception InvalidQuery of string

module Request : sig
  type t =
    | Attributes of Reference.t
    | Batch of t list
    | Callees of Reference.t
    | CalleesWithLocation of Reference.t
    | Defines of Reference.t list
    | DumpCallGraph
    | DumpClassHierarchy
    | Help of string
    | IsCompatibleWith of Expression.t * Expression.t
    | LessOrEqual of Expression.t * Expression.t
    | Methods of Expression.t
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
end

val help : unit -> string

val parse_query : configuration:Configuration.Analysis.t -> string -> Request.t
