(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast

module T : sig
  type incompatible_model_error_reason =
    | UnexpectedPositionalOnlyParameter of string
    | UnexpectedNamedParameter of string
    | UnexpectedStarredParameter
    | UnexpectedDoubleStarredParameter
  [@@deriving eq]

  type kind =
    | InvalidDefaultValue of {
        callable_name: string;
        name: string;
        expression: Expression.t;
      }
    | IncompatibleModelError of {
        name: string;
        callable_type: Type.Callable.t;
        reasons: incompatible_model_error_reason list;
      }
    | ImportedFunctionModel of {
        name: Reference.t;
        actual_name: Reference.t;
      }
    | MissingAttribute of {
        class_name: string;
        attribute_name: string;
      }
    | NotInEnvironment of string
    | UnclassifiedError of {
        model_name: string;
        message: string;
      }
  [@@deriving eq]

  type t = {
    kind: kind;
    path: Pyre.Path.t option;
    location: Location.t;
  }
  [@@deriving eq, show]
end

type t = T.t

val to_json : t -> Yojson.Safe.t

val display : t -> string
