(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module UnannotatedDefine : sig
  type t = {
    define: Ast.Statement.Define.Signature.t;
    location: Ast.Location.WithModule.t;
  }
  [@@deriving sexp, compare]
end

module ImportEntry : sig
  type t =
    | Module of {
        target: Ast.Reference.t;
        implicit_alias: bool;
      }
    | Name of {
        from: Ast.Reference.t;
        target: Ast.Identifier.t;
        implicit_alias: bool;
      }
  [@@deriving sexp, compare]

  (* Do NOT use this API in new code *)
  val deprecated_original_name : t -> Ast.Reference.t
end

type t =
  | SimpleAssign of {
      explicit_annotation: Ast.Expression.t option;
      value: Ast.Expression.t option;
      target_location: Ast.Location.WithModule.t;
    }
  | TupleAssign of {
      value: Ast.Expression.t option;
      target_location: Ast.Location.WithModule.t;
      index: int;
      total_length: int;
    }
  | Imported of ImportEntry.t
  | Define of UnannotatedDefine.t list
  | Class
[@@deriving sexp, compare]

module Collector : sig
  module Result : sig
    type nonrec t = {
      name: Ast.Identifier.t;
      unannotated_global: t;
    }
    [@@deriving sexp, compare]
  end

  val from_source : Ast.Source.t -> Result.t list
end
