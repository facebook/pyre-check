(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement
module Error = AnalysisError

module Resolution : sig
  type t

  val of_list : (Reference.t * ReadOnlyness.t) list -> t

  val to_alist : t -> (Reference.t * ReadOnlyness.t) list
end

module Resolved : sig
  type t = {
    resolution: Resolution.t;
    resolved: ReadOnlyness.t;
    errors: Error.t list;
  }
  [@@deriving show]
end

module State : sig
  val forward_expression : resolution:Resolution.t -> Expression.t -> Resolved.t
end

val readonly_errors_for_define : Define.t Node.t -> Error.t list
