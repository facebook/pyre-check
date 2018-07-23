(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

(* A generalization of what can be a call target during the analysis.
   - RealTarget refers to an actual definition.
   - OverrideTarget is a set of target represented by an override tree.
*)

open Ast
open Expression
open Statement


type real_target = [ `RealTarget of Access.t ]
[@@deriving show, compare]

type override_target = [ `OverrideTarget of Access.t ]
[@@deriving show, compare]

type t = [ real_target | override_target ]
[@@deriving show, compare]

type target_with_stored_result = real_target

module Key : sig
  type nonrec t = t
  val to_string: t -> string
  val compare: t -> t -> int
end

val get_definition: real_target -> Define.t option
