(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

module NestedDefines : sig
  type 'state nested = {
    nested_define: Define.t;
    state: 'state;
  }

  and 'state t = 'state nested Location.Reference.Map.t

  val initial : 'state Location.Reference.Map.t

  val update_nested_defines
    :  (Location.t, 'state nested, 'a) Base.Map.t ->
    statement:statement Node.t ->
    state:'state ->
    (Location.t, 'state nested, 'a) Base.Map.t
end

val nested_defines_deep_to_shallow : Define.t Node.t -> Define.t Node.t list
