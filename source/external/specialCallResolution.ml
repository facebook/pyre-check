(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

module NestedCallees = struct
  type t =
    (* Given call `x(1)(...)`, this is the callees on `x(1)` *)
    | NestedCall of Target.t list
    (* Given call `x.y.z(...)`, this is the callees on `x.y` *)
    | NestedAttributeAccess of Target.t list
    | None
end

let shim_calls_for_pyre1 ~resolve_expression_to_type:_ _ = None

let shim_calls_for_pyrefly ~callees:_ ~nested_callees:_ ~arguments:_ = None
