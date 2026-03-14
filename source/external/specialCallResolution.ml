(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

module CallTarget = struct
  type t =
    | Regular of {
        target: Target.t;
        receiver_class: string option;
      }
    | Init of {
        target: Target.t;
        receiver_class: string option;
      }
    | New of {
        target: Target.t;
        receiver_class: string option;
      }
    | Property of {
        target: Target.t;
        receiver_class: string option;
      }

  let target = function
    | Regular { target; _ } -> target
    | Init { target; _ } -> target
    | New { target; _ } -> target
    | Property { target; _ } -> target


  let receiver_class = function
    | Regular { receiver_class; _ } -> receiver_class
    | Init { receiver_class; _ } -> receiver_class
    | New { receiver_class; _ } -> receiver_class
    | Property { receiver_class; _ } -> receiver_class
end

module NestedCallees = struct
  type t =
    (* Given call `x(1)(...)`, this is the callees on `x(1)` *)
    | NestedCall of CallTarget.t list
    (* Given call `x.y.z(...)`, this is the callees on `x.y` *)
    | NestedAttributeAccess of CallTarget.t list
    | None
end

let shim_calls_for_pyre1 ~resolve_expression_to_type:_ ~class_mro:_ ~callable_exists:_ _ = None

let shim_calls_for_pyrefly ~class_mro:_ ~callable_exists:_ ~callees:_ ~nested_callees:_ ~arguments:_
  =
  None
