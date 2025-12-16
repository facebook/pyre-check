(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

let shim_calls_for_pyre1 ~resolve_expression_to_type:_ _ = None

let shim_calls_for_pyrefly ~callees:_ ~nested_callees:_ ~arguments:_ = None
