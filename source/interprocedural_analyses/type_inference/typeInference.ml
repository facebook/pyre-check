(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This directory allows Pyre to infer type annotations (as exposed via `pyre infer`).

   We add aliases for the modules in this directory so that external modules can refer to them
   without repeating the common prefix. *)

module Data = TypeInferenceData
module Local = TypeInferenceLocal
