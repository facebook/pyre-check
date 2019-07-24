(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Analysis

val transform_dataclass : Environment.t -> GlobalResolution.t -> Source.t -> unit

val transform_attrs : Environment.t -> GlobalResolution.t -> Source.t -> unit
