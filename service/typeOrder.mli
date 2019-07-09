(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis

val compute_hashes_to_keys : indices:int list -> annotations:string list -> string String.Map.t

module Handler : ClassHierarchy.Handler
