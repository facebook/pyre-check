(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val parse_and_translate
  :  configuration:Configuration.Analysis.t ->
  state:State.t ->
  request:Yojson.Safe.t ->
  Protocol.Request.t option
