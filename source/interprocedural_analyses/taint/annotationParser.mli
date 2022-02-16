(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type taint_kind =
  | Named
  | Parametric

type source_or_sink = {
  name: string;
  kind: taint_kind;
}

val parse_source
  :  allowed:source_or_sink list ->
  ?subkind:string ->
  string ->
  (Sources.t, string) result

val parse_sink
  :  allowed:source_or_sink list ->
  ?subkind:string ->
  string ->
  (Sinks.t, string) result

val parse_transform : allowed:TaintTransform.t list -> string -> (TaintTransform.t, string) result

val parse_tito
  :  allowed_transforms:TaintTransform.t list ->
  ?subkind:string ->
  string ->
  (Sinks.t, string) result
