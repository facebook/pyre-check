(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = {
  sources: Sources.t list;
  sinks: Sinks.t list;
  transforms: TaintTransform.t list;
  code: int;
  name: string;
  message_format: string; (* format *)
  location: JsonParsing.JsonAst.LocationWithPath.t option; (* location where the rule was defined *)
}
[@@deriving compare, show]

module CodeSet : Stdlib.Set.S with type elt = int

val transform_splits : 'a list -> ('a list * 'a list) list
