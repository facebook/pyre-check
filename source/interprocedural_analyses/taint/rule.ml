(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Rule: defines rules, which describe flows that the analysis should consider
 * as valid issues. Each rule contains a set of sources matching a set of sinks.
 *)

open Core

type filters = {
  maximum_source_distance: int option;
  maximum_sink_distance: int option;
}
[@@deriving equal, compare, show]

type t = {
  sources: Sources.t list;
  sinks: Sinks.t list;
  transforms: TaintTransform.t list;
  code: int;
  name: string;
  message_format: string; (* format *)
  filters: filters option;
  location: JsonParsing.JsonAst.LocationWithPath.t option; (* location where the rule was defined *)
}
[@@deriving compare, show]

module CodeSet = Stdlib.Set.Make (Int)

(* Given a rule to find flows of the form:
 *   source -> T1 -> T2 -> T3 -> ... -> Tn -> sink
 * Following are different ways we can find matching flows:
 *   source -> T1:T2:T3:...:Tn:sink
 *   T1:source -> T2:T3:...:Tn:sink
 *   T2:T1:source -> T3:...:Tn:sink
 *   ...
 *   Tn:...:T3:T2:T1:source -> sink
 *)
let transform_splits transforms =
  let rec split ~result ~prefix ~suffix =
    let result = (prefix, suffix) :: result in
    match suffix with
    | [] -> result
    | next :: suffix -> (
        match next with
        | TaintTransform.Named _ -> split ~result ~prefix:(next :: prefix) ~suffix
        | TaintTransform.Sanitize _ -> failwith "Sanitize transforms shouldn't appear in rules"
        | TaintTransform.TriggeredPartialSink _ ->
            (* We don't allow transform `TriggeredPartialSink` to be prepended to sources, because
               this transform can be added only when creating triggered partial sinks. *)
            result)
  in
  split ~result:[] ~prefix:[] ~suffix:transforms
