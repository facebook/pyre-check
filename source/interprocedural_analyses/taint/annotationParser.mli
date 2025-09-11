(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Represents a source or sink kind from a model (e.g, UserControlled). *)
module KindExpression : sig
  type t =
    | Name of {
        name: string;
        subkind: string option;
      }
    | Updates of Analysis.TaintAccessPath.Root.t
  [@@deriving equal]

  val from_name : string -> t
end

(* Represents a source or sink defined in a taint.config file. *)
module KindDefinition : sig
  type taint_kind =
    | Named
    | Parametric

  type t = {
    name: string;
    kind: taint_kind;
    location: JsonParsing.JsonAst.LocationWithPath.t option;
  }
end

val parse_source : allowed:KindDefinition.t list -> KindExpression.t -> (Sources.t, string) result

val parse_sink : allowed:KindDefinition.t list -> KindExpression.t -> (Sinks.t, string) result

val parse_transform : allowed:TaintTransform.t list -> string -> (TaintTransform.t, string) result

val parse_tito
  :  allowed_transforms:TaintTransform.t list ->
  KindExpression.t ->
  (Sinks.t, string) result
