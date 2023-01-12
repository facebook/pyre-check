(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Interprocedural

(* A unique identifier that represents the first sink of an issue. *)
module Sink : sig
  type t =
    | Call of {
        callee: Target.t;
        index: int;
        parameter: AccessPath.Root.t;
      }
    | Global of {
        callee: Target.t;
        index: int;
      }
    | Return
    | LiteralStringSink of Sinks.t
    | ConditionalTestSink of Sinks.t
  [@@deriving compare, hash, sexp]

  val make_call : call_target:CallGraph.CallTarget.t -> root:AccessPath.Root.t -> t

  val make_global : call_target:CallGraph.CallTarget.t -> t

  val to_json : t -> Yojson.Safe.t
end

type t = {
  code: int;
  callable: Target.t;
  sink: Sink.t;
}
[@@deriving compare]

val master_handle : t -> string

module Map : sig
  include Core.Map.S with type Key.t = t
end
