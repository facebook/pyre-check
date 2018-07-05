(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression


val of_source:
  (module AnalysisEnvironment.Handler)
  -> Source.t
  -> (Access.t list) Access.Map.t

(** Returns a partition of nodes for strongly connected components in the call graph *)
val partition: edges: (Access.t list) Access.Map.t -> (Access.t list) list

val pp_partitions: Format.formatter -> (Access.t list) list -> unit
