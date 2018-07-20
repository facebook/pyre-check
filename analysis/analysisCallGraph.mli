(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Core
open Expression


type t = (Access.t list) Access.Map.t

val create:
  environment: (module AnalysisEnvironment.Handler)
  -> source: Source.t
  -> t

(** Returns a partition of nodes for strongly connected components in the call graph *)
val partition: edges: (Access.t list) Access.Map.t -> (Access.t list) list

(** Reverse edges in the call graph *)
val reverse: t -> t

val pp_partitions: Format.formatter -> (Access.t list) list -> unit
