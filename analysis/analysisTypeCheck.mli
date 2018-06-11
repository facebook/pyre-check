(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

module Annotation = AnalysisAnnotation
module Environment = AnalysisEnvironment
module Error = AnalysisError
module Lookup = AnalysisLookup
module Coverage = AnalysisCoverage
module Cfg = AnalysisCfg
module CallGraph = AnalysisCallGraph
module Resolution = AnalysisResolution


module State : sig
  (* Keep track of nested functions to analyze and their initial states. *)
  type nested_define

  (* `configuration` provides access to global options.

     `resolution` provides access to the local and global type environment.

     `errors` is a map from locations to errors. We assume there can be only
     one error per location.

     `define` is the function we're currently checking.

     `lookup` keeps track of symbols and their types to later expose them to IDEs.

     `call_graph` is an interface that builds a call graph in memory during type checking.

     `nested_defines` keeps track of entry points and states for nested function definitions.

     `bottom` indicates whether the state is reachable.

     The order is defined by values of the map, i.e.
     left <= right <=>
        keys(left) \subset \keys(right) \and
        \forall key \in keys(left): left(key) <= right(key)

     The join takes the union of keys and does an element-wise join on the
     values. *)
  and t = {
    configuration: Configuration.t;
    resolution: Resolution.t;
    errors: Error.t Location.Map.t;
    define: Define.t Node.t;
    lookup: Lookup.t option;
    call_graph: (module CallGraph.Handler);
    nested_defines: nested_define Location.Map.t;
    bottom: bool;
  }
  [@@deriving eq, show]

  val create
    :  ?configuration: Configuration.t
    -> resolution: Resolution.t
    -> define: Statement.Define.t Node.t
    -> ?lookup: Lookup.t
    -> call_graph: (module CallGraph.Handler)
    -> unit
    -> t

  val errors: t -> Error.t list
  val coverage: t -> Coverage.t

  val initial
    :  ?configuration: Configuration.t
    -> ?lookup: Lookup.t
    -> call_graph: (module CallGraph.Handler)
    -> resolution: Resolution.t
    -> Define.t Node.t
    -> t

  include AnalysisFixpoint.State with type t := t
end

module Fixpoint : AnalysisFixpoint.Fixpoint with type state := State.t

module Result : sig
  type t = {
    errors: Error.t list;
    lookup: Lookup.t option;
    coverage: Coverage.t;
  }
end

val check
  :  Configuration.t
  -> (module Environment.Handler)
  -> (module CallGraph.Handler)
  -> ?mode_override: Source.mode
  -> Source.t
  -> Result.t
