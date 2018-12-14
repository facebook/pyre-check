(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast

module Kind = AnalysisKind


(* Implemented by an individual analysis to be plugged into the global fixpoint. *)
module type ANALYSIS_PROVIDED = sig
  (* Registration info. *)
  type call_model  (* Used in fixpoint computation (widened), used at call sites. *)
  type result      (* Produced in each iteration (replaced), not used at call sites. *)

  (* Used as part of a filename, so avoid spaces and slashes *)
  val name: string

  (* Functions to construct global operations. *)
  val empty_model: call_model
  val obscure_model: call_model  (* For call sites where we don't know the targets. *)
  val join: iteration:int -> call_model -> call_model -> call_model
  val widen: iteration:int -> previous:call_model -> next:call_model -> call_model
  val reached_fixpoint: iteration:int -> previous:call_model -> next:call_model -> bool

  val get_errors: result -> InterproceduralError.t list
  val externalize: Callable.t -> result option -> call_model -> Yojson.Safe.json list
  (* Additional metadata an analysis wants to save, e.g., warning code explanation. *)
  val metadata: unit -> Yojson.Safe.json

  val show_call_model: call_model -> string
end

(* Representation of the kind of data manipulated by each individual analysis.
   NOTE, we use an object type, but never any actual objects, since object types
   are more flexible for specifying a set of labeled types than other mechanisms.
*)
type ('result, 'model) analysis_data = < result: 'result; model: 'model >

(* Stored kind (storable in the shared heap). *)
type 'a storable_kind = 'a Kind.storable_kind
  constraint 'a = ('result, 'model) analysis_data

(* External kind (can't be stored in the shared heap). *)
type 'a kind = 'a Kind.kind
  constraint 'a = ('result, 'model) analysis_data

(* Type markers to distinguish model, result parts in generic data structures
   statically.
   E.g. model pkg vs. result pkg below. This allows splitting off more distinct parts
   from the analysis data without having to write new code to package and access the new
   parts. *)
type model = MK
type result = RK

(* Abstract a full kind to just the part necessary. The model and result markers
   allow the static type system to avoid having to pattern match parts that are not
   used.
*)
type ('part, _) partial_kind =
  | ModelPart: <model:'model; ..> storable_kind -> (model, 'model) partial_kind
  | ResultPart: <result:'result; ..> storable_kind -> (result, 'result) partial_kind

(* Abstracts part of an analysis' data value by storing it along with
   its kind. Used for both model and result parts from each analysis. *)
type 'part pkg = Pkg: {
    kind: ('part, 'value) partial_kind;
    value: 'value;
  } -> 'part pkg
[@@deriving show]

type result_pkg = result pkg
[@@deriving show]

type model_pkg = model pkg
[@@deriving show]

type model_t = {
  models: model_pkg Kind.Map.t;
  is_obscure: bool;
}
[@@deriving show]

type result_t = result_pkg Kind.Map.t
[@@deriving show]

module type ANALYZER = sig
  type result
  type call_model
  val analyze
    :  callable: Callable.real_target
    -> environment: (module Analysis.Environment.Handler)
    -> define: Statement.Define.t Node.t
    -> result * call_model

  (* Called once on master before analysis of individual callables. *)
  val init: types:string list -> functions:Callable.t list -> unit
end

module type ANALYSIS_RESULT = sig
  include ANALYSIS_PROVIDED

  val kind: (result, call_model) analysis_data Kind.kind
end

module type ANALYSIS_RESULT_WITH_REGISTRATION = sig
  include ANALYSIS_RESULT

  module Register(Analyzer : ANALYZER
                  with type result := result
                   and type call_model := call_model): sig
    val abstract_kind: Kind.abstract
  end
end

(* The full signature of an individual analysis. This is what the general
   framework stores per analysis and uses to manipulate its data. NOTE: this gets
   built in 2 steps, a) from the ANALYSIS_DATA provided to create the result kind,
   and b) from the analyzer function. This split breaks the recursion between
   analyses and their results, allowing analysis A to refer to ResultB, where
   ResultB is the result of analysis B, and vice-versa. *)
module type ANALYSIS = sig
  include ANALYSIS_RESULT
  include ANALYZER
    with type result := result
     and type call_model := call_model
end

type 'a analysis_module =
  (module ANALYSIS
    with type result = 'result
     and type call_model = 'model
  )
  constraint 'a = ('result, 'model) analysis_data

type 'a analysis = {
  kind: 'a Kind.storable_kind;
  analysis: 'a analysis_module;
}

type abstract_analysis = Analysis: 'a analysis -> abstract_analysis

val get_abstract_analysis: Kind.abstract -> abstract_analysis
val get_analysis:
  ('result,'model) analysis_data Kind.storable_kind
  -> ('result,'model) analysis_data analysis_module

(* To add a new analysis, the analysis should invoke the Make functor below. The
   functor adds a new constant constructor to 'analysis kind.

   This Kind constructor can then be used by analyses to lookup their own
   results/fixpoint information (or from another analysis).

   To register the analysis, call the Register functor in the resulting module.
*)
module Make(Analysis : ANALYSIS_PROVIDED): ANALYSIS_RESULT_WITH_REGISTRATION
  with type result := Analysis.result
   and type call_model := Analysis.call_model

val empty_model: model_t
val obscure_model: model_t
val empty_result: result_t

val get: ('part, 'value) partial_kind -> 'part pkg Kind.Map.t -> 'value option
val get_model: ('result, 'model) analysis_data Kind.kind -> model_t -> 'model option
val with_model:
  ('result, 'model) analysis_data Kind.kind
  -> 'model
  -> model_t
  -> model_t

val get_result: ('result, 'model) analysis_data Kind.kind -> result_t -> 'result option
