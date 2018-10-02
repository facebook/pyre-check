(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Statement

module Kind = AnalysisKind


(* Implemented by an individual analysis to be plugged into the global fixpoint. *)
module type ANALYSIS_PROVIDED = sig
  (* Registration info. *)
  type call_model  (* Used in fixpoint computation (widened), used at call sites. *)
  type result      (* Produced in each iteration (replaced), not used at call sites. *)

  val name: string

  (* Functions to construct global operations. *)
  val empty_model: call_model
  val obscure_model: call_model  (* For call sites where we don't know the targets. *)
  val join: iteration:int -> call_model -> call_model -> call_model
  val widen: iteration:int -> previous:call_model -> next:call_model -> call_model
  val reached_fixpoint: iteration:int -> previous:call_model -> next:call_model -> bool

  val get_errors: result -> InterproceduralError.t list
  val summaries: Callable.t -> result option -> call_model -> Yojson.Safe.json list

  val show_call_model: call_model -> string
end


(* Representation of the kind of data manipulated by each individual analysis.
   NOTE, we use an object type, but never any actual objects, since object types
   are more flexible for specifying a set of labeled types than other mechanisms.
*)
type ('result, 'model) analysis_data = < result: 'result; model: 'model >


(* Internal kind (stored in the shared heap). *)
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


type result_pkg = result pkg
type model_pkg = model pkg


type model_t = model_pkg Kind.Map.t
type result_t = result_pkg Kind.Map.t


module type ANALYZER = sig
  type result
  type call_model
  val analyze: Callable.real_target -> Define.t Node.t -> result * call_model

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


type abstract_analysis = Analysis : 'a analysis -> abstract_analysis


let analyses: abstract_analysis Kind.Map.t ref = ref Kind.Map.empty


module Register(Analysis : ANALYSIS) = struct
  let kind = Kind.cast Analysis.kind
  let () =
    let analysis = Analysis { kind; analysis = (module Analysis); } in
    let analysis_kind = Kind.register Analysis.kind ~name:Analysis.name
    in
    analyses := Kind.Map.add analysis_kind analysis !analyses

  let abstract_kind = Kind.abstract kind
end


module Make(Analysis : ANALYSIS_PROVIDED) = struct
  type _ Kind.kind +=
    | Kind: (Analysis.result, Analysis.call_model) analysis_data Kind.kind
  let kind = Kind

  module Register(
      Analyzer : ANALYZER
      with type result := Analysis.result
       and type call_model := Analysis.call_model
    ) = Register(struct
      include Analysis
      let kind = kind
      include Analyzer
    end)

  include Analysis
end


let get_analysis (type a b) (kind_to_find : (a,b) analysis_data Kind.storable_kind) =
  let abstract_kind = Kind.abstract kind_to_find in
  match Kind.Map.find_opt abstract_kind !analyses with
  | None -> failwith ("analysis kind does not exist: " ^ Kind.show abstract_kind)
  | Some (Analysis { kind; analysis; _ }) ->
      match Kind.are_equal kind kind_to_find with
      | Kind.Equal -> (analysis : (a,b) analysis_data analysis_module)
      | Kind.Distinct -> failwith ("analysis kind mismatch: " ^ Kind.show abstract_kind)


let get_abstract_analysis analysis_kind =
  match Kind.Map.find_opt analysis_kind !analyses with
  | None -> failwith ("analysis kind does not exist: " ^ Kind.show analysis_kind)
  | Some analysis -> analysis


(* A first class polymorphic function value for getting kinds from partial
   kinds 'a , producing values of type 'b.

   It is polymorphic over model and result, whereas the free 'a is either
   'model, or 'result based on the partial_kind. *)
type ('part, 'a, 'b) partial_kind_function = {
  f : 'model 'result. <model:'model; result:'result> storable_kind
    -> ('part, 'a) partial_kind -> 'b;
}


(* Equality for partial kinds. *)
let are_equal (type part a b) (a : (part, a) partial_kind) (b : (part, b) partial_kind)
  : (a,b) Kind.equality_witness =
  match a, b with
  | ModelPart k1, ModelPart k2 -> begin
      match Kind.are_equal k1 k2 with
      | Kind.Equal -> Kind.Equal  (* Necessary because the types of these Equal are different. *)
      | Kind.Distinct -> Kind.Distinct
    end
  | ResultPart k1, ResultPart k2 -> begin
      match Kind.are_equal k1 k2 with
      | Kind.Equal -> Kind.Equal
      | Kind.Distinct -> Kind.Distinct
    end
(* Note: no other cases are necessary, because statically, the 'part' makes sure
   that both kinds talk about the same part. *)


(* Unpacks a partial kind and applies the polymorphic function to it. *)
let apply_to_partial_kind
    (type part a b)
    (partial_kind : (part, _) partial_kind)
    (f : (part, a, b) partial_kind_function) =
  match partial_kind with
  | ModelPart k -> f.f k partial_kind
  | ResultPart k -> f.f k partial_kind


(* Polymorphic extractor for package values: given a map of 'part package values
   indexed by ikind, and given a 'part pkind, extracts the analysis specific 'part
   value in a generic way. *)
let get (type part a)
    (partial_kind : (part, a) partial_kind)
    (values : part pkg Kind.Map.t) =
  let get kind (partial_kind : (part, a) partial_kind) =
    match Kind.Map.find_opt (Kind.abstract kind) values, partial_kind with
    | None, _ -> None
    | Some (Pkg { kind = kind1; value; }), kind2 -> begin
        match are_equal kind1 kind2 with
        | Kind.Equal -> Some (value : a)
        | Kind.Distinct -> failwith "kind mismatch in results."
      end
  in
  apply_to_partial_kind partial_kind { f = get }


let get_model kind models =
  let kind = Kind.cast kind in
  get (ModelPart kind) models


let get_result kind results =
  let kind = Kind.cast kind in
  get (ResultPart kind) results


let empty_model = Kind.Map.empty
let empty_result = Kind.Map.empty


let with_model kind model models =
  let kind = Kind.cast kind in
  let model = Pkg { kind = ModelPart kind; value = model }
  in
  Kind.Map.add (Kind.abstract kind) model models
