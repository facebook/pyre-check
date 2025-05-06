(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* CallResolution: utility functions to resolve types, ignoring untracked types
 * (i.e, when annotation refer to undefined symbols).
 *)

open Core
open Ast
open Expression
open Pyre
module PyrePysaEnvironment = Analysis.PyrePysaEnvironment
module PyrePysaLogic = Analysis.PyrePysaLogic

(* Check whether `successor` extends `predecessor`.
 * Returns false on untracked types.
 * Returns `reflexive` if `predecessor` and `successor` are equal. *)
let has_transitive_successor_ignoring_untracked ~pyre_api ~reflexive ~predecessor ~successor =
  if String.equal predecessor successor then
    reflexive
  else
    try PyrePysaEnvironment.ReadOnly.has_transitive_successor pyre_api ~successor predecessor with
    | PyrePysaLogic.UntrackedClass untracked_type ->
        Log.warning
          "Found untracked type `%s` when checking whether `%s` is a subclass of `%s`. This could \
           lead to false negatives."
          untracked_type
          successor
          predecessor;
        false


(* Evaluates to whether the provided expression is a superclass of define. *)
let is_super ~pyre_in_context ~define expression =
  match expression.Node.value with
  | Expression.Call { callee = { Node.value = Name (Name.Identifier "super"); _ }; _ } -> true
  | _ ->
      (* We also support explicit calls to superclass constructors. *)
      let annotation =
        PyrePysaEnvironment.InContext.resolve_expression_to_type pyre_in_context expression
      in
      if Type.is_class_type annotation then
        let type_parameter = Type.single_argument annotation in
        match type_parameter with
        | Type.Parametric { name = parent_name; _ }
        | Type.Primitive parent_name ->
            let class_name =
              Reference.prefix define.Node.value.Statement.Define.signature.name >>| Reference.show
            in
            class_name
            >>| (fun class_name ->
                  has_transitive_successor_ignoring_untracked
                    ~pyre_api:(PyrePysaEnvironment.InContext.pyre_api pyre_in_context)
                    ~reflexive:false
                    ~predecessor:class_name
                    ~successor:parent_name)
            |> Option.value ~default:false
        | _ -> false
      else
        false


(* A nonlocal variable is neither global nor local *)
let is_nonlocal ~pyre_in_context ~define variable =
  let define_list = Reference.as_list (Reference.delocalize define) in
  let variable_list = Reference.as_list (Reference.delocalize variable) in
  let is_prefix = List.is_prefix ~equal:String.equal ~prefix:define_list variable_list in
  let is_global = PyrePysaEnvironment.InContext.is_global pyre_in_context ~reference:variable in
  let is_nonlocal = (not is_prefix) && (not is_global) && Reference.is_local variable in
  is_nonlocal


(* Resolve an expression into a type. Untracked types are resolved into `Any`. *)
let resolve_ignoring_untracked ~pyre_in_context expression =
  try PyrePysaEnvironment.InContext.resolve_expression_to_type pyre_in_context expression with
  | PyrePysaLogic.UntrackedClass untracked_type ->
      Log.warning
        "Found untracked type `%s` when resolving the type of `%a`. This could lead to false \
         negatives."
        untracked_type
        Expression.pp
        (Ast.Expression.delocalize ~create_origin:(fun _ -> None) expression);
      Type.Any


(* Resolve an attribute access into a type. Untracked types are resolved into `Any`. *)
let resolve_attribute_access_ignoring_untracked ~pyre_in_context ~base_type ~attribute =
  try
    PyrePysaEnvironment.InContext.resolve_attribute_access pyre_in_context ~base_type ~attribute
  with
  | PyrePysaLogic.UntrackedClass untracked_type ->
      Log.warning
        "Found untracked type `%s` when resolving the type of attribute `%s` in `%a`. This could \
         lead to false negatives."
        untracked_type
        attribute
        Type.pp
        base_type;
      Type.Any


let defining_attribute ~pyre_in_context type_for_lookup attribute =
  Type.split type_for_lookup
  |> fst
  |> Type.primitive_name
  >>= fun class_name ->
  let instantiated_attribute =
    PyrePysaEnvironment.ReadOnly.attribute_from_class_name
      (PyrePysaEnvironment.InContext.pyre_api pyre_in_context)
      ~transitive:true
      ~name:attribute
      ~type_for_lookup
      class_name
  in
  instantiated_attribute
  >>= fun instantiated_attribute ->
  if PyrePysaLogic.AnnotatedAttribute.defined instantiated_attribute then
    Some instantiated_attribute
  else
    PyrePysaEnvironment.InContext.fallback_attribute pyre_in_context ~name:attribute class_name


let strip_optional annotation =
  annotation |> Type.optional_value |> Option.value ~default:annotation


(* Convert `TypeVar["X", bound="Y"]` to `Y` *)
let unbind_type_variable = function
  | Type.Variable { constraints = Type.Record.TypeVarConstraints.Bound bound; _ } -> bound
  | annotation -> annotation


(* Convert `ReadOnly[X]` back to just `X` *)
let strip_readonly annotation =
  if Type.PyreReadOnly.is_readonly annotation then
    Type.PyreReadOnly.strip_readonly annotation
  else
    annotation


let extract_coroutine_value annotation =
  annotation |> Type.coroutine_value |> Option.value ~default:annotation


(* Resolve an expression into a type, ignoring
 * errors related to accessing `None`, `ReadOnly`, and bound `TypeVar`s. *)
let rec resolve_ignoring_errors ~pyre_in_context ~callables_to_definitions_map expression =
  let resolve_expression_to_type expression =
    match resolve_ignoring_untracked ~pyre_in_context expression, Node.value expression with
    | ( (Type.Callable ({ Type.Callable.kind = Anonymous; _ } as callable) as annotation),
        Expression.Name (Name.Identifier function_name) )
      when function_name |> String.is_prefix ~prefix:"$local_" ->
        let function_name = Reference.create function_name in
        if
          function_name
          |> Reference.delocalize
          |> Target.create_function
          |> Target.CallablesSharedMemory.ReadOnly.mem callables_to_definitions_map
        then
          (* Treat nested functions as named callables, only if `function_name` refers to functions
             / methods that have bodies. *)
          Type.Callable { callable with kind = Named function_name }
        else
          annotation
    | annotation, _ -> annotation
  in
  let annotation =
    match Node.value expression with
    | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
        let base_type =
          resolve_ignoring_errors ~pyre_in_context ~callables_to_definitions_map base
          |> fun annotation -> Type.optional_value annotation |> Option.value ~default:annotation
        in
        match defining_attribute ~pyre_in_context base_type attribute with
        | Some _ ->
            resolve_attribute_access_ignoring_untracked ~pyre_in_context ~base_type ~attribute
        | None -> resolve_expression_to_type expression
        (* Lookup the base_type for the attribute you were interested in *))
    | _ -> resolve_expression_to_type expression
  in
  annotation |> strip_optional |> strip_readonly |> unbind_type_variable
