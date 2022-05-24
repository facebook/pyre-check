(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Expression
open Pyre

let extract_constant_name { Node.value = expression; _ } =
  match expression with
  | Expression.Constant (Constant.String literal) -> Some literal.value
  | Expression.Constant (Constant.Integer i) -> Some (string_of_int i)
  | Expression.Constant Constant.False -> Some "False"
  | Expression.Constant Constant.True -> Some "True"
  | Expression.Name name -> (
      let name = name_to_reference name >>| Reference.delocalize >>| Reference.last in
      match name with
      (* Heuristic: All uppercase names tend to be enums, so only taint the field in those cases. *)
      | Some name
        when String.for_all name ~f:(fun character ->
                 (not (Char.is_alpha character)) || Char.is_uppercase character) ->
          Some name
      | _ -> None)
  | _ -> None


let is_transitive_successor_ignoring_untracked global_resolution ~predecessor ~successor =
  try GlobalResolution.is_transitive_successor global_resolution ~predecessor ~successor with
  | Analysis.ClassHierarchy.Untracked untracked_type ->
      Log.warning
        "Found untracked type `%s` when checking whether `%s` is a subclass of `%s`. This could \
         lead to false negatives."
        untracked_type
        successor
        predecessor;
      false


let is_super ~resolution ~define expression =
  match expression.Node.value with
  | Expression.Call { callee = { Node.value = Name (Name.Identifier "super"); _ }; _ } -> true
  | _ ->
      (* We also support explicit calls to superclass constructors. *)
      let annotation = Resolution.resolve_expression_to_type resolution expression in
      if Type.is_meta annotation then
        let type_parameter = Type.single_parameter annotation in
        match type_parameter with
        | Type.Parametric { name = parent_name; _ }
        | Type.Primitive parent_name ->
            let class_name =
              Reference.prefix define.Node.value.Statement.Define.signature.name >>| Reference.show
            in
            class_name
            >>| (fun class_name ->
                  is_transitive_successor_ignoring_untracked
                    (Resolution.global_resolution resolution)
                    ~predecessor:class_name
                    ~successor:parent_name)
            |> Option.value ~default:false
        | _ -> false
      else
        false


let resolve_ignoring_untracked ~resolution expression =
  try Resolution.resolve_expression_to_type resolution expression with
  | Analysis.ClassHierarchy.Untracked untracked_type ->
      Log.warning
        "Found untracked type `%s` when resolving the type of `%a`. This could lead to false \
         negatives."
        untracked_type
        Expression.pp
        (Ast.Expression.delocalize expression);
      Type.Any


let resolve_attribute_access_ignoring_untracked ~resolution ~base_type ~attribute =
  try Resolution.resolve_attribute_access resolution ~base_type ~attribute with
  | Analysis.ClassHierarchy.Untracked untracked_type ->
      Log.warning
        "Found untracked type `%s` when resolving the type of attribute `%s` in `%a`. This could \
         lead to false negatives."
        untracked_type
        attribute
        Type.pp
        base_type;
      Type.Any


let defining_attribute ~resolution parent_type attribute =
  let global_resolution = Resolution.global_resolution resolution in
  Type.split parent_type
  |> fst
  |> Type.primitive_name
  >>= fun class_name ->
  let instantiated_attribute =
    GlobalResolution.attribute_from_class_name
      ~transitive:true
      ~resolution:global_resolution
      ~name:attribute
      ~instantiated:parent_type
      class_name
  in
  instantiated_attribute
  >>= fun instantiated_attribute ->
  if Annotated.Attribute.defined instantiated_attribute then
    Some instantiated_attribute
  else
    Resolution.fallback_attribute ~resolution ~name:attribute class_name


let rec resolve_ignoring_optional ~resolution expression =
  let resolve_expression_to_type expression =
    match resolve_ignoring_untracked ~resolution expression, Node.value expression with
    | ( Type.Callable ({ Type.Callable.kind = Anonymous; _ } as callable),
        Expression.Name (Name.Identifier function_name) )
      when function_name |> String.is_prefix ~prefix:"$local_" ->
        (* Treat nested functions as named callables. *)
        Type.Callable { callable with kind = Named (Reference.create function_name) }
    | annotation, _ -> annotation
  in
  let annotation =
    match Node.value expression with
    | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
        let base_type =
          resolve_ignoring_optional ~resolution base
          |> fun annotation -> Type.optional_value annotation |> Option.value ~default:annotation
        in
        match defining_attribute ~resolution base_type attribute with
        | Some _ -> resolve_attribute_access_ignoring_untracked ~resolution ~base_type ~attribute
        | None -> resolve_expression_to_type expression
        (* Lookup the base_type for the attribute you were interested in *))
    | _ -> resolve_expression_to_type expression
  in
  Type.optional_value annotation |> Option.value ~default:annotation
