(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Statement
module PreviousEnvironment = ClassHierarchyEnvironment

module UndecoratedFunctionValue = struct
  type t = Type.t Type.Callable.overload option [@@deriving compare]

  let prefix = Prefix.make ()

  let description = "Undecorated functions"

  let unmarshall value = Marshal.from_string value 0
end

let unannotated_global_environment alias_environment =
  AliasEnvironment.ReadOnly.unannotated_global_environment alias_environment


let produce_undecorated_function class_hierarchy_environment name ~dependency =
  let alias_environment =
    ClassHierarchyEnvironment.ReadOnly.alias_environment class_hierarchy_environment
  in
  let global =
    UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
      ?dependency
      (unannotated_global_environment alias_environment)
      name
  in
  let handle = function
    | UnannotatedGlobalEnvironment.Define signatures ->
        let handle { Node.value = signature; _ } =
          let parse_annotation =
            AliasEnvironment.ReadOnly.parse_annotation_without_validating_type_parameters
              ?dependency
              alias_environment
          in
          let parse_as_concatenation =
            AliasEnvironment.ReadOnly.parse_as_concatenation ?dependency alias_environment
          in
          let parse_as_parameter_specification_instance_annotation =
            AliasEnvironment.ReadOnly.parse_as_parameter_specification_instance_annotation
              ?dependency
              alias_environment
          in
          let parser =
            {
              AnnotatedCallable.parse_annotation;
              parse_as_concatenation;
              parse_as_parameter_specification_instance_annotation;
            }
          in
          let variables =
            ClassHierarchyEnvironment.ReadOnly.variables class_hierarchy_environment ?dependency
          in
          AnnotatedCallable.create_overload_without_applying_decorators ~parser ~variables signature
        in
        List.find signatures ~f:(fun signature ->
            not (Define.Signature.is_overloaded_function (Node.value signature)))
        >>| handle
    | _ -> None
  in
  global >>= handle


module UndecoratedFunctions = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = SharedMemoryKeys.ReferenceKey
  module Value = UndecoratedFunctionValue

  type trigger = Reference.t [@@deriving sexp, compare]

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  module TriggerSet = Reference.Set

  let lazy_incremental = false

  let produce_value = produce_undecorated_function

  let filter_upstream_dependency = function
    | SharedMemoryKeys.UndecoratedFunction name -> Some name
    | _ -> None


  let trigger_to_dependency name = SharedMemoryKeys.UndecoratedFunction name

  let legacy_invalidated_keys =
    UnannotatedGlobalEnvironment.UpdateResult.previous_unannotated_globals


  let all_keys = UnannotatedGlobalEnvironment.ReadOnly.all_unannotated_globals

  let serialize_value = function
    | Some overload -> Type.Callable.show_overload Type.pp overload
    | None -> "None"


  let show_key = Reference.show

  let equal_value = Option.equal (Type.Callable.equal_overload Type.equal)
end)

include UndecoratedFunctions

module ReadOnly = struct
  include UndecoratedFunctions.ReadOnly

  let get_undecorated_function = get

  let class_hierarchy_environment = upstream_environment
end

module UndecoratedReadOnly = ReadOnly
