(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Statement
module PreviousEnvironment = ClassMetadataEnvironment

let class_hierarchy_environment class_metadata_environment =
  ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment class_metadata_environment


let alias_environment environment =
  class_hierarchy_environment environment |> ClassHierarchyEnvironment.ReadOnly.alias_environment


let unannotated_global_environment environment =
  alias_environment environment |> AliasEnvironment.ReadOnly.unannotated_global_environment


module GlobalValue = struct
  type t = GlobalResolution.global option

  let prefix = Prefix.make ()

  let description = "Global"

  let unmarshall value = Marshal.from_string value 0

  let compare = Option.compare GlobalResolution.compare_global
end

let produce_global_annotation class_metadata_environment name ~track_dependencies =
  let dependency = Option.some_if track_dependencies (SharedMemoryKeys.AnnotateGlobal name) in
  let resolution =
    GlobalResolution.create
      ?dependency
      ~class_metadata_environment
      ~global:(fun _ -> None)
      (module Annotated.Class)
  in
  let produce_class_meta_annotation { Node.location; _ } =
    let primitive = Type.Primitive (Reference.show name) in
    Annotation.create_immutable ~global:true (Type.meta primitive) |> Node.create ~location
  in
  let process_unannotated_global global =
    let produce_assignment_global ~target_location ~is_explicit annotation =
      let original =
        if is_explicit then
          None
        else if
          (* Treat literal globals as having been explicitly annotated. *)
          Type.is_partially_typed annotation
        then
          Some Type.Top
        else
          None
      in
      Annotation.create_immutable ~global:true ~original annotation
      |> Node.create ~location:target_location
    in
    match global with
    | UnannotatedGlobalEnvironment.Define (head :: _ as defines) ->
        let create_overload
            { Node.location; Node.value = { Define.Signature.name; parent; _ } as signature }
          =
          let parent =
            if Define.Signature.is_class_method signature then
              parent >>| Reference.show >>| (fun name -> Type.Primitive name) >>| Type.meta
            else
              None
          in
          Node.create ~location signature
          |> ResolvedCallable.apply_decorators ~resolution
          |> (fun overload -> [Define.Signature.is_overloaded_function signature, overload])
          |> ResolvedCallable.create_callable ~resolution ~parent ~name:(Reference.show name)
        in
        List.map defines ~f:create_overload
        |> Type.Callable.from_overloads
        >>| (fun callable -> Type.Callable callable)
        >>| Annotation.create_immutable ~global:true
        >>| Node.create ~location:(Node.location head)
    | SimpleAssign { explicit_annotation; value; target_location } ->
        let explicit_annotation =
          explicit_annotation
          >>| GlobalResolution.parse_annotation resolution
          >>= fun annotation -> Option.some_if (not (Type.is_type_alias annotation)) annotation
        in
        let annotation =
          match explicit_annotation with
          | Some explicit -> explicit
          | None -> GlobalResolution.resolve_literal resolution value
        in
        produce_assignment_global
          ~target_location
          ~is_explicit:(Option.is_some explicit_annotation)
          annotation
        |> Option.some
    | TupleAssign { value; target_location; index; total_length } ->
        let extracted =
          match GlobalResolution.resolve_literal resolution value with
          | Type.Tuple (Type.Bounded (Concrete parameters))
            when List.length parameters = total_length ->
              List.nth parameters index
              (* This should always be Some, but I don't think its worth being fragile here *)
              |> Option.value ~default:Type.Top
          | Type.Tuple (Type.Unbounded parameter) -> parameter
          | _ -> Type.Top
        in
        produce_assignment_global ~target_location ~is_explicit:false extracted |> Option.some
    | _ -> None
  in
  let class_lookup =
    Reference.show name
    |> UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
         (unannotated_global_environment class_metadata_environment)
         ?dependency
  in
  match class_lookup with
  | Some retrieved_class -> produce_class_meta_annotation retrieved_class |> Option.some
  | None ->
      UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
        (unannotated_global_environment class_metadata_environment)
        ?dependency
        name
      >>= process_unannotated_global


module GlobalTable = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = SharedMemoryKeys.ReferenceKey
  module Value = GlobalValue

  type trigger = Reference.t

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  module TriggerSet = Reference.Set

  let produce_value = produce_global_annotation

  let filter_upstream_dependency = function
    | SharedMemoryKeys.AnnotateGlobal name -> Some name
    | _ -> None


  let legacy_invalidated_keys upstream =
    let previous_classes =
      UnannotatedGlobalEnvironment.UpdateResult.previous_classes upstream
      |> Type.Primitive.Set.to_list
      |> List.map ~f:Reference.create
    in
    let previous_unannotated_globals =
      UnannotatedGlobalEnvironment.UpdateResult.previous_unannotated_globals upstream
    in
    List.fold ~init:previous_unannotated_globals ~f:Set.add previous_classes


  let all_keys = UnannotatedGlobalEnvironment.ReadOnly.all_unannotated_globals

  let serialize_value = function
    | Some annotation -> Node.value annotation |> Annotation.sexp_of_t |> Sexp.to_string
    | None -> "None"


  let show_key = Reference.show

  let equal_value =
    Option.equal (fun first second -> Annotation.equal (Node.value first) (Node.value second))
end)

let update_this_and_all_preceding_environments
    ast_environment
    ~scheduler
    ~configuration
    ~ast_environment_update_result
    modified_qualifiers
  =
  GlobalResolution.AnnotationCache.clear ~scheduler ~configuration;
  GlobalTable.update_this_and_all_preceding_environments
    ast_environment
    ~scheduler
    ~configuration
    ~ast_environment_update_result
    modified_qualifiers


module ReadOnly = struct
  include GlobalTable.ReadOnly

  let get_global = get

  let class_metadata_environment = upstream_environment

  let resolution_implementation ?dependency environment =
    let class_metadata_environment = class_metadata_environment environment in
    GlobalResolution.create
      ?dependency
      ~class_metadata_environment
      ~global:(get_global environment ?dependency)
      (module AnnotatedClass)


  let resolution = resolution_implementation ?dependency:None

  let dependency_tracked_resolution environment ~dependency =
    resolution_implementation ~dependency environment


  let ast_environment environment =
    class_metadata_environment environment
    |> ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
    |> ClassHierarchyEnvironment.ReadOnly.alias_environment
    |> AliasEnvironment.ReadOnly.unannotated_global_environment
    |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment
end

module UpdateResult = GlobalTable.UpdateResult
module AnnotatedReadOnly = ReadOnly
