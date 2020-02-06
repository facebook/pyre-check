(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
open Statement
module PreviousEnvironment = AttributeResolution

type global = Annotation.t Node.t [@@deriving eq, show, compare, sexp]

let class_hierarchy_environment class_metadata_environment =
  ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment class_metadata_environment


let alias_environment environment =
  class_hierarchy_environment environment |> ClassHierarchyEnvironment.ReadOnly.alias_environment


let unannotated_global_environment environment =
  alias_environment environment |> AliasEnvironment.ReadOnly.unannotated_global_environment


module GlobalValue = struct
  type t = global option

  let prefix = Prefix.make ()

  let description = "Global"

  let unmarshall value = Marshal.from_string value 0

  let compare = Option.compare compare_global
end

let produce_global_annotation attribute_resolution name ~track_dependencies =
  let class_metadata_environment =
    AttributeResolution.ReadOnly.class_metadata_environment attribute_resolution
  in
  let dependency = Option.some_if track_dependencies (SharedMemoryKeys.AnnotateGlobal name) in
  let produce_class_meta_annotation { Node.location; _ } =
    let primitive = Type.Primitive (Reference.show name) in
    Annotation.create_immutable (Type.meta primitive) |> Node.create ~location
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
      Annotation.create_immutable ~original annotation |> Node.create ~location:target_location
    in
    match global with
    | UnannotatedGlobalEnvironment.Define (head :: _ as defines) ->
        let create_overload
            { Node.value = { Define.Signature.name = { Node.value = name; _ }; _ } as signature; _ }
          =
          let overload =
            AttributeResolution.ReadOnly.create_overload attribute_resolution ?dependency signature
          in
          if Define.Signature.is_overloaded_function signature then
            {
              Type.Callable.kind = Named name;
              implementation = { annotation = Type.Top; parameters = Undefined };
              overloads = [overload];
              implicit = None;
            }
          else
            {
              Type.Callable.kind = Named name;
              implementation = overload;
              overloads = [];
              implicit = None;
            }
        in

        List.map defines ~f:create_overload
        |> Type.Callable.from_overloads
        >>| (fun callable -> Type.Callable callable)
        >>| Annotation.create_immutable
        >>| Node.create ~location:(Node.location head)
    | SimpleAssign
        {
          explicit_annotation = None;
          value =
            {
              Node.value =
                Call
                  {
                    callee =
                      {
                        value =
                          Name
                            (Attribute
                              {
                                base = { Node.value = Name (Identifier "typing"); _ };
                                attribute = "TypeAlias";
                                _;
                              });
                        _;
                      };
                    _;
                  };
              _;
            };
          target_location = location;
        } ->
        Ast.Expression.Expression.Name (Expression.create_name_from_reference ~location name)
        |> Node.create ~location
        |> AttributeResolution.ReadOnly.parse_annotation
             ~allow_invalid_type_parameters:true
             ?dependency
             attribute_resolution
        |> Type.meta
        |> Annotation.create_immutable
        |> Node.create ~location
        |> Option.some
    | SimpleAssign { explicit_annotation; value; target_location } ->
        let explicit_annotation =
          explicit_annotation
          >>| AttributeResolution.ReadOnly.parse_annotation ?dependency attribute_resolution
          >>= fun annotation -> Option.some_if (not (Type.is_type_alias annotation)) annotation
        in
        let annotation =
          match explicit_annotation with
          | Some explicit -> explicit
          | None ->
              AttributeResolution.ReadOnly.resolve_literal ?dependency attribute_resolution value
        in
        produce_assignment_global
          ~target_location
          ~is_explicit:(Option.is_some explicit_annotation)
          annotation
        |> Option.some
    | TupleAssign { value; target_location; index; total_length } ->
        let extracted =
          match
            AttributeResolution.ReadOnly.resolve_literal ?dependency attribute_resolution value
          with
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
      >>= fun global ->
      let timer = Timer.start () in
      let result = process_unannotated_global global in
      Statistics.performance
        ~flush:false
        ~randomly_log_every:500
        ~section:`Check
        ~name:"SingleGlobalTypeCheck"
        ~timer
        ~normals:["name", Reference.show name; "request kind", "SingleGlobalTypeCheck"]
        ();
      result


module GlobalTable = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = SharedMemoryKeys.ReferenceKey
  module Value = GlobalValue

  type trigger = Reference.t

  let convert_trigger = Fn.id

  let key_to_trigger = Fn.id

  module TriggerSet = Reference.Set

  let lazy_incremental = false

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

include GlobalTable

module ReadOnly = struct
  include GlobalTable.ReadOnly

  let get_global = get

  let attribute_resolution = upstream_environment

  let class_metadata_environment read_only =
    attribute_resolution read_only |> AttributeResolution.ReadOnly.class_metadata_environment


  let ast_environment environment =
    class_metadata_environment environment
    |> ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
    |> ClassHierarchyEnvironment.ReadOnly.alias_environment
    |> AliasEnvironment.ReadOnly.unannotated_global_environment
    |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment
end

module UpdateResult = GlobalTable.UpdateResult
module AnnotatedReadOnly = ReadOnly
