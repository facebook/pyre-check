(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ClassHierarchyEnvironment: layer of the environment stack
 * - upstream: AliasEnvironment
 * - downstream: ClassMetadataEnvironment
 * - key: the name type, as an IndexTracker.Value
 * - value: ClassHierarchy.Target.t
 *
 * The ClassHierarchyEnvironment tracks the direct parents
 * of types.
 *
 * It is keyed on IndexTracker.Value, which is an int value
 * that "interns" class names to make lookups cheaper; we want
 * fast lookups because we have to do a graph search when computing
 * all ancestors of classes in ClassMetadataEnvironment.
 *)

open Core
open Ast
open Pyre
open Expression
module PreviousEnvironment = AliasEnvironment

let unannotated_global_environment alias_environment =
  AliasEnvironment.ReadOnly.unannotated_global_environment alias_environment


let empty_stub_environment alias_environment =
  AliasEnvironment.ReadOnly.empty_stub_environment alias_environment


type edges = {
  parents: ClassHierarchy.Target.t list;
  has_placeholder_stub_parent: bool;
}
[@@deriving compare]

module EdgesValue = struct
  type t = edges option

  let prefix = Hack_parallel.Std.Prefix.make ()

  let description = "Edges"

  let equal = Memory.equal_from_compare (Option.compare compare_edges)
end

(* Note: We want to preserve order when deduplicating, so we can't use `List.dedup_and_sort`. This
   is quadratic, but it should be fine given the small number of generic variables. *)
let deduplicate ~equal xs =
  let add_if_not_seen_so_far (seen, unique_items) x =
    if List.mem seen x ~equal then
      seen, unique_items
    else
      x :: seen, x :: unique_items
  in
  List.fold xs ~init:([], []) ~f:add_if_not_seen_so_far |> snd |> List.rev


let find_propagated_type_variables bases ~parse_annotation =
  let find_type_variables base_expression =
    parse_annotation base_expression |> Type.Variable.all_free_variables
  in
  List.concat_map ~f:find_type_variables bases
  |> deduplicate ~equal:Type.Variable.equal
  |> List.map ~f:Type.Variable.to_parameter


let compute_inferred_generic_base
    { Node.value = { ClassSummary.bases = { base_classes; metaclass; _ }; _ }; _ }
    ~parse_annotation
  =
  let bases =
    match metaclass with
    | Some metaclass -> metaclass :: base_classes
    | _ -> base_classes
  in
  let is_generic base_expression =
    let primitive, _ = parse_annotation base_expression |> Type.split in
    Type.is_generic_primitive primitive
  in
  let extract_protocol_parameters base_expression =
    let primitive, parameters = parse_annotation base_expression |> Type.split in
    let is_protocol =
      primitive
      |> Type.primitive_name
      >>| String.equal "typing.Protocol"
      |> Option.value ~default:false
    in
    Option.some_if is_protocol parameters
  in
  if List.exists ~f:is_generic bases then
    []
  else
    let create variables = [Type.parametric "typing.Generic" variables |> Type.expression] in
    match List.find_map bases ~f:extract_protocol_parameters with
    | Some parameters -> create parameters
    | None ->
        (* TODO:(T60673574) Ban propagating multiple type variables *)
        let variables = find_propagated_type_variables bases ~parse_annotation in
        if List.is_empty variables then
          []
        else
          create variables


let get_parents alias_environment name ~dependency =
  let object_index = IndexTracker.index "object" in
  let parse_annotation =
    AliasEnvironment.ReadOnly.parse_annotation_without_validating_type_parameters
      ?dependency
      alias_environment
  in
  (* Register normal annotations. *)
  let extract_supertype base_expression =
    let value = delocalize base_expression in
    match Node.value value with
    | Call _
    | Name _ -> (
        let supertype, parameters = parse_annotation ~allow_untracked:true value |> Type.split in
        match supertype with
        | Type.Top ->
            Log.log ~section:`Environment "Unresolved superclass name: %a" Expression.pp value;
            ();
            None
        | Type.Primitive primitive
          when not
                 (UnannotatedGlobalEnvironment.ReadOnly.class_exists
                    ?dependency
                    (unannotated_global_environment alias_environment)
                    primitive) ->
            Log.log ~section:`Environment "Superclass annotation %a is missing" Type.pp supertype;
            None
        | Type.Primitive supertype -> Some (supertype, parameters)
        | _ -> None)
    | _ -> None
  in
  let bases ({ Node.value = { ClassSummary.bases = { base_classes; _ }; _ }; _ } as definition) =
    let inferred_generic_base = compute_inferred_generic_base definition ~parse_annotation in
    base_classes @ inferred_generic_base
  in
  let add_special_parents parents =
    let simples = List.map ~f:(fun parent -> parent, []) in
    match parents, name with
    | _, "int" -> simples ["float"; "numbers.Integral"]
    | _, "float" -> simples ["complex"; "numbers.Rational"; "numbers.Real"]
    | _, "complex" -> simples ["numbers.Complex"]
    | _, "numbers.Complex" -> simples ["numbers.Number"]
    | [], _ -> simples ["object"]
    | _ -> parents
  in
  let is_not_primitive_cycle (parent, _) = not (String.equal name parent) in
  let convert_to_targets =
    List.map ~f:(fun (name, parameters) ->
        { ClassHierarchy.Target.target = IndexTracker.index name; parameters })
  in
  let deduplicate targets =
    let deduplicate (visited, sofar) ({ ClassHierarchy.Target.target; _ } as edge) =
      if Set.mem visited target then
        visited, sofar
      else
        Set.add visited target, edge :: sofar
    in
    List.fold targets ~f:deduplicate ~init:(IndexTracker.Set.empty, []) |> snd |> List.rev
  in
  let remove_extra_edges_to_object targets =
    let not_object_edge { ClassHierarchy.Target.target; _ } =
      not ([%compare.equal: IndexTracker.t] target object_index)
    in
    match List.filter targets ~f:not_object_edge with
    | [] -> targets
    | filtered -> filtered
  in
  match
    UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
      ?dependency
      (unannotated_global_environment alias_environment)
      name
  with
  | None -> None
  | Some class_summary ->
      let parents =
        class_summary
        |> bases
        |> List.filter_map ~f:extract_supertype
        |> add_special_parents
        |> List.filter ~f:is_not_primitive_cycle
        |> convert_to_targets
        |> deduplicate
        |> remove_extra_edges_to_object
      in
      let has_placeholder_stub_parent =
        AnnotatedBases.extends_placeholder_stub_class
          class_summary
          ~aliases:(AliasEnvironment.ReadOnly.get_alias alias_environment ?dependency)
          ~from_empty_stub:
            (EmptyStubEnvironment.ReadOnly.from_empty_stub
               (empty_stub_environment alias_environment))
      in
      Some { parents; has_placeholder_stub_parent }


module Edges = Environment.EnvironmentTable.WithCache (struct
  module PreviousEnvironment = PreviousEnvironment
  module Key = IndexTracker.IndexKey
  module Value = EdgesValue

  type trigger = string [@@deriving sexp, compare]

  let convert_trigger = IndexTracker.index

  let key_to_trigger = IndexTracker.annotation

  module TriggerSet = Type.Primitive.Set

  let lazy_incremental = false

  let produce_value = get_parents

  let filter_upstream_dependency = function
    | SharedMemoryKeys.ClassConnect name -> Some name
    | _ -> None


  let trigger_to_dependency name = SharedMemoryKeys.ClassConnect name

  let show_key = IndexTracker.annotation

  let overlay_owns_key module_tracker_overlay index =
    key_to_trigger index |> ModuleTracker.Overlay.owns_identifier module_tracker_overlay


  let equal_value = Option.equal [%compare.equal: edges]
end)

module ReadOnly = struct
  include Edges.ReadOnly

  let alias_environment = upstream_environment

  let get_edges read_only ?dependency key =
    get read_only ?dependency key >>| fun { parents; _ } -> parents


  let extends_placeholder_stub read_only ?dependency key =
    get read_only ?dependency key
    >>| (fun { has_placeholder_stub_parent; _ } -> has_placeholder_stub_parent)
    |> Option.value ~default:false


  let check_integrity read_only =
    let unannotated_global_environment =
      alias_environment read_only |> AliasEnvironment.ReadOnly.unannotated_global_environment
    in
    let indices =
      unannotated_global_environment |> UnannotatedGlobalEnvironment.ReadOnly.all_indices
    in
    let class_hierarchy =
      (module struct
        let edges = get_edges read_only ?dependency:None

        let extends_placeholder_stub = extends_placeholder_stub read_only ?dependency:None

        let contains key =
          UnannotatedGlobalEnvironment.ReadOnly.get_class_summary unannotated_global_environment key
          |> Option.is_some
      end : ClassHierarchy.Handler)
    in
    ClassHierarchy.check_integrity class_hierarchy ~indices


  let class_hierarchy ?dependency read_only =
    let alias_environment = alias_environment read_only in
    let unannotated_global_environment =
      AliasEnvironment.ReadOnly.unannotated_global_environment alias_environment
    in
    (module struct
      let edges = get_edges read_only ?dependency

      let extends_placeholder_stub = extends_placeholder_stub read_only ?dependency

      let contains key =
        let env_controls = AliasEnvironment.ReadOnly.controls alias_environment in
        if EnvironmentControls.no_validation_on_class_lookup_failure env_controls then
          true
        else
          UnannotatedGlobalEnvironment.ReadOnly.class_exists
            unannotated_global_environment
            ?dependency
            key
    end : ClassHierarchy.Handler)


  let variables ?(default = None) read_only ?dependency class_name =
    ClassHierarchy.variables ~default (class_hierarchy ?dependency read_only) class_name
end

type t = { edges: Edges.t }

let create controls = { edges = Edges.create controls }

let store { edges } = Edges.store edges

let load controls = { edges = Edges.load controls }

let ast_environment { edges } = Edges.ast_environment edges

let read_only { edges } = Edges.read_only edges

let update_this_and_all_preceding_environments
    ({ edges } as this_environment)
    ~scheduler
    ast_environment_trigger
  =
  let result =
    Edges.update_this_and_all_preceding_environments edges ~scheduler ast_environment_trigger
  in
  let read_only = read_only this_environment in
  let controls = ReadOnly.controls read_only in
  if
    EnvironmentControls.debug controls
    && not (EnvironmentControls.use_lazy_module_tracking controls)
  then
    ReadOnly.check_integrity read_only;
  result


module HierarchyReadOnly = ReadOnly
module UpdateResult = Edges.UpdateResult
module Overlay = Edges.Overlay
module Testing = Edges.Testing
