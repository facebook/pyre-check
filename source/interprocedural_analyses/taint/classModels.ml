(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ClassModels: infers a set of models for methods of a given class.
 *
 * For instance, this defines the behavior of `NamedTuple` and dataclasses,
 * rather than trying to infer the behavior from the actual implementation,
 * which is highly dynamic and not well suited for static analysis (e.g,
 * `NamedTuple` uses `eval`:
 * https://github.com/python/cpython/blob/7029c1a1c5b864056aa00298b1d0e0269f073f99/Lib/collections/__init__.py#L441).
 *)

open Core
open Pyre
open Ast
open Interprocedural
open Domains
module PyrePysaApi = Interprocedural.PyrePysaApi
module PyrePysaLogic = Analysis.PyrePysaLogic

module FeatureSet = struct
  type t = {
    breadcrumbs: Features.BreadcrumbMayAlwaysSet.t;
    via_features: Features.ViaFeatureSet.t;
  }

  let empty =
    {
      breadcrumbs = Features.BreadcrumbMayAlwaysSet.bottom;
      via_features = Features.ViaFeatureSet.bottom;
    }


  let from_taint taint =
    {
      breadcrumbs = BackwardState.Tree.joined_breadcrumbs taint;
      via_features =
        BackwardState.Tree.fold
          Features.ViaFeatureSet.Self
          ~f:Features.ViaFeatureSet.join
          ~init:Features.ViaFeatureSet.bottom
          taint;
    }
end

let infer ~scheduler ~scheduler_policies ~pyre_api ~user_models =
  let step_logger =
    StepLogger.start
      ~start_message:"Computing inferred models"
      ~end_message:"Computed inferred models"
      ()
  in
  (* Translate ViaXXX features on attributes to ViaXX features on callables. *)
  let translate_via_features_on_attribute attribute root tree =
    let expand_via_feature via_feature taint =
      match via_feature with
      | Features.ViaFeature.ViaAttributeName { tag } ->
          BackwardTaint.add_local_breadcrumb
            (Features.Breadcrumb.ViaAttributeName { tag; value = attribute }
            |> Features.BreadcrumbInterned.intern)
            taint
      | Features.ViaFeature.ViaValueOf { tag; _ } ->
          BackwardTaint.transform
            Features.ViaFeatureSet.Element
            Add
            ~f:(Features.ViaFeature.ViaValueOf { parameter = root; tag })
            taint
      | Features.ViaFeature.ViaTypeOf { tag; _ } ->
          BackwardTaint.transform
            Features.ViaFeatureSet.Element
            Add
            ~f:(Features.ViaFeature.ViaTypeOf { parameter = root; tag })
            taint
    in
    let transform taint =
      let via_features = BackwardTaint.via_features taint in
      let taint =
        BackwardTaint.transform
          Features.ViaFeatureSet.Self
          Map
          ~f:(fun _ -> Features.ViaFeatureSet.bottom)
          taint
      in
      Features.ViaFeatureSet.fold
        Features.ViaFeatureSet.Element
        ~f:expand_via_feature
        ~init:taint
        via_features
    in
    BackwardState.Tree.transform BackwardTaint.Self Map ~f:transform tree
  in
  let get_attribute_model class_name attribute =
    Reference.create ~prefix:(Reference.create class_name) attribute
    |> Target.create_object
    |> SharedModels.ReadOnly.get user_models ~cache:false
  in
  let get_attribute_tito_features class_name attribute root =
    match get_attribute_model class_name attribute with
    | Some { Model.backward = { taint_in_taint_out; _ }; _ } ->
        BackwardState.read ~root:GlobalModel.global_root ~path:[] taint_in_taint_out
        |> translate_via_features_on_attribute attribute root
        |> FeatureSet.from_taint
    | None -> FeatureSet.empty
  in
  let add_tito
      ~input_root
      ~input_path
      ~output_root
      ~output_path
      ~collapse_depth
      ~breadcrumbs
      ~via_features
      existing_state
    =
    let leaf =
      BackwardTaint.singleton (CallInfo.tito ()) output_root Frame.initial
      |> BackwardState.Tree.create_leaf
      |> BackwardState.Tree.transform Features.ReturnAccessPathTree.Self Map ~f:(fun _ ->
             Features.ReturnAccessPathTree.create
               [Part (Features.ReturnAccessPathTree.Path, (output_path, collapse_depth))])
      |> BackwardState.Tree.add_local_breadcrumbs breadcrumbs
      |> BackwardState.Tree.add_via_features via_features
    in
    BackwardState.assign ~root:input_root ~path:input_path leaf existing_state
  in
  let add_parameter_to_self_attribute_tito ~class_name ~positional position existing_state attribute
    =
    let input_root =
      if positional then
        AccessPath.Root.PositionalParameter { position; name = attribute; positional_only = false }
      else
        AccessPath.Root.NamedParameter { name = attribute }
    in
    let { FeatureSet.breadcrumbs; via_features } =
      get_attribute_tito_features class_name attribute input_root
    in
    let self =
      AccessPath.Root.PositionalParameter { position = 0; name = "self"; positional_only = false }
    in
    add_tito
      ~input_root
      ~input_path:[]
      ~output_root:(Sinks.ParameterUpdate self)
      ~output_path:[Abstract.TreeDomain.Label.create_name_index attribute]
      ~collapse_depth:Features.CollapseDepth.no_collapse
      ~breadcrumbs
      ~via_features
      existing_state
  in
  let add_sink_from_attribute_model ~class_name ~positional position existing_state attribute =
    match get_attribute_model class_name attribute with
    | Some { Model.backward = { sink_taint; _ }; _ } ->
        let root =
          if positional then
            AccessPath.Root.PositionalParameter
              { position; name = attribute; positional_only = false }
          else
            AccessPath.Root.NamedParameter { name = attribute }
        in
        let { FeatureSet.breadcrumbs; via_features } =
          get_attribute_tito_features class_name attribute root
        in
        let taint =
          BackwardState.read ~root:GlobalModel.global_root ~path:[] sink_taint
          |> translate_via_features_on_attribute attribute root
          |> BackwardState.Tree.add_local_breadcrumbs breadcrumbs
          |> BackwardState.Tree.add_via_features via_features
        in
        BackwardState.assign ~weak:true ~root ~path:[] taint existing_state
    | None -> existing_state
  in
  let get_attributes_in_alphabetical_order class_name =
    PyrePysaApi.ReadOnly.get_class_summary pyre_api class_name
    >>| Node.value
    >>| PyrePysaLogic.ClassSummary.attributes ~include_generated_attributes:false ~in_test:false
    |> Option.value ~default:Identifier.SerializableMap.empty
  in
  let has_attribute class_name name =
    let attributes = get_attributes_in_alphabetical_order class_name in
    Identifier.SerializableMap.mem name attributes
  in
  let get_attributes_in_declaration_order class_name =
    let compare_by_location left right =
      Ast.Location.compare (Node.location left) (Node.location right)
    in
    get_attributes_in_alphabetical_order class_name
    |> Identifier.SerializableMap.bindings
    |> List.unzip
    |> snd
    |> List.sort ~compare:compare_by_location
  in

  let compute_dataclass_models class_name =
    let attributes =
      get_attributes_in_declaration_order class_name
      |> List.map ~f:(fun { Node.value = { PyrePysaLogic.ClassSummary.Attribute.name; _ }; _ } ->
             name)
    in
    let taint_in_taint_out =
      List.foldi
        ~f:(fun position ->
          add_parameter_to_self_attribute_tito ~class_name ~positional:true (position + 1))
        ~init:BackwardState.empty
        attributes
    in
    let sink_taint =
      List.foldi attributes ~init:BackwardState.empty ~f:(fun position ->
          add_sink_from_attribute_model ~class_name ~positional:true (position + 1))
    in
    [
      ( Target.create_method (Reference.create class_name) "__init__",
        { Model.empty_model with backward = { Model.Backward.taint_in_taint_out; sink_taint } } );
    ]
  in
  (* We always generate a special `_fields` attribute for NamedTuples which is a tuple containing
     field names. *)
  let compute_named_tuple_models class_name =
    (* If a user-specified __new__ exist, don't override it. *)
    if has_attribute class_name "__new__" then
      []
    else
      (* Should not omit this model. Otherwise the mode is "obscure", thus leading to a tito model,
         which joins the taint on every element of the tuple. *)
      [Target.create_method (Reference.create class_name) "__new__", Model.empty_model]
  in
  let compute_typed_dict_models class_name =
    let fields =
      PyrePysaApi.ReadOnly.typed_dictionary_field_names pyre_api (Type.Primitive class_name)
    in
    let self =
      AccessPath.Root.PositionalParameter { position = 0; name = "self"; positional_only = false }
    in
    let taint_in_taint_out =
      List.foldi
        ~f:(add_parameter_to_self_attribute_tito ~class_name ~positional:false)
        ~init:BackwardState.empty
        fields
      (* `TypedDict.__init__ also accepts iterables and **kwargs. *)
      |> add_tito
           ~input_root:
             (AccessPath.Root.PositionalParameter
                { position = 1; name = "__iterable"; positional_only = true })
           ~input_path:[Abstract.TreeDomain.Label.AnyIndex]
           ~output_root:(Sinks.ParameterUpdate self)
           ~output_path:[Abstract.TreeDomain.Label.AnyIndex]
           ~collapse_depth:0
           ~breadcrumbs:Features.BreadcrumbMayAlwaysSet.bottom
           ~via_features:Features.ViaFeatureSet.bottom
      |> add_tito
           ~input_root:
             (AccessPath.Root.PositionalParameter
                { position = 1; name = "__iterable"; positional_only = true })
           ~input_path:[AccessPath.dictionary_keys]
           ~output_root:(Sinks.ParameterUpdate self)
           ~output_path:[AccessPath.dictionary_keys]
           ~collapse_depth:Features.CollapseDepth.no_collapse
           ~breadcrumbs:Features.BreadcrumbMayAlwaysSet.bottom
           ~via_features:Features.ViaFeatureSet.bottom
      |> add_tito
           ~input_root:(AccessPath.Root.StarStarParameter { excluded = fields })
           ~input_path:[]
           ~output_root:(Sinks.ParameterUpdate self)
           ~output_path:[Abstract.TreeDomain.Label.AnyIndex]
           ~collapse_depth:Features.CollapseDepth.no_collapse
           ~breadcrumbs:Features.BreadcrumbMayAlwaysSet.bottom
           ~via_features:Features.ViaFeatureSet.bottom
    in
    let sink_taint =
      List.foldi
        fields
        ~init:BackwardState.empty
        ~f:(add_sink_from_attribute_model ~class_name ~positional:false)
    in
    [
      ( Target.create_method (Reference.create class_name) "__init__",
        { Model.empty_model with backward = { Model.Backward.taint_in_taint_out; sink_taint } } );
    ]
  in
  let compute_models class_name class_summary =
    if
      PyrePysaApi.ReadOnly.exists_matching_class_decorator
        pyre_api
        ~names:["dataclasses.dataclass"; "dataclass"]
        class_summary
    then
      compute_dataclass_models class_name
    else if
      CallResolution.has_transitive_successor_ignoring_untracked
        ~pyre_api
        ~reflexive:false
        ~predecessor:class_name
        ~successor:"typing.NamedTuple"
    then
      compute_named_tuple_models class_name
    else if
      CallResolution.has_transitive_successor_ignoring_untracked
        ~pyre_api
        ~reflexive:false
        ~predecessor:class_name
        ~successor:"TypedDictionary"
      || CallResolution.has_transitive_successor_ignoring_untracked
           ~pyre_api
           ~reflexive:false
           ~predecessor:class_name
           ~successor:"NonTotalTypedDictionary"
    then
      compute_typed_dict_models class_name
    else
      []
  in
  let inferred_models class_name =
    PyrePysaApi.ReadOnly.get_class_summary pyre_api class_name
    >>| compute_models class_name
    |> Option.value ~default:[]
  in
  let all_classes = PyrePysaApi.ReadOnly.all_classes pyre_api ~scheduler in
  let models =
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.InferClassModels
        ~default:
          (Scheduler.Policy.fixed_chunk_size
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunk_size:5000
             ())
    in
    let map classes =
      List.concat_map classes ~f:inferred_models |> Registry.of_alist ~join:Model.join_user_models
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:Registry.empty
      ~map
      ~reduce:(Registry.merge ~join:Model.join_user_models)
      ~inputs:all_classes
      ()
  in
  let () = StepLogger.finish ~integers:["models", Registry.size models] step_logger in
  models
