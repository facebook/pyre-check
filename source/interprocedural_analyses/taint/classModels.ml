(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Analysis
open Interprocedural
open Domains

let infer ~environment ~user_models =
  Log.info "Computing inferred models...";
  let timer = Timer.start () in
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let add_parameter_tito position existing_state attribute =
    let leaf =
      BackwardTaint.singleton Sinks.LocalReturn Frame.initial
      |> BackwardState.Tree.create_leaf
      |> BackwardState.Tree.transform Features.ReturnAccessPathSet.Self Map ~f:(fun _ ->
             Features.ReturnAccessPathSet.singleton
               [Abstract.TreeDomain.Label.create_name_index attribute])
    in
    BackwardState.assign
      ~root:
        (AccessPath.Root.PositionalParameter { position; name = attribute; positional_only = false })
      ~path:[]
      leaf
      existing_state
  in
  let add_sink_from_attribute_model class_name position existing_state attribute =
    let qualified_attribute =
      Target.create_object (Reference.create ~prefix:class_name attribute)
    in
    match Registry.get user_models qualified_attribute with
    | Some { Model.backward = { sink_taint; _ }; _ } ->
        let taint = BackwardState.read ~root:GlobalModel.global_root ~path:[] sink_taint in
        BackwardState.assign
          ~weak:true
          ~root:
            (AccessPath.Root.PositionalParameter
               { position; name = attribute; positional_only = false })
          ~path:[]
          taint
          existing_state
    | None -> existing_state
  in
  let attributes class_name =
    GlobalResolution.attributes
      ~resolution:global_resolution
      ~transitive:false
      ~accessed_through_class:false
      ~include_generated_attributes:false
      class_name
  in

  let compute_dataclass_models class_name =
    let attributes =
      attributes class_name >>| List.map ~f:Annotated.Attribute.name |> Option.value ~default:[]
    in
    [
      ( Target.Method { Target.class_name; method_name = "__init__"; kind = Normal },
        {
          Model.forward = Model.Forward.empty;
          backward =
            {
              Model.Backward.taint_in_taint_out =
                List.foldi ~f:add_parameter_tito ~init:BackwardState.empty attributes;
              sink_taint =
                List.foldi
                  attributes
                  ~init:BackwardState.empty
                  ~f:(add_sink_from_attribute_model (Reference.create class_name));
            };
          sanitizers = Model.Sanitizers.empty;
          modes = Model.ModeSet.empty;
        } );
    ]
  in
  (* We always generate a special `_fields` attribute for NamedTuples which is a tuple containing
     field names. *)
  let compute_named_tuple_models class_name =
    let attributes = attributes class_name |> Option.value ~default:[] in
    let has_attribute name =
      List.exists attributes ~f:(fun attribute ->
          String.equal (Annotated.Attribute.name attribute) name)
    in
    (* If a user-specified __new__ exist, don't override it. *)
    if has_attribute "__new__" then
      []
    else
      (* Should not omit this model. Otherwise the mode is "obscure", thus leading to a tito model,
         which joins the taint on every element of the tuple. *)
      [
        ( Target.Method { Target.class_name; method_name = "__new__"; kind = Normal },
          {
            Model.forward = Model.Forward.empty;
            backward = Model.Backward.empty;
            sanitizers = Model.Sanitizers.empty;
            modes = Model.ModeSet.empty;
          } );
      ]
  in
  let compute_models class_name class_summary =
    let is_dataclass =
      UnannotatedGlobalEnvironment.ReadOnly.exists_matching_class_decorator
        (TypeEnvironment.ReadOnly.unannotated_global_environment environment)
        ~names:["dataclasses.dataclass"; "dataclass"]
        class_summary
    in
    if is_dataclass then
      compute_dataclass_models class_name
    else if
      CallResolution.is_transitive_successor_ignoring_untracked
        global_resolution
        ~predecessor:class_name
        ~successor:"typing.NamedTuple"
    then
      compute_named_tuple_models class_name
    else
      []
  in
  let inferred_models class_name =
    GlobalResolution.class_summary global_resolution (Type.Primitive class_name)
    >>| compute_models class_name
    |> Option.value ~default:[]
  in
  let all_classes =
    TypeEnvironment.ReadOnly.global_resolution environment
    |> GlobalResolution.unannotated_global_environment
    |> UnannotatedGlobalEnvironment.ReadOnly.all_classes
  in
  let models =
    List.concat_map all_classes ~f:inferred_models |> Registry.of_alist ~join:Model.join_user_models
  in
  Statistics.performance
    ~name:"Computed inferred models"
    ~phase_name:"Computing inferred models"
    ~timer
    ();
  models
