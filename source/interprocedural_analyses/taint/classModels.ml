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

let infer ~environment =
  Log.info "Computing inferred models...";
  let timer = Timer.start () in
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let fold_taint position existing_state attribute =
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
      ( `Method { Target.class_name; method_name = "__init__" },
        {
          Model.forward = Model.Forward.empty;
          backward =
            {
              Model.Backward.taint_in_taint_out =
                List.foldi ~f:fold_taint ~init:BackwardState.empty attributes;
              sink_taint = BackwardState.empty;
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
    (* If a user-specified __new__ or __init__ exist, don't override it. *)
    let models =
      if has_attribute "__init__" then
        []
      else
        GlobalResolution.class_definition global_resolution (Primitive class_name)
        >>| Node.value
        >>= ClassSummary.fields_tuple_value
        >>| (fun attributes ->
              [
                ( `Method { Target.class_name; method_name = "__init__" },
                  {
                    Model.forward = Model.Forward.empty;
                    backward =
                      {
                        Model.Backward.taint_in_taint_out =
                          List.foldi ~f:fold_taint ~init:BackwardState.empty attributes;
                        sink_taint = BackwardState.empty;
                      };
                    sanitizers = Model.Sanitizers.empty;
                    modes = Model.ModeSet.empty;
                  } );
              ])
        |> Option.value ~default:[]
    in
    let models =
      if has_attribute "__new__" then
        models
      else
        ( `Method { Target.class_name; method_name = "__new__" },
          {
            Model.forward = Model.Forward.empty;
            backward = Model.Backward.empty;
            sanitizers = Model.Sanitizers.empty;
            modes = Model.ModeSet.empty;
          } )
        :: models
    in
    models
  in
  let compute_models class_name class_summary =
    let is_dataclass =
      UnannotatedGlobalEnvironment.ReadOnly.get_decorator
        (TypeEnvironment.ReadOnly.unannotated_global_environment environment)
        class_summary
        ~decorator:"dataclasses.dataclass"
      |> fun decorators -> not (List.is_empty decorators)
    in
    if is_dataclass then
      compute_dataclass_models class_name
    else if
      GlobalResolution.is_transitive_successor
        global_resolution
        ~predecessor:class_name
        ~successor:"typing.NamedTuple"
    then
      compute_named_tuple_models class_name
    else
      []
  in
  let inferred_models class_name =
    GlobalResolution.class_definition global_resolution (Type.Primitive class_name)
    >>| compute_models class_name
    |> Option.value ~default:[]
  in
  let all_classes =
    TypeEnvironment.ReadOnly.global_resolution environment
    |> GlobalResolution.unannotated_global_environment
    |> UnannotatedGlobalEnvironment.ReadOnly.all_classes
  in
  let models =
    List.concat_map all_classes ~f:inferred_models |> Target.Map.of_alist_reduce ~f:Model.join
  in
  Statistics.performance
    ~name:"Computed inferred models"
    ~phase_name:"Computing inferred models"
    ~timer
    ();
  models
