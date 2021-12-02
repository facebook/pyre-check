(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Expression
open Pyre
open Interprocedural
open Domains
open TaintResult

exception InvalidModel of string

type t = {
  call_target: Target.t;
  model: TaintResult.call_model;
}
[@@deriving show]

type model_t = t

let is_obscure { modes; _ } = ModeSet.contains Obscure modes

let remove_obscureness ({ modes; _ } as model) = { model with modes = ModeSet.remove Obscure modes }

let remove_sinks model =
  { model with backward = { model.backward with sink_taint = BackwardState.empty } }


let add_obscure_sink ~resolution ~call_target model =
  match Target.get_callable_t call_target with
  | None -> model
  | Some real_target -> (
      match
        Target.get_module_and_definition
          ~resolution:(Resolution.global_resolution resolution)
          real_target
      with
      | None ->
          let () = Log.warning "Found no definition for %s" (Target.show call_target) in
          model
      | Some (_, { value = { signature = { parameters; _ }; _ }; _ }) ->
          let open Domains in
          let sink =
            BackwardState.Tree.create_leaf (BackwardTaint.singleton (Sinks.NamedSink "Obscure"))
          in
          let parameters = AccessPath.Root.normalize_parameters parameters in
          let add_parameter_sink sink_taint (root, _, _) =
            BackwardState.assign ~root ~path:[] sink sink_taint
          in
          let sink_taint =
            List.fold_left ~init:model.backward.sink_taint ~f:add_parameter_sink parameters
          in
          { model with backward = { model.backward with sink_taint } })


let unknown_callee ~location ~call =
  let callee =
    match call with
    | Expression.Call { callee; _ } -> callee
    | _ -> Node.create ~location:(Location.strip_module location) call
  in
  Interprocedural.Target.create_function
    (Reference.create
       (Format.asprintf "%a:%a" Location.WithModule.pp location Expression.pp callee))


let register_unknown_callee_model callable =
  (* Add a model with sinks on *args and **kwargs. *)
  let sink_leaf =
    BackwardState.Tree.create_leaf (BackwardTaint.singleton (Sinks.NamedSink "UnknownCallee"))
  in
  let sink_taint =
    BackwardState.assign
      ~root:(AccessPath.Root.StarParameter { position = 0 })
      ~path:[]
      sink_leaf
      BackwardState.empty
    |> BackwardState.assign
         ~root:(AccessPath.Root.StarStarParameter { excluded = [] })
         ~path:[]
         sink_leaf
  in
  (* Add taint-in-taint-out for all parameters. *)
  let local_return = BackwardState.Tree.create_leaf (BackwardTaint.singleton Sinks.LocalReturn) in
  let taint_in_taint_out =
    BackwardState.assign
      ~root:(AccessPath.Root.StarParameter { position = 0 })
      ~path:[]
      local_return
      BackwardState.empty
    |> BackwardState.assign
         ~root:(AccessPath.Root.StarStarParameter { excluded = [] })
         ~path:[]
         local_return
  in
  Interprocedural.FixpointState.add_predefined
    Interprocedural.FixpointState.Epoch.predefined
    callable
    (Interprocedural.AnalysisResult.make_model
       TaintResult.kind
       {
         TaintResult.forward = Forward.empty;
         backward = { sink_taint; taint_in_taint_out };
         sanitizers = Sanitizers.empty;
         modes = ModeSet.singleton Mode.SkipAnalysis;
       })


let get_callsite_model ~resolution ~call_target ~arguments =
  let call_target = (call_target :> Target.t) in
  match Interprocedural.FixpointState.get_model call_target with
  | None -> { call_target; model = TaintResult.obscure_model }
  | Some model ->
      let expand_via_value_of
          {
            forward = { source_taint };
            backward = { sink_taint; taint_in_taint_out };
            sanitizers;
            modes;
          }
        =
        let expand frame =
          let transform via_feature frame =
            let match_all_arguments_to_parameter parameter =
              AccessPath.match_actuals_to_formals arguments [parameter]
              |> List.filter_map ~f:(fun (argument, matches) ->
                     if not (List.is_empty matches) then
                       Some argument
                     else
                       None)
            in
            let match_argument_to_parameter parameter =
              match match_all_arguments_to_parameter parameter with
              | [] -> None
              | argument :: _ -> Some argument.value
            in
            match via_feature with
            | Features.ViaFeature.ViaValueOf { parameter; tag } ->
                let arguments = match_all_arguments_to_parameter parameter in
                Frame.add_breadcrumb
                  (Features.ViaFeature.via_value_of_breadcrumb ?tag ~arguments)
                  frame
            | Features.ViaFeature.ViaTypeOf { parameter; tag } ->
                let breadcrumb =
                  match call_target with
                  | `Object object_target ->
                      Features.ViaFeature.via_type_of_breadcrumb_for_object
                        ?tag
                        ~resolution
                        ~object_target
                  | _ ->
                      Features.ViaFeature.via_type_of_breadcrumb
                        ?tag
                        ~resolution
                        ~argument:(match_argument_to_parameter parameter)
                in
                Frame.add_breadcrumb breadcrumb frame
          in
          Frame.fold Features.ViaFeatureSet.Element ~f:transform ~init:frame frame
        in
        let source_taint = ForwardState.transform Frame.Self Map ~f:expand source_taint in
        let sink_taint = BackwardState.transform Frame.Self Map ~f:expand sink_taint in
        let taint_in_taint_out =
          BackwardState.transform Frame.Self Map ~f:expand taint_in_taint_out
        in
        {
          forward = { source_taint };
          backward = { sink_taint; taint_in_taint_out };
          sanitizers;
          modes;
        }
      in
      let taint_model =
        Interprocedural.AnalysisResult.get_model TaintResult.kind model
        |> Option.value ~default:TaintResult.empty_model
        |> expand_via_value_of
      in
      let taint_model =
        if model.is_obscure then
          { taint_model with modes = ModeSet.add Obscure taint_model.modes }
        else
          taint_model
      in
      { call_target; model = taint_model }


let get_global_targets ~resolution ~call_graph ~expression =
  match Node.value expression with
  | Expression.Name (Name.Identifier _) ->
      Interprocedural.CallGraph.as_global_reference ~resolution expression
      >>| (fun global -> [Target.create_object global])
      |> Option.value ~default:[]
  | Expression.Name (Name.Attribute { attribute; _ }) ->
      Interprocedural.CallGraph.DefineCallGraph.resolve_attribute_access
        call_graph
        ~location:(Node.location expression)
        ~attribute
      >>| (fun { global_targets; _ } -> global_targets)
      |> Option.value ~default:[]
  | _ -> []


let global_root =
  AccessPath.Root.PositionalParameter { position = 0; name = "$global"; positional_only = false }


module GlobalModel = struct
  type t = {
    models: model_t list;
    location: Location.WithModule.t;
  }

  let get_source { models; location } =
    let to_source
        existing
        {
          call_target;
          model = { TaintResult.forward = { TaintResult.Forward.source_taint }; _ };
          _;
        }
      =
      ForwardState.read ~root:AccessPath.Root.LocalResult ~path:[] source_taint
      |> ForwardState.Tree.apply_call
           location
           ~callees:[call_target]
           ~port:AccessPath.Root.LocalResult
      |> ForwardState.Tree.join existing
    in
    List.fold ~init:ForwardState.Tree.bottom ~f:to_source models


  let get_sink { models; location } =
    let to_sink
        existing
        {
          call_target;
          model = { TaintResult.backward = { TaintResult.Backward.sink_taint; _ }; _ };
          _;
        }
      =
      BackwardState.read ~root:global_root ~path:[] sink_taint
      |> BackwardState.Tree.apply_call
           location
           ~callees:[call_target]
           ~port:AccessPath.Root.LocalResult
      |> BackwardState.Tree.join existing
    in
    List.fold ~init:BackwardState.Tree.bottom ~f:to_sink models


  let get_tito { models; _ } =
    let to_tito
        existing
        { model = { TaintResult.backward = { TaintResult.Backward.taint_in_taint_out; _ }; _ }; _ }
      =
      BackwardState.read ~root:global_root ~path:[] taint_in_taint_out
      |> BackwardState.Tree.join existing
    in
    List.fold ~init:BackwardState.Tree.bottom ~f:to_tito models


  let get_sanitize { models; _ } =
    let get_sanitize
        existing
        { model = { TaintResult.sanitizers = { global = sanitize; _ }; _ }; _ }
      =
      Sanitize.join sanitize existing
    in
    List.fold ~init:Sanitize.empty ~f:get_sanitize models


  let get_modes { models; _ } =
    let get_modes existing { model = { TaintResult.modes; _ }; _ } = ModeSet.join modes existing in
    List.fold ~init:ModeSet.empty ~f:get_modes models


  let is_sanitized { models; _ } =
    let is_sanitized_model { model = { TaintResult.sanitizers = { global = sanitize; _ }; _ }; _ } =
      match sanitize with
      | {
       sources = Some Sanitize.AllSources;
       sinks = Some Sanitize.AllSinks;
       tito = Some Sanitize.AllTito;
      } ->
          true
      | _ -> false
    in
    List.exists ~f:is_sanitized_model models
end

let get_global_model ~resolution ~call_graph ~qualifier ~expression =
  let fetch_model target = get_callsite_model ~resolution ~call_target:target ~arguments:[] in
  let models = get_global_targets ~resolution ~call_graph ~expression |> List.map ~f:fetch_model in
  let location = Node.location expression |> Location.with_module ~qualifier in
  { GlobalModel.models; location }


let get_model_sources ~paths =
  let path_and_content file =
    match File.content file with
    | Some content -> Some (File.path file, content)
    | None -> None
  in
  let model_files = Path.get_matching_files_recursively ~suffix:".pysa" ~paths in
  Log.info
    "Finding taint models in `%s`."
    (paths |> List.map ~f:Path.show |> String.concat ~sep:", ");
  model_files |> List.map ~f:File.create |> List.filter_map ~f:path_and_content


let infer_class_models ~environment =
  let open Domains in
  Log.info "Computing inferred models...";
  let timer = Timer.start () in
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let fold_taint position existing_state attribute =
    let leaf =
      BackwardState.Tree.create_leaf (BackwardTaint.singleton Sinks.LocalReturn)
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
          TaintResult.forward = Forward.empty;
          backward =
            {
              TaintResult.Backward.taint_in_taint_out =
                List.foldi ~f:fold_taint ~init:BackwardState.empty attributes;
              sink_taint = BackwardState.empty;
            };
          sanitizers = Sanitizers.empty;
          modes = ModeSet.empty;
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
                    TaintResult.forward = Forward.empty;
                    backward =
                      {
                        TaintResult.Backward.taint_in_taint_out =
                          List.foldi ~f:fold_taint ~init:BackwardState.empty attributes;
                        sink_taint = BackwardState.empty;
                      };
                    sanitizers = Sanitizers.empty;
                    modes = ModeSet.empty;
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
            TaintResult.forward = Forward.empty;
            backward = Backward.empty;
            sanitizers = Sanitizers.empty;
            modes = ModeSet.empty;
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
    List.concat_map all_classes ~f:inferred_models
    |> Target.Map.of_alist_reduce ~f:(TaintResult.join ~iteration:0)
  in
  Statistics.performance
    ~name:"Computed inferred models"
    ~phase_name:"Computing inferred models"
    ~timer
    ();
  models
