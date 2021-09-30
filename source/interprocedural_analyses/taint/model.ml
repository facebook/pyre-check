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
        let expand flow_details =
          let transform via_feature flow_details =
            let match_argument_to_parameter parameter =
              AccessPath.match_actuals_to_formals arguments [parameter]
              |> List.find ~f:(fun (_, matches) -> not (List.is_empty matches))
              >>| fst
            in
            match via_feature with
            | Features.ViaFeature.ViaValueOf { parameter; tag } ->
                FlowDetails.add_breadcrumb
                  (Features.ViaFeature.via_value_of_breadcrumb
                     ?tag
                     ~argument:(match_argument_to_parameter parameter))
                  flow_details
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
                FlowDetails.add_breadcrumb breadcrumb flow_details
          in
          FlowDetails.fold
            Features.ViaFeatureSet.Element
            ~f:transform
            ~init:flow_details
            flow_details
        in
        let source_taint = ForwardState.transform FlowDetails.Self Map ~f:expand source_taint in
        let sink_taint = BackwardState.transform FlowDetails.Self Map ~f:expand sink_taint in
        let taint_in_taint_out =
          BackwardState.transform FlowDetails.Self Map ~f:expand taint_in_taint_out
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


let get_global_targets ~resolution ~expression =
  let global_resolution = Resolution.global_resolution resolution in
  match Node.value expression, AccessPath.get_global ~resolution expression with
  | _, Some global -> [global]
  | Name (Name.Attribute { base; attribute; _ }), _ ->
      let rec find_targets targets = function
        | Type.Union annotations -> List.fold ~init:targets ~f:find_targets annotations
        | Parametric { name = "type"; parameters = [Single annotation] } ->
            (* Access on a class, i.e `Foo.bar`, translated into `Foo.__class__.bar`. *)
            let parent =
              let attribute =
                Type.split annotation
                |> fst
                |> Type.primitive_name
                >>= GlobalResolution.attribute_from_class_name
                      ~transitive:true
                      ~resolution:global_resolution
                      ~name:attribute
                      ~instantiated:annotation
              in
              match attribute with
              | Some attribute when Annotated.Attribute.defined attribute ->
                  Type.Primitive (Annotated.Attribute.parent attribute) |> Type.class_name
              | _ -> Type.class_name annotation
            in
            let attribute = Format.sprintf "__class__.%s" attribute in
            let target = Reference.create ~prefix:parent attribute in
            target :: targets
        | annotation ->
            (* Access on an instance, i.e `self.foo`. *)
            let parents =
              let successors =
                GlobalResolution.class_metadata (Resolution.global_resolution resolution) annotation
                >>| (fun { ClassMetadataEnvironment.successors; _ } -> successors)
                |> Option.value ~default:[]
                |> List.map ~f:(fun name -> Type.Primitive name)
              in
              annotation :: successors
            in
            let add_target targets parent =
              let target = Reference.create ~prefix:(Type.class_name parent) attribute in
              target :: targets
            in
            List.fold ~init:targets ~f:add_target parents
      in
      let annotation = Interprocedural.CallGraph.resolve_ignoring_optional ~resolution base in
      find_targets [] annotation
  | _ -> []


let get_global_models ~resolution ~expression =
  let fetch_model target =
    let call_target = Target.create_object target in
    get_callsite_model ~resolution ~call_target ~arguments:[]
  in
  get_global_targets ~resolution ~expression |> List.map ~f:fetch_model


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

let get_global_model ~resolution ~location ~expression =
  let models = get_global_models ~resolution ~expression in
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
  let attributes class_summary =
    GlobalResolution.attributes
      ~resolution:global_resolution
      ~transitive:false
      ~accessed_through_class:false
      ~include_generated_attributes:false
      class_summary
  in

  let compute_dataclass_model class_summary =
    let attributes =
      attributes class_summary >>| List.map ~f:Annotated.Attribute.name |> Option.value ~default:[]
    in
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
    }
  in
  (* We always generate a special `_fields` attribute for NamedTuples which is a tuple containing
     field names. *)
  let compute_named_tuple_model class_summary =
    let attributes = attributes class_summary |> Option.value ~default:[] in
    (* If a user-specified constructor exists, don't override it. *)
    if
      List.exists attributes ~f:(fun attribute ->
          String.equal (Annotated.Attribute.name attribute) "__init__")
    then
      None
    else
      GlobalResolution.class_definition global_resolution (Primitive class_summary)
      >>| Node.value
      >>= ClassSummary.fields_tuple_value
      >>| fun attributes ->
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
      }
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
      Some (compute_dataclass_model class_name)
    else if
      GlobalResolution.is_transitive_successor
        global_resolution
        ~predecessor:class_name
        ~successor:"typing.NamedTuple"
    then
      compute_named_tuple_model class_name
    else
      None
  in
  let inferred_models class_name =
    GlobalResolution.class_definition global_resolution (Type.Primitive class_name)
    >>= compute_models class_name
    >>| fun model -> `Method { Target.class_name; method_name = "__init__" }, model
  in
  let all_classes =
    TypeEnvironment.ReadOnly.global_resolution environment
    |> GlobalResolution.unannotated_global_environment
    |> UnannotatedGlobalEnvironment.ReadOnly.all_classes
  in
  let models =
    List.filter_map all_classes ~f:inferred_models
    |> Target.Map.of_alist_reduce ~f:(TaintResult.join ~iteration:0)
  in
  Statistics.performance
    ~name:"Computed inferred models"
    ~phase_name:"Computing inferred models"
    ~timer
    ();
  models


let apply_sanitizers
    {
      forward = { source_taint };
      backward = { taint_in_taint_out; sink_taint };
      sanitizers = { global; parameters; roots } as sanitizers;
      modes;
    }
  =
  let kinds_to_taint_transforms ~sources ~sinks =
    let source_transforms = Sources.Set.to_sanitize_taint_transforms_exn sources in
    let sink_transforms = Sinks.Set.to_sanitize_taint_transforms_exn sinks in
    List.rev_append source_transforms sink_transforms
  in
  let sanitize_tito ?(sources = Sources.Set.empty) ?(sinks = Sinks.Set.empty) taint_in_taint_out =
    let transforms = kinds_to_taint_transforms ~sources ~sinks in
    BackwardState.apply_taint_transforms transforms taint_in_taint_out
  in
  let sanitize_tito_parameter
      parameter
      ?(sources = Sources.Set.empty)
      ?(sinks = Sinks.Set.empty)
      taint_in_taint_out
    =
    let sanitize_tito_taint_tree = function
      | None -> BackwardState.Tree.bottom
      | Some taint_tree ->
          let transforms = kinds_to_taint_transforms ~sources ~sinks in
          BackwardState.Tree.apply_taint_transforms transforms taint_tree
    in
    BackwardState.update taint_in_taint_out parameter ~f:sanitize_tito_taint_tree
  in

  (* Apply the global sanitizer. *)
  (* Here, we are applying the legacy behavior of sanitizers, where we only
   * sanitize the forward trace or the backward trace. *)
  let source_taint =
    (* @Sanitize(TaintSource[...]) *)
    match global.sources with
    | Some Sanitize.AllSources -> ForwardState.empty
    | Some (Sanitize.SpecificSources sanitized_sources) ->
        ForwardState.sanitize sanitized_sources source_taint
    | None -> source_taint
  in
  let taint_in_taint_out =
    (* @Sanitize(TaintInTaintOut[...]) *)
    match global.tito with
    | Some AllTito -> BackwardState.empty
    | Some (SpecificTito { sanitized_tito_sources; sanitized_tito_sinks }) ->
        sanitize_tito ~sources:sanitized_tito_sources ~sinks:sanitized_tito_sinks taint_in_taint_out
    | None -> taint_in_taint_out
  in
  let sink_taint =
    (* @Sanitize(TaintSink[...]) *)
    match global.sinks with
    | Some Sanitize.AllSinks -> BackwardState.empty
    | Some (Sanitize.SpecificSinks sanitized_sinks) ->
        BackwardState.sanitize sanitized_sinks sink_taint
    | None -> sink_taint
  in

  (* Apply the parameters sanitizer. *)
  (* Here, we apply sanitizers both in the forward and backward trace. *)
  (* Note that by design, sanitizing a specific source or sink also sanitizes
   * taint-in-taint-out for that source/sink. *)
  let taint_in_taint_out =
    (* Sanitize(Parameters[TaintSource[...]]) *)
    match parameters.sources with
    | Some Sanitize.AllSources -> taint_in_taint_out
    | Some (Sanitize.SpecificSources sanitized_sources) ->
        sanitize_tito ~sources:sanitized_sources taint_in_taint_out
        (* TODO(T91641396): sanitize sources in the sink trace. *)
    | None -> taint_in_taint_out
  in
  let taint_in_taint_out =
    (* Sanitize(Parameters[TaintInTaintOut[...]]) *)
    match parameters.tito with
    | Some AllTito -> BackwardState.empty
    | Some (SpecificTito { sanitized_tito_sources; sanitized_tito_sinks }) ->
        sanitize_tito ~sources:sanitized_tito_sources ~sinks:sanitized_tito_sinks taint_in_taint_out
    | _ -> taint_in_taint_out
  in
  let sink_taint, taint_in_taint_out =
    (* Sanitize(Parameters[TaintSink[...]]) *)
    match parameters.sinks with
    | Some Sanitize.AllSinks ->
        let sink_taint = BackwardState.empty in
        sink_taint, taint_in_taint_out
    | Some (Sanitize.SpecificSinks sanitized_sinks) ->
        let sink_taint = BackwardState.sanitize sanitized_sinks sink_taint in
        let taint_in_taint_out = sanitize_tito ~sinks:sanitized_sinks taint_in_taint_out in
        sink_taint, taint_in_taint_out
    | None -> sink_taint, taint_in_taint_out
  in

  (* Apply the return sanitizer. *)
  let sanitize_return sanitize (source_taint, taint_in_taint_out, sink_taint) =
    let root = AccessPath.Root.LocalResult in
    let source_taint, taint_in_taint_out =
      (* def foo() -> Sanitize[TaintSource[...]] *)
      match sanitize.Sanitize.sources with
      | Some Sanitize.AllSources ->
          let source_taint = ForwardState.remove root source_taint in
          source_taint, taint_in_taint_out
      | Some (Sanitize.SpecificSources sanitized_sources) ->
          let filter_sources = function
            | None -> ForwardState.Tree.bottom
            | Some taint_tree -> ForwardState.Tree.sanitize sanitized_sources taint_tree
          in
          let source_taint = ForwardState.update source_taint root ~f:filter_sources in
          let taint_in_taint_out = sanitize_tito ~sources:sanitized_sources taint_in_taint_out in
          source_taint, taint_in_taint_out
      | None -> source_taint, taint_in_taint_out
    in
    let taint_in_taint_out =
      (* def foo() -> Sanitize[TaintInTaintOut[...]] *)
      match sanitize.Sanitize.tito with
      | Some AllTito -> BackwardState.remove root taint_in_taint_out
      | Some (SpecificTito { sanitized_tito_sources; sanitized_tito_sinks }) ->
          sanitize_tito
            ~sources:sanitized_tito_sources
            ~sinks:sanitized_tito_sinks
            taint_in_taint_out
      | _ -> taint_in_taint_out
    in
    let taint_in_taint_out =
      (* def foo() -> Sanitize[TaintSink[...]] *)
      match sanitize.Sanitize.sinks with
      | Some Sanitize.AllSinks -> taint_in_taint_out
      | Some (Sanitize.SpecificSinks sanitized_sinks) ->
          sanitize_tito ~sinks:sanitized_sinks taint_in_taint_out
          (* TODO(T91641396): sanitize sinks in the source trace. *)
      | None -> taint_in_taint_out
    in
    source_taint, taint_in_taint_out, sink_taint
  in

  (* Apply the parameter-specific sanitizers. *)
  let sanitize_parameter (parameter, sanitize) (source_taint, taint_in_taint_out, sink_taint) =
    let taint_in_taint_out =
      (* def foo(x: Sanitize[TaintSource[...]]): ... *)
      match sanitize.Sanitize.sources with
      | Some Sanitize.AllSources -> taint_in_taint_out
      | Some (Sanitize.SpecificSources sanitized_sources) ->
          sanitize_tito_parameter parameter ~sources:sanitized_sources taint_in_taint_out
          (* TODO(T91641396): sanitize sources in the sink trace. *)
      | None -> taint_in_taint_out
    in
    let taint_in_taint_out =
      (* def foo(x: Sanitize[TaintInTaintOut[...]]): ... *)
      match sanitize.Sanitize.tito with
      | Some AllTito -> BackwardState.remove parameter taint_in_taint_out
      | Some (SpecificTito { sanitized_tito_sources; sanitized_tito_sinks }) ->
          sanitize_tito_parameter
            parameter
            ~sources:sanitized_tito_sources
            ~sinks:sanitized_tito_sinks
            taint_in_taint_out
      | None -> taint_in_taint_out
    in
    let sink_taint, taint_in_taint_out =
      (* def foo(x: Sanitize[TaintSink[...]]): ... *)
      match sanitize.Sanitize.sinks with
      | Some Sanitize.AllSinks ->
          let sink_taint = BackwardState.remove parameter sink_taint in
          sink_taint, taint_in_taint_out
      | Some (Sanitize.SpecificSinks sanitized_sinks) ->
          let filter_sinks = function
            | None -> BackwardState.Tree.bottom
            | Some taint_tree -> BackwardState.Tree.sanitize sanitized_sinks taint_tree
          in
          let sink_taint = BackwardState.update sink_taint parameter ~f:filter_sinks in
          let taint_in_taint_out =
            sanitize_tito_parameter parameter ~sinks:sanitized_sinks taint_in_taint_out
          in
          sink_taint, taint_in_taint_out
      | None -> sink_taint, taint_in_taint_out
    in
    source_taint, taint_in_taint_out, sink_taint
  in

  let sanitize_root (root, sanitize) (source_taint, taint_in_taint_out, sink_taint) =
    match root with
    | AccessPath.Root.LocalResult ->
        sanitize_return sanitize (source_taint, taint_in_taint_out, sink_taint)
    | PositionalParameter _
    | NamedParameter _
    | StarParameter _
    | StarStarParameter _ ->
        sanitize_parameter (root, sanitize) (source_taint, taint_in_taint_out, sink_taint)
    | Variable _ -> failwith "unexpected"
  in
  let source_taint, taint_in_taint_out, sink_taint =
    SanitizeRootMap.fold
      SanitizeRootMap.KeyValue
      ~f:sanitize_root
      ~init:(source_taint, taint_in_taint_out, sink_taint)
      roots
  in
  { forward = { source_taint }; backward = { sink_taint; taint_in_taint_out }; sanitizers; modes }
