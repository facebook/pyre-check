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
  is_obscure: bool;
  call_target: Callable.t;
  model: TaintResult.call_model;
}
[@@deriving show]

let remove_sinks model =
  { model with backward = { model.backward with sink_taint = BackwardState.empty } }


let add_obscure_sink ~resolution ~call_target model =
  match Callable.get_real_target call_target with
  | None -> model
  | Some real_target -> (
      match
        Callable.get_module_and_definition
          ~resolution:(Resolution.global_resolution resolution)
          real_target
      with
      | None ->
          let () = Log.warning "Found no definition for %s" (Callable.show call_target) in
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
          { model with backward = { model.backward with sink_taint } } )


let unknown_callee ~location ~call =
  let callee =
    match call with
    | Expression.Call { callee; _ } -> callee
    | _ -> Node.create ~location:(Location.strip_module location) call
  in
  Interprocedural.Callable.create_function
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
  Interprocedural.Fixpoint.add_predefined
    Interprocedural.Fixpoint.Epoch.predefined
    callable
    (Interprocedural.Result.make_model
       TaintResult.kind
       {
         TaintResult.forward = TaintResult.Forward.empty;
         backward = { sink_taint; taint_in_taint_out };
         mode = SkipAnalysis;
       })


let get_callsite_model ~resolution ~call_target ~arguments =
  let call_target = (call_target :> Callable.t) in
  match Interprocedural.Fixpoint.get_model call_target with
  | None -> { is_obscure = true; call_target; model = TaintResult.empty_model }
  | Some model ->
      let expand_via_value_of
          { forward = { source_taint }; backward = { sink_taint; taint_in_taint_out }; mode }
        =
        let expand features =
          let transform feature features =
            let match_argument_to_parameter parameter =
              AccessPath.match_actuals_to_formals arguments [parameter]
              |> List.find ~f:(fun (_, matches) -> not (List.is_empty matches))
              >>| fst
            in
            match feature with
            | Features.Simple.ViaValueOf { parameter; tag } ->
                features
                |> Features.SimpleSet.remove feature
                |> Features.SimpleSet.add
                     (Features.Simple.via_value_of_breadcrumb
                        ?tag
                        ~argument:(match_argument_to_parameter parameter))
            | Features.Simple.ViaTypeOf { parameter; tag } ->
                features
                |> Features.SimpleSet.remove feature
                |> Features.SimpleSet.add
                     (Features.Simple.via_type_of_breadcrumb
                        ?tag
                        ~resolution
                        ~argument:(match_argument_to_parameter parameter))
            | _ -> features
          in
          Features.SimpleSet.fold Features.SimpleSet.Element features ~f:transform ~init:features
        in
        let source_taint =
          ForwardState.transform
            ForwardTaint.simple_feature_self
            Abstract.Domain.Map
            ~f:expand
            source_taint
        in
        let sink_taint =
          BackwardState.transform
            BackwardTaint.simple_feature_self
            Abstract.Domain.Map
            ~f:expand
            sink_taint
        in
        let taint_in_taint_out =
          BackwardState.transform
            BackwardTaint.simple_feature_self
            Abstract.Domain.Map
            ~f:expand
            taint_in_taint_out
        in
        { forward = { source_taint }; backward = { sink_taint; taint_in_taint_out }; mode }
      in
      let taint_model =
        Interprocedural.Result.get_model TaintResult.kind model
        |> Option.value ~default:TaintResult.empty_model
        |> expand_via_value_of
      in
      { is_obscure = model.is_obscure; call_target; model = taint_model }


let get_global_model ~resolution ~expression =
  let call_target =
    match Node.value expression, AccessPath.get_global ~resolution expression with
    | _, Some global -> Some global
    | Name (Name.Attribute { base; attribute; _ }), _ ->
        let is_meta, annotation =
          let rec is_meta = function
            | Type.Union [Type.NoneType; annotation]
            | Type.Union [annotation; Type.NoneType] ->
                is_meta annotation
            | annotation ->
                if Type.is_meta annotation then
                  true, Type.single_parameter annotation
                else
                  false, annotation
          in
          is_meta (Resolution.resolve_expression_to_type resolution base)
        in
        let global_resolution = Resolution.global_resolution resolution in
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
        let attribute =
          if is_meta then
            Format.sprintf "__class__.%s" attribute
          else
            attribute
        in
        Some (Reference.create ~prefix:parent attribute)
    | _ -> None
  in
  match call_target with
  | Some target ->
      let model =
        Callable.create_object target
        |> fun call_target -> get_callsite_model ~resolution ~call_target ~arguments:[]
      in
      Some (target, model)
  | None -> None


let global_root =
  AccessPath.Root.PositionalParameter { position = 0; name = "$global"; positional_only = false }


let get_global_sink_model ~resolution ~location ~expression =
  let to_sink
      (name, { model = { TaintResult.backward = { TaintResult.Backward.sink_taint; _ }; _ }; _ })
    =
    BackwardState.read ~root:global_root ~path:[] sink_taint
    |> BackwardState.Tree.apply_call
         location
         ~callees:[`Function (Reference.show name)]
         ~port:AccessPath.Root.LocalResult
  in
  get_global_model ~resolution ~expression >>| to_sink


let get_global_tito_model_and_mode ~resolution ~expression =
  let to_tito
      ( _,
        { model = { TaintResult.backward = { TaintResult.Backward.taint_in_taint_out; _ }; _ }; _ }
      )
    =
    BackwardState.read ~root:global_root ~path:[] taint_in_taint_out
  in
  let get_mode (_, { model = { TaintResult.mode; _ }; _ }) = mode in
  let global_model = get_global_model ~resolution ~expression in
  global_model >>| to_tito, global_model >>| get_mode


let global_is_sanitized ~resolution ~expression =
  let is_sanitized (_, { model = { TaintResult.mode; _ }; _ }) =
    match mode with
    | TaintResult.Mode.Sanitize
        {
          sources = Some TaintResult.Mode.AllSources;
          sinks = Some TaintResult.Mode.AllSinks;
          tito = Some AllTito;
        } ->
        true
    | _ -> false
  in
  get_global_model ~resolution ~expression >>| is_sanitized |> Option.value ~default:false


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
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let fold_taint position existing_state attribute =
    let leaf =
      BackwardState.Tree.create_leaf (BackwardTaint.singleton Sinks.LocalReturn)
      |> BackwardState.Tree.transform BackwardTaint.complex_feature_set Map ~f:(fun _ ->
             [
               Features.Complex.ReturnAccessPath
                 [Abstract.TreeDomain.Label.create_name_field attribute];
             ])
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
      mode = Normal;
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
        mode = Normal;
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
    >>| fun model -> `Method { Callable.class_name; method_name = "__init__" }, model
  in
  let all_classes =
    TypeEnvironment.ReadOnly.global_resolution environment
    |> GlobalResolution.unannotated_global_environment
    |> UnannotatedGlobalEnvironment.ReadOnly.all_classes
  in
  List.filter_map all_classes ~f:inferred_models
  |> Callable.Map.of_alist_reduce ~f:(TaintResult.join ~iteration:0)
