(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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

let get_callsite_model ~call_target ~arguments =
  let call_target = (call_target :> Callable.t) in
  match Interprocedural.Fixpoint.get_model call_target with
  | None -> { is_obscure = true; call_target; model = TaintResult.empty_model }
  | Some model ->
      let expand_via_value_of
          { forward = { source_taint }; backward = { sink_taint; taint_in_taint_out }; mode }
        =
        let expand features =
          let transform feature =
            let open Features in
            match feature.Abstract.OverUnderSetDomain.element with
            | Simple.ViaValueOf { position } ->
                List.nth arguments position
                >>= fun argument -> Simple.via_value_of_breadcrumb ~argument >>| SimpleSet.inject
            | _ -> Some feature
          in
          List.filter_map features ~f:transform
        in
        let source_taint =
          ForwardState.transform
            ForwardTaint.simple_feature_set
            Abstract.Domain.(Map expand)
            source_taint
        in
        let sink_taint =
          BackwardState.transform
            BackwardTaint.simple_feature_set
            Abstract.Domain.(Map expand)
            sink_taint
        in
        let taint_in_taint_out =
          BackwardState.transform
            BackwardTaint.simple_feature_set
            Abstract.Domain.(Map expand)
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
            | Type.Optional annotation -> is_meta annotation
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
        |> fun call_target -> get_callsite_model ~call_target ~arguments:[]
      in
      Some (target, model)
  | None -> None


let global_root = AccessPath.Root.PositionalParameter { position = 0; name = "$global" }

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


let get_global_tito_model ~resolution ~location:_ ~expression =
  let to_tito
      ( _,
        { model = { TaintResult.backward = { TaintResult.Backward.taint_in_taint_out; _ }; _ }; _ }
      )
    =
    BackwardState.read ~root:global_root ~path:[] taint_in_taint_out
  in
  get_global_model ~resolution ~expression >>| to_tito


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
      |> BackwardState.Tree.transform
           BackwardTaint.complex_feature_set
           (Abstract.Domain.Map
              (fun _ ->
                [
                  Features.Complex.ReturnAccessPath
                    [Abstract.TreeDomain.Label.create_name_field attribute];
                ]))
    in
    BackwardState.assign
      ~root:(AccessPath.Root.PositionalParameter { position; name = attribute })
      ~path:[]
      leaf
      existing_state
  in
  let attributes class_summary =
    GlobalResolution.attributes
      ~resolution:global_resolution
      ~transitive:false
      ~class_attributes:false
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
    if List.exists attributes ~f:(fun attribute -> Annotated.Attribute.name attribute = "__init__")
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
      AstEnvironment.ReadOnly.get_decorator
        (TypeEnvironment.ReadOnly.ast_environment environment)
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
