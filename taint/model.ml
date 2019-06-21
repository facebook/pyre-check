(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Expression
open Pyre
open PyreParser
open Interprocedural
open Statement
open Domains
open TaintResult

type t = {
  is_obscure: bool;
  call_target: Callable.t;
  model: TaintResult.call_model
}
[@@deriving show, sexp]

type breadcrumbs = Features.Breadcrumb.t list [@@deriving show, sexp]

let _ = show_breadcrumbs (* unused but derived *)

type taint_annotation =
  | Sink of { sink: Sinks.t; breadcrumbs: breadcrumbs }
  | Source of { source: Sources.t; breadcrumbs: breadcrumbs }
  | Tito of { tito: Sinks.t; breadcrumbs: breadcrumbs }
  | SkipAnalysis (* Don't analyze methods with SkipAnalysis *)
  | Sanitize (* Don't propagate inferred model of methods with Sanitize *)
[@@deriving sexp]

exception InvalidModel of string

let raise_invalid_model message = raise (InvalidModel message)

let add_breadcrumbs breadcrumbs init =
  List.fold
    breadcrumbs
    ~f:(fun set breadcrumb -> Features.Simple.Breadcrumb breadcrumb :: set)
    ~init


let introduce_sink_taint
    ~root
    ({ TaintResult.backward = { sink_taint; _ }; _ } as taint)
    taint_sink_kind
    breadcrumbs
  =
  let backward =
    let assign_backward_taint environment taint =
      BackwardState.assign ~weak:true ~root ~path:[] taint environment
    in
    match taint_sink_kind with
    | Sinks.LocalReturn -> raise_invalid_model "Invalid TaintSink annotation `LocalReturn`"
    | _ ->
        let leaf_taint =
          BackwardTaint.singleton taint_sink_kind
          |> BackwardTaint.transform
               BackwardTaint.simple_feature_set
               ~f:(add_breadcrumbs breadcrumbs)
          |> BackwardState.Tree.create_leaf
        in
        let sink_taint = assign_backward_taint sink_taint leaf_taint in
        { taint.backward with sink_taint }
  in
  { taint with backward }


let introduce_taint_in_taint_out
    ~root
    ({ TaintResult.backward = { taint_in_taint_out; _ }; _ } as taint)
    taint_sink_kind
    breadcrumbs
  =
  let backward =
    let assign_backward_taint environment taint =
      BackwardState.assign ~weak:true ~root ~path:[] taint environment
    in
    match taint_sink_kind with
    | Sinks.LocalReturn ->
        let return_taint =
          Domains.local_return_taint
          |> BackwardTaint.transform
               BackwardTaint.simple_feature_set
               ~f:(add_breadcrumbs breadcrumbs)
          |> BackwardState.Tree.create_leaf
        in
        let taint_in_taint_out = assign_backward_taint taint_in_taint_out return_taint in
        { taint.backward with taint_in_taint_out }
    | Sinks.ParameterUpdate _ ->
        let update_taint =
          BackwardTaint.singleton taint_sink_kind
          |> BackwardTaint.transform
               BackwardTaint.simple_feature_set
               ~f:(add_breadcrumbs breadcrumbs)
          |> BackwardState.Tree.create_leaf
        in
        let taint_in_taint_out = assign_backward_taint taint_in_taint_out update_taint in
        { taint.backward with taint_in_taint_out }
    | _ ->
        Format.asprintf "Invalid TaintInTaintOut annotation `%s`" (Sinks.show taint_sink_kind)
        |> raise_invalid_model
  in
  { taint with backward }


let introduce_source_taint
    ~root
    ({ TaintResult.forward = { source_taint }; _ } as taint)
    taint_source_kind
    breadcrumbs
  =
  let source_taint =
    let leaf_taint =
      ForwardTaint.singleton taint_source_kind
      |> ForwardTaint.transform ForwardTaint.simple_feature_set ~f:(add_breadcrumbs breadcrumbs)
      |> ForwardState.Tree.create_leaf
    in
    ForwardState.assign ~weak:true ~root ~path:[] leaf_taint source_taint
  in
  { taint with forward = { source_taint } }


type leaf_kind =
  | Leaf of string
  | Breadcrumbs of breadcrumbs

let rec parse_annotations ~configuration ~parameters annotation =
  let get_parameter_position name =
    let matches_parameter_name index { Node.value = parameter; _ } =
      if parameter.Parameter.name = name then
        Some index
      else
        None
    in
    match List.find_mapi parameters ~f:matches_parameter_name with
    | Some index -> index
    | None -> raise_invalid_model (Format.sprintf "No such parameter `%s`" name)
  in
  let rec extract_breadcrumbs expression =
    let open Configuration in
    match expression.Node.value with
    | Name (Name.Identifier breadcrumb) ->
        [Features.Breadcrumb.simple_via ~allowed:configuration.features breadcrumb]
    | Tuple expressions -> List.concat_map ~f:extract_breadcrumbs expressions
    | _ -> []
  in
  let rec extract_names expression =
    match expression.Node.value with
    | Name (Name.Identifier name) -> [name]
    | Tuple expressions -> List.concat_map ~f:extract_names expressions
    | _ -> []
  in
  let base_matches expected = function
    | { Node.value =
          Name (Name.Attribute { base = { Node.value = Name (Name.Identifier identifier); _ }; _ })
      ; _
      } ->
        Identifier.equal expected identifier
    | _ -> false
  in
  let rec extract_kinds expression =
    match expression.Node.value with
    | Name (Name.Identifier taint_kind) -> [Leaf taint_kind]
    | Name (Name.Attribute { base; _ }) -> extract_kinds base
    | Call { callee; arguments = { Call.Argument.value = expression; _ } :: _ }
      when base_matches "Via" callee ->
        [Breadcrumbs (extract_breadcrumbs expression)]
    | Call { callee; arguments = { Call.Argument.value = expression; _ } :: _ }
      when base_matches "Updates" callee ->
        extract_names expression
        |> List.map ~f:(fun name ->
               Leaf (Format.sprintf "ParameterUpdate%d" (get_parameter_position name)))
    | Call { callee; _ } -> extract_kinds callee
    | Tuple expressions -> List.concat_map ~f:extract_kinds expressions
    | _ -> []
  in
  let extract_leafs expression =
    let kinds, breadcrumbs =
      extract_kinds expression
      |> List.partition_map ~f:(function
             | Leaf l -> `Fst l
             | Breadcrumbs b -> `Snd b)
    in
    kinds, List.concat breadcrumbs
  in
  let get_source_kinds expression =
    let open Configuration in
    let kinds, breadcrumbs = extract_leafs expression in
    List.map kinds ~f:(fun kind ->
        Source { source = Sources.parse ~allowed:configuration.sources kind; breadcrumbs })
  in
  let get_sink_kinds expression =
    let open Configuration in
    let kinds, breadcrumbs = extract_leafs expression in
    List.map kinds ~f:(fun kind ->
        Sink { sink = Sinks.parse ~allowed:configuration.sinks kind; breadcrumbs })
  in
  let get_taint_in_taint_out expression =
    let open Configuration in
    let kinds, breadcrumbs = extract_leafs expression in
    match kinds with
    | [] -> [Tito { tito = Sinks.LocalReturn; breadcrumbs }]
    | _ ->
        List.map kinds ~f:(fun kind ->
            Tito { tito = Sinks.parse ~allowed:configuration.sinks kind; breadcrumbs })
  in
  match annotation with
  | Some ({ Node.value; _ } as expression) ->
      let rec parse_annotation = function
        | Call
            { callee;
              arguments = { Call.Argument.value = { value = Tuple expressions; _ }; _ } :: _
            }
          when base_matches "Union" callee ->
            List.concat_map expressions ~f:(fun expression ->
                parse_annotations ~configuration ~parameters (Some expression))
        | Call { callee; arguments = { Call.Argument.value = expression; _ } :: _ }
          when base_matches "TaintSink" callee ->
            get_sink_kinds expression
        | Call { callee; arguments = { Call.Argument.value = expression; _ } :: _ }
          when base_matches "TaintSource" callee ->
            get_source_kinds expression
        | Call { callee; arguments = { Call.Argument.value = expression; _ } :: _ }
          when base_matches "TaintInTaintOut" callee ->
            get_taint_in_taint_out expression
        | Name (Name.Identifier "TaintInTaintOut") ->
            [Tito { tito = Sinks.LocalReturn; breadcrumbs = [] }]
        | Name (Name.Identifier "SkipAnalysis") -> [SkipAnalysis]
        | Name (Name.Identifier "Sanitize") -> [Sanitize]
        | _ ->
            Format.asprintf "Unrecognized taint annotation `%s`" (Expression.show expression)
            |> raise_invalid_model
      in
      parse_annotation value
  | None -> []


let taint_parameter ~configuration ~parameters model (root, _name, parameter) =
  let add_to_model model annotation =
    match annotation with
    | Sink { sink; breadcrumbs } -> introduce_sink_taint ~root model sink breadcrumbs
    | Source { source; breadcrumbs } -> introduce_source_taint ~root model source breadcrumbs
    | Tito { tito; breadcrumbs } -> introduce_taint_in_taint_out ~root model tito breadcrumbs
    | SkipAnalysis -> raise_invalid_model "SkipAnalysis annotation must be in return position"
    | Sanitize -> raise_invalid_model "Sanitize annotation must be in return position"
  in
  let annotation = parameter.Node.value.Parameter.annotation in
  parse_annotations ~configuration ~parameters annotation |> List.fold ~init:model ~f:add_to_model


let taint_return ~configuration ~parameters model expression =
  let add_to_model model annotation =
    let root = AccessPath.Root.LocalResult in
    match annotation with
    | Sink { sink; breadcrumbs } -> introduce_sink_taint ~root model sink breadcrumbs
    | Source { source; breadcrumbs } -> introduce_source_taint ~root model source breadcrumbs
    | Tito _ -> raise_invalid_model "Invalid return annotation: TaintInTaintOut"
    | SkipAnalysis -> { model with mode = TaintResult.SkipAnalysis }
    | Sanitize -> { model with mode = TaintResult.Sanitize }
  in
  parse_annotations ~configuration ~parameters expression |> List.fold ~init:model ~f:add_to_model


let create ~resolution ?(verify = true) ~configuration source =
  let signatures =
    let filter_define_signature = function
      | { Node.value = Define { signature = { name; _ } as signature; _ }; _ } ->
          let class_candidate =
            Reference.prefix name
            >>| Resolution.parse_reference resolution
            >>= Resolution.class_definition resolution
          in
          let call_target =
            match class_candidate with
            | Some _ -> Callable.create_method name
            | None -> Callable.create_function name
          in
          [signature, call_target]
      | { Node.value = Class { Class.name; bases; _ }; _ } ->
          let sink_annotation =
            let class_sink_base { Call.Argument.value; _ } =
              if Expression.show value |> String.is_prefix ~prefix:"TaintSink[" then
                Some value
              else
                None
            in
            List.find_map bases ~f:class_sink_base
          in
          let source_annotation =
            let class_source_base { Call.Argument.value; _ } =
              if Expression.show value |> String.is_prefix ~prefix:"TaintSource[" then
                Some value
              else
                None
            in
            List.find_map bases ~f:class_source_base
          in
          if Option.is_some sink_annotation || Option.is_some source_annotation then
            Resolution.class_definition resolution (Type.Primitive (Reference.show name))
            >>| (fun { Node.value = { Class.body; _ }; _ } ->
                  let signature { Node.value; _ } =
                    match value with
                    | Define { Define.signature = { Define.name; parameters; _ } as signature; _ }
                      ->
                        let signature =
                          let parameters =
                            let sink_parameter parameter =
                              let update_annotation parameter =
                                { parameter with Parameter.annotation = sink_annotation }
                              in
                              Node.map parameter ~f:update_annotation
                            in
                            List.map parameters ~f:sink_parameter
                          in
                          { signature with
                            Define.parameters;
                            return_annotation = source_annotation
                          }
                        in
                        Some (signature, Callable.create_method name)
                    | _ -> None
                  in
                  List.filter_map body ~f:signature)
            |> Option.value ~default:[]
          else
            []
      | { Node.value =
            Assign
              { Assign.target = { Node.value = Name name; _ }; annotation = Some annotation; _ }
        ; _
        }
        when Expression.is_simple_name name
             && Expression.show annotation |> String.is_prefix ~prefix:"TaintSource[" ->
          let name = Reference.from_name_exn name in
          let signature =
            { Define.name;
              parameters = [];
              decorators = [];
              docstring = None;
              return_annotation = Some annotation;
              async = false;
              parent = None
            }
          in
          [signature, Callable.create_object name]
      | { Node.value =
            Assign
              { Assign.target = { Node.value = Name name; _ }; annotation = Some annotation; _ }
        ; _
        }
        when Expression.is_simple_name name
             && Expression.show annotation |> String.is_prefix ~prefix:"TaintSink[" ->
          let name = Reference.from_name_exn name in
          let signature =
            { Define.name;
              parameters =
                [Parameter.create ~location:Location.Reference.any ~annotation ~name:"$global" ()];
              decorators = [];
              docstring = None;
              return_annotation = None;
              async = false;
              parent = None
            }
          in
          [signature, Callable.create_object name]
      | _ -> []
    in
    String.split ~on:'\n' source
    |> Parser.parse
    |> Source.create
    |> Source.statements
    |> List.concat_map ~f:filter_define_signature
  in
  let create_model ({ Define.name; parameters; return_annotation; _ }, call_target) =
    (* Make sure we know about what we model. *)
    try
      let call_target = (call_target :> Callable.t) in
      let annotation = Resolution.resolve_reference resolution name in
      let () =
        if Type.is_top annotation && not (Resolution.is_global resolution ~reference:name) then
          raise_invalid_model "Modeled entity is not part of the environment!"
      in
      let normalized = AccessPath.Root.normalize_parameters parameters in
      (* Check model matches callables primary signature. *)
      let () =
        match verify, annotation with
        | ( true,
            ( Type.Callable
                { Type.Callable.implementation =
                    { Type.Callable.parameters = Type.Callable.Defined implementation_parameters
                    ; _
                    };
                  implicit
                ; _
                } as callable ) ) ->
            let model_compatible =
              let optional_parameters =
                List.count implementation_parameters ~f:Type.Callable.Parameter.default
              in
              let self_length = if Option.is_some implicit then 1 else 0 in
              let required_parameters =
                self_length + List.length implementation_parameters - optional_parameters
              in
              let normalized = List.length normalized in
              normalized >= required_parameters
              && normalized <= required_parameters + optional_parameters
            in
            if not model_compatible then (
              let message =
                Format.asprintf
                  "Model signature parameters do not match implementation `%a`"
                  Type.pp
                  callable
              in
              Log.error "%s" message;
              raise_invalid_model message )
        | _ -> ()
      in
      normalized
      |> List.fold ~init:TaintResult.empty_model ~f:(taint_parameter ~configuration ~parameters)
      |> (fun model -> taint_return ~configuration ~parameters model return_annotation)
      |> fun model -> { model; call_target; is_obscure = false }
    with
    | Failure message
    | InvalidModel message ->
        Format.asprintf "Invalid model for `%a`: %s" Reference.pp name message
        |> raise_invalid_model
  in
  List.map signatures ~f:create_model


let get_callsite_model ~call_target =
  let call_target = (call_target :> Callable.t) in
  match Interprocedural.Fixpoint.get_model call_target with
  | None -> { is_obscure = true; call_target; model = TaintResult.empty_model }
  | Some model ->
      let strip_for_call_site model = model in
      let taint_model =
        Interprocedural.Result.get_model TaintResult.kind model
        |> Option.value ~default:TaintResult.empty_model
        |> strip_for_call_site
      in
      { is_obscure = model.is_obscure; call_target; model = taint_model }


let get_global_model ~resolution ~expression =
  let call_target =
    match Node.value expression, AccessPath.get_global ~resolution expression with
    | _, Some global -> Some global
    | Name (Name.Attribute { base; attribute; _ }), _ ->
        Resolution.resolve resolution base
        |> Type.class_name
        |> (fun class_name -> Reference.create ~prefix:class_name attribute)
        |> Option.some
    | _ -> None
  in
  call_target >>| Callable.create_object >>| fun call_target -> get_callsite_model ~call_target


let parse ~resolution ~source ~configuration models =
  create ~resolution ~configuration source
  |> List.map ~f:(fun model -> model.call_target, model.model)
  |> Callable.Map.of_alist_reduce ~f:(join ~iteration:0)
  |> Callable.Map.merge models ~f:(fun ~key:_ ->
       function
       | `Both (a, b) -> Some (join ~iteration:0 a b)
       | `Left model
       | `Right model ->
           Some model)
