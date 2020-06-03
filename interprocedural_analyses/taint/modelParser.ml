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
open Model

let raise_invalid_model message = raise (Model.InvalidModel message)

type breadcrumbs = Features.Simple.t list [@@deriving show]

let _ = show_breadcrumbs (* unused but derived *)

let add_breadcrumbs breadcrumbs init = List.rev_append breadcrumbs init

module DefinitionsCache (Type : sig
  type t
end) =
struct
  let cache : Type.t Reference.Table.t = Reference.Table.create ()

  let set key value = Hashtbl.set cache ~key ~data:value

  let get = Hashtbl.find cache

  let invalidate () = Hashtbl.clear cache
end

module ClassDefinitionsCache = DefinitionsCache (struct
  type t = Class.t Node.t list option
end)

let containing_source resolution reference =
  let ast_environment = GlobalResolution.ast_environment resolution in
  let rec qualifier ~lead ~tail =
    match tail with
    | head :: (_ :: _ as tail) ->
        let new_lead = Reference.create ~prefix:lead head in
        if not (GlobalResolution.module_exists resolution new_lead) then
          lead
        else
          qualifier ~lead:new_lead ~tail
    | _ -> lead
  in
  qualifier ~lead:Reference.empty ~tail:(Reference.as_list reference)
  |> AstEnvironment.ReadOnly.get_processed_source ast_environment


let class_definitions resolution reference =
  match ClassDefinitionsCache.get reference with
  | Some result -> result
  | None ->
      let result =
        containing_source resolution reference
        >>| Preprocessing.classes
        >>| List.filter ~f:(fun { Node.value = { Class.name; _ }; _ } ->
                Reference.equal reference (Node.value name))
        (* Prefer earlier definitions. *)
        >>| List.rev
      in
      ClassDefinitionsCache.set reference result;
      result


module T = struct
  type parse_result = {
    models: TaintResult.call_model Interprocedural.Callable.Map.t;
    errors: string list;
  }
end

include T

type leaf_kind =
  | Leaf of string
  | Breadcrumbs of breadcrumbs

type taint_annotation =
  | Sink of {
      sink: Sinks.t;
      breadcrumbs: breadcrumbs;
      path: Abstract.TreeDomain.Label.path;
      leaf_name_provided: bool;
    }
  | Source of {
      source: Sources.t;
      breadcrumbs: breadcrumbs;
      path: Abstract.TreeDomain.Label.path;
      leaf_name_provided: bool;
    }
  | Tito of {
      tito: Sinks.t;
      breadcrumbs: breadcrumbs;
      path: Abstract.TreeDomain.Label.path;
    }
  | AddFeatureToArgument of {
      breadcrumbs: breadcrumbs;
      path: Abstract.TreeDomain.Label.path;
    }
  | SkipAnalysis (* Don't analyze methods with SkipAnalysis *)
  | Sanitize

(* Don't propagate inferred model of methods with Sanitize *)

let decorators = String.Set.union Recognized.property_decorators Recognized.classproperty_decorators

let is_property define = String.Set.exists decorators ~f:(Define.has_decorator define)

let signature_is_property signature =
  String.Set.exists decorators ~f:(Define.Signature.has_decorator signature)


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
    | Expression.Name (Name.Identifier breadcrumb) ->
        [Features.simple_via ~allowed:configuration.features breadcrumb]
    | Tuple expressions -> List.concat_map ~f:extract_breadcrumbs expressions
    | _ ->
        Format.sprintf
          "Invalid expression for breadcrumb: %s"
          (show_expression expression.Node.value)
        |> failwith
  in
  let rec extract_via_value_of expression =
    match expression.Node.value with
    | Expression.Name (Name.Identifier name) ->
        [Features.Simple.ViaValueOf { position = get_parameter_position name }]
    | Tuple expressions -> List.concat_map ~f:extract_via_value_of expressions
    | _ ->
        Format.sprintf
          "Invalid expression for ViaValueOf: %s"
          (show_expression expression.Node.value)
        |> failwith
  in
  let rec extract_names expression =
    match expression.Node.value with
    | Expression.Name (Name.Identifier name) -> [name]
    | Tuple expressions -> List.concat_map ~f:extract_names expressions
    | _ ->
        Format.sprintf "Invalid expression name: %s" (show_expression expression.Node.value)
        |> failwith
  in
  let base_name = function
    | {
        Node.value =
          Expression.Name
            (Name.Attribute { base = { Node.value = Name (Name.Identifier identifier); _ }; _ });
        _;
      } ->
        Some identifier
    | _ -> None
  in
  let rec extract_kinds expression =
    match expression.Node.value with
    | Expression.Name (Name.Identifier taint_kind) -> [Leaf taint_kind]
    | Name (Name.Attribute { base; _ }) -> extract_kinds base
    | Call { callee; arguments = { Call.Argument.value = expression; _ } :: _ } -> (
        match base_name callee with
        | Some "Via" -> [Breadcrumbs (extract_breadcrumbs expression)]
        | Some "ViaValueOf" -> [Breadcrumbs (extract_via_value_of expression)]
        | Some "Updates" ->
            extract_names expression
            |> List.map ~f:(fun name ->
                   Leaf (Format.sprintf "ParameterUpdate%d" (get_parameter_position name)))
        | _ -> extract_kinds callee )
    | Call { callee; _ } -> extract_kinds callee
    | Tuple expressions -> List.concat_map ~f:extract_kinds expressions
    | _ ->
        Format.sprintf
          "Invalid expression for taint kind: %s"
          (show_expression expression.Node.value)
        |> failwith
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
        Source
          {
            source = Sources.parse ~allowed:configuration.sources kind;
            breadcrumbs;
            path = [];
            leaf_name_provided = false;
          })
  in
  let get_sink_kinds expression =
    let open Configuration in
    let kinds, breadcrumbs = extract_leafs expression in
    List.map kinds ~f:(fun kind ->
        Sink
          {
            sink = Sinks.parse ~allowed:configuration.sinks kind;
            breadcrumbs;
            path = [];
            leaf_name_provided = false;
          })
  in
  let get_taint_in_taint_out expression =
    let open Configuration in
    let kinds, breadcrumbs = extract_leafs expression in
    match kinds with
    | [] -> [Tito { tito = Sinks.LocalReturn; breadcrumbs; path = [] }]
    | _ ->
        List.map kinds ~f:(fun kind ->
            Tito { tito = Sinks.parse ~allowed:configuration.sinks kind; breadcrumbs; path = [] })
  in
  let extract_attach_features ~name expression =
    let keep_features = function
      | Breadcrumbs breadcrumbs -> Some breadcrumbs
      | _ -> None
    in
    (* Ensure AttachToX annotations don't have any non-Via annotations for now. *)
    extract_kinds expression
    |> List.map ~f:keep_features
    |> Option.all
    >>| List.concat
    |> function
    | Some features -> features
    | None ->
        raise_invalid_model
          (Format.sprintf "All parameters to `%s` must be of the form `Via[feature]`." name)
  in
  match annotation with
  | Some ({ Node.value; _ } as expression) ->
      let raise_invalid_annotation () =
        Format.asprintf "Unrecognized taint annotation `%s`" (Expression.show expression)
        |> raise_invalid_model
      in
      let rec parse_annotation = function
        | Expression.Call
            {
              callee;
              arguments =
                {
                  Call.Argument.value =
                    {
                      Node.value =
                        Expression.Tuple [{ Node.value = index; _ }; { Node.value = expression; _ }];
                      _;
                    };
                  _;
                }
                :: _;
            }
        | Call
            {
              callee;
              arguments =
                [
                  { Call.Argument.value = { Node.value = index; _ }; _ };
                  { Call.Argument.value = { Node.value = expression; _ }; _ };
                ];
            }
          when base_name callee = Some "AppliesTo" ->
            let extend_path annotation =
              let field =
                match index with
                | Expression.Integer index -> Abstract.TreeDomain.Label.create_int_field index
                | Expression.String { StringLiteral.value = index; _ } ->
                    Abstract.TreeDomain.Label.create_name_field index
                | _ ->
                    raise_invalid_model
                      "Expected either integer or string as index in AppliesTo annotation."
              in
              match annotation with
              | Sink ({ path; _ } as sink) -> Sink { sink with path = field :: path }
              | Source ({ path; _ } as source) -> Source { source with path = field :: path }
              | Tito ({ path; _ } as tito) -> Tito { tito with path = field :: path }
              | AddFeatureToArgument ({ path; _ } as add_feature_to_argument) ->
                  AddFeatureToArgument { add_feature_to_argument with path = field :: path }
              | SkipAnalysis
              | Sanitize ->
                  annotation
            in
            parse_annotation expression |> List.map ~f:extend_path
        | Call { callee; arguments } when base_name callee = Some "CrossRepositoryTaint" -> (
            match arguments with
            | [
             {
               Call.Argument.value =
                 {
                   Node.value =
                     Expression.Tuple
                       [
                         { Node.value = taint; _ };
                         {
                           Node.value =
                             Expression.String { StringLiteral.value = canonical_name; _ };
                           _;
                         };
                         {
                           Node.value =
                             Expression.String { StringLiteral.value = canonical_port; _ };
                           _;
                         };
                         { Node.value = Expression.Integer producer_id; _ };
                       ];
                   _;
                 };
               _;
             };
            ] ->
                let add_cross_repository_information annotation =
                  let leaf_name =
                    Features.Simple.LeafName
                      {
                        leaf = Format.sprintf "producer:%d:%s" producer_id canonical_name;
                        port = Some canonical_port;
                      }
                  in
                  match annotation with
                  | Source source ->
                      Source
                        {
                          source with
                          breadcrumbs = leaf_name :: source.breadcrumbs;
                          leaf_name_provided = true;
                        }
                  | Sink sink ->
                      Sink
                        {
                          sink with
                          breadcrumbs = leaf_name :: sink.breadcrumbs;
                          leaf_name_provided = true;
                        }
                  | _ -> annotation
                in
                parse_annotation taint |> List.map ~f:add_cross_repository_information
            | _ ->
                raise_invalid_model
                  "Cross repository taint must be of the form CrossRepositoryTaint[taint, \
                   canonical_name, canonical_port, producer_id]." )
        | Call
            {
              callee;
              arguments = { Call.Argument.value = { value = Tuple expressions; _ }; _ } :: _;
            }
          when base_name callee = Some "Union" ->
            List.concat_map expressions ~f:(fun expression ->
                parse_annotations ~configuration ~parameters (Some expression))
        | Call { callee; arguments = { Call.Argument.value = expression; _ } :: _ } -> (
            match base_name callee with
            | Some "TaintSink" -> get_sink_kinds expression
            | Some "TaintSource" -> get_source_kinds expression
            | Some "TaintInTaintOut" -> get_taint_in_taint_out expression
            | Some "AddFeatureToArgument" ->
                let _, breadcrumbs = extract_leafs expression in
                [AddFeatureToArgument { breadcrumbs; path = [] }]
            | Some "AttachToSink" ->
                [
                  Sink
                    {
                      sink = Sinks.Attach;
                      breadcrumbs = extract_attach_features ~name:"AttachToSink" expression;
                      path = [];
                      leaf_name_provided = false;
                    };
                ]
            | Some "AttachToTito" ->
                [
                  Tito
                    {
                      tito = Sinks.Attach;
                      breadcrumbs = extract_attach_features ~name:"AttachToTito" expression;
                      path = [];
                    };
                ]
            | Some "AttachToSource" ->
                [
                  Source
                    {
                      source = Sources.Attach;
                      breadcrumbs = extract_attach_features ~name:"AttachToSource" expression;
                      path = [];
                      leaf_name_provided = false;
                    };
                ]
            | Some "PartialSink" ->
                let kind, label =
                  match Node.value expression with
                  | Call
                      {
                        callee =
                          {
                            Node.value =
                              Name
                                (Name.Attribute
                                  {
                                    base =
                                      { Node.value = Expression.Name (Name.Identifier kind); _ };
                                    attribute = "__getitem__";
                                    _;
                                  });
                            _;
                          };
                        arguments =
                          {
                            Call.Argument.value = { Node.value = Name (Name.Identifier label); _ };
                            _;
                          }
                          :: _;
                      } ->
                      if not (List.mem configuration.sinks kind ~equal:String.equal) then
                        Format.asprintf "Unrecognized sink for partial sink: `%s`." kind
                        |> raise_invalid_model;
                      if not (String.Map.Tree.mem configuration.acceptable_sink_labels kind) then
                        raise_invalid_model (Format.asprintf "No labels specified for `%s`" kind);
                      let label_options =
                        String.Map.Tree.find_exn configuration.acceptable_sink_labels kind
                      in
                      if not (List.mem label_options label ~equal:String.equal) then
                        Format.asprintf
                          "Unrecognized label `%s` for partial sink `%s` (choices: `%s`)"
                          label
                          kind
                          (String.concat label_options ~sep:", ")
                        |> raise_invalid_model;
                      kind, label
                  | _ -> raise_invalid_annotation ()
                in
                [
                  Sink
                    {
                      sink = Sinks.PartialSink { kind; label };
                      breadcrumbs = [];
                      path = [];
                      leaf_name_provided = false;
                    };
                ]
            | _ -> raise_invalid_annotation () )
        | Name (Name.Identifier "TaintInTaintOut") ->
            [Tito { tito = Sinks.LocalReturn; breadcrumbs = []; path = [] }]
        | Name (Name.Identifier "SkipAnalysis") -> [SkipAnalysis]
        | Name (Name.Identifier "Sanitize") -> [Sanitize]
        | _ -> raise_invalid_annotation ()
      in
      parse_annotation value
  | None -> []


let introduce_sink_taint
    ~root
    ~sinks_to_keep
    ~path
    ~leaf_name_provided
    ({ TaintResult.backward = { sink_taint; _ }; _ } as taint)
    taint_sink_kind
    breadcrumbs
  =
  let should_keep_taint =
    match sinks_to_keep with
    | None -> true
    | Some sinks_to_keep -> Core.Set.mem sinks_to_keep taint_sink_kind
  in
  if should_keep_taint then
    let backward =
      let assign_backward_taint environment taint =
        BackwardState.assign ~weak:true ~root ~path taint environment
      in
      match taint_sink_kind with
      | Sinks.LocalReturn -> raise_invalid_model "Invalid TaintSink annotation `LocalReturn`"
      | _ ->
          let transform_trace_information taint =
            if leaf_name_provided then
              BackwardTaint.transform
                BackwardTaint.trace_info
                Abstract.Domain.(
                  Map
                    (function
                    | TraceInfo.Declaration _ -> TraceInfo.Declaration { leaf_name_provided = true }
                    | trace_info -> trace_info))
                taint
            else
              taint
          in
          let leaf_taint =
            BackwardTaint.singleton taint_sink_kind
            |> BackwardTaint.transform
                 BackwardTaint.simple_feature_set
                 Abstract.Domain.(Map (add_breadcrumbs breadcrumbs))
            |> transform_trace_information
            |> BackwardState.Tree.create_leaf
          in
          let sink_taint = assign_backward_taint sink_taint leaf_taint in
          { taint.backward with sink_taint }
    in
    { taint with backward }
  else
    taint


let introduce_taint_in_taint_out
    ~root
    ~path
    ({ TaintResult.backward = { taint_in_taint_out; _ }; _ } as taint)
    taint_sink_kind
    breadcrumbs
  =
  let backward =
    let assign_backward_taint environment taint =
      BackwardState.assign ~weak:true ~root ~path taint environment
    in
    match taint_sink_kind with
    | Sinks.LocalReturn ->
        let return_taint =
          Domains.local_return_taint
          |> BackwardTaint.transform
               BackwardTaint.simple_feature_set
               Abstract.Domain.(Map (add_breadcrumbs breadcrumbs))
          |> BackwardState.Tree.create_leaf
        in
        let taint_in_taint_out = assign_backward_taint taint_in_taint_out return_taint in
        { taint.backward with taint_in_taint_out }
    | Sinks.Attach when List.is_empty breadcrumbs ->
        raise_invalid_model "`Attach` must be accompanied by a list of features to attach."
    | Sinks.ParameterUpdate _
    | Sinks.Attach ->
        let update_taint =
          BackwardTaint.singleton taint_sink_kind
          |> BackwardTaint.transform
               BackwardTaint.simple_feature_set
               Abstract.Domain.(Map (add_breadcrumbs breadcrumbs))
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
    ~sources_to_keep
    ~path
    ~leaf_name_provided
    ({ TaintResult.forward = { source_taint }; _ } as taint)
    taint_source_kind
    breadcrumbs
  =
  let should_keep_taint =
    match sources_to_keep with
    | None -> true
    | Some sources_to_keep -> Core.Set.mem sources_to_keep taint_source_kind
  in
  if Sources.equal taint_source_kind Sources.Attach && List.is_empty breadcrumbs then
    raise_invalid_model "`Attach` must be accompanied by a list of features to attach.";
  if should_keep_taint then
    let source_taint =
      let transform_trace_information taint =
        if leaf_name_provided then
          ForwardTaint.transform
            ForwardTaint.trace_info
            Abstract.Domain.(
              Map
                (function
                | TraceInfo.Declaration _ -> TraceInfo.Declaration { leaf_name_provided = true }
                | trace_info -> trace_info))
            taint
        else
          taint
      in

      let leaf_taint =
        ForwardTaint.singleton taint_source_kind
        |> ForwardTaint.transform
             ForwardTaint.simple_feature_set
             Abstract.Domain.(Map (add_breadcrumbs breadcrumbs))
        |> transform_trace_information
        |> ForwardState.Tree.create_leaf
      in
      ForwardState.assign ~weak:true ~root ~path leaf_taint source_taint
    in
    { taint with forward = { source_taint } }
  else
    taint


let find_positional_parameter_annotation position parameters =
  List.nth parameters position >>= Type.Record.Callable.RecordParameter.annotation


let find_named_parameter_annotation search_name parameters =
  let has_name = function
    | Type.Record.Callable.RecordParameter.KeywordOnly { name; _ } ->
        name = "$parameter$" ^ search_name
    | Type.Record.Callable.RecordParameter.Named { name; _ } -> name = search_name
    | _ -> false
  in
  List.find ~f:has_name parameters >>= Type.Record.Callable.RecordParameter.annotation


let add_signature_based_breadcrumbs ~resolution root ~callable_annotation breadcrumbs =
  match root, callable_annotation with
  | ( AccessPath.Root.PositionalParameter { position; _ },
      Some
        {
          Type.Callable.implementation =
            { Type.Callable.parameters = Type.Callable.Defined implementation_parameters; _ };
          _;
        } ) ->
      let parameter_annotation =
        find_positional_parameter_annotation position implementation_parameters
      in
      Features.add_type_breadcrumb ~resolution parameter_annotation breadcrumbs
  | ( AccessPath.Root.NamedParameter { name; _ },
      Some
        {
          Type.Callable.implementation =
            { Type.Callable.parameters = Type.Callable.Defined implementation_parameters; _ };
          _;
        } ) ->
      let parameter_annotation = find_named_parameter_annotation name implementation_parameters in
      Features.add_type_breadcrumb ~resolution parameter_annotation breadcrumbs
  | ( AccessPath.Root.LocalResult,
      Some { Type.Callable.implementation = { Type.Callable.annotation; _ }; _ } ) ->
      Features.add_type_breadcrumb ~resolution (Some annotation) breadcrumbs
  | _ -> breadcrumbs


let taint_parameter
    ~configuration
    ~resolution
    ~parameters
    model
    (root, _name, parameter)
    ~callable_annotation
    ~sources_to_keep
    ~sinks_to_keep
  =
  let add_to_model model annotation =
    match annotation with
    | Sink { sink; breadcrumbs; path; leaf_name_provided } ->
        List.map ~f:Features.SimpleSet.inject breadcrumbs
        |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
        |> introduce_sink_taint ~root ~path ~leaf_name_provided ~sinks_to_keep model sink
    | Source { source; breadcrumbs; path; leaf_name_provided } ->
        List.map ~f:Features.SimpleSet.inject breadcrumbs
        |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
        |> introduce_source_taint ~root ~path ~leaf_name_provided ~sources_to_keep model source
    | Tito { tito; breadcrumbs; path } ->
        (* For tito, both the parameter and the return type can provide type based breadcrumbs *)
        List.map ~f:Features.SimpleSet.inject breadcrumbs
        |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
        |> add_signature_based_breadcrumbs
             ~resolution
             AccessPath.Root.LocalResult
             ~callable_annotation
        |> introduce_taint_in_taint_out ~root ~path model tito
    | AddFeatureToArgument { breadcrumbs; path } ->
        List.map ~f:Features.SimpleSet.inject breadcrumbs
        |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
        |> introduce_sink_taint
             ~root
             ~path
             ~leaf_name_provided:false
             ~sinks_to_keep
             model
             Sinks.AddFeatureToArgument
    | SkipAnalysis -> raise_invalid_model "SkipAnalysis annotation must be in return position"
    | Sanitize -> raise_invalid_model "Sanitize annotation must be in return position"
  in
  let annotation = parameter.Node.value.Parameter.annotation in
  parse_annotations ~configuration ~parameters annotation |> List.fold ~init:model ~f:add_to_model


let taint_return
    ~configuration
    ~resolution
    ~parameters
    model
    expression
    ~callable_annotation
    ~sources_to_keep
    ~sinks_to_keep
  =
  let add_to_model model annotation =
    let root = AccessPath.Root.LocalResult in
    match annotation with
    | Sink { sink; breadcrumbs; path; leaf_name_provided } ->
        List.map ~f:Features.SimpleSet.inject breadcrumbs
        |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
        |> introduce_sink_taint ~root ~path ~leaf_name_provided ~sinks_to_keep model sink
    | Source { source; breadcrumbs; path; leaf_name_provided } ->
        List.map ~f:Features.SimpleSet.inject breadcrumbs
        |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
        |> introduce_source_taint ~root ~path ~leaf_name_provided ~sources_to_keep model source
    | Tito _ -> raise_invalid_model "Invalid return annotation: TaintInTaintOut"
    | AddFeatureToArgument _ ->
        raise_invalid_model "Invalid return annotation: AddFeatureToArgument"
    | SkipAnalysis -> { model with mode = TaintResult.SkipAnalysis }
    | Sanitize -> { model with mode = TaintResult.Sanitize }
  in
  parse_annotations ~configuration ~parameters expression |> List.fold ~init:model ~f:add_to_model


let create ~resolution ?path ~configuration ~rule_filter source =
  let sources_to_keep, sinks_to_keep =
    match rule_filter with
    | None -> None, None
    | Some rule_filter ->
        let rule_filter = Int.Set.of_list rule_filter in
        let sources_to_keep, sinks_to_keep =
          let { Configuration.rules; _ } = configuration in
          let rules =
            List.filter_map rules ~f:(fun { Configuration.Rule.code; sources; sinks; _ } ->
                if Core.Set.mem rule_filter code then Some (sources, sinks) else None)
          in
          List.fold
            rules
            ~init:
              ( Sources.Set.singleton Sources.Attach,
                Sinks.Set.of_list [Sinks.AddFeatureToArgument; Sinks.Attach] )
            ~f:(fun (sources, sinks) (rule_sources, rule_sinks) ->
              ( Core.Set.union sources (Sources.Set.of_list rule_sources),
                Core.Set.union sinks (Sinks.Set.of_list rule_sinks) ))
        in
        Some sources_to_keep, Some sinks_to_keep
  in
  let global_resolution = Resolution.global_resolution resolution in
  let invalid_model_error ~location ~name message =
    let model_origin =
      match path with
      | None -> ""
      | Some path ->
          Format.sprintf
            " defined in `%s:%d`"
            (Path.absolute path)
            location.Location.start.Location.line
    in
    let message =
      Format.asprintf "Invalid model for `%a`%s: %s" Reference.pp name model_origin message
    in
    Core.Result.Error message
  in

  let signatures, errors =
    let filter_define_signature = function
      | {
          Node.value =
            Statement.Define { signature = { name = { Node.value = name; _ }; _ } as signature; _ };
          location;
        } ->
          let class_candidate =
            Reference.prefix name
            >>| GlobalResolution.parse_reference global_resolution
            >>= GlobalResolution.class_definition global_resolution
          in
          let call_target =
            match class_candidate with
            | Some _ when Define.Signature.is_property_setter signature ->
                Callable.create_property_setter name
            | Some _ -> Callable.create_method name
            | None -> Callable.create_function name
          in
          Core.Result.Ok [signature, location, call_target]
      | {
          Node.value =
            Class
              {
                Class.name = { Node.value = name; _ };
                bases;
                body =
                  [{ Node.value = Statement.Expression { Node.value = Expression.Ellipsis; _ }; _ }];
                _;
              };
          _;
        } ->
          let sink_annotations =
            let class_sink_base { Call.Argument.value; _ } =
              if Expression.show value |> String.is_prefix ~prefix:"TaintSink[" then
                Some value
              else
                None
            in
            List.filter_map bases ~f:class_sink_base
          in
          let source_annotations =
            let class_source_base { Call.Argument.value; _ } =
              if Expression.show value |> String.is_prefix ~prefix:"TaintSource[" then
                Some value
              else if Expression.show value |> String.equal "SkipAnalysis" then
                Some value
              else
                None
            in
            List.filter_map bases ~f:class_source_base
          in
          if (not (List.is_empty sink_annotations)) || not (List.is_empty source_annotations) then
            class_definitions global_resolution name
            >>= List.hd
            >>| (fun { Node.value = { Class.body; _ }; _ } ->
                  let signature { Node.value; location } =
                    match value with
                    | Statement.Define
                        {
                          Define.signature =
                            {
                              Define.Signature.name = { Node.value = name; _ };
                              parameters;
                              decorators;
                              _;
                            } as signature;
                          _;
                        } ->
                        let signature ~source_annotation ~sink_annotation =
                          let parameters =
                            let sink_parameter parameter =
                              let update_annotation { Parameter.name; value; _ } =
                                let value =
                                  match value with
                                  | None -> None
                                  | Some _ ->
                                      Some (Node.create_with_default_location Expression.Ellipsis)
                                in
                                { Parameter.name; annotation = sink_annotation; value }
                              in
                              Node.map parameter ~f:update_annotation
                            in
                            List.map parameters ~f:sink_parameter
                          in
                          let decorators =
                            if
                              signature_is_property signature
                              || Define.Signature.is_property_setter signature
                            then
                              decorators
                            else
                              []
                          in
                          ( {
                              signature with
                              Define.Signature.parameters;
                              return_annotation = source_annotation;
                              decorators;
                            },
                            location,
                            Callable.create_method name )
                        in
                        let sources =
                          List.map source_annotations ~f:(fun source_annotation ->
                              signature
                                ~source_annotation:(Some source_annotation)
                                ~sink_annotation:None)
                        in
                        let sinks =
                          List.map sink_annotations ~f:(fun sink_annotation ->
                              signature
                                ~source_annotation:None
                                ~sink_annotation:(Some sink_annotation))
                        in
                        sources @ sinks
                    | _ -> []
                  in

                  List.concat_map body ~f:signature)
            |> Option.value ~default:[]
            |> Core.Result.return
          else
            Core.Result.Ok []
      | { Node.value = Class { Class.name = { Node.value = name; _ }; _ }; location } ->
          invalid_model_error ~location ~name "Class model must have a body of `...`."
      | {
          Node.value =
            Assign
              {
                Assign.target = { Node.value = Name name; location = name_location };
                annotation = Some annotation;
                _;
              };
          location;
        }
        when is_simple_name name
             && Expression.show annotation |> String.is_prefix ~prefix:"TaintSource[" ->
          let name = name_to_reference_exn name in
          let signature =
            {
              Define.Signature.name = Node.create ~location:name_location name;
              parameters = [];
              decorators = [];
              return_annotation = Some annotation;
              async = false;
              generator = false;
              parent = None;
              nesting_define = None;
            }
          in
          Core.Result.Ok [signature, location, Callable.create_object name]
      | {
          Node.value =
            Assign
              {
                Assign.target = { Node.value = Name name; location = name_location };
                annotation = Some annotation;
                _;
              };
          location;
        }
        when is_simple_name name
             && Expression.show annotation |> String.is_prefix ~prefix:"TaintSink["
             || Expression.show annotation |> String.is_prefix ~prefix:"TaintInTaintOut[" ->
          let name = name_to_reference_exn name in
          let signature =
            {
              Define.Signature.name = Node.create ~location:name_location name;
              parameters = [Parameter.create ~location:Location.any ~annotation ~name:"$global" ()];
              decorators = [];
              return_annotation = None;
              async = false;
              generator = false;
              parent = None;
              nesting_define = None;
            }
          in
          Core.Result.Ok [signature, location, Callable.create_object name]
      | _ -> Core.Result.Ok []
    in
    String.split ~on:'\n' source
    |> Parser.parse
    |> Source.create
    |> Source.statements
    |> List.map ~f:filter_define_signature
    |> List.partition_result
    |> fun (results, errors) -> List.concat results, errors
  in
  let create_model
      ( ( {
            Define.Signature.name = { Node.value = name; _ };
            parameters;
            return_annotation;
            decorators;
            _;
          } as define ),
        location,
        call_target )
    =
    (* Make sure we know about what we model. *)
    let global_resolution = Resolution.global_resolution resolution in
    try
      let call_target = (call_target :> Callable.t) in
      let callable_annotation =
        (* Since properties and setters share the same undecorated name, we need to special-case
           them. *)
        let global_type () =
          name
          |> from_reference ~location:Location.any
          |> Resolution.resolve_expression_to_annotation resolution
        in
        let parent = Option.value_exn (Reference.prefix name) in
        let get_matching_method ~predicate =
          let get_matching_define = function
            | { Node.value = Statement.Define ({ signature; _ } as define); _ } ->
                if
                  predicate define
                  && Reference.equal (Node.value define.Define.signature.Define.Signature.name) name
                then
                  let parser = GlobalResolution.annotation_parser global_resolution in
                  let variables = GlobalResolution.variables global_resolution in
                  Annotated.Define.Callable.create_overload_without_applying_decorators
                    ~parser
                    ~variables
                    signature
                  |> Type.Callable.create_from_implementation
                  |> Option.some
                else
                  None
            | _ -> None
          in
          class_definitions global_resolution parent
          >>= List.hd
          >>| (fun definition -> definition.Node.value.Class.body)
          >>= List.find_map ~f:get_matching_define
          >>| Annotation.create
          |> function
          | Some annotation -> annotation
          | None -> global_type ()
        in
        if signature_is_property define then
          get_matching_method ~predicate:is_property
        else if Define.Signature.is_property_setter define then
          get_matching_method ~predicate:Define.is_property_setter
        else if not (List.is_empty decorators) then
          (* Ensure that models don't declare decorators that our taint analyses doesn't understand. *)
          raise_invalid_model
            (Format.sprintf
               "Unexpected decorators found when parsing model: `%s`"
               ( List.map decorators ~f:Decorator.to_expression
               |> List.map ~f:Expression.show
               |> String.concat ~sep:", " ))
        else
          global_type ()
      in
      let () =
        if
          Type.is_top (Annotation.annotation callable_annotation)
          (* FIXME: We are relying on the fact that nonexistent functions&attributes resolve to
             mutable callable annotation, while existing ones resolve to immutable callable
             annotation. This is fragile! *)
          && not (Annotation.is_immutable callable_annotation)
        then
          raise_invalid_model "Modeled entity is not part of the environment!"
      in
      (* Check model matches callables primary signature. *)
      let callable_annotation =
        callable_annotation
        |> Annotation.annotation
        |> function
        | Type.Callable t -> Some t
        | Type.Parametric
            { name = "BoundMethod"; parameters = [Type.Parameter.Single (Type.Callable t); _] } ->
            Some t
        | _ -> None
      in
      let normalized_model_parameters =
        let parameters = AccessPath.Root.normalize_parameters parameters in
        (* If there were optional parameters omitted from the model, the positioning will be off in
           the access path conversion. Let's fix the positions after the fact to make sure that our
           models aren't off. *)
        let callable_parameter_names_to_positions =
          match callable_annotation with
          | Some
              {
                Type.Callable.implementation =
                  { Type.Callable.parameters = Type.Callable.Defined parameters; _ };
                _;
              } ->
              let name = function
                | Type.Callable.Parameter.Named { name; _ }
                | Type.Callable.Parameter.KeywordOnly { name; _ } ->
                    Some name
                | _ -> None
              in
              let add_parameter_to_position position map parameter =
                match name parameter with
                | Some name -> Map.set map ~key:(Identifier.sanitized name) ~data:position
                | None -> map
              in
              List.foldi parameters ~f:add_parameter_to_position ~init:String.Map.empty
          | _ -> String.Map.empty
        in
        let adjust_position (root, name, parameter) =
          let root =
            match root with
            | AccessPath.Root.PositionalParameter { position; name } ->
                AccessPath.Root.PositionalParameter
                  {
                    position =
                      Map.find callable_parameter_names_to_positions name
                      |> Option.value ~default:position;
                    name;
                  }
            | _ -> root
          in

          root, name, parameter
        in
        List.map parameters ~f:adjust_position
      in
      let () =
        ModelVerifier.verify_signature ~normalized_model_parameters ~name callable_annotation
      in
      normalized_model_parameters
      |> List.fold
           ~init:TaintResult.empty_model
           ~f:
             (taint_parameter
                ~configuration
                ~resolution:global_resolution
                ~parameters
                ~callable_annotation
                ~sources_to_keep
                ~sinks_to_keep)
      |> (fun model ->
           taint_return
             ~configuration
             ~resolution:global_resolution
             ~parameters
             model
             return_annotation
             ~callable_annotation
             ~sources_to_keep
             ~sinks_to_keep)
      |> fun model -> Core.Result.Ok { model; call_target; is_obscure = false }
    with
    | Failure message
    | Model.InvalidModel message ->
        invalid_model_error ~location ~name message
  in
  List.rev_append
    (List.map errors ~f:(fun error -> Core.Result.Error error))
    (List.map signatures ~f:create_model)


let parse ~resolution ?path ?rule_filter ~source ~configuration models =
  let new_models, errors =
    create ~resolution ?path ~rule_filter ~configuration source |> List.partition_result
  in
  {
    models =
      List.map new_models ~f:(fun model -> model.call_target, model.model)
      |> Callable.Map.of_alist_reduce ~f:(join ~iteration:0)
      |> Callable.Map.merge models ~f:(fun ~key:_ ->
           function
           | `Both (a, b) -> Some (join ~iteration:0 a b)
           | `Left model
           | `Right model ->
               Some model);
    errors;
  }


let verify_model_syntax ~path ~source =
  try String.split ~on:'\n' source |> Parser.parse |> ignore with
  | exn ->
      raise
        (Model.InvalidModel
           (Format.sprintf "Invalid model at `%s`: %s" (Path.show path) (Exn.to_string exn)))
