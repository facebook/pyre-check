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
open PyreParser
open Interprocedural
open Statement
open Domains
open TaintResult
open Model

module T = struct
  type breadcrumbs = Features.Simple.t list [@@deriving show, compare]

  let _ = show_breadcrumbs (* unused but derived *)

  type leaf_kind =
    | Leaf of {
        name: string;
        subkind: string option;
      }
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
  [@@deriving show, compare]

  type annotation_kind =
    | ParameterAnnotation of AccessPath.Root.t
    | ReturnAnnotation
  [@@deriving show, compare]

  module ModelQuery = struct
    type annotation_constraint = IsAnnotatedTypeConstraint [@@deriving compare, show]

    type parameter_constraint = AnnotationConstraint of annotation_constraint
    [@@deriving compare, show]

    type model_constraint =
      | NameConstraint of string
      | ReturnConstraint of annotation_constraint
      | AnyParameterConstraint of parameter_constraint
      | AnyOf of model_constraint list
    [@@deriving compare, show]

    type kind =
      | FunctionModel
      | MethodModel
    [@@deriving show, compare]

    type produced_taint =
      | TaintAnnotation of taint_annotation
      | ParametricSourceFromAnnotation of {
          source_pattern: string;
          kind: string;
        }
    [@@deriving show, compare]

    type production =
      | AllParametersTaint of produced_taint list
      | ParameterTaint of {
          name: string;
          taint: produced_taint list;
        }
      | PositionalParameterTaint of {
          index: int;
          taint: produced_taint list;
        }
      | ReturnTaint of produced_taint list
    [@@deriving show, compare]

    type rule = {
      query: model_constraint list;
      productions: production list;
      rule_kind: kind;
      name: string option;
    }
    [@@deriving show, compare]
  end

  type parse_result = {
    models: TaintResult.call_model Interprocedural.Callable.Map.t;
    queries: ModelQuery.rule list;
    skip_overrides: Reference.Set.t;
    errors: string list;
  }
end

include T

let raise_invalid_model message = raise (Model.InvalidModel message)

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


(* Don't propagate inferred model of methods with Sanitize *)

let decorators = String.Set.union Recognized.property_decorators Recognized.classproperty_decorators

let is_property define = String.Set.exists decorators ~f:(Define.has_decorator define)

let signature_is_property signature =
  String.Set.exists decorators ~f:(Define.Signature.has_decorator signature)


let rec parse_annotations ~configuration ~parameters annotation =
  let get_parameter_position name =
    let matches_parameter_name index { Node.value = parameter; _ } =
      if String.equal parameter.Parameter.name name then
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
  let extract_subkind { Node.value = expression; _ } =
    match expression with
    | Expression.Name (Name.Identifier subkind) -> Some subkind
    | _ -> None
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

  let rec extract_via_positions expression =
    match expression.Node.value with
    | Expression.Name (Name.Identifier name) -> [get_parameter_position name]
    | Tuple expressions -> List.concat_map ~f:extract_via_positions expressions
    | Call { callee; _ } when Option.equal String.equal (base_name callee) (Some "WithTag") -> []
    | _ ->
        Format.sprintf
          "Invalid expression for ViaValueOf or ViaTypeOf: %s"
          (show_expression expression.Node.value)
        |> failwith
  in
  let rec extract_via_tag expression =
    match expression.Node.value with
    | Expression.Call
        {
          callee;
          arguments =
            [
              {
                Call.Argument.value =
                  { Node.value = Expression.String { StringLiteral.value; _ }; _ };
                _;
              };
            ];
        }
      when Option.equal String.equal (base_name callee) (Some "WithTag") ->
        Some value
    | Expression.Call _ ->
        Format.sprintf
          "Invalid expression in ViaValueOf or ViaTypeOf declaration: %s"
          (Expression.show expression)
        |> failwith
    | Tuple expressions -> List.find_map expressions ~f:extract_via_tag
    | _ -> None
  in
  let rec extract_names expression =
    match expression.Node.value with
    | Expression.Name (Name.Identifier name) -> [name]
    | Tuple expressions -> List.concat_map ~f:extract_names expressions
    | _ ->
        Format.sprintf "Invalid expression name: %s" (show_expression expression.Node.value)
        |> failwith
  in
  let rec extract_kinds expression =
    match expression.Node.value with
    | Expression.Name (Name.Identifier taint_kind) -> [Leaf { name = taint_kind; subkind = None }]
    | Name (Name.Attribute { base; _ }) -> extract_kinds base
    | Call { callee; arguments = { Call.Argument.value = expression; _ } :: _ } -> (
        match base_name callee with
        | Some "Via" -> [Breadcrumbs (extract_breadcrumbs expression)]
        | Some "ViaValueOf" ->
            let tag = extract_via_tag expression in
            [
              Breadcrumbs
                ( extract_via_positions expression
                |> List.map ~f:(fun position -> Features.Simple.ViaValueOf { position; tag }) );
            ]
        | Some "ViaTypeOf" ->
            let tag = extract_via_tag expression in
            [
              Breadcrumbs
                ( extract_via_positions expression
                |> List.map ~f:(fun position -> Features.Simple.ViaTypeOf { position; tag }) );
            ]
        | Some "Updates" ->
            extract_names expression
            |> List.map ~f:(fun name ->
                   Leaf
                     {
                       name = Format.sprintf "ParameterUpdate%d" (get_parameter_position name);
                       subkind = None;
                     })
        | _ ->
            let kinds = extract_kinds callee in
            let subkind = extract_subkind expression in
            List.map kinds ~f:(fun kind ->
                match kind with
                | Leaf { name; subkind = None } -> Leaf { name; subkind }
                | _ -> kind) )
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
             | Leaf { name = leaf; subkind } -> Either.First (leaf, subkind)
             | Breadcrumbs b -> Either.Second b)
    in
    kinds, List.concat breadcrumbs
  in
  let get_source_kinds expression =
    let open Configuration in
    let kinds, breadcrumbs = extract_leafs expression in
    List.map kinds ~f:(fun (kind, subkind) ->
        Source
          {
            source = AnnotationParser.parse_source ~allowed:configuration.sources ?subkind kind;
            breadcrumbs;
            path = [];
            leaf_name_provided = false;
          })
  in
  let get_sink_kinds expression =
    let open Configuration in
    let kinds, breadcrumbs = extract_leafs expression in
    List.map kinds ~f:(fun (kind, subkind) ->
        Sink
          {
            sink = AnnotationParser.parse_sink ~allowed:configuration.sinks ?subkind kind;
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
        List.map kinds ~f:(fun (kind, _) ->
            Tito
              {
                tito = AnnotationParser.parse_sink ~allowed:configuration.sinks kind;
                breadcrumbs;
                path = [];
              })
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
          when [%compare.equal: string option] (base_name callee) (Some "AppliesTo") ->
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
            in
            parse_annotation expression |> List.map ~f:extend_path
        | Call { callee; arguments }
          when [%compare.equal: string option] (base_name callee) (Some "CrossRepositoryTaint") -> (
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
                        leaf = canonical_name;
                        port = Some (Format.sprintf "producer:%d:%s" producer_id canonical_port);
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
        | Call { callee; arguments }
          when [%compare.equal: string option]
                 (base_name callee)
                 (Some "CrossRepositoryTaintAnchor") -> (
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
                        leaf = canonical_name;
                        port = Some (Format.sprintf "anchor:%s" canonical_port);
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
                  "Cross repository taint anchor must be of the form \
                   CrossRepositoryTaintAnchor[taint, canonical_name, canonical_port]." )
        | Call
            {
              callee;
              arguments = { Call.Argument.value = { value = Tuple expressions; _ }; _ } :: _;
            }
          when [%compare.equal: string option] (base_name callee) (Some "Union") ->
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
                      if not (String.Map.Tree.mem configuration.partial_sink_labels kind) then
                        raise_invalid_model (Format.asprintf "Unrecognized partial sink `%s`." kind);
                      let label_options =
                        String.Map.Tree.find_exn configuration.partial_sink_labels kind
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


let parse_find_clause ({ Node.value; _ } as expression) =
  match value with
  | Expression.String { StringLiteral.value; _ } -> (
      match value with
      | "functions" -> Core.Result.Ok ModelQuery.FunctionModel
      | "methods" -> Core.Result.Ok ModelQuery.MethodModel
      | unsupported -> Core.Result.Error (Format.sprintf "Unsupported find clause `%s`" unsupported)
      )
  | _ ->
      Core.Result.Error
        (Format.sprintf "Find clauses must be strings, got: `%s`" (Expression.show expression))


let parse_where_clause ({ Node.value; _ } as expression) =
  let open Core.Result in
  let parse_annotation_constraint ~name ~arguments =
    match name, arguments with
    | "is_annotated_type", [] -> Ok ModelQuery.IsAnnotatedTypeConstraint
    | _ ->
        Error
          (Format.sprintf
             "`%s(%s)` does not correspond to an annotation constraint."
             name
             (List.to_string arguments ~f:Call.Argument.show))
  in
  let parse_parameter_constraint
      ~parameter_constraint_kind
      ~parameter_constraint
      ~parameter_constraint_arguments
    =
    match parameter_constraint_kind with
    | "annotation" ->
        parse_annotation_constraint
          ~name:parameter_constraint
          ~arguments:parameter_constraint_arguments
        >>| fun annotation_constraint -> ModelQuery.AnnotationConstraint annotation_constraint
    | _ ->
        Error
          (Format.sprintf
             "Unsupported constraint kind for parameters: `%s`"
             parameter_constraint_kind)
  in
  let rec parse_constraint ({ Node.value; _ } as constraint_expression) =
    match value with
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    {
                      base = { Node.value = Name (Name.Identifier "name"); _ };
                      attribute = "matches";
                      _;
                    });
              _;
            };
          arguments =
            [
              {
                Call.Argument.value =
                  { Node.value = Expression.String { StringLiteral.value = name_constraint; _ }; _ };
                _;
              };
            ];
        } ->
        Ok (ModelQuery.NameConstraint name_constraint)
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    {
                      base = { Node.value = Name (Name.Identifier "return_annotation"); _ };
                      attribute = annotation_constraint_name;
                      _;
                    });
              _;
            };
          arguments = annotation_constraint_arguments;
        } ->
        parse_annotation_constraint
          ~name:annotation_constraint_name
          ~arguments:annotation_constraint_arguments
        >>= fun annotation_constraint -> Ok (ModelQuery.ReturnConstraint annotation_constraint)
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    {
                      base =
                        {
                          Node.value =
                            Name
                              (Name.Attribute
                                {
                                  base = { Node.value = Name (Name.Identifier "any_parameter"); _ };
                                  attribute = parameter_constraint_kind;
                                  _;
                                });
                          _;
                        };
                      attribute = parameter_constraint;
                      _;
                    });
              _;
            };
          arguments = parameter_constraint_arguments;
        } ->
        parse_parameter_constraint
          ~parameter_constraint_kind
          ~parameter_constraint
          ~parameter_constraint_arguments
        >>= fun parameter_constraint -> Ok (ModelQuery.AnyParameterConstraint parameter_constraint)
    | Expression.Call
        {
          Call.callee = { Node.value = Expression.Name (Name.Identifier "AnyOf"); _ };
          arguments = constraints;
        } ->
        List.map constraints ~f:(fun { Call.Argument.value; _ } -> parse_constraint value)
        |> Core.Result.all
        >>| fun constraints -> ModelQuery.AnyOf constraints
    | Expression.Call { Call.callee; arguments = _ } ->
        Error (Format.sprintf "Unsupported callee: %s" (Expression.show callee))
    | _ ->
        Error (Format.sprintf "Unsupported constraint: %s" (Expression.show constraint_expression))
  in
  match value with
  | Expression.List items -> List.map items ~f:parse_constraint |> all
  | _ -> parse_constraint expression >>| List.return


let parse_model_clause ~configuration ({ Node.value; _ } as expression) =
  let open Core.Result in
  let parse_model ({ Node.value; _ } as model_expression) =
    let parse_taint taint_expression =
      let parse_produced_taint expression =
        match Node.value expression with
        | Expression.Call
            {
              Call.callee =
                {
                  Node.value = Expression.Name (Name.Identifier "ParametricSourceFromAnnotation");
                  _;
                };
              arguments =
                [
                  {
                    Call.Argument.name = Some { Node.value = "pattern"; _ };
                    value = { Node.value = Expression.Name (Name.Identifier source_pattern); _ };
                  };
                  {
                    Call.Argument.name = Some { Node.value = "kind"; _ };
                    value = { Node.value = Expression.Name (Name.Identifier kind); _ };
                  };
                ];
            } ->
            [ModelQuery.ParametricSourceFromAnnotation { source_pattern; kind }]
        | _ ->
            parse_annotations ~configuration ~parameters:[] (Some expression)
            |> List.map ~f:(fun taint -> ModelQuery.TaintAnnotation taint)
      in

      try
        match Node.value taint_expression with
        | Expression.List taint_annotations ->
            List.concat_map taint_annotations ~f:parse_produced_taint |> return
        | _ -> parse_produced_taint taint_expression |> return
      with
      | Failure failure -> Core.Result.Error failure
    in
    match value with
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "Returns"); _ };
          arguments = [{ Call.Argument.value = taint; _ }];
        } ->
        parse_taint taint >>| fun taint -> ModelQuery.ReturnTaint taint
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "NamedParameter"); _ };
          arguments =
            [
              {
                Call.Argument.value = { Node.value = String { StringLiteral.value = name; _ }; _ };
                name = Some { Node.value = "name"; _ };
              };
              { Call.Argument.value = taint; name = Some { Node.value = "taint"; _ } };
            ];
        } ->
        parse_taint taint >>| fun taint -> ModelQuery.ParameterTaint { name; taint }
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "PositionalParameter"); _ };
          arguments =
            [
              {
                Call.Argument.value = { Node.value = Integer index; _ };
                name = Some { Node.value = "index"; _ };
              };
              { Call.Argument.value = taint; name = Some { Node.value = "taint"; _ } };
            ];
        } ->
        parse_taint taint >>| fun taint -> ModelQuery.PositionalParameterTaint { index; taint }
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "AllParameters"); _ };
          arguments = [{ Call.Argument.value = taint; _ }];
        } ->
        parse_taint taint >>| fun taint -> ModelQuery.AllParametersTaint taint
    | _ ->
        Error
          (Format.sprintf "Unexpected model expression: `%s`" (Expression.show model_expression))
  in
  match value with
  | Expression.List items -> List.map items ~f:parse_model |> all
  | _ -> parse_model expression >>| List.return


let find_positional_parameter_annotation position parameters =
  List.nth parameters position >>= Type.Record.Callable.RecordParameter.annotation


let find_named_parameter_annotation search_name parameters =
  let has_name = function
    | Type.Record.Callable.RecordParameter.KeywordOnly { name; _ } ->
        String.equal name ("$parameter$" ^ search_name)
    | Type.Record.Callable.RecordParameter.Named { name; _ } -> String.equal name search_name
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


let parse_parameter_taint ~configuration ~parameters (root, _name, parameter) =
  let annotation = parameter.Node.value.Parameter.annotation in
  parse_annotations ~configuration ~parameters annotation
  |> List.map ~f:(fun annotation -> annotation, ParameterAnnotation root)


let add_taint_annotation_to_model
    ~resolution
    ~annotation_kind
    ~callable_annotation
    ~sources_to_keep
    ~sinks_to_keep
    model
    annotation
  =
  match annotation_kind with
  | ReturnAnnotation -> (
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
          raise_invalid_model "Invalid return annotation: AddFeatureToArgument" )
  | ParameterAnnotation root -> (
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
               Sinks.AddFeatureToArgument )


let parse_return_taint ~configuration ~parameters expression =
  parse_annotations ~configuration ~parameters expression
  |> List.map ~f:(fun annotation -> annotation, ReturnAnnotation)


type parsed_signature_or_query =
  | ParsedSignature of Define.Signature.t * Location.t * Callable.t
  | ParsedQuery of ModelQuery.rule

type model_or_query =
  | Model of (Model.t * Reference.t option)
  | Query of ModelQuery.rule

let callable_annotation
    ~resolution
    ({ Define.Signature.name = { Node.value = name; _ }; decorators; _ } as define)
  =
  (* Since properties and setters share the same undecorated name, we need to special-case them. *)
  let global_resolution = Resolution.global_resolution resolution in
  let global_type () =
    match GlobalResolution.global global_resolution name with
    | Some { AttributeResolution.Global.undecorated_signature; annotation; _ } -> (
        match undecorated_signature with
        | Some signature -> Type.Callable signature |> Annotation.create
        | None -> annotation )
    | None ->
        (* Fallback for fields, which are not globals. *)
        from_reference name ~location:Location.any
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


let compute_sources_and_sinks_to_keep ~configuration ~rule_filter =
  match rule_filter with
  | None -> None, None
  | Some rule_filter ->
      let rule_filter = Int.Set.of_list rule_filter in
      let sources_to_keep, sinks_to_keep =
        let { Configuration.rules; _ } = configuration in
        let rules =
          (* The user annotations for partial sinks will be the untriggered ones, even though the
             rule expects triggered sinks. *)
          let untrigger_partial_sinks sink =
            match sink with
            | Sinks.TriggeredPartialSink { kind; label } -> Sinks.PartialSink { kind; label }
            | _ -> sink
          in
          List.filter_map rules ~f:(fun { Configuration.Rule.code; sources; sinks; _ } ->
              if Core.Set.mem rule_filter code then
                Some (sources, List.map sinks ~f:untrigger_partial_sinks)
              else
                None)
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


let create ~resolution ?path ~configuration ~rule_filter source =
  let sources_to_keep, sinks_to_keep =
    compute_sources_and_sinks_to_keep ~configuration ~rule_filter
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
    let message = Format.asprintf "Invalid model for `%s`%s: %s" name model_origin message in
    Core.Result.Error message
  in

  let signatures_and_queries, errors =
    let filter_define_signature signature =
      try
        match signature with
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
            Core.Result.Ok [ParsedSignature (signature, location, call_target)]
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
            let source_annotations, extra_decorators =
              let decorator_with_name name =
                {
                  Decorator.name = Node.create_with_default_location (Reference.create name);
                  arguments = None;
                }
              in
              let class_source_base { Call.Argument.value; _ } =
                let name = Expression.show value in
                if String.is_prefix name ~prefix:"TaintSource[" then
                  Some (Either.First value)
                else if String.equal name "SkipAnalysis" then
                  Some (Either.Second (decorator_with_name "SkipAnalysis"))
                else if String.equal name "SkipOverrides" then
                  Some (Either.Second (decorator_with_name "SkipOverrides"))
                else
                  None
              in
              List.filter_map bases ~f:class_source_base
              |> List.fold ~init:([], []) ~f:(fun (source_annotations, decorators) ->
                   function
                   | Either.First source_annotation ->
                       source_annotation :: source_annotations, decorators
                   | Either.Second decorator -> source_annotations, decorator :: decorators)
            in
            if
              (not (List.is_empty sink_annotations))
              || (not (List.is_empty source_annotations))
              || not (List.is_empty extra_decorators)
            then
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
                          let signature ~extra_decorators ~source_annotation ~sink_annotation =
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
                            let decorators = List.rev_append extra_decorators decorators in
                            ParsedSignature
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
                                  ~extra_decorators:[]
                                  ~source_annotation:(Some source_annotation)
                                  ~sink_annotation:None)
                          in
                          let sinks =
                            List.map sink_annotations ~f:(fun sink_annotation ->
                                signature
                                  ~extra_decorators:[]
                                  ~source_annotation:None
                                  ~sink_annotation:(Some sink_annotation))
                          in
                          let skip_analysis_or_overrides_defines =
                            if not (List.is_empty extra_decorators) then
                              [
                                signature
                                  ~extra_decorators
                                  ~source_annotation:None
                                  ~sink_annotation:None;
                              ]
                            else
                              []
                          in
                          skip_analysis_or_overrides_defines @ sources @ sinks
                      | _ -> []
                    in

                    List.concat_map body ~f:signature)
              |> Option.value ~default:[]
              |> Core.Result.return
            else
              Core.Result.Ok []
        | { Node.value = Class { Class.name = { Node.value = name; _ }; _ }; location } ->
            invalid_model_error
              ~location
              ~name:(Reference.show name)
              "Class model must have a body of `...`."
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
            ModelVerifier.verify_global ~resolution ~name;
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
            Core.Result.Ok [ParsedSignature (signature, location, Callable.create_object name)]
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
            ModelVerifier.verify_global ~resolution ~name;
            let signature =
              {
                Define.Signature.name = Node.create ~location:name_location name;
                parameters =
                  [Parameter.create ~location:Location.any ~annotation ~name:"$global" ()];
                decorators = [];
                return_annotation = None;
                async = false;
                generator = false;
                parent = None;
                nesting_define = None;
              }
            in
            Core.Result.Ok [ParsedSignature (signature, location, Callable.create_object name)]
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
          when is_simple_name name && Expression.show annotation |> String.equal "Sanitize" ->
            let name = name_to_reference_exn name in
            ModelVerifier.verify_global ~resolution ~name;
            let signature =
              {
                Define.Signature.name = Node.create ~location:name_location name;
                parameters = [Parameter.create ~location:Location.any ~name:"$global" ()];
                decorators =
                  [
                    {
                      Decorator.name =
                        Node.create_with_default_location (Reference.create "Sanitize");
                      arguments = None;
                    };
                  ];
                return_annotation = None;
                async = false;
                generator = false;
                parent = None;
                nesting_define = None;
              }
            in
            Core.Result.Ok [ParsedSignature (signature, location, Callable.create_object name)]
        | {
         Node.value =
           Expression
             {
               Node.value =
                 Expression.Call
                   {
                     Call.callee =
                       { Node.value = Expression.Name (Name.Identifier "ModelQuery"); _ };
                     arguments;
                   };
               _;
             };
         _;
        } ->
            let clauses =
              match arguments with
              | [
               { Call.Argument.name = Some { Node.value = "find"; _ }; value = find_clause };
               { Call.Argument.name = Some { Node.value = "where"; _ }; value = where_clause };
               { Call.Argument.name = Some { Node.value = "model"; _ }; value = model_clause };
              ] ->
                  Core.Result.Ok
                    ( None,
                      parse_find_clause find_clause,
                      parse_where_clause where_clause,
                      parse_model_clause ~configuration model_clause )
              | [
               {
                 Call.Argument.name = Some { Node.value = "name"; _ };
                 value = { Node.value = Expression.String { StringLiteral.value = name; _ }; _ };
               };
               { Call.Argument.name = Some { Node.value = "find"; _ }; value = find_clause };
               { Call.Argument.name = Some { Node.value = "where"; _ }; value = where_clause };
               { Call.Argument.name = Some { Node.value = "model"; _ }; value = model_clause };
              ] ->
                  Core.Result.Ok
                    ( Some name,
                      parse_find_clause find_clause,
                      parse_where_clause where_clause,
                      parse_model_clause ~configuration model_clause )
              | _ ->
                  Core.Result.Error
                    "Malformed model query arguments: expected a find, where and model clause."
            in

            let open Core.Result in
            clauses
            >>= fun (name, find_clause, where_clause, model_clause) ->
            find_clause
            >>= fun rule_kind ->
            where_clause
            >>= fun query ->
            model_clause
            >>| fun productions -> [ParsedQuery { ModelQuery.rule_kind; query; productions; name }]
        | _ -> Core.Result.Ok []
      with
      | ModelVerifier.GlobalVerificationError { name; message } ->
          invalid_model_error ~location:signature.location ~name message
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
      ( ( { Define.Signature.name = { Node.value = name; _ }; parameters; return_annotation; _ } as
        define ),
        location,
        call_target )
    =
    (* Strip off the decorators only used for taint annotations. *)
    let top_level_decorators, define =
      let is_taint_decorator decorator =
        match Reference.show (Node.value decorator.Decorator.name) with
        | "Sanitize"
        | "SkipAnalysis"
        | "SkipOverrides" ->
            true
        | _ -> false
      in
      let sanitizers, nonsanitizers = List.partition_tf define.decorators ~f:is_taint_decorator in
      sanitizers, { define with decorators = nonsanitizers }
    in
    (* Make sure we know about what we model. *)
    try
      let callable_annotation = callable_annotation ~resolution define in
      let call_target = (call_target :> Callable.t) in
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
            | AccessPath.Root.PositionalParameter { position; name; positional_only } ->
                AccessPath.Root.PositionalParameter
                  {
                    position =
                      Map.find callable_parameter_names_to_positions name
                      |> Option.value ~default:position;
                    name;
                    positional_only;
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
      let annotations =
        List.rev_append
          (List.concat_map
             normalized_model_parameters
             ~f:(parse_parameter_taint ~configuration ~parameters))
          (parse_return_taint ~configuration ~parameters return_annotation)
      in
      let model =
        List.fold
          annotations
          ~init:TaintResult.empty_model
          ~f:(fun accumulator (annotation, annotation_kind) ->
            add_taint_annotation_to_model
              ~resolution:(Resolution.global_resolution resolution)
              ~annotation_kind
              ~callable_annotation
              ~sources_to_keep
              ~sinks_to_keep
              accumulator
              annotation)
      in
      (* Adjust analysis mode and whether we skip overrides by applying top-level decorators. *)
      let model, skipped_override =
        let define_name = name in
        let mode, skipped_override =
          let adjust_mode
              (mode, skipped_override)
              { Decorator.name = { Node.value = name; _ }; arguments }
            =
            match Reference.show name with
            | "Sanitize" -> (
                let new_sanitize_kinds =
                  match arguments with
                  | None -> [SanitizeAll]
                  | Some arguments ->
                      let to_sanitize_kind { Call.Argument.value; _ } =
                        match Node.value value with
                        | Expression.Name (Name.Identifier name) -> (
                            match name with
                            | "TaintSource" -> Some SanitizeSources
                            | "TaintSink" -> Some SanitizeSinks
                            | "TaintInTaintOut" -> Some SanitizeTITO
                            | _ -> None )
                        | _ -> None
                      in
                      List.filter_map arguments ~f:to_sanitize_kind
                in
                match mode with
                | TaintResult.Sanitize kinds ->
                    TaintResult.Sanitize (kinds @ new_sanitize_kinds), skipped_override
                | _ -> TaintResult.Sanitize new_sanitize_kinds, skipped_override )
            | "SkipAnalysis" -> TaintResult.SkipAnalysis, skipped_override
            | "SkipOverrides" -> mode, Some define_name
            | _ -> mode, skipped_override
          in
          List.fold top_level_decorators ~f:adjust_mode ~init:(model.mode, None)
        in
        { model with mode }, skipped_override
      in
      Core.Result.Ok (Model ({ model; call_target; is_obscure = false }, skipped_override))
    with
    | Failure message
    | Model.InvalidModel message ->
        invalid_model_error ~location ~name:(Reference.show name) message
  in
  let signatures, queries =
    List.fold signatures_and_queries ~init:([], []) ~f:(fun (signatures, queries) ->
      function
      | ParsedSignature (signature, location, callable) ->
          (signature, location, callable) :: signatures, queries
      | ParsedQuery query -> signatures, query :: queries)
  in
  List.rev_append
    (List.map errors ~f:(fun error -> Core.Result.Error error))
    ( List.map signatures ~f:create_model
    |> List.rev_append (List.map queries ~f:(fun query -> Core.Result.return (Query query))) )


let parse ~resolution ?path ?rule_filter ~source ~configuration models =
  let new_models_and_queries, errors =
    create ~resolution ?path ~rule_filter ~configuration source |> List.partition_result
  in
  let new_models, new_queries =
    List.fold
      new_models_and_queries
      ~f:(fun (models, queries) -> function
        | Model (model, skipped_override) -> (model, skipped_override) :: models, queries
        | Query query -> models, query :: queries)
      ~init:([], [])
  in
  let is_empty_model model =
    equal_mode model.mode Normal
    && ForwardState.is_bottom model.forward.source_taint
    && BackwardState.is_bottom model.backward.sink_taint
    && BackwardState.is_bottom model.backward.taint_in_taint_out
  in
  {
    models =
      List.map new_models ~f:(fun (model, _) -> model.call_target, model.model)
      |> Callable.Map.of_alist_reduce ~f:(join ~iteration:0)
      |> Callable.Map.filter ~f:(fun model -> not (is_empty_model model))
      |> Callable.Map.merge models ~f:(fun ~key:_ ->
           function
           | `Both (a, b) -> Some (join ~iteration:0 a b)
           | `Left model
           | `Right model ->
               Some model);
    skip_overrides =
      List.filter_map new_models ~f:(fun (_, skipped_override) -> skipped_override)
      |> Reference.Set.of_list;
    queries = new_queries;
    errors;
  }


let create_model_from_annotations ~resolution ~callable ~sources_to_keep ~sinks_to_keep annotations =
  let global_resolution = Resolution.global_resolution resolution in
  match
    Interprocedural.Callable.get_module_and_definition ~resolution:global_resolution callable
  with
  | None -> None
  | Some (_, { Node.value = { Define.signature = define; _ }; _ }) ->
      let callable_annotation =
        callable_annotation ~resolution define
        |> Annotation.annotation
        |> function
        | Type.Callable t -> Some t
        | Type.Parametric
            { name = "BoundMethod"; parameters = [Type.Parameter.Single (Type.Callable t); _] } ->
            Some t
        | _ -> None
      in
      List.fold
        annotations
        ~init:TaintResult.empty_model
        ~f:(fun accumulator (annotation_kind, annotation) ->
          add_taint_annotation_to_model
            ~resolution:global_resolution
            ~annotation_kind
            ~callable_annotation
            ~sources_to_keep
            ~sinks_to_keep
            accumulator
            annotation)
      |> Option.some


let verify_model_syntax ~path ~source =
  try String.split ~on:'\n' source |> Parser.parse |> ignore with
  | exn ->
      Log.error "Unable to parse model at `%s`: %s" (Path.show path) (Exn.to_string exn);
      raise
        (Model.InvalidModel
           (Format.sprintf "Invalid model at `%s`: %s" (Path.show path) (Exn.to_string exn)))
