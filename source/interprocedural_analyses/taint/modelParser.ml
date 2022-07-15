(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

module Internal = struct
  type breadcrumbs = Features.Breadcrumb.t list [@@deriving show, compare]

  type via_features = Features.ViaFeature.t list [@@deriving show, compare]

  type leaf_kind =
    | Leaf of {
        name: string;
        subkind: string option;
      }
    | Breadcrumbs of breadcrumbs
    | ViaFeatures of via_features
  [@@deriving show, compare]

  type sanitize_annotation =
    | AllSources
    | SpecificSource of Sources.t
    | AllSinks
    | SpecificSink of Sinks.t
    | AllTito
    | SpecificTito of {
        sources: Sources.t list;
        sinks: Sinks.t list;
      }
  [@@deriving show, compare]

  type taint_annotation =
    | Sink of {
        sink: Sinks.t;
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
        leaf_names: Features.LeafName.t list;
        leaf_name_provided: bool;
        trace_length: int option;
      }
    | Source of {
        source: Sources.t;
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
        leaf_names: Features.LeafName.t list;
        leaf_name_provided: bool;
        trace_length: int option;
      }
    | Tito of {
        tito: Sinks.t;
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
      }
    | AddFeatureToArgument of {
        breadcrumbs: breadcrumbs;
        via_features: via_features;
        path: Abstract.TreeDomain.Label.path;
      }
    | Sanitize of sanitize_annotation list
  [@@deriving show, compare]

  type annotation_kind =
    | ParameterAnnotation of AccessPath.Root.t
    | ReturnAnnotation
  [@@deriving show, compare]

  module ModelQuery = struct
    type name_constraint =
      | Equals of string
      | Matches of Re2.t
    [@@deriving compare]

    let pp_name_constraint formatter name_constraint =
      match name_constraint with
      | Equals equals -> Format.fprintf formatter "Equals(%s)" equals
      | Matches regular_expression ->
          Format.fprintf formatter "Matches(%s)" (Re2.to_string regular_expression)


    let show_name_constraint = Format.asprintf "%a" pp_name_constraint

    type annotation_constraint =
      | IsAnnotatedTypeConstraint
      | AnnotationNameConstraint of name_constraint
    [@@deriving compare, show]

    module ArgumentsConstraint = struct
      type t =
        | Equals of Ast.Expression.Call.Argument.t list
        | Contains of Ast.Expression.Call.Argument.t list
      [@@deriving compare, show]
    end

    module ParameterConstraint = struct
      type t =
        | AnnotationConstraint of annotation_constraint
        | NameConstraint of name_constraint
        | IndexConstraint of int
        | AnyOf of t list
        | AllOf of t list
        | Not of t
      [@@deriving compare, show]
    end

    module DecoratorConstraint = struct
      type t = {
        name_constraint: name_constraint;
        arguments_constraint: ArgumentsConstraint.t option;
      }
      [@@deriving compare, show]
    end

    module ClassConstraint = struct
      type t =
        | NameSatisfies of name_constraint
        | Extends of {
            class_name: string;
            is_transitive: bool;
          }
        | DecoratorSatisfies of DecoratorConstraint.t
      [@@deriving compare, show]
    end

    type model_constraint =
      | NameConstraint of name_constraint
      | AnnotationConstraint of annotation_constraint
      | ReturnConstraint of annotation_constraint
      | AnyParameterConstraint of ParameterConstraint.t
      | AnyOf of model_constraint list
      | AllOf of model_constraint list
      | ParentConstraint of ClassConstraint.t
      | AnyDecoratorConstraint of DecoratorConstraint.t
      | Not of model_constraint
    [@@deriving compare, show]

    type kind =
      | FunctionModel
      | MethodModel
      | AttributeModel
    [@@deriving show, compare]

    type produced_taint =
      | TaintAnnotation of taint_annotation
      | ParametricSourceFromAnnotation of {
          source_pattern: string;
          kind: string;
        }
      | ParametricSinkFromAnnotation of {
          sink_pattern: string;
          kind: string;
        }
    [@@deriving show, compare]

    type production =
      | AllParametersTaint of {
          excludes: string list;
          taint: produced_taint list;
        }
      | NamedParameterTaint of {
          name: string;
          taint: produced_taint list;
        }
      | PositionalParameterTaint of {
          index: int;
          taint: produced_taint list;
        }
      | ParameterTaint of {
          where: ParameterConstraint.t list;
          taint: produced_taint list;
        }
      | ReturnTaint of produced_taint list
      | AttributeTaint of produced_taint list
    [@@deriving show, compare]

    type rule = {
      location: Location.t;
      query: model_constraint list;
      productions: production list;
      rule_kind: kind;
      name: string;
    }
    [@@deriving show, compare]
  end

  type parse_result = {
    models: Registry.t;
    queries: ModelQuery.rule list;
    skip_overrides: Reference.Set.t;
    errors: ModelVerificationError.t list;
  }
end

include Internal

let model_verification_error ~path ~location kind = { ModelVerificationError.kind; path; location }

module ClassDefinitionsCache = ModelVerifier.ClassDefinitionsCache

(* We don't have real models for attributes, so we make a fake callable model with a 'parameter'
   $global which acts as the taint sink whenever attributes are marked as sinks. *)
let attribute_symbolic_parameter = "$global"

let decorators = String.Set.union Recognized.property_decorators Recognized.classproperty_decorators

let is_property define = String.Set.exists decorators ~f:(Define.has_decorator define)

let signature_is_property signature =
  String.Set.exists decorators ~f:(Define.Signature.has_decorator signature)


(* Return `X` if the expression is of the form `X[Y]`, otherwise `None`. *)
let base_name expression =
  match expression with
  | {
   Node.value =
     Expression.Name
       (Name.Attribute
         {
           base = { Node.value = Name (Name.Identifier identifier); _ };
           attribute = "__getitem__";
           special = true;
         });
   _;
  } ->
      Some identifier
  | _ -> None


let rec parse_annotations
    ~path
    ~location
    ~model_name
    ~configuration
    ~parameters
    ~callable_parameter_names_to_positions
    ?(is_object_target = false)
    ?(is_model_query = false)
    annotation
  =
  let open Core.Result in
  let annotation_error reason =
    model_verification_error
      ~path
      ~location
      (InvalidTaintAnnotation { taint_annotation = annotation; reason })
  in
  let get_parameter_position name =
    let callable_parameter_names_to_positions =
      Option.value ~default:String.Map.empty callable_parameter_names_to_positions
    in
    match Map.find callable_parameter_names_to_positions name with
    | Some (position :: _) -> Ok position
    | Some []
    | None -> (
        (* `callable_parameter_names_to_positions` might be missing the `self` parameter. *)
        let matches_parameter_name index { Node.value = parameter; _ } =
          if String.equal parameter.Parameter.name name then
            Some index
          else
            None
        in
        match List.find_mapi parameters ~f:matches_parameter_name with
        | Some index -> Ok index
        | None -> Error (annotation_error (Format.sprintf "No such parameter `%s`" name)))
  in
  let rec extract_breadcrumbs ?(is_dynamic = false) expression =
    let open TaintConfiguration in
    match expression.Node.value with
    | Expression.Name (Name.Identifier breadcrumb) ->
        if is_dynamic then
          Ok [Features.Breadcrumb.SimpleVia breadcrumb]
        else
          Features.Breadcrumb.simple_via ~allowed:configuration.features breadcrumb
          >>| (fun breadcrumb -> [breadcrumb])
          |> map_error ~f:annotation_error
    | Tuple expressions ->
        List.map ~f:(extract_breadcrumbs ~is_dynamic) expressions |> all |> map ~f:List.concat
    | _ ->
        Error
          (annotation_error
             (Format.sprintf
                "Invalid expression for breadcrumb: %s"
                (show_expression expression.Node.value)))
  in
  let extract_subkind { Node.value = expression; _ } =
    match expression with
    | Expression.Name (Name.Identifier subkind) -> Some subkind
    | _ -> None
  in
  let rec extract_via_parameters expression =
    match expression.Node.value with
    | Expression.Name (Name.Identifier name) ->
        get_parameter_position name
        >>| fun position ->
        [AccessPath.Root.PositionalParameter { name; position; positional_only = false }]
    | Tuple expressions -> List.map ~f:extract_via_parameters expressions |> all >>| List.concat
    | Call { callee; _ } when Option.equal String.equal (base_name callee) (Some "WithTag") -> Ok []
    | _ ->
        Error
          (annotation_error
             (Format.sprintf
                "Invalid expression for ViaValueOf or ViaTypeOf: %s"
                (show_expression expression.Node.value)))
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
                  {
                    Node.value = Expression.Constant (Constant.String { StringLiteral.value; _ });
                    _;
                  };
                _;
              };
            ];
        }
      when Option.equal String.equal (base_name callee) (Some "WithTag") ->
        Ok (Some value)
    | Expression.Call _ ->
        Error
          (annotation_error
             (Format.sprintf
                "Invalid expression in ViaValueOf or ViaTypeOf declaration: %s"
                (Expression.show expression)))
    | Tuple expressions -> List.map expressions ~f:extract_via_tag |> all >>| List.find_map ~f:ident
    | _ -> Ok None
  in
  let rec extract_names expression =
    match expression.Node.value with
    | Expression.Name (Name.Identifier name) -> Ok [name]
    | Tuple expressions -> List.map ~f:extract_names expressions |> all >>| List.concat
    | _ ->
        Error
          (annotation_error
             (Format.sprintf "Invalid expression name: %s" (show_expression expression.Node.value)))
  in
  let rec extract_kinds expression =
    match expression.Node.value with
    | Expression.Name (Name.Identifier "ViaTypeOf") ->
        if is_object_target or is_model_query then (* ViaTypeOf is treated as ViaTypeOf[$global] *)
          Ok
            [
              ViaFeatures
                [
                  Features.ViaFeature.ViaTypeOf
                    {
                      parameter =
                        AccessPath.Root.PositionalParameter
                          {
                            name = attribute_symbolic_parameter;
                            position = 0;
                            positional_only = false;
                          };
                      tag = None;
                    };
                ];
            ]
        else
          Error
            (annotation_error
               "A standalone `ViaTypeOf` without arguments can only be used in attribute or global \
                models.")
    | Expression.Name (Name.Identifier taint_kind) ->
        Ok [Leaf { name = taint_kind; subkind = None }]
    | Name (Name.Attribute { base; _ }) -> extract_kinds base
    | Call { callee; arguments = { Call.Argument.value = expression; _ } :: _ } -> (
        match base_name callee with
        | Some "Via" ->
            extract_breadcrumbs expression >>| fun breadcrumbs -> [Breadcrumbs breadcrumbs]
        | Some "ViaDynamicFeature" ->
            extract_breadcrumbs ~is_dynamic:true expression
            >>| fun breadcrumbs -> [Breadcrumbs breadcrumbs]
        | Some "ViaValueOf" ->
            extract_via_tag expression
            >>= fun tag ->
            extract_via_parameters expression
            >>| List.map ~f:(fun parameter -> Features.ViaFeature.ViaValueOf { parameter; tag })
            >>| fun via_features -> [ViaFeatures via_features]
        | Some "ViaTypeOf" ->
            extract_via_tag expression
            >>= fun tag ->
            extract_via_parameters expression
            >>| List.map ~f:(fun parameter -> Features.ViaFeature.ViaTypeOf { parameter; tag })
            >>| fun via_features -> [ViaFeatures via_features]
        | Some "Updates" ->
            let to_leaf name =
              get_parameter_position name
              >>| fun position ->
              Leaf { name = Format.sprintf "ParameterUpdate%d" position; subkind = None }
            in
            extract_names expression >>= fun names -> List.map ~f:to_leaf names |> all
        | _ ->
            let subkind = extract_subkind expression in
            extract_kinds callee
            >>| fun kinds ->
            List.map kinds ~f:(fun kind ->
                match kind with
                | Leaf { name; subkind = None } -> Leaf { name; subkind }
                | _ -> kind))
    | Call { callee; _ } -> extract_kinds callee
    | Tuple expressions -> List.map ~f:extract_kinds expressions |> all >>| List.concat
    | _ ->
        Error
          (annotation_error
             (Format.sprintf
                "Invalid expression for taint kind: %s"
                (show_expression expression.Node.value)))
  in
  let extract_leafs expression =
    extract_kinds expression
    >>| fun leaves ->
    let kinds, leaves =
      List.partition_map
        ~f:(function
          | Leaf { name = leaf; subkind } -> Either.First (leaf, subkind)
          | other -> Either.Second other)
        leaves
    in
    let breadcrumbs, via_features =
      List.partition_map
        ~f:(function
          | Breadcrumbs b -> Either.First b
          | ViaFeatures f -> Either.Second f
          | _ -> failwith "impossible")
        leaves
    in
    kinds, List.concat breadcrumbs, List.concat via_features
  in
  let get_source_kinds expression =
    let open TaintConfiguration in
    extract_leafs expression
    >>= fun (kinds, breadcrumbs, via_features) ->
    List.map kinds ~f:(fun (kind, subkind) ->
        AnnotationParser.parse_source ~allowed:configuration.sources ?subkind kind
        >>| fun source ->
        Source
          {
            source;
            breadcrumbs;
            via_features;
            path = [];
            leaf_names = [];
            leaf_name_provided = false;
            trace_length = None;
          })
    |> all
    |> map_error ~f:annotation_error
  in
  let get_sink_kinds expression =
    let open TaintConfiguration in
    extract_leafs expression
    >>= fun (kinds, breadcrumbs, via_features) ->
    List.map kinds ~f:(fun (kind, subkind) ->
        AnnotationParser.parse_sink ~allowed:configuration.sinks ?subkind kind
        >>| fun sink ->
        Sink
          {
            sink;
            breadcrumbs;
            via_features;
            path = [];
            leaf_names = [];
            leaf_name_provided = false;
            trace_length = None;
          })
    |> all
    |> map_error ~f:annotation_error
  in
  let get_taint_in_taint_out expression =
    let open TaintConfiguration in
    extract_leafs expression
    >>= fun (kinds, breadcrumbs, via_features) ->
    match kinds with
    | [] -> Ok [Tito { tito = Sinks.LocalReturn; breadcrumbs; via_features; path = [] }]
    | _ ->
        List.map kinds ~f:(fun (kind, subkind) ->
            AnnotationParser.parse_tito ~allowed_transforms:configuration.transforms ?subkind kind
            >>| fun tito -> Tito { tito; breadcrumbs; via_features; path = [] })
        |> all
        |> map_error ~f:annotation_error
  in
  let extract_attach_features ~name expression =
    let keep_features = function
      | Breadcrumbs breadcrumbs -> Some (Either.First breadcrumbs)
      | ViaFeatures via_features -> Some (Either.Second via_features)
      | _ -> None
    in
    (* Ensure AttachToX annotations don't have any non-Via annotations for now. *)
    extract_kinds expression
    >>| List.map ~f:keep_features
    >>| Option.all
    >>| Option.map ~f:(List.partition_map ~f:Fn.id)
    >>= function
    | Some (breadcrumbs, via_features) -> Ok (List.concat breadcrumbs, List.concat via_features)
    | None ->
        Error
          (annotation_error
             (Format.sprintf "All parameters to `%s` must be of the form `Via[feature]`." name))
  in
  let invalid_annotation_error () =
    Error (annotation_error "Failed to parse the given taint annotation.")
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
      when [%compare.equal: string option] (base_name callee) (Some "AppliesTo") ->
        let field =
          match index with
          | Expression.Constant (Constant.Integer index) ->
              Ok (Abstract.TreeDomain.Label.create_int_index index)
          | Expression.Constant (Constant.String { StringLiteral.value = index; _ }) ->
              Ok (Abstract.TreeDomain.Label.create_name_index index)
          | _ ->
              Error
                (annotation_error
                   "Expected either integer or string as index in AppliesTo annotation.")
        in
        let extend_path field = function
          | Sink ({ path; _ } as sink) -> Ok (Sink { sink with path = field :: path })
          | Source ({ path; _ } as source) -> Ok (Source { source with path = field :: path })
          | Tito ({ path; _ } as tito) -> Ok (Tito { tito with path = field :: path })
          | AddFeatureToArgument ({ path; _ } as add_feature_to_argument) ->
              Ok (AddFeatureToArgument { add_feature_to_argument with path = field :: path })
          | Sanitize _ -> Error (annotation_error "`AppliesTo[Sanitize[...]]` is not supported.")
        in
        field
        >>= fun field ->
        parse_annotation expression
        >>= fun annotations -> List.map ~f:(extend_path field) annotations |> all
    | Call { callee; arguments }
      when [%compare.equal: string option] (base_name callee) (Some "CrossRepositoryTaint") -> (
        let required_arguments, optional_arguments =
          match arguments with
          | [
           {
             Call.Argument.value =
               {
                 Node.value =
                   Expression.Tuple
                     ({ Node.value = taint; _ }
                     :: {
                          Node.value =
                            Expression.Constant
                              (Constant.String { StringLiteral.value = canonical_name; _ });
                          _;
                        }
                        :: {
                             Node.value =
                               Expression.Constant
                                 (Constant.String { StringLiteral.value = canonical_port; _ });
                             _;
                           }
                           :: { Node.value = Expression.Constant (Constant.Integer producer_id); _ }
                              :: remaining_arguments);
                 _;
               };
             _;
           };
          ] ->
              Some (taint, canonical_name, canonical_port, producer_id), remaining_arguments
          | _ -> None, []
        in
        let optional_arguments =
          match optional_arguments with
          | [{ Node.value = Expression.Constant (Constant.Integer trace_length); _ }] ->
              Some trace_length
          | [] -> Some 0
          | _ -> None
        in
        match required_arguments, optional_arguments with
        | Some (taint, canonical_name, canonical_port, producer_id), Some trace_length ->
            let add_cross_repository_information annotation =
              let leaf_name =
                Features.LeafName.
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
                      leaf_names = leaf_name :: source.leaf_names;
                      leaf_name_provided = true;
                      trace_length = Option.merge ~f:min source.trace_length (Some trace_length);
                      breadcrumbs = Features.Breadcrumb.Crtex :: source.breadcrumbs;
                    }
              | Sink sink ->
                  Sink
                    {
                      sink with
                      leaf_names = leaf_name :: sink.leaf_names;
                      leaf_name_provided = true;
                      trace_length = Option.merge ~f:min sink.trace_length (Some trace_length);
                      breadcrumbs = Features.Breadcrumb.Crtex :: sink.breadcrumbs;
                    }
              | _ -> annotation
            in
            parse_annotation taint |> map ~f:(List.map ~f:add_cross_repository_information)
        | _ ->
            Error
              (annotation_error
                 "Cross repository taint must be of the form CrossRepositoryTaint[taint, \
                  canonical_name, canonical_port, producer_id, trace_length]."))
    | Call { callee; arguments }
      when [%compare.equal: string option] (base_name callee) (Some "CrossRepositoryTaintAnchor")
      -> (
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
                         Expression.Constant
                           (Constant.String { StringLiteral.value = canonical_name; _ });
                       _;
                     };
                     {
                       Node.value =
                         Expression.Constant
                           (Constant.String { StringLiteral.value = canonical_port; _ });
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
                Features.LeafName.
                  { leaf = canonical_name; port = Some (Format.sprintf "anchor:%s" canonical_port) }
              in
              match annotation with
              | Source source ->
                  Source
                    {
                      source with
                      leaf_names = leaf_name :: source.leaf_names;
                      leaf_name_provided = true;
                    }
              | Sink sink ->
                  Sink
                    {
                      sink with
                      leaf_names = leaf_name :: sink.leaf_names;
                      leaf_name_provided = true;
                    }
              | _ -> annotation
            in
            parse_annotation taint |> map ~f:(List.map ~f:add_cross_repository_information)
        | _ ->
            Error
              (annotation_error
                 "Cross repository taint anchor must be of the form \
                  CrossRepositoryTaintAnchor[taint, canonical_name, canonical_port]."))
    | Name (Name.Identifier "Sanitize") -> Ok [Sanitize [AllSources; AllSinks; AllTito]]
    | Call { callee; arguments }
      when [%compare.equal: string option] (base_name callee) (Some "Sanitize") ->
        let parse_argument { Call.Argument.value = { Node.value = expression; _ }; _ } =
          parse_sanitize_annotation expression
        in
        List.map ~f:parse_argument arguments
        |> all
        >>| fun annotations -> [Sanitize (List.concat annotations)]
    | Call { callee; arguments = [{ Call.Argument.value = { value = Tuple expressions; _ }; _ }] }
      when [%compare.equal: string option] (base_name callee) (Some "Union") ->
        List.map expressions ~f:(fun expression ->
            parse_annotations
              ~path
              ~location:expression.Node.location
              ~model_name
              ~configuration
              ~parameters
              ~callable_parameter_names_to_positions
              ~is_object_target
              expression)
        |> all
        |> map ~f:List.concat
    | Call { callee; arguments = [{ Call.Argument.value = expression; _ }] } -> (
        let open Core.Result in
        match base_name callee with
        | Some "TaintSink" -> get_sink_kinds expression
        | Some "TaintSource" -> get_source_kinds expression
        | Some "TaintInTaintOut" -> get_taint_in_taint_out expression
        | Some "AddFeatureToArgument" ->
            extract_leafs expression
            >>| fun (_, breadcrumbs, via_features) ->
            [AddFeatureToArgument { breadcrumbs; via_features; path = [] }]
        | Some "AttachToSink" ->
            extract_attach_features ~name:"AttachToSink" expression
            >>| fun (breadcrumbs, via_features) ->
            [
              Sink
                {
                  sink = Sinks.Attach;
                  breadcrumbs;
                  via_features;
                  path = [];
                  leaf_names = [];
                  leaf_name_provided = false;
                  trace_length = None;
                };
            ]
        | Some "AttachToTito" ->
            extract_attach_features ~name:"AttachToTito" expression
            >>| fun (breadcrumbs, via_features) ->
            [Tito { tito = Sinks.Attach; breadcrumbs; via_features; path = [] }]
        | Some "AttachToSource" ->
            extract_attach_features ~name:"AttachToSource" expression
            >>| fun (breadcrumbs, via_features) ->
            [
              Source
                {
                  source = Sources.Attach;
                  breadcrumbs;
                  via_features;
                  path = [];
                  leaf_names = [];
                  leaf_name_provided = false;
                  trace_length = None;
                };
            ]
        | Some "PartialSink" ->
            let partial_sink =
              match Node.value expression with
              | Call
                  {
                    callee =
                      {
                        Node.value =
                          Name
                            (Name.Attribute
                              {
                                base = { Node.value = Expression.Name (Name.Identifier kind); _ };
                                attribute = "__getitem__";
                                _;
                              });
                        _;
                      };
                    arguments =
                      [
                        { Call.Argument.value = { Node.value = Name (Name.Identifier label); _ }; _ };
                      ];
                  } ->
                  if not (String.Map.Tree.mem configuration.partial_sink_labels kind) then
                    Error
                      (annotation_error (Format.asprintf "Unrecognized partial sink `%s`." kind))
                  else
                    let label_options =
                      String.Map.Tree.find_exn configuration.partial_sink_labels kind
                    in
                    if not (List.mem label_options label ~equal:String.equal) then
                      Error
                        (annotation_error
                           (Format.sprintf
                              "Unrecognized label `%s` for partial sink `%s` (choices: `%s`)"
                              label
                              kind
                              (String.concat label_options ~sep:", ")))
                    else
                      Ok (Sinks.PartialSink { kind; label })
              | _ -> invalid_annotation_error ()
            in
            partial_sink
            >>| fun partial_sink ->
            [
              Sink
                {
                  sink = partial_sink;
                  breadcrumbs = [];
                  via_features = [];
                  path = [];
                  leaf_names = [];
                  leaf_name_provided = false;
                  trace_length = None;
                };
            ]
        | _ -> invalid_annotation_error ())
    | Name (Name.Identifier "TaintInTaintOut") ->
        Ok [Tito { tito = Sinks.LocalReturn; breadcrumbs = []; via_features = []; path = [] }]
    | Name (Name.Identifier "ViaTypeOf") ->
        if is_object_target then
          (* Attribute annotations of the form `a: ViaTypeOf = ...` is equivalent to:
             TaintInTaintOut[ViaTypeOf[$global]] = ...` *)
          let via_features =
            [
              Features.ViaFeature.ViaTypeOf
                {
                  parameter =
                    AccessPath.Root.PositionalParameter
                      { name = attribute_symbolic_parameter; position = 0; positional_only = false };
                  tag = None;
                };
            ]
          in
          Ok [Tito { tito = Sinks.LocalReturn; breadcrumbs = []; via_features; path = [] }]
        else
          Error
            (annotation_error
               "A standalone `ViaTypeOf` without arguments can only be used in attribute or global \
                models.")
    | Expression.Tuple expressions ->
        List.map expressions ~f:(fun expression ->
            parse_annotations
              ~path
              ~location:expression.Node.location
              ~model_name
              ~configuration
              ~parameters
              ~callable_parameter_names_to_positions
              ~is_object_target
              expression)
        |> all
        >>| List.concat
    | _ -> invalid_annotation_error ()
  and parse_sanitize_annotation = function
    | Expression.Tuple expressions ->
        List.map ~f:(fun expression -> parse_sanitize_annotation expression.Node.value) expressions
        |> all
        >>| List.concat
    | Expression.Call
        { Call.callee; arguments = [{ Call.Argument.value = { Node.value = expression; _ }; _ }] }
      when [%compare.equal: string option] (base_name callee) (Some "TaintInTaintOut") ->
        let gather_sources_sinks (sources, sinks) = function
          | Source
              {
                source;
                breadcrumbs = [];
                via_features = [];
                leaf_names = [];
                leaf_name_provided = false;
                trace_length = None;
                path = [];
              } ->
              Ok (source :: sources, sinks)
          | Sink
              {
                sink;
                breadcrumbs = [];
                via_features = [];
                leaf_names = [];
                leaf_name_provided = false;
                trace_length = None;
                path = [];
              } ->
              Ok (sources, sink :: sinks)
          | taint_annotation ->
              Error
                (annotation_error
                   (Format.asprintf
                      "`%a` is not supported within `Sanitize[TaintInTaintOut[...]]`"
                      pp_taint_annotation
                      taint_annotation))
        in
        parse_annotation expression
        >>= List.fold_result ~init:([], []) ~f:gather_sources_sinks
        >>| fun (sources, sinks) -> [SpecificTito { sources; sinks }]
    | Expression.Name (Name.Identifier ("TaintSource" as identifier))
    | Expression.Name (Name.Identifier ("TaintSink" as identifier)) ->
        Error
          (annotation_error
             (Format.asprintf
                "`Sanitize[%s]` is ambiguous here. Did you mean `Sanitize`?"
                identifier))
    | Expression.Name (Name.Identifier "TaintInTaintOut") -> Ok [AllTito]
    | expression ->
        let to_sanitize = function
          | Source
              {
                source;
                breadcrumbs = [];
                via_features = [];
                leaf_names = [];
                leaf_name_provided = false;
                trace_length = None;
                path = [];
              } ->
              Ok (SpecificSource source)
          | Sink
              {
                sink;
                breadcrumbs = [];
                via_features = [];
                leaf_names = [];
                leaf_name_provided = false;
                trace_length = None;
                path = [];
              } ->
              Ok (SpecificSink sink)
          | taint_annotation ->
              Error
                (annotation_error
                   (Format.asprintf
                      "`%a` is not supported within `Sanitize[...]`"
                      pp_taint_annotation
                      taint_annotation))
        in
        parse_annotation expression
        >>= fun annotations -> List.map ~f:to_sanitize annotations |> all
  in
  parse_annotation (Node.value annotation)


let introduce_sink_taint
    ~root
    ~sinks_to_keep
    ~path
    ~leaf_names
    ~leaf_name_provided
    ~trace_length
    ({ Model.backward = { sink_taint; _ }; _ } as taint)
    taint_sink_kind
    via_features
    breadcrumbs
  =
  let open Core.Result in
  let should_keep_taint =
    match sinks_to_keep with
    | None -> true
    | Some sinks_to_keep -> Sinks.Set.mem taint_sink_kind sinks_to_keep
  in
  if should_keep_taint then
    let backward =
      let assign_backward_taint environment taint =
        BackwardState.assign ~weak:true ~root ~path taint environment
      in
      match taint_sink_kind with
      | Sinks.LocalReturn -> Error "Invalid TaintSink annotation `LocalReturn`"
      | _ ->
          let transform_call_information taint =
            if leaf_name_provided then
              BackwardTaint.transform
                BackwardTaint.call_info
                Map
                ~f:(function
                  | CallInfo.Declaration _ -> CallInfo.Declaration { leaf_name_provided = true }
                  | call_info -> call_info)
                taint
            else
              taint
          in
          let transform_trace_length taint =
            match trace_length with
            | Some trace_length ->
                Frame.transform TraceLength.Self Map ~f:(fun _ -> trace_length) taint
            | None -> taint
          in
          let leaf_names =
            leaf_names
            |> List.map ~f:Features.LeafNameInterned.intern
            |> Features.LeafNameSet.of_list
          in
          let breadcrumbs = Features.BreadcrumbSet.of_approximation breadcrumbs in
          let via_features = Features.ViaFeatureSet.of_list via_features in
          let leaf_taint =
            Frame.initial
            |> Frame.transform Features.LeafNameSet.Self Add ~f:leaf_names
            |> Frame.transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs
            |> Frame.transform Features.ViaFeatureSet.Self Add ~f:via_features
            |> transform_trace_length
            |> BackwardTaint.singleton taint_sink_kind
            |> transform_call_information
            |> BackwardState.Tree.create_leaf
          in
          let sink_taint = assign_backward_taint sink_taint leaf_taint in
          Ok { taint.backward with sink_taint }
    in
    backward >>| fun backward -> { taint with backward }
  else
    Ok taint


let introduce_taint_in_taint_out
    ~root
    ~path
    ({ Model.backward = { taint_in_taint_out; _ }; _ } as taint)
    taint_sink_kind
    via_features
    breadcrumbs
  =
  let open Core.Result in
  let backward =
    let assign_backward_taint environment taint =
      BackwardState.assign ~weak:true ~root ~path taint environment
    in
    let breadcrumbs = Features.BreadcrumbSet.of_approximation breadcrumbs in
    let via_features = Features.ViaFeatureSet.of_list via_features in
    match taint_sink_kind with
    | Sinks.LocalReturn
    | Sinks.Transform _ ->
        let return_taint =
          Domains.local_return_frame
          |> Frame.transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs
          |> Frame.transform Features.ViaFeatureSet.Self Add ~f:via_features
          |> BackwardTaint.singleton taint_sink_kind
          |> BackwardState.Tree.create_leaf
        in
        let taint_in_taint_out = assign_backward_taint taint_in_taint_out return_taint in
        Ok { taint.backward with taint_in_taint_out }
    | Sinks.Attach
      when Features.BreadcrumbSet.is_empty breadcrumbs
           && Features.ViaFeatureSet.is_bottom via_features ->
        Error "`Attach` must be accompanied by a list of features to attach."
    | Sinks.ParameterUpdate _
    | Sinks.Attach ->
        let update_taint =
          Frame.initial
          |> Frame.transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs
          |> Frame.transform Features.ViaFeatureSet.Self Add ~f:via_features
          |> BackwardTaint.singleton taint_sink_kind
          |> BackwardState.Tree.create_leaf
        in
        let taint_in_taint_out = assign_backward_taint taint_in_taint_out update_taint in
        Ok { taint.backward with taint_in_taint_out }
    | _ ->
        let error =
          Format.asprintf "Invalid TaintInTaintOut annotation `%s`" (Sinks.show taint_sink_kind)
        in
        Error error
  in
  backward >>| fun backward -> { taint with backward }


let introduce_source_taint
    ~root
    ~sources_to_keep
    ~path
    ~leaf_names
    ~leaf_name_provided
    ~trace_length
    ({ Model.forward = { source_taint }; _ } as taint)
    taint_source_kind
    via_features
    breadcrumbs
  =
  let open Core.Result in
  let should_keep_taint =
    match sources_to_keep with
    | None -> true
    | Some sources_to_keep -> Sources.Set.mem taint_source_kind sources_to_keep
  in
  if
    Sources.equal taint_source_kind Sources.Attach
    && List.is_empty breadcrumbs
    && List.is_empty via_features
  then
    Error "`Attach` must be accompanied by a list of features to attach."
  else if should_keep_taint then
    let breadcrumbs = Features.BreadcrumbSet.of_approximation breadcrumbs in
    let via_features = Features.ViaFeatureSet.of_list via_features in
    let source_taint =
      let transform_call_information taint =
        if leaf_name_provided then
          ForwardTaint.transform
            ForwardTaint.call_info
            Map
            ~f:(function
              | CallInfo.Declaration _ -> CallInfo.Declaration { leaf_name_provided = true }
              | call_info -> call_info)
            taint
        else
          taint
      in
      let transform_trace_length taint =
        match trace_length with
        | Some trace_length -> Frame.transform TraceLength.Self Map ~f:(fun _ -> trace_length) taint
        | None -> taint
      in

      let leaf_taint =
        let leaf_names =
          leaf_names |> List.map ~f:Features.LeafNameInterned.intern |> Features.LeafNameSet.of_list
        in
        Frame.initial
        |> Frame.transform Features.LeafNameSet.Self Add ~f:leaf_names
        |> Frame.transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs
        |> Frame.transform Features.ViaFeatureSet.Self Add ~f:via_features
        |> transform_trace_length
        |> ForwardTaint.singleton taint_source_kind
        |> transform_call_information
        |> ForwardState.Tree.create_leaf
      in
      ForwardState.assign ~weak:true ~root ~path leaf_taint source_taint
    in
    Ok { taint with forward = { source_taint } }
  else
    Ok taint


let sanitize_from_annotations annotations =
  let open Domains in
  let to_sanitize = function
    | AllSources -> { Sanitize.empty with sources = Some All }
    | SpecificSource source ->
        { Sanitize.empty with sources = Some (Specific (Sources.Set.singleton source)) }
    | AllSinks -> { Sanitize.empty with sinks = Some All }
    | SpecificSink sink ->
        { Sanitize.empty with sinks = Some (Specific (Sinks.Set.singleton sink)) }
    | AllTito -> { Sanitize.empty with tito = Some All }
    | SpecificTito { sources; sinks } ->
        {
          Sanitize.empty with
          tito =
            Some
              (Specific
                 {
                   sanitized_tito_sources = Sources.Set.of_list sources;
                   sanitized_tito_sinks = Sinks.Set.of_list sinks;
                 });
        }
  in
  annotations |> List.map ~f:to_sanitize |> List.fold ~init:Sanitize.empty ~f:Sanitize.join


let introduce_sanitize ~root model annotations =
  let roots =
    Domains.SanitizeRootMap.of_list [root, sanitize_from_annotations annotations]
    |> Domains.SanitizeRootMap.join model.Model.sanitizers.roots
  in
  let sanitizers = { model.sanitizers with roots } in
  { model with sanitizers }


let parse_find_clause ~path ({ Node.value; location } as expression) =
  match value with
  | Expression.Constant (Constant.String { StringLiteral.value; _ }) -> (
      match value with
      | "functions" -> Ok ModelQuery.FunctionModel
      | "methods" -> Ok ModelQuery.MethodModel
      | "attributes" -> Ok ModelQuery.AttributeModel
      | unsupported ->
          Error (model_verification_error ~path ~location (UnsupportedFindClause unsupported)))
  | _ -> Error (model_verification_error ~path ~location (InvalidFindClauseType expression))


let get_find_clause_as_string find_clause =
  match find_clause with
  | Ok ModelQuery.AttributeModel -> "attributes"
  | Ok ModelQuery.MethodModel -> "methods"
  | Ok ModelQuery.FunctionModel -> "functions"
  | _ -> "unsupported"


let is_callable_clause_kind find_clause =
  match find_clause with
  | Ok ModelQuery.MethodModel
  | Ok ModelQuery.FunctionModel ->
      true
  | _ -> false


let is_class_member_clause_kind find_clause =
  match find_clause with
  | Ok ModelQuery.MethodModel
  | Ok ModelQuery.AttributeModel ->
      true
  | _ -> false


let parse_name_constraint ~path ~location ({ Node.value; _ } as constraint_expression) =
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
                    attribute = ("matches" | "equals") as attribute;
                    _;
                  });
            _;
          } as callee;
        arguments;
      } -> (
      match arguments with
      | [
       {
         Call.Argument.value =
           {
             Node.value =
               Expression.Constant (Constant.String { StringLiteral.value = name_constraint; _ });
             _;
           };
         _;
       };
      ] -> (
          match attribute with
          | "matches" -> Ok (ModelQuery.Matches (Re2.create_exn name_constraint))
          | "equals" -> Ok (ModelQuery.Equals name_constraint)
          | _ -> failwith "impossible case")
      | _ ->
          Error
            (model_verification_error
               ~path
               ~location
               (InvalidModelQueryClauseArguments { callee; arguments })))
  | _ -> Error (model_verification_error ~path ~location (InvalidNameClause constraint_expression))


let parse_annotation_constraint ~path ~location ({ Node.value; _ } as constraint_expression) =
  match value with
  | Expression.Call
      {
        Call.callee =
          {
            Node.value =
              Expression.Name
                (Name.Attribute
                  { attribute = ("equals" | "matches" | "is_annotated_type") as attribute; _ });
            _;
          } as callee;
        arguments;
      } -> (
      match attribute, arguments with
      | ( "equals",
          [
            {
              Call.Argument.value =
                {
                  Node.value =
                    Expression.Constant (Constant.String { StringLiteral.value = type_name; _ });
                  _;
                };
              _;
            };
          ] ) ->
          Ok (ModelQuery.AnnotationNameConstraint (ModelQuery.Equals type_name))
      | ( "matches",
          [
            {
              Call.Argument.value =
                {
                  Node.value =
                    Expression.Constant
                      (Constant.String { StringLiteral.value = type_name_pattern; _ });
                  _;
                };
              _;
            };
          ] ) ->
          Ok
            (ModelQuery.AnnotationNameConstraint
               (ModelQuery.Matches (Re2.create_exn type_name_pattern)))
      | "is_annotated_type", [] -> Ok ModelQuery.IsAnnotatedTypeConstraint
      | _ ->
          Error
            (model_verification_error
               ~path
               ~location
               (InvalidModelQueryClauseArguments { callee; arguments })))
  | _ ->
      Error
        (model_verification_error
           ~path
           ~location
           (InvalidTypeAnnotationClause constraint_expression))


let parse_arguments_constraint ~path ~location ({ Node.value; _ } as constraint_expression) =
  match value with
  | Expression.Call
      {
        Call.callee =
          {
            Node.value =
              Expression.Name
                (Name.Attribute
                  {
                    base = { Node.value = Name (Name.Identifier "arguments"); _ };
                    attribute = ("contains" | "equals") as attribute;
                    _;
                  });
            _;
          };
        arguments;
      } -> (
      match attribute with
      | "contains" -> Ok (ModelQuery.ArgumentsConstraint.Contains arguments)
      | "equals" -> Ok (ModelQuery.ArgumentsConstraint.Equals arguments)
      | _ -> failwith "impossible case")
  | _ ->
      Error
        (model_verification_error ~path ~location (InvalidArgumentsClause constraint_expression))


let parse_decorator_constraint ~path ~location ({ Node.value; _ } as constraint_expression) =
  let open Core.Result in
  match value with
  | Expression.Call { Call.callee = { Node.value = Expression.Name _; _ } as callee; arguments }
    -> (
      match arguments with
      | [{ Call.Argument.name = None; Call.Argument.value = decorator_name_constraint }] ->
          parse_name_constraint ~path ~location decorator_name_constraint
          >>= fun name_constraint ->
          Ok { ModelQuery.DecoratorConstraint.name_constraint; arguments_constraint = None }
      | [
       { Call.Argument.name = None; value = first_constraint };
       { Call.Argument.name = None; value = second_constraint };
      ] -> (
          match
            ( parse_name_constraint ~path ~location first_constraint,
              parse_arguments_constraint ~path ~location second_constraint )
          with
          | Ok name_constraint, Ok arguments_constraint ->
              Ok
                {
                  ModelQuery.DecoratorConstraint.name_constraint;
                  arguments_constraint = Some arguments_constraint;
                }
          | _ ->
              parse_name_constraint ~path ~location second_constraint
              >>= fun name_constraint ->
              parse_arguments_constraint ~path ~location first_constraint
              >>= fun arguments_constraint ->
              Ok
                {
                  ModelQuery.DecoratorConstraint.name_constraint;
                  arguments_constraint = Some arguments_constraint;
                })
      | _ ->
          Error
            (model_verification_error
               ~path
               ~location
               (InvalidModelQueryClauseArguments { callee; arguments })))
  | _ ->
      Error
        (model_verification_error ~path ~location (InvalidDecoratorClause constraint_expression))


let parse_where_clause ~path ~find_clause ({ Node.value; location } as expression) =
  let open Core.Result in
  let invalid_model_query_where_clause ~path ~location callee =
    model_verification_error
      ~path
      ~location
      (InvalidModelQueryWhereClause
         { expression = callee; find_clause_kind = get_find_clause_as_string find_clause })
  in
  let rec parse_constraint ({ Node.value; _ } as constraint_expression) =
    match value with
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute { base = { Node.value = Name (Name.Identifier "name"); _ }; _ });
              _;
            };
          _;
        } ->
        parse_name_constraint ~path ~location constraint_expression
        >>= fun name_constraint -> Ok (ModelQuery.NameConstraint name_constraint)
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    { base = { Node.value = Name (Name.Identifier "type_annotation"); _ }; _ });
              _;
            } as callee;
          _;
        } ->
        if is_callable_clause_kind find_clause then
          Error (invalid_model_query_where_clause ~path ~location callee)
        else
          parse_annotation_constraint ~path ~location constraint_expression
          >>= fun annotation_constraint ->
          Ok (ModelQuery.AnnotationConstraint annotation_constraint)
    | Expression.Call
        {
          Call.callee = { Node.value = Expression.Name (Name.Identifier "Decorator"); _ } as callee;
          _;
        } ->
        if not (is_callable_clause_kind find_clause) then
          Error (invalid_model_query_where_clause ~path ~location callee)
        else
          parse_decorator_constraint ~path ~location constraint_expression
          >>= fun decorator_constraint ->
          Ok (ModelQuery.AnyDecoratorConstraint decorator_constraint)
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    { base = { Node.value = Name (Name.Identifier "return_annotation"); _ }; _ });
              _;
            } as callee;
          _;
        } ->
        if not (is_callable_clause_kind find_clause) then
          Error (invalid_model_query_where_clause ~path ~location callee)
        else
          parse_annotation_constraint ~path ~location constraint_expression
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
                      _;
                    });
              _;
            } as callee;
          _;
        } -> (
        match is_callable_clause_kind find_clause, parameter_constraint_kind with
        | true, "annotation" ->
            parse_annotation_constraint ~path ~location constraint_expression
            >>= fun parameter_constraint ->
            Ok
              (ModelQuery.AnyParameterConstraint
                 (ModelQuery.ParameterConstraint.AnnotationConstraint parameter_constraint))
        | _ -> Error (invalid_model_query_where_clause ~path ~location callee))
    | Expression.Call
        {
          Call.callee = { Node.value = Expression.Name (Name.Identifier "AnyOf"); _ };
          arguments = constraints;
        } ->
        List.map constraints ~f:(fun { Call.Argument.value; _ } -> parse_constraint value)
        |> all
        >>| fun constraints -> ModelQuery.AnyOf constraints
    | Expression.Call
        {
          Call.callee = { Node.value = Expression.Name (Name.Identifier "AllOf"); _ };
          arguments = constraints;
        } ->
        List.map constraints ~f:(fun { Call.Argument.value; _ } -> parse_constraint value)
        |> all
        >>| fun constraints -> ModelQuery.AllOf constraints
    | Expression.Call
        {
          Call.callee = { Node.value = Expression.Name (Name.Identifier "Not"); _ };
          arguments = [{ Call.Argument.value; _ }];
        } ->
        parse_constraint value >>= fun model_constraint -> Ok (ModelQuery.Not model_constraint)
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    {
                      base = { Node.value = Name (Name.Identifier "parent"); _ };
                      attribute = ("equals" | "matches") as attribute;
                      _;
                    });
              _;
            } as callee;
          arguments;
        } -> (
        if not (is_class_member_clause_kind find_clause) then
          Error (invalid_model_query_where_clause ~path ~location callee)
        else
          match arguments with
          | [
           {
             Call.Argument.value =
               {
                 Node.value =
                   Expression.Constant (Constant.String { StringLiteral.value = class_name; _ });
                 _;
               };
             _;
           };
          ] ->
              let name_constraint =
                match attribute with
                | "equals" -> ModelQuery.Equals class_name
                | "matches" -> ModelQuery.Matches (Re2.create_exn class_name)
                | _ -> failwith "impossible case"
              in
              Ok
                (ModelQuery.ParentConstraint
                   (ModelQuery.ClassConstraint.NameSatisfies name_constraint))
          | _ ->
              Error
                (model_verification_error
                   ~path
                   ~location
                   (InvalidModelQueryClauseArguments { callee; arguments })))
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    {
                      base = { Node.value = Name (Name.Identifier "parent"); _ };
                      attribute = "extends";
                      _;
                    });
              _;
            } as callee;
          arguments;
        } -> (
        if not (is_class_member_clause_kind find_clause) then
          Error (invalid_model_query_where_clause ~path ~location callee)
        else
          match arguments with
          | [
           {
             Call.Argument.value =
               {
                 Node.value =
                   Expression.Constant (Constant.String { StringLiteral.value = class_name; _ });
                 _;
               };
             _;
           };
          ] ->
              Ok (ModelQuery.ParentConstraint (Extends { class_name; is_transitive = false }))
          | [
           {
             Call.Argument.value =
               {
                 Node.value =
                   Expression.Constant (Constant.String { StringLiteral.value = class_name; _ });
                 _;
               };
             _;
           };
           {
             Call.Argument.name = Some { Node.value = "is_transitive"; _ };
             value = { Node.value = is_transitive_value; _ } as is_transitive_expression;
           };
          ] ->
              (match is_transitive_value with
              | Expression.Constant Constant.True -> Ok true
              | Expression.Constant Constant.False -> Ok false
              | _ ->
                  Error
                    (model_verification_error
                       ~path
                       ~location
                       (InvalidExtendsIsTransitive is_transitive_expression)))
              >>= fun is_transitive ->
              Ok (ModelQuery.ParentConstraint (Extends { class_name; is_transitive }))
          | _ ->
              Error
                (model_verification_error
                   ~path
                   ~location
                   (InvalidModelQueryClauseArguments { callee; arguments })))
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    {
                      base = { Node.value = Name (Name.Identifier "parent"); _ };
                      attribute = "decorator";
                      _;
                    });
              _;
            } as callee;
          _;
        } ->
        if not (is_class_member_clause_kind find_clause) then
          Error (invalid_model_query_where_clause ~path ~location callee)
        else
          parse_decorator_constraint ~path ~location constraint_expression
          >>= fun decorator_constraint ->
          Ok (ModelQuery.ParentConstraint (DecoratorSatisfies decorator_constraint))
    | Expression.Call { Call.callee; arguments = _ } ->
        Error (model_verification_error ~path ~location (UnsupportedCallee callee))
    | _ ->
        Error
          (model_verification_error ~path ~location (UnsupportedConstraint constraint_expression))
  in
  match value with
  | Expression.List items -> List.map items ~f:parse_constraint |> all
  | _ -> parse_constraint expression >>| List.return


let parse_parameter_where_clause ~path ({ Node.value; location } as expression) =
  let open Core.Result in
  let rec parse_constraint ({ Node.value; _ } as constraint_expression) =
    match value with
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute { base = { Node.value = Name (Name.Identifier "name"); _ }; _ });
              _;
            };
          _;
        } ->
        parse_name_constraint ~path ~location constraint_expression
        >>= fun name_constraint ->
        Ok (ModelQuery.ParameterConstraint.NameConstraint name_constraint)
    | Expression.Call
        {
          Call.callee = { Node.value = Expression.Name (Name.Identifier "AnyOf"); _ };
          arguments = constraints;
        } ->
        List.map constraints ~f:(fun { Call.Argument.value; _ } -> parse_constraint value)
        |> all
        >>| fun constraints -> ModelQuery.ParameterConstraint.AnyOf constraints
    | Expression.Call
        {
          Call.callee = { Node.value = Expression.Name (Name.Identifier "AllOf"); _ };
          arguments = constraints;
        } ->
        List.map constraints ~f:(fun { Call.Argument.value; _ } -> parse_constraint value)
        |> all
        >>| fun constraints -> ModelQuery.ParameterConstraint.AllOf constraints
    | Expression.Call
        {
          Call.callee = { Node.value = Expression.Name (Name.Identifier "Not"); _ };
          arguments = [{ Call.Argument.value; _ }];
        } ->
        parse_constraint value
        >>= fun query_constraint -> Ok (ModelQuery.ParameterConstraint.Not query_constraint)
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    { base = { Node.value = Name (Name.Identifier "type_annotation"); _ }; _ });
              _;
            };
          _;
        } ->
        parse_annotation_constraint ~path ~location constraint_expression
        >>= fun annotation_constraint ->
        Ok (ModelQuery.ParameterConstraint.AnnotationConstraint annotation_constraint)
    | Expression.Call
        {
          Call.callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    {
                      base = { Node.value = Name (Name.Identifier "index"); _ };
                      attribute = "equals" as attribute;
                      _;
                    });
              _;
            } as callee;
          arguments;
        } -> (
        match attribute, arguments with
        | ( "equals",
            [
              {
                Call.Argument.value =
                  { Node.value = Expression.Constant (Constant.Integer index); _ };
                _;
              };
            ] ) ->
            Ok (ModelQuery.ParameterConstraint.IndexConstraint index)
        | _ ->
            Error
              (model_verification_error
                 ~path
                 ~location
                 (InvalidModelQueryClauseArguments { callee; arguments })))
    | Expression.Call { Call.callee; _ } ->
        Error
          (model_verification_error
             ~path
             ~location
             (InvalidModelQueryWhereClause { expression = callee; find_clause_kind = "parameters" }))
    | _ ->
        Error
          (model_verification_error ~path ~location (UnsupportedConstraint constraint_expression))
  in
  match value with
  | Expression.List items -> List.map items ~f:parse_constraint |> all
  | _ -> parse_constraint expression >>| List.return


let parse_model_clause
    ~path
    ~configuration
    ~find_clause
    ~is_object_target
    ({ Node.value; location } as expression)
  =
  let open Core.Result in
  let invalid_model_query_model_clause ~path ~location callee =
    model_verification_error
      ~path
      ~location
      (InvalidModelQueryModelClause
         { expression = callee; find_clause_kind = get_find_clause_as_string find_clause })
  in
  let parse_model ({ Node.value; _ } as model_expression) =
    let parse_taint taint_expression =
      let parse_produced_taint expression =
        match Node.value expression with
        | Expression.Call
            {
              Call.callee =
                {
                  Node.value =
                    Expression.Name
                      (Name.Identifier
                        (("ParametricSourceFromAnnotation" | "ParametricSinkFromAnnotation") as
                        parametric_annotation));
                  _;
                };
              arguments =
                [
                  {
                    Call.Argument.name = Some { Node.value = "pattern"; _ };
                    value = { Node.value = Expression.Name (Name.Identifier pattern); _ };
                  };
                  {
                    Call.Argument.name = Some { Node.value = "kind"; _ };
                    value = { Node.value = Expression.Name (Name.Identifier kind); _ };
                  };
                ];
            } -> (
            match parametric_annotation with
            | "ParametricSourceFromAnnotation" ->
                Ok [ModelQuery.ParametricSourceFromAnnotation { source_pattern = pattern; kind }]
            | "ParametricSinkFromAnnotation" ->
                Ok [ModelQuery.ParametricSinkFromAnnotation { sink_pattern = pattern; kind }]
            | _ ->
                Error
                  (model_verification_error
                     ~path
                     ~location
                     (UnexpectedTaintAnnotation parametric_annotation)))
        | _ ->
            parse_annotations
              ~path
              ~location
              ~model_name:"model query"
              ~configuration
              ~parameters:[]
              ~callable_parameter_names_to_positions:None
              ~is_object_target
              ~is_model_query:true
              expression
            >>| List.map ~f:(fun taint -> ModelQuery.TaintAnnotation taint)
      in

      match Node.value taint_expression with
      | Expression.List taint_annotations ->
          List.map taint_annotations ~f:parse_produced_taint |> all >>| List.concat
      | _ -> parse_produced_taint taint_expression
    in
    match value with
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "Returns"); _ } as callee;
          arguments = [{ Call.Argument.value = taint; _ }];
        } ->
        if not (is_callable_clause_kind find_clause) then
          Error (invalid_model_query_model_clause ~path ~location callee)
        else
          parse_taint taint >>| fun taint -> ModelQuery.ReturnTaint taint
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "AttributeModel"); _ } as callee;
          arguments = [{ Call.Argument.value = taint; _ }];
        } ->
        if is_callable_clause_kind find_clause then
          Error (invalid_model_query_model_clause ~path ~location callee)
        else
          parse_taint taint >>| fun taint -> ModelQuery.AttributeTaint taint
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "NamedParameter"); _ } as callee;
          arguments =
            [
              {
                Call.Argument.value =
                  { Node.value = Constant (Constant.String { StringLiteral.value = name; _ }); _ };
                name = Some { Node.value = "name"; _ };
              };
              { Call.Argument.value = taint; name = Some { Node.value = "taint"; _ } };
            ];
        } ->
        if not (is_callable_clause_kind find_clause) then
          Error (invalid_model_query_model_clause ~path ~location callee)
        else
          parse_taint taint >>| fun taint -> ModelQuery.NamedParameterTaint { name; taint }
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "PositionalParameter"); _ } as callee;
          arguments =
            [
              {
                Call.Argument.value = { Node.value = Constant (Constant.Integer index); _ };
                name = Some { Node.value = "index"; _ };
              };
              { Call.Argument.value = taint; name = Some { Node.value = "taint"; _ } };
            ];
        } ->
        if not (is_callable_clause_kind find_clause) then
          Error (invalid_model_query_model_clause ~path ~location callee)
        else
          parse_taint taint >>| fun taint -> ModelQuery.PositionalParameterTaint { index; taint }
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "AllParameters"); _ } as callee;
          arguments = [{ Call.Argument.value = taint; _ }];
        } ->
        if not (is_callable_clause_kind find_clause) then
          Error (invalid_model_query_model_clause ~path ~location callee)
        else
          parse_taint taint >>| fun taint -> ModelQuery.AllParametersTaint { excludes = []; taint }
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "AllParameters"); _ } as callee;
          arguments =
            [
              { Call.Argument.value = taint; _ };
              { Call.Argument.name = Some { Node.value = "exclude"; _ }; value = excludes };
            ];
        } ->
        if not (is_callable_clause_kind find_clause) then
          Error (invalid_model_query_model_clause ~path ~location callee)
        else
          let excludes =
            let parse_string_to_exclude ({ Node.value; location } as exclude) =
              match value with
              | Expression.Constant (Constant.String { StringLiteral.value; _ }) ->
                  Core.Result.Ok value
              | _ ->
                  Error (model_verification_error ~path ~location (InvalidParameterExclude exclude))
            in
            match Node.value excludes with
            | Expression.List exclude_strings ->
                List.map exclude_strings ~f:parse_string_to_exclude |> Core.Result.all
            | _ -> parse_string_to_exclude excludes >>| fun exclude -> [exclude]
          in
          excludes
          >>= fun excludes ->
          parse_taint taint >>| fun taint -> ModelQuery.AllParametersTaint { excludes; taint }
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "Parameters"); _ } as callee;
          arguments;
        } -> (
        match is_callable_clause_kind find_clause, arguments with
        | false, _ -> Error (invalid_model_query_model_clause ~path ~location callee)
        | _, [{ Call.Argument.value = taint; _ }] ->
            parse_taint taint >>| fun taint -> ModelQuery.ParameterTaint { where = []; taint }
        | ( _,
            [
              { Call.Argument.value = taint; _ };
              { Call.Argument.name = Some { Node.value = "where"; _ }; value = where_clause };
            ] ) ->
            parse_parameter_where_clause ~path where_clause
            >>= fun where ->
            parse_taint taint >>| fun taint -> ModelQuery.ParameterTaint { where; taint }
        | _ ->
            Error
              (model_verification_error
                 ~path
                 ~location
                 (InvalidModelQueryClauseArguments { callee; arguments })))
    | _ ->
        Error
          (model_verification_error ~path ~location (UnexpectedModelExpression model_expression))
  in
  match value with
  | Expression.List items -> List.map items ~f:parse_model |> all
  | _ -> parse_model expression >>| List.return


let parameters_of_callable_annotation { Type.Callable.implementation; overloads; _ } =
  let parameters_of_overload = function
    | { Type.Callable.parameters = Type.Callable.Defined parameters; _ } ->
        List.mapi parameters ~f:(fun position parameter -> position, parameter)
    | _ -> []
  in
  parameters_of_overload implementation
  @ (List.map overloads ~f:parameters_of_overload |> List.concat)


let add_signature_based_breadcrumbs ~resolution root ~callable_annotation breadcrumbs =
  match callable_annotation with
  | None -> breadcrumbs
  | Some callable_annotation ->
      let type_breadcrumbs =
        match root with
        | AccessPath.Root.PositionalParameter { position; _ } ->
            parameters_of_callable_annotation callable_annotation
            |> List.filter_map ~f:(fun (parameter_position, parameter) ->
                   Option.some_if (parameter_position = position) parameter)
            |> List.fold ~init:Features.BreadcrumbSet.bottom ~f:(fun sofar parameter ->
                   parameter
                   |> Type.Callable.Parameter.annotation
                   >>| CallGraph.ReturnType.from_annotation ~resolution
                   |> Option.value ~default:CallGraph.ReturnType.none
                   |> Features.type_breadcrumbs
                   |> Features.BreadcrumbSet.add_set ~to_add:sofar)
        | AccessPath.Root.NamedParameter { name; _ } ->
            parameters_of_callable_annotation callable_annotation
            |> List.filter_map ~f:(fun (_, parameter) ->
                   match parameter with
                   | Type.Callable.Parameter.KeywordOnly { name = parameter_name; _ }
                     when String.equal parameter_name ("$parameter$" ^ name) ->
                       Some parameter
                   | Type.Callable.Parameter.Named { name = parameter_name; _ }
                     when String.equal parameter_name name ->
                       Some parameter
                   | _ -> None)
            |> List.fold ~init:Features.BreadcrumbSet.bottom ~f:(fun sofar parameter ->
                   parameter
                   |> Type.Callable.Parameter.annotation
                   >>| CallGraph.ReturnType.from_annotation ~resolution
                   |> Option.value ~default:CallGraph.ReturnType.none
                   |> Features.type_breadcrumbs
                   |> Features.BreadcrumbSet.add_set ~to_add:sofar)
        | AccessPath.Root.LocalResult ->
            let { Type.Callable.implementation = { Type.Callable.annotation; _ }; _ } =
              callable_annotation
            in
            annotation
            |> CallGraph.ReturnType.from_annotation ~resolution
            |> Features.type_breadcrumbs
        | _ -> Features.BreadcrumbSet.empty
      in
      List.rev_append (Features.BreadcrumbSet.to_approximation type_breadcrumbs) breadcrumbs


let parse_parameter_taint
    ~path
    ~location
    ~model_name
    ~configuration
    ~parameters
    ~callable_parameter_names_to_positions
    ~is_object_target
    (root, _name, parameter)
  =
  parameter.Node.value.Parameter.annotation
  >>| parse_annotations
        ~path
        ~location
        ~model_name
        ~configuration
        ~parameters
        ~callable_parameter_names_to_positions
        ~is_object_target
  |> Option.value ~default:(Ok [])
  |> Core.Result.map ~f:(List.map ~f:(fun annotation -> annotation, ParameterAnnotation root))


let add_taint_annotation_to_model
    ~resolution
    ~path
    ~location
    ~model_name
    ~annotation_kind
    ~callable_annotation
    ~sources_to_keep
    ~sinks_to_keep
    model
    annotation
  =
  let open Core.Result in
  let invalid_model_for_taint error_message =
    model_verification_error
      ~path
      ~location
      (InvalidModelForTaint { model_name; error = error_message })
  in
  match annotation_kind with
  | ReturnAnnotation -> (
      let root = AccessPath.Root.LocalResult in
      match annotation with
      | Sink { sink; breadcrumbs; via_features; path; leaf_names; leaf_name_provided; trace_length }
        ->
          breadcrumbs
          |> List.map ~f:Features.BreadcrumbInterned.intern
          |> List.map ~f:Features.BreadcrumbSet.inject
          |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
          |> introduce_sink_taint
               ~root
               ~path
               ~leaf_names
               ~leaf_name_provided
               ~trace_length
               ~sinks_to_keep
               model
               sink
               via_features
          |> map_error ~f:invalid_model_for_taint
      | Source
          { source; breadcrumbs; via_features; path; leaf_names; leaf_name_provided; trace_length }
        ->
          breadcrumbs
          |> List.map ~f:Features.BreadcrumbInterned.intern
          |> List.map ~f:Features.BreadcrumbSet.inject
          |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
          |> introduce_source_taint
               ~root
               ~path
               ~leaf_names
               ~leaf_name_provided
               ~trace_length
               ~sources_to_keep
               model
               source
               via_features
          |> map_error ~f:invalid_model_for_taint
      | Tito _ ->
          Error
            (model_verification_error
               ~path
               ~location
               (InvalidReturnAnnotation { model_name; annotation = "TaintInTaintOut" }))
      | AddFeatureToArgument _ ->
          Error
            (model_verification_error
               ~path
               ~location
               (InvalidReturnAnnotation { model_name; annotation = "AddFeatureToArgument" }))
      | Sanitize annotations -> Ok (introduce_sanitize ~root model annotations))
  | ParameterAnnotation root -> (
      match annotation with
      | Sink { sink; breadcrumbs; via_features; path; leaf_names; leaf_name_provided; trace_length }
        ->
          breadcrumbs
          |> List.map ~f:Features.BreadcrumbInterned.intern
          |> List.map ~f:Features.BreadcrumbSet.inject
          |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
          |> introduce_sink_taint
               ~root
               ~path
               ~leaf_names
               ~leaf_name_provided
               ~trace_length
               ~sinks_to_keep
               model
               sink
               via_features
          |> map_error ~f:invalid_model_for_taint
      | Source
          { source; breadcrumbs; via_features; path; leaf_names; leaf_name_provided; trace_length }
        ->
          breadcrumbs
          |> List.map ~f:Features.BreadcrumbInterned.intern
          |> List.map ~f:Features.BreadcrumbSet.inject
          |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
          |> introduce_source_taint
               ~root
               ~path
               ~leaf_names
               ~leaf_name_provided
               ~trace_length
               ~sources_to_keep
               model
               source
               via_features
          |> map_error ~f:invalid_model_for_taint
      | Tito { tito; breadcrumbs; via_features; path } ->
          (* For tito, both the parameter and the return type can provide type based breadcrumbs *)
          breadcrumbs
          |> List.map ~f:Features.BreadcrumbInterned.intern
          |> List.map ~f:Features.BreadcrumbSet.inject
          |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
          |> add_signature_based_breadcrumbs
               ~resolution
               AccessPath.Root.LocalResult
               ~callable_annotation
          |> introduce_taint_in_taint_out ~root ~path model tito via_features
          |> map_error ~f:invalid_model_for_taint
      | AddFeatureToArgument { breadcrumbs; via_features; path } ->
          breadcrumbs
          |> List.map ~f:Features.BreadcrumbInterned.intern
          |> List.map ~f:Features.BreadcrumbSet.inject
          |> add_signature_based_breadcrumbs ~resolution root ~callable_annotation
          |> introduce_sink_taint
               ~root
               ~path
               ~leaf_names:[]
               ~leaf_name_provided:false
               ~trace_length:None
               ~sinks_to_keep
               model
               Sinks.AddFeatureToArgument
               via_features
          |> map_error ~f:invalid_model_for_taint
      | Sanitize annotations -> Ok (introduce_sanitize ~root model annotations))


let parse_return_taint
    ~path
    ~location
    ~model_name
    ~configuration
    ~parameters
    ~callable_parameter_names_to_positions
    ~is_object_target
    expression
  =
  let open Core.Result in
  parse_annotations
    ~path
    ~location
    ~model_name
    ~configuration
    ~parameters
    ~callable_parameter_names_to_positions
    ~is_object_target
    expression
  |> map ~f:(List.map ~f:(fun annotation -> annotation, ReturnAnnotation))


type parsed_signature = {
  signature: Define.Signature.t;
  location: Location.t;
  call_target: Target.t;
}

type parsed_attribute = {
  name: Reference.t;
  source_annotation: Expression.t option;
  sink_annotation: Expression.t option;
  decorators: Decorator.t list;
  location: Location.t;
  call_target: Target.t;
}

type parsed_statement =
  | ParsedSignature of parsed_signature
  | ParsedAttribute of parsed_attribute
  | ParsedQuery of ModelQuery.rule

type model_or_query =
  | Model of (Model.WithTarget.t * Reference.t option)
  | Query of ModelQuery.rule

let resolve_global_callable
    ~path
    ~location
    ~verify_decorators
    ~resolution
    ({ Define.Signature.name; decorators; _ } as define)
  =
  (* Since properties and setters share the same undecorated name, we need to special-case them. *)
  let open ModelVerifier in
  if signature_is_property define then
    find_method_definitions ~resolution ~predicate:is_property name
    |> List.hd
    >>| Type.Callable.create_from_implementation
    >>| (fun callable -> Global.Attribute callable)
    |> Core.Result.return
  else if Define.Signature.is_property_setter define then
    find_method_definitions ~resolution ~predicate:Define.is_property_setter name
    |> List.hd
    >>| Type.Callable.create_from_implementation
    >>| (fun callable -> Global.Attribute callable)
    |> Core.Result.return
  else if verify_decorators && not (List.is_empty decorators) then
    (* Ensure that models don't declare decorators that our taint analyses doesn't understand. We
       check for the verify_decorators flag, as defines originating from
       `create_model_from_annotation` are not user-specified models that we're parsing. *)
    Error
      (model_verification_error
         ~path
         ~location
         (UnexpectedDecorators { name; unexpected_decorators = decorators }))
  else
    Ok (resolve_global ~resolution name)


let adjust_sanitize_and_modes_and_skipped_override
    ~path
    ~define_name
    ~configuration
    ~top_level_decorators
    ~is_object_target
    model
  =
  let open Core.Result in
  let create_get_item_call ~location callee arguments =
    List.map ~f:(fun { Call.Argument.value; _ } -> value) arguments
    |> Ast.Expression.get_item_call callee ~location
    |> Node.create ~location
  in
  let create_function_call ~location callee arguments =
    Ast.Expression.Expression.Call
      { callee = { Node.location; value = Name (Name.Identifier callee) }; arguments }
    |> Node.create ~location
  in
  let parse_sanitize_annotations ~location ~original_expression arguments =
    (* Pretend that it is a `Sanitize[...]` expression and use the annotation parser. *)
    let expression = create_get_item_call ~location "Sanitize" arguments in
    parse_annotations
      ~path
      ~location
      ~model_name:(Reference.show define_name)
      ~configuration
      ~parameters:[]
      ~callable_parameter_names_to_positions:None
      ~is_object_target
      expression
    |> function
    | Ok [Sanitize sanitize_annotations] -> Ok (sanitize_from_annotations sanitize_annotations)
    | Ok _ -> failwith "impossible case"
    | Error ({ ModelVerificationError.kind = InvalidTaintAnnotation { reason; _ }; _ } as error) ->
        Error
          {
            error with
            kind = InvalidTaintAnnotation { taint_annotation = original_expression; reason };
          }
    | Error error -> Error error
  in
  let join_with_sanitize_decorator ~sanitizers ~location ~original_expression arguments =
    let annotation_error reason =
      model_verification_error
        ~path
        ~location
        (InvalidTaintAnnotation { taint_annotation = original_expression; reason })
    in
    match arguments with
    | None -> Ok { sanitizers with Model.Sanitizers.global = Sanitize.all }
    | Some
        [
          {
            Call.Argument.value =
              {
                Node.value =
                  Expression.Name
                    (Name.Identifier
                      (("TaintSource" | "TaintSink" | "TaintInTaintOut" | "Parameters") as
                      identifier));
                _;
              };
            _;
          };
        ] -> (
        match identifier with
        | "TaintSource" when not is_object_target ->
            Error
              (annotation_error
                 "`TaintSource` is not supported within `Sanitize()`. Did you mean to use \
                  `SanitizeSingleTrace(...)`?")
        | "TaintSink" when not is_object_target ->
            Error
              (annotation_error
                 "`TaintSink` is not supported within `Sanitize()`. Did you mean to use \
                  `SanitizeSingleTrace(...)`?")
        | "TaintSource" ->
            let global = { sanitizers.global with sources = Some All } in
            Ok { sanitizers with global }
        | "TaintSink" ->
            let global = { sanitizers.global with sinks = Some All } in
            Ok { sanitizers with global }
        | "TaintInTaintOut" ->
            let global = { sanitizers.global with tito = Some All } in
            Ok { sanitizers with global }
        | "Parameters" -> Ok { sanitizers with parameters = Sanitize.all }
        | _ -> failwith "impossible")
    | Some
        [
          { Call.Argument.value = { Node.value = Expression.Call { Call.callee; arguments }; _ }; _ };
        ]
      when [%compare.equal: string option] (base_name callee) (Some "Parameters") ->
        parse_sanitize_annotations ~location ~original_expression arguments
        >>| fun parameters_sanitize ->
        { sanitizers with parameters = Sanitize.join sanitizers.parameters parameters_sanitize }
    | Some arguments -> (
        parse_sanitize_annotations ~location ~original_expression arguments
        >>= function
        | { Sanitize.sources = Some _; _ } when not is_object_target ->
            Error
              (annotation_error
                 "`TaintSource` is not supported within `Sanitize(...)`. Did you mean to use \
                  `SanitizeSingleTrace(...)`?")
        | { Sanitize.sinks = Some _; _ } when not is_object_target ->
            Error
              (annotation_error
                 "`TaintSink` is not supported within `Sanitize(...)`. Did you mean to use \
                  `SanitizeSingleTrace(...)`?")
        | global_sanitize ->
            Ok { sanitizers with global = Sanitize.join sanitizers.global global_sanitize })
  in
  let join_with_sanitize_single_trace_decorator ~sanitizers ~location ~original_expression arguments
    =
    let annotation_error reason =
      model_verification_error
        ~path
        ~location
        (InvalidTaintAnnotation { taint_annotation = original_expression; reason })
    in
    match arguments with
    | None ->
        Error
          (annotation_error
             "`SanitizeSingleTrace()` is ambiguous. Did you mean \
              `SanitizeSingleTrace(TaintSource)` or `SanitizeSingleTrace(TaintSink)`?")
    | Some
        [
          {
            Call.Argument.value = { Node.value = Expression.Name (Name.Identifier "TaintSource"); _ };
            _;
          };
        ] ->
        let global = { sanitizers.Model.Sanitizers.global with sources = Some All } in
        Ok { sanitizers with global }
    | Some
        [
          {
            Call.Argument.value = { Node.value = Expression.Name (Name.Identifier "TaintSink"); _ };
            _;
          };
        ] ->
        let global = { sanitizers.Model.Sanitizers.global with sinks = Some All } in
        Ok { sanitizers with global }
    | Some arguments -> (
        parse_sanitize_annotations ~location ~original_expression arguments
        >>= function
        | { Sanitize.tito = Some _; _ } ->
            Error
              (annotation_error
                 "`TaintInTaintOut` is not supported within `SanitizeSingleTrace(...)`. Did you \
                  mean to use `Sanitize(...)`?")
        | sanitizer -> Ok { sanitizers with global = Sanitize.join sanitizers.global sanitizer })
  in
  let join_with_decorator
      (sanitizers, modes, skipped_override)
      { Decorator.name = { Node.value = name; location = decorator_location }; arguments }
    =
    let name = Reference.show name in
    let original_expression =
      create_function_call ~location:decorator_location name (Option.value ~default:[] arguments)
    in
    match name with
    | "Sanitize" ->
        join_with_sanitize_decorator
          ~sanitizers
          ~location:decorator_location
          ~original_expression
          arguments
        >>| fun sanitizers -> sanitizers, modes, skipped_override
    | "SanitizeSingleTrace" ->
        join_with_sanitize_single_trace_decorator
          ~sanitizers
          ~location:decorator_location
          ~original_expression
          arguments
        >>| fun sanitizers -> sanitizers, modes, skipped_override
    | "SkipAnalysis" -> Ok (sanitizers, Model.ModeSet.add SkipAnalysis modes, skipped_override)
    | "SkipDecoratorWhenInlining" ->
        Ok (sanitizers, Model.ModeSet.add SkipDecoratorWhenInlining modes, skipped_override)
    | "SkipOverrides" -> Ok (sanitizers, Model.ModeSet.add SkipOverrides modes, Some define_name)
    | "SkipObscure" -> Ok (sanitizers, Model.ModeSet.remove Obscure modes, skipped_override)
    | _ -> Ok (sanitizers, modes, skipped_override)
  in
  List.fold_result
    top_level_decorators
    ~f:join_with_decorator
    ~init:(model.Model.sanitizers, model.Model.modes, None)
  >>| fun (sanitizers, modes, skipped_override) ->
  { model with sanitizers; modes }, skipped_override


let compute_sources_and_sinks_to_keep ~configuration ~rule_filter =
  match rule_filter with
  | None -> None, None
  | Some rule_filter ->
      let rule_filter = Int.Set.of_list rule_filter in
      let sources_to_keep, sinks_to_keep =
        let { TaintConfiguration.rules; _ } = configuration in
        let rules =
          (* The user annotations for partial sinks will be the untriggered ones, even though the
             rule expects triggered sinks. *)
          let untrigger_partial_sinks sink =
            match sink with
            | Sinks.TriggeredPartialSink { kind; label } -> Sinks.PartialSink { kind; label }
            | _ -> sink
          in
          List.filter_map rules ~f:(fun { TaintConfiguration.Rule.code; sources; sinks; _ } ->
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
            ( Sources.Set.union sources (Sources.Set.of_list rule_sources),
              Sinks.Set.union sinks (Sinks.Set.of_list rule_sinks) ))
      in
      Some sources_to_keep, Some sinks_to_keep


let parse_statement ~resolution ~path ~configuration statement =
  let open Core.Result in
  let global_resolution = Resolution.global_resolution resolution in
  match statement with
  | {
   Node.value =
     Statement.Define
       {
         signature = { name; _ } as signature;
         body =
           [
             { value = Statement.Expression { value = Expression.Constant Constant.Ellipsis; _ }; _ };
           ];
         _;
       };
   location;
  } ->
      let class_candidate =
        Reference.prefix name
        |> Option.map ~f:(GlobalResolution.parse_reference global_resolution)
        |> Option.bind ~f:(GlobalResolution.class_summary global_resolution)
      in
      let call_target =
        match class_candidate with
        | Some _ when Define.Signature.is_property_setter signature ->
            Target.create_property_setter name
        | Some _ -> Target.create_method name
        | None -> Target.create_function name
      in
      Ok [ParsedSignature { signature; location; call_target }]
  | { Node.value = Statement.Define { signature = { name; _ }; _ }; location } ->
      Error (model_verification_error ~path ~location (DefineBodyNotEllipsis (Reference.show name)))
  | {
   Node.value =
     Class
       {
         Class.name;
         base_arguments;
         body =
           [
             {
               Node.value =
                 Statement.Expression { Node.value = Expression.Constant Constant.Ellipsis; _ };
               _;
             };
           ];
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
        List.filter_map base_arguments ~f:class_sink_base
      in
      let source_annotations, extra_decorators =
        let decorator_with_name name =
          Node.create_with_default_location (Expression.Name (Name.Identifier name))
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
        List.filter_map base_arguments ~f:class_source_base
        |> List.fold ~init:([], []) ~f:(fun (source_annotations, decorators) -> function
             | Either.First source_annotation -> source_annotation :: source_annotations, decorators
             | Either.Second decorator -> source_annotations, decorator :: decorators)
      in
      if
        (not (List.is_empty sink_annotations))
        || (not (List.is_empty source_annotations))
        || not (List.is_empty extra_decorators)
      then
        ModelVerifier.class_summaries ~resolution name
        |> Option.bind ~f:List.hd
        |> Option.map ~f:(fun { Node.value = { Class.body; _ }; _ } ->
               let signature { Node.value; location } =
                 match value with
                 | Statement.Define
                     {
                       Define.signature =
                         { Define.Signature.name; parameters; decorators; _ } as signature;
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
                                   Some
                                     (Node.create_with_default_location
                                        (Expression.Constant Constant.Ellipsis))
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
                         {
                           signature =
                             {
                               signature with
                               Define.Signature.parameters;
                               return_annotation = source_annotation;
                               decorators;
                             };
                           location;
                           call_target = Target.create_method name;
                         }
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
                         [signature ~extra_decorators ~source_annotation:None ~sink_annotation:None]
                       else
                         []
                     in
                     skip_analysis_or_overrides_defines @ sources @ sinks
                 | _ -> []
               in

               List.concat_map body ~f:signature)
        |> Option.value ~default:[]
        |> return
      else
        Ok []
  | { Node.value = Class { Class.name; _ }; location } ->
      Error (model_verification_error ~path ~location (ClassBodyNotEllipsis (Reference.show name)))
  | {
   Node.value =
     Assign
       { Assign.target = { Node.value = Name name; _ } as target; annotation = Some annotation; _ };
   location;
  } ->
      if not (is_simple_name name) then
        Error (model_verification_error ~path ~location (InvalidIdentifier target))
      else if Expression.show annotation |> String.is_prefix ~prefix:"Sanitize[TaintInTaintOut["
      then
        Error
          (model_verification_error
             ~path
             ~location
             (InvalidTaintAnnotation
                {
                  taint_annotation = annotation;
                  reason = "TaintInTaintOut sanitizers cannot be modelled on attributes";
                }))
      else if
        Expression.show annotation |> String.equal "Sanitize"
        || Expression.show annotation |> String.is_prefix ~prefix:"Sanitize[TaintSource"
        || Expression.show annotation |> String.is_prefix ~prefix:"Sanitize[TaintSink"
      then
        let name = name_to_reference_exn name in
        let arguments =
          match annotation.Node.value with
          | Expression.Call { arguments; _ } -> Some arguments
          | _ -> None
        in
        let decorator =
          {
            Decorator.name = Node.create_with_default_location (Reference.create "Sanitize");
            arguments;
          }
        in
        Ok
          [
            ParsedAttribute
              {
                name;
                source_annotation = None;
                sink_annotation = None;
                decorators = [decorator];
                location;
                call_target = Target.create_object name;
              };
          ]
      else if Expression.show annotation |> String.is_substring ~substring:"TaintSource[" then
        let name = name_to_reference_exn name in
        Ok
          [
            ParsedAttribute
              {
                name;
                source_annotation = Some annotation;
                sink_annotation = None;
                decorators = [];
                location;
                call_target = Target.create_object name;
              };
          ]
      else if
        Expression.show annotation |> String.is_substring ~substring:"TaintSink["
        || Expression.show annotation |> String.is_substring ~substring:"TaintInTaintOut["
      then
        let name = name_to_reference_exn name in
        Ok
          [
            ParsedAttribute
              {
                name;
                source_annotation = None;
                sink_annotation = Some annotation;
                decorators = [];
                location;
                call_target = Target.create_object name;
              };
          ]
      else if Expression.show annotation |> String.equal "ViaTypeOf" then
        let name = name_to_reference_exn name in
        Ok
          [
            ParsedAttribute
              {
                name;
                source_annotation = None;
                sink_annotation = Some annotation;
                decorators = [];
                location;
                call_target = Target.create_object name;
              };
          ]
      else
        Error
          (model_verification_error
             ~path
             ~location
             (InvalidTaintAnnotation
                { taint_annotation = annotation; reason = "Unsupported annotation for attributes" }))
  | {
   Node.value =
     Expression
       {
         Node.value =
           Expression.Call
             {
               Call.callee = { Node.value = Expression.Name (Name.Identifier "ModelQuery"); _ };
               arguments;
             };
         _;
       };
   location;
  } ->
      let clauses =
        match arguments with
        | [
         {
           Call.Argument.name = Some { Node.value = "name"; _ };
           value =
             {
               Node.value = Expression.Constant (Constant.String { StringLiteral.value = name; _ });
               _;
             };
         };
         { Call.Argument.name = Some { Node.value = "find"; _ }; value = find_clause };
         { Call.Argument.name = Some { Node.value = "where"; _ }; value = where_clause };
         { Call.Argument.name = Some { Node.value = "model"; _ }; value = model_clause };
        ] ->
            let parsed_find_clause = parse_find_clause ~path find_clause in
            let is_object_target = not (is_callable_clause_kind parsed_find_clause) in
            Ok
              ( name,
                parsed_find_clause,
                parse_where_clause ~path ~find_clause:parsed_find_clause where_clause,
                parse_model_clause
                  ~path
                  ~configuration
                  ~find_clause:parsed_find_clause
                  ~is_object_target
                  model_clause )
        | _ -> Error (model_verification_error ~path ~location (InvalidModelQueryClauses arguments))
      in

      clauses
      >>= fun (name, find_clause, where_clause, model_clause) ->
      find_clause
      >>= fun rule_kind ->
      where_clause
      >>= fun query ->
      model_clause
      >>| fun productions ->
      [ParsedQuery { ModelQuery.rule_kind; query; productions; name; location }]
  | { Node.location; _ } ->
      Error (model_verification_error ~path ~location (UnexpectedStatement statement))


let create_model_from_signature
    ~resolution
    ~path
    ~configuration
    ~sources_to_keep
    ~sinks_to_keep
    ~is_obscure
    {
      signature =
        { Define.Signature.name = callable_name; parameters; return_annotation; decorators; _ } as
        define;
      location;
      call_target;
    }
  =
  let open Core.Result in
  let open ModelVerifier in
  let is_object_target = false in
  (* Strip off the decorators only used for taint annotations. *)
  let top_level_decorators, define =
    let get_taint_decorator decorator_expression =
      match Decorator.from_expression decorator_expression with
      | None -> Either.Second decorator_expression
      | Some ({ Decorator.name = { Node.value = name; _ }; _ } as decorator) -> (
          match Reference.as_list name with
          | ["Sanitize"]
          | ["SanitizeSingleTrace"]
          | ["SkipAnalysis"]
          | ["SkipDecoratorWhenInlining"]
          | ["SkipOverrides"]
          | ["SkipObscure"] ->
              Either.first decorator
          | _ -> Either.Second decorator_expression)
    in
    let sanitizers, nonsanitizers = List.partition_map define.decorators ~f:get_taint_decorator in
    sanitizers, { define with decorators = nonsanitizers }
  in
  (* To ensure that the start/stop lines can be used for commenting out models,
   * we include the earliest decorator location. *)
  let location =
    let start =
      match decorators with
      | [] -> location.Location.start
      | first :: _ -> first.location.start
    in
    { location with start }
  in
  (* Make sure we know about what we model. *)
  let model_verification_error kind = Error { ModelVerificationError.kind; path; location } in
  let callable_annotation =
    resolve_global_callable ~path ~location ~verify_decorators:true ~resolution define
    >>= function
    | None -> (
        let module_name = Reference.first callable_name in
        let module_resolved = resolve_global ~resolution (Reference.create module_name) in
        match module_resolved with
        | Some _ ->
            model_verification_error
              (MissingSymbol { module_name; symbol_name = Reference.show callable_name })
        | None ->
            model_verification_error
              (NotInEnvironment { module_name; name = Reference.show callable_name }))
    | Some Global.Class ->
        model_verification_error (ModelingClassAsDefine (Reference.show callable_name))
    | Some Global.Module ->
        model_verification_error (ModelingModuleAsDefine (Reference.show callable_name))
    | Some (Global.Attribute (Type.Callable t))
    | Some
        (Global.Attribute
          (Type.Parametric
            { name = "BoundMethod"; parameters = [Type.Parameter.Single (Type.Callable t); _] })) ->
        Ok (Some t)
    | Some (Global.Attribute Type.Any) -> Ok None
    | Some (Global.Attribute Type.Top) -> Ok None
    | Some (Global.Attribute _) ->
        model_verification_error (ModelingAttributeAsDefine (Reference.show callable_name))
  in
  (* Check model matches callables primary signature. *)
  let callable_parameter_names_to_positions =
    match callable_annotation with
    | Ok (Some callable_annotation) ->
        let add_parameter_to_position map (position, parameter) =
          match parameter with
          | Type.Callable.Parameter.Named { name; _ }
          | Type.Callable.Parameter.KeywordOnly { name; _ } ->
              Map.update map (Identifier.sanitized name) ~f:(function
                  | None -> [position]
                  | Some positions -> position :: positions)
          | _ -> map
        in
        callable_annotation
        |> parameters_of_callable_annotation
        |> List.fold ~init:String.Map.empty ~f:add_parameter_to_position
        |> String.Map.map ~f:(List.dedup_and_sort ~compare:Int.compare)
        |> Option.some
    | _ -> None
  in
  (* If there were parameters omitted from the model, the positioning will be off in the access path
     conversion. Let's fix the positions after the fact to make sure that our models aren't off. *)
  let normalized_model_parameters =
    let parameters = AccessPath.Root.normalize_parameters parameters in
    match callable_parameter_names_to_positions with
    | None -> Ok parameters
    | Some names_to_positions ->
        let create_error reason =
          {
            ModelVerificationError.kind =
              IncompatibleModelError
                {
                  name = Reference.show callable_name;
                  callable_type = Option.value_exn (Caml.Result.get_ok callable_annotation);
                  errors =
                    [ModelVerificationError.IncompatibleModelError.{ reason; overload = None }];
                };
            path;
            location;
          }
        in
        let adjust_position_of_positional_parameter ~name ~position =
          match Map.find names_to_positions name with
          | Some [accurate_position] -> Ok accurate_position
          | Some accurate_positions when List.mem ~equal:Int.equal accurate_positions position ->
              Ok position
          | Some valid_positions ->
              Error
                (create_error
                   (ModelVerificationError.IncompatibleModelError.InvalidNamedParameterPosition
                      { name; position; valid_positions }))
          | None ->
              Error
                (create_error
                   (ModelVerificationError.IncompatibleModelError.UnexpectedNamedParameter name))
        in
        let adjust_position_of_root = function
          | AccessPath.Root.PositionalParameter { name; position; positional_only = false }
            when not (String.is_prefix ~prefix:"__" name) ->
              adjust_position_of_positional_parameter ~name ~position
              >>| fun position ->
              AccessPath.Root.PositionalParameter { name; position; positional_only = false }
          | root -> Ok root
        in
        let adjust_position (root, name, parameter) =
          adjust_position_of_root root >>| fun root -> root, name, parameter
        in
        List.map parameters ~f:adjust_position |> all
  in
  let annotations () =
    normalized_model_parameters
    >>= fun normalized_model_parameters ->
    List.map
      normalized_model_parameters
      ~f:
        (parse_parameter_taint
           ~path
           ~location
           ~model_name:(Reference.show callable_name)
           ~configuration
           ~parameters
           ~callable_parameter_names_to_positions
           ~is_object_target)
    |> all
    >>| List.concat
    >>= fun parameter_taint ->
    return_annotation
    |> Option.map
         ~f:
           (parse_return_taint
              ~path
              ~location
              ~model_name:(Reference.show callable_name)
              ~configuration
              ~parameters
              ~callable_parameter_names_to_positions
              ~is_object_target)
    |> Option.value ~default:(Ok [])
    >>| fun return_taint -> List.rev_append parameter_taint return_taint
  in
  let model =
    callable_annotation
    >>= fun callable_annotation ->
    normalized_model_parameters
    >>= fun normalized_model_parameters ->
    ModelVerifier.verify_signature
      ~path
      ~location
      ~normalized_model_parameters
      ~name:callable_name
      callable_annotation
    >>= fun () ->
    annotations ()
    >>= fun annotations ->
    let default_model = if is_obscure then Model.obscure_model else Model.empty_model in
    List.fold_result
      annotations
      ~init:default_model
      ~f:(fun accumulator (annotation, annotation_kind) ->
        add_taint_annotation_to_model
          ~path
          ~location
          ~model_name:(Reference.show callable_name)
          ~resolution:(Resolution.global_resolution resolution)
          ~annotation_kind
          ~callable_annotation
          ~sources_to_keep
          ~sinks_to_keep
          accumulator
          annotation)
  in
  model
  >>= adjust_sanitize_and_modes_and_skipped_override
        ~path
        ~configuration
        ~top_level_decorators
        ~define_name:callable_name
        ~is_object_target
  >>| fun (model, skipped_override) ->
  Model ({ Model.WithTarget.model; target = call_target }, skipped_override)


let create_model_from_attribute
    ~resolution
    ~path
    ~configuration
    ~sources_to_keep
    ~sinks_to_keep
    { name; source_annotation; sink_annotation; decorators; location; call_target }
  =
  let open Core.Result in
  let is_object_target = true in
  ModelVerifier.verify_global ~path ~location ~resolution ~name
  >>= fun () ->
  source_annotation
  |> Option.map
       ~f:
         (parse_return_taint
            ~path
            ~location
            ~model_name:(Reference.show name)
            ~configuration
            ~parameters:[]
            ~callable_parameter_names_to_positions:None
            ~is_object_target)
  |> Option.value ~default:(Ok [])
  >>= fun source_taint ->
  let parse_sink_taint annotation =
    let root =
      AccessPath.Root.PositionalParameter
        { position = 0; name = attribute_symbolic_parameter; positional_only = false }
    in
    let parameter =
      Parameter.create ~location:Location.any ~annotation ~name:attribute_symbolic_parameter ()
    in
    parse_parameter_taint
      ~path
      ~location
      ~model_name:(Reference.show name)
      ~configuration
      ~parameters:[]
      ~callable_parameter_names_to_positions:None
      ~is_object_target
      (root, attribute_symbolic_parameter, parameter)
  in
  sink_annotation
  |> Option.map ~f:parse_sink_taint
  |> Option.value ~default:(Ok [])
  >>= fun sink_taint ->
  Ok (List.rev_append source_taint sink_taint)
  >>= fun annotations ->
  List.fold_result
    annotations
    ~init:Model.empty_model
    ~f:(fun accumulator (annotation, annotation_kind) ->
      add_taint_annotation_to_model
        ~path
        ~location
        ~model_name:(Reference.show name)
        ~resolution:(Resolution.global_resolution resolution)
        ~annotation_kind
        ~callable_annotation:None
        ~sources_to_keep
        ~sinks_to_keep
        accumulator
        annotation)
  >>= adjust_sanitize_and_modes_and_skipped_override
        ~path
        ~configuration
        ~top_level_decorators:decorators
        ~define_name:name
        ~is_object_target
  >>| fun (model, skipped_override) ->
  Model ({ Model.WithTarget.model; target = call_target }, skipped_override)


let verify_no_duplicate_model_query_names ~path (results, errors) =
  let parsed_statement_to_query_name_and_location_option = function
    | ParsedSignature _ -> None
    | ParsedAttribute _ -> None
    | ParsedQuery query -> Some (query.name, query.location)
  in
  let names_and_locations =
    List.filter_map results ~f:parsed_statement_to_query_name_and_location_option
  in
  match List.find_a_dup ~compare:(fun (x, _) (y, _) -> String.compare x y) names_and_locations with
  | Some (name, location) ->
      results, model_verification_error ~path ~location (DuplicateNameClauses name) :: errors
  | None -> results, errors


let create ~resolution ~path ~configuration ~rule_filter ~callables ~stubs source =
  let sources_to_keep, sinks_to_keep =
    compute_sources_and_sinks_to_keep ~configuration ~rule_filter
  in
  let signatures_and_queries, errors =
    let open Core.Result in
    String.split ~on:'\n' source
    |> Parser.parse
    >>| Source.create
    >>| Source.statements
    >>| List.map ~f:(parse_statement ~resolution ~path ~configuration)
    >>| List.partition_result
    >>| (fun (results, errors) -> List.concat results, errors)
    >>| verify_no_duplicate_model_query_names ~path
    |> function
    | Ok results_errors -> results_errors
    | Error { Parser.Error.location; _ } ->
        [], [model_verification_error ~path ~location ParseError]
  in
  let is_obscure call_target =
    (* The callable is obscure if and only if it is a type stub or it is not in the set of known
       callables. *)
    Hash_set.mem stubs call_target
    || callables >>| Core.Fn.flip Hash_set.mem call_target >>| not |> Option.value ~default:false
  in
  let create_model_or_query = function
    | ParsedSignature ({ call_target; _ } as parsed_signature) ->
        create_model_from_signature
          ~resolution
          ~path
          ~configuration
          ~sources_to_keep
          ~sinks_to_keep
          ~is_obscure:(is_obscure call_target)
          parsed_signature
    | ParsedAttribute parsed_attribute ->
        create_model_from_attribute
          ~resolution
          ~path
          ~configuration
          ~sources_to_keep
          ~sinks_to_keep
          parsed_attribute
    | ParsedQuery query -> Core.Result.return (Query query)
  in
  List.rev_append
    (List.map errors ~f:(fun error -> Error error))
    (List.map signatures_and_queries ~f:create_model_or_query)


let get_model_sources ~paths =
  let path_and_content file =
    match File.content file with
    | Some content -> Some (File.path file, content)
    | None -> None
  in
  let model_files = PyrePath.get_matching_files_recursively ~suffix:".pysa" ~paths in
  Log.info
    "Finding taint models in `%s`."
    (paths |> List.map ~f:PyrePath.show |> String.concat ~sep:", ");
  model_files |> List.map ~f:File.create |> List.filter_map ~f:path_and_content


let parse ~resolution ?path ?rule_filter ~source ~configuration ~callables ~stubs () =
  let new_models_and_queries, errors =
    create ~resolution ~path ~rule_filter ~configuration ~callables ~stubs source
    |> List.partition_result
  in
  let new_models, new_queries =
    List.fold
      new_models_and_queries
      ~f:
        (fun (models, queries) -> function
          | Model (model, skipped_override) -> (model, skipped_override) :: models, queries
          | Query query -> models, query :: queries)
      ~init:([], [])
  in
  {
    models =
      List.map new_models ~f:(fun (model, _) -> model.target, model.model)
      |> Registry.of_alist ~join:Model.join_user_models;
    skip_overrides =
      List.filter_map new_models ~f:(fun (_, skipped_override) -> skipped_override)
      |> Reference.Set.of_list;
    queries = new_queries;
    errors;
  }


let invalid_model_query_error error =
  model_verification_error ~path:None ~location:Location.any error


let create_callable_model_from_annotations
    ~resolution
    ~callable
    ~sources_to_keep
    ~sinks_to_keep
    ~is_obscure
    annotations
  =
  let open Core.Result in
  let open ModelVerifier in
  let global_resolution = Resolution.global_resolution resolution in
  match Interprocedural.Target.get_module_and_definition ~resolution:global_resolution callable with
  | None ->
      Error (invalid_model_query_error (NoCorrespondingCallable (Target.show_pretty callable)))
  | Some (_, { Node.value = { Define.signature = define; _ }; _ }) ->
      resolve_global_callable
        ~path:None
        ~location:Location.any
        ~resolution
        ~verify_decorators:false
        define
      >>| (function
            | Some (Global.Attribute (Type.Callable t))
            | Some
                (Global.Attribute
                  (Type.Parametric
                    {
                      name = "BoundMethod";
                      parameters = [Type.Parameter.Single (Type.Callable t); _];
                    })) ->
                Some t
            | _ -> None)
      >>= fun callable_annotation ->
      let default_model = if is_obscure then Model.obscure_model else Model.empty_model in
      List.fold
        annotations
        ~init:(Ok default_model)
        ~f:(fun accumulator (annotation_kind, annotation) ->
          accumulator
          >>= fun accumulator ->
          add_taint_annotation_to_model
            ~path:None
            ~location:Location.any
            ~model_name:"Model query"
            ~resolution:global_resolution
            ~annotation_kind
            ~callable_annotation
            ~sources_to_keep
            ~sinks_to_keep
            accumulator
            annotation)


let create_attribute_model_from_annotations
    ~resolution
    ~name
    ~sources_to_keep
    ~sinks_to_keep
    annotations
  =
  let open Core.Result in
  let global_resolution = Resolution.global_resolution resolution in
  List.fold annotations ~init:(Ok Model.empty_model) ~f:(fun accumulator annotation ->
      accumulator
      >>= fun accumulator ->
      let annotation_kind =
        match annotation with
        | Source _ -> Ok ReturnAnnotation
        | Sink _
        | Tito _ ->
            Ok
              (ParameterAnnotation
                 (AccessPath.Root.PositionalParameter
                    { position = 0; name = attribute_symbolic_parameter; positional_only = false }))
        | _ ->
            Error
              (invalid_model_query_error
                 (InvalidAnnotationForAttributeModel
                    { name; annotation = show_taint_annotation annotation }))
      in
      annotation_kind
      >>= fun annotation_kind ->
      add_taint_annotation_to_model
        ~path:None
        ~location:Location.any
        ~model_name:"Model query"
        ~resolution:global_resolution
        ~annotation_kind
        ~callable_annotation:None
        ~sources_to_keep
        ~sinks_to_keep
        accumulator
        annotation)


let verify_model_syntax ~path ~source =
  match String.split ~on:'\n' source |> Parser.parse with
  | Ok _ -> ()
  | Error { Parser.Error.location; _ } ->
      let error = model_verification_error ~path:(Some path) ~location ParseError in
      raise (ModelVerificationError.ModelVerificationErrors [error])
