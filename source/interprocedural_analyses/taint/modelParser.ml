(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ModelParser: implements a parser for pysa model files (`.pysa`).
 * Given a source file, it returns the set of models within that file.
 *
 * Each model describes sources and sinks on a given callable, global variable
 * or class attribute.
 *)

open Core
open Ast
open Expression
open Pyre
open PyreParser
open Interprocedural
open Statement
open Domains
open ModelParseResult
module PyrePysaApi = Analysis.PyrePysaApi
module ClassSummary = Analysis.ClassSummary
module DecoratorPreprocessing = Analysis.DecoratorPreprocessing

module PythonVersion = struct
  (* Not putting the functions there to prevent circular dependency errors *)
  include Configuration.PythonVersion

  let parse_from_tuple tuple =
    let parse_element = function
      | Some { Node.value = Ast.Expression.Expression.Constant const; _ } -> (
          match const with
          | Integer integer -> Some (Ok integer)
          | _ -> Some (Error "Only integer values are allowed inside the version tuple."))
      | Some _ -> Some (Error "The value in the version tuple must be an integer constant.")
      | None -> None
    in
    let open Core.Result in
    (* we need at least a single value to emulate python tuple comparison *)
    Option.value ~default:(Error "The tuple must not be empty") (parse_element (List.nth tuple 0))
    >>= fun major ->
    Option.value ~default:(Ok 0) (parse_element (List.nth tuple 1))
    >>= fun minor ->
    Option.value ~default:(Ok 0) (parse_element (List.nth tuple 2))
    >>| fun micro -> { major; minor; micro }


  let from_configuration { Configuration.Analysis.python_version; _ } = python_version

  let compare_with left operator right =
    match operator with
    | ComparisonOperator.Equals -> Ok (equal left right)
    | NotEquals -> Ok (not (equal left right))
    | GreaterThan -> Ok (compare left right > 0)
    | GreaterThanOrEquals -> Ok (compare left right >= 0)
    | LessThan -> Ok (compare left right < 0)
    | LessThanOrEquals -> Ok (compare left right <= 0)
    | In -> Error ComparisonOperator.In
    | NotIn -> Error ComparisonOperator.NotIn
    | Is -> Error ComparisonOperator.Is
    | IsNot -> Error ComparisonOperator.IsNot
end

let model_verification_error ~path ~location kind = { ModelVerificationError.kind; path; location }

(* We don't have real models for attributes, so we make a fake callable model with a 'parameter'
   $global which acts as the taint sink whenever attributes are marked as sinks. *)
let attribute_symbolic_parameter =
  AccessPath.Root.PositionalParameter { name = "$global"; position = 0; positional_only = false }


let decorators = Set.union Recognized.property_decorators Recognized.classproperty_decorators

let is_property define = Set.exists decorators ~f:(Define.has_decorator define)

let signature_is_property signature =
  Set.exists decorators ~f:(Define.Signature.has_decorator signature)


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


(* Return true if the expression has the form `name[X]`. *)
let is_base_name expression name = Option.equal String.equal (base_name expression) (Some name)

let parse_access_path ~path ~location expression =
  let module TreeLabel = Abstract.TreeDomain.Label in
  let module ModelLabel = TaintPath.Label in
  let open Core.Result in
  let annotation_error reason =
    model_verification_error
      ~path
      ~location
      (InvalidAccessPath { access_path = expression; reason })
  in
  let rec parse_expression expression =
    match Node.value expression with
    | Expression.Name (Name.Identifier "_") -> Ok (TaintPath.Path [])
    | Expression.Name (Name.Identifier _) ->
        Error (annotation_error "access path must start with `_`")
    | Expression.Name (Name.Attribute { base; attribute; _ }) ->
        (* The analysis does not currently distinguish between fields and indices.
         * Silently convert fields to indices to prevent confusion. *)
        parse_model_labels base
        >>| fun base ->
        TaintPath.Path (base @ [ModelLabel.TreeLabel (TreeLabel.create_name_index attribute)])
    | Expression.Call
        {
          callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
          arguments = [{ Call.Argument.value = argument; _ }];
        } -> (
        parse_model_labels base
        >>= fun base ->
        match Node.value argument with
        | Expression.Constant (Constant.Integer index) ->
            Ok (TaintPath.Path (base @ [ModelLabel.TreeLabel (TreeLabel.create_int_index index)]))
        | Expression.Constant (Constant.String { StringLiteral.value = key; _ }) ->
            Ok (TaintPath.Path (base @ [ModelLabel.TreeLabel (TreeLabel.create_name_index key)]))
        | _ ->
            Error
              (annotation_error
                 (Format.sprintf
                    "expected int or string literal argument for index access, got `%s`"
                    (Expression.show argument))))
    | Expression.Call
        {
          callee = { Node.value = Name (Name.Attribute { base; attribute = "keys"; _ }); _ };
          arguments = [];
        } ->
        parse_model_labels base
        >>| fun base -> TaintPath.Path (base @ [ModelLabel.TreeLabel AccessPath.dictionary_keys])
    | Expression.Call
        {
          callee = { Node.value = Name (Name.Attribute { base; attribute = "all"; _ }); _ };
          arguments = [];
        } ->
        parse_model_labels base
        >>| fun base -> TaintPath.Path (base @ [ModelLabel.TreeLabel TreeLabel.AnyIndex])
    | Expression.Call
        {
          callee =
            { Node.value = Name (Name.Attribute { base; attribute = "parameter_name"; _ }); _ };
          arguments = [];
        } ->
        parse_model_labels base >>| fun base -> TaintPath.Path (base @ [ModelLabel.ParameterName])
    | Expression.Call
        {
          callee =
            { Node.value = Name (Name.Attribute { base; attribute = "all_static_fields"; _ }); _ };
          arguments = [];
        } -> (
        parse_model_labels base
        >>= function
        | [] -> Ok TaintPath.AllStaticFields
        | _ -> Error (annotation_error "`all_static_fields()` can only be used on `_`"))
    | Expression.Call
        { callee = { Node.value = Name (Name.Attribute { base; attribute; _ }); _ }; _ } ->
        parse_expression base
        >>= fun _ ->
        Error
          (annotation_error
             (Format.sprintf
                "unexpected method call `%s` (allowed: `keys`, `all`, `all_static_fields`, \
                 `parameter_name`)"
                attribute))
    | _ -> Error (annotation_error "unexpected expression")
  and parse_model_labels expression =
    match parse_expression expression with
    | Ok (TaintPath.Path labels) -> Ok labels
    | Ok TaintPath.AllStaticFields ->
        Error (annotation_error "cannot access attributes or methods of `all_static_fields()`")
    | Error _ as error -> error
  in
  parse_expression expression


module AnnotationOrigin = struct
  type t =
    | DefineParameter
    | DefineReturn
    | DefineDecorator
    | DefineDecoratorCapturedVariables
    | Attribute
    | ModelQueryParameter
    | ModelQueryReturn
    | ModelQueryCapturedVariables
    | ModelQueryAttribute
    | ModelQueryGlobal
  [@@deriving equal]

  let is_attribute = function
    | Attribute
    | ModelQueryAttribute ->
        true
    | _ -> false


  let is_model_query = function
    | ModelQueryParameter
    | ModelQueryReturn
    | ModelQueryCapturedVariables
    | ModelQueryAttribute
    | ModelQueryGlobal ->
        true
    | _ -> false


  let is_parameter = function
    | DefineParameter
    | DefineDecoratorCapturedVariables
    | ModelQueryCapturedVariables
    | ModelQueryParameter ->
        true
    | _ -> false


  let is_return = function
    | DefineReturn
    | ModelQueryReturn ->
        true
    | _ -> false
end

module AnnotationName = struct
  type t =
    | Source
    | Sink
    | TaintInTaintOut
    | AddFeatureToArgument
    | AttachToSource
    | AttachToSink
    | AttachToTito
  [@@deriving equal]

  let pp formatter = function
    | Source -> Format.fprintf formatter "Source"
    | Sink -> Format.fprintf formatter "Sink"
    | TaintInTaintOut -> Format.fprintf formatter "TaintInTaintOut"
    | AddFeatureToArgument -> Format.fprintf formatter "AddFeatureToArgument"
    | AttachToSource -> Format.fprintf formatter "AttachToSource"
    | AttachToSink -> Format.fprintf formatter "AttachToSink"
    | AttachToTito -> Format.fprintf formatter "AttachToTito"


  let is_tito = equal TaintInTaintOut
end

let rec parse_annotations
    ~path
    ~location
    ~origin
    ~taint_configuration
    ~parameters
    ~callable_parameter_names_to_roots
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
    let callable_parameter_names_to_roots =
      Option.value ~default:String.Map.empty callable_parameter_names_to_roots
    in
    match Map.find callable_parameter_names_to_roots name with
    | Some (AccessPath.Root.PositionalParameter { position; _ } :: _) -> Ok position
    | _ -> (
        (* `callable_parameter_names_to_roots` might be missing the `self` parameter. *)
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
    let open TaintConfiguration.Heap in
    match expression.Node.value with
    | Expression.Name (Name.Identifier breadcrumb)
    | Expression.Constant (Constant.String { value = breadcrumb; kind = String }) ->
        if is_dynamic then
          Ok [Features.Breadcrumb.SimpleVia breadcrumb]
        else
          Features.Breadcrumb.simple_via ~allowed:taint_configuration.features breadcrumb
          >>| (fun breadcrumb -> [breadcrumb])
          |> map_error ~f:annotation_error
    | Tuple expressions ->
        List.map ~f:(extract_breadcrumbs ~is_dynamic) expressions |> all |> map ~f:List.concat
    | _ ->
        Error
          (annotation_error
             (Format.sprintf "Invalid expression for breadcrumb: %s" (Expression.show expression)))
  in
  let extract_subkind expression =
    match Node.value expression with
    | Expression.Name (Name.Identifier subkind) -> Ok subkind
    | _ ->
        Error
          (annotation_error
             (Format.sprintf
                "Invalid expression for taint subkind: %s"
                (Expression.show expression)))
  in
  let extract_via_parameters via_kind expression =
    let rec parse_expression expression =
      match expression.Node.value with
      | Expression.Name (Name.Identifier name) ->
          get_parameter_position name
          >>| fun position ->
          [AccessPath.Root.PositionalParameter { name; position; positional_only = false }]
      | Tuple expressions -> List.map ~f:parse_expression expressions |> all >>| List.concat
      | Call { callee; _ } when is_base_name callee "WithTag" -> Ok []
      | _ -> Error (annotation_error (Format.sprintf "Invalid expression for `%s`" via_kind))
    in
    parse_expression expression
    |> function
    | Ok [] -> Error (annotation_error (Format.sprintf "Missing parameter name for `%s`" via_kind))
    | parameters -> parameters
  in
  let rec extract_via_tag ~requires_parameter_name via_kind expression =
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
      when is_base_name callee "WithTag" ->
        Ok (Some value)
    | Expression.Name (Name.Identifier _) when requires_parameter_name -> Ok None
    | Tuple expressions ->
        List.map expressions ~f:(extract_via_tag ~requires_parameter_name via_kind)
        |> all
        >>| List.find_map ~f:Fn.id
    | _ ->
        Error
          (annotation_error
             (Format.asprintf
                "Invalid expression in `%s` declaration: %a"
                via_kind
                Expression.pp
                expression))
  in
  let rec extract_names expression =
    match expression.Node.value with
    | Expression.Name (Name.Identifier name) -> Ok [name]
    | Tuple expressions -> List.map ~f:extract_names expressions |> all >>| List.concat
    | _ ->
        Error
          (annotation_error
             (Format.sprintf "Invalid expression name: %s" (Expression.show expression)))
  in
  let extract_collapse_depth expression =
    match expression.Node.value with
    | Expression.Constant (Constant.Integer depth) when depth >= 0 -> Ok depth
    | _ ->
        Error
          (annotation_error
             (Format.sprintf
                "expected non-negative int literal argument for CollapseDepth, got `%s`"
                (Expression.show expression)))
  in
  let check_attribute_annotation identifier origin =
    if AnnotationOrigin.is_attribute origin then
      Ok ()
    else
      Error
        (annotation_error
           (Format.sprintf "`%s` can only be used in attribute or global models." identifier))
  in
  let check_parameter_annotation identifier origin =
    if AnnotationOrigin.is_parameter origin then
      Ok ()
    else
      Error (annotation_error (Format.sprintf "`%s` can only be used on parameters" identifier))
  in
  let check_tito_annotation identifier name =
    if AnnotationName.is_tito name then
      Ok ()
    else
      Error
        (annotation_error
           (Format.sprintf "`%s` can only be used within `TaintInTaintOut[]`" identifier))
  in
  let error_on_path_with_parameter_name
      ({ TaintKindsWithFeatures.features; _ } as kinds_with_features)
    =
    if TaintFeatures.has_path_with_parameter_name features then
      Error (annotation_error "`parameter_name()` can only be used within `TaintInTaintOut[]`")
    else
      Ok kinds_with_features
  in
  let rec extract_kinds_with_features ~name expression =
    match expression.Node.value with
    | Expression.Name (Name.Identifier identifier) -> (
        match identifier with
        | "ViaTypeOf" ->
            if not (AnnotationOrigin.is_attribute origin || AnnotationOrigin.is_model_query origin)
            then
              Error
                (annotation_error
                   "A standalone `ViaTypeOf` without arguments can only be used in attribute or \
                    global models.")
            else (* ViaTypeOf is treated as ViaTypeOf[$global] *)
              Ok
                (TaintKindsWithFeatures.from_via_feature
                   (Features.ViaFeature.ViaTypeOf
                      { parameter = attribute_symbolic_parameter; tag = None }))
        | "ViaAttributeName" ->
            check_attribute_annotation identifier origin
            >>| fun () ->
            TaintKindsWithFeatures.from_via_feature
              (Features.ViaFeature.ViaAttributeName { tag = None })
        | "Collapse" ->
            check_tito_annotation identifier name
            >>| fun () -> TaintKindsWithFeatures.from_collapse_depth CollapseDepth.Collapse
        | "NoCollapse" ->
            check_tito_annotation identifier name
            >>| fun () -> TaintKindsWithFeatures.from_collapse_depth CollapseDepth.NoCollapse
        | taint_kind -> Ok (TaintKindsWithFeatures.from_kind (Kind.from_name taint_kind)))
    | Call { callee; arguments = [{ Call.Argument.value = argument; _ }] } -> (
        match base_name callee with
        | Some "Via" -> extract_breadcrumbs argument >>| TaintKindsWithFeatures.from_breadcrumbs
        | Some "ViaDynamicFeature" ->
            extract_breadcrumbs ~is_dynamic:true argument
            >>| TaintKindsWithFeatures.from_breadcrumbs
        | Some "ViaValueOf" ->
            extract_via_tag ~requires_parameter_name:true "ViaValueOf" argument
            >>= fun tag ->
            extract_via_parameters "ViaValueOf" argument
            >>| List.map ~f:(fun parameter -> Features.ViaFeature.ViaValueOf { parameter; tag })
            >>| TaintKindsWithFeatures.from_via_features
        | Some "ViaTypeOf" ->
            let requires_parameter_name =
              not (AnnotationOrigin.is_attribute origin || AnnotationOrigin.is_model_query origin)
            in
            extract_via_tag ~requires_parameter_name "ViaTypeOf" argument
            >>= fun tag ->
            let parameters =
              if requires_parameter_name then
                extract_via_parameters "ViaTypeOf" argument
              else
                Ok [attribute_symbolic_parameter]
            in
            parameters
            >>| List.map ~f:(fun parameter -> Features.ViaFeature.ViaTypeOf { parameter; tag })
            >>| TaintKindsWithFeatures.from_via_features
        | Some "ViaAttributeName" ->
            check_attribute_annotation "ViaAttributeName" origin
            >>= fun () ->
            extract_via_tag ~requires_parameter_name:false "ViaAttributeName" argument
            >>| fun tag ->
            [Features.ViaFeature.ViaAttributeName { tag }]
            |> TaintKindsWithFeatures.from_via_features
        | Some "Updates" ->
            check_tito_annotation "Updates" name
            >>= fun () ->
            check_parameter_annotation "Updates" origin
            >>= fun () ->
            let to_leaf name =
              get_parameter_position name
              >>| fun position -> Kind.from_name (Format.sprintf "ParameterUpdate%d" position)
            in
            extract_names argument
            >>= fun names -> List.map ~f:to_leaf names |> all >>| TaintKindsWithFeatures.from_kinds
        | Some "ParameterPath" ->
            check_parameter_annotation "ParameterPath" origin
            >>= fun () ->
            parse_access_path ~path ~location argument
            >>| TaintKindsWithFeatures.from_parameter_path
        | Some "ReturnPath" ->
            if not (AnnotationOrigin.is_return origin || AnnotationName.is_tito name) then
              Error
                (annotation_error
                   "`ReturnPath[]` can only be used as a return annotation or within \
                    `TaintInTaintOut[]`")
            else
              parse_access_path ~path ~location argument >>| TaintKindsWithFeatures.from_return_path
        | Some "UpdatePath" ->
            check_tito_annotation "UpdatePath" name
            >>= fun () ->
            check_parameter_annotation "UpdatePath" origin
            >>= fun () ->
            parse_access_path ~path ~location argument >>| TaintKindsWithFeatures.from_update_path
        | Some "CollapseDepth" ->
            check_tito_annotation "CollapseDepth" name
            >>= fun () ->
            extract_collapse_depth argument
            >>| fun depth -> TaintKindsWithFeatures.from_collapse_depth (CollapseDepth.Value depth)
        | Some taint_kind ->
            extract_subkind argument
            >>| fun subkind ->
            TaintKindsWithFeatures.from_kind { Kind.name = taint_kind; subkind = Some subkind }
        | None ->
            Error
              (annotation_error
                 (Format.sprintf
                    "Invalid expression for taint kind: %s"
                    (Expression.show expression))))
    | Tuple expressions ->
        List.map ~f:(extract_kinds_with_features ~name) expressions
        |> all
        >>= fun kinds_with_features ->
        kinds_with_features
        |> TaintKindsWithFeatures.concat
        |> Core.Result.map_error ~f:annotation_error
    | _ ->
        Error
          (annotation_error
             (Format.sprintf "Invalid expression for taint kind: %s" (Expression.show expression)))
  in
  let get_source_kinds expression =
    let open TaintConfiguration.Heap in
    extract_kinds_with_features ~name:Source expression
    >>= error_on_path_with_parameter_name
    >>= fun { kinds; features } ->
    List.map kinds ~f:(fun { name; subkind } ->
        AnnotationParser.parse_source ~allowed:taint_configuration.sources ?subkind name
        >>| fun source -> TaintAnnotation.Source { source; features })
    |> all
    |> map_error ~f:annotation_error
  in
  let get_sink_kinds expression =
    let open TaintConfiguration.Heap in
    extract_kinds_with_features ~name:Sink expression
    >>= error_on_path_with_parameter_name
    >>= fun { kinds; features } ->
    List.map kinds ~f:(fun { name; subkind } ->
        AnnotationParser.parse_sink ~allowed:taint_configuration.sinks ?subkind name
        >>| fun sink -> TaintAnnotation.Sink { sink; features })
    |> all
    |> map_error ~f:annotation_error
  in
  let get_taint_in_taint_out expression =
    let open TaintConfiguration.Heap in
    extract_kinds_with_features ~name:TaintInTaintOut expression
    >>= fun { kinds; features } ->
    match kinds with
    | _ when not (AnnotationOrigin.is_parameter origin || AnnotationOrigin.is_attribute origin) ->
        Error
          (annotation_error
             "`TaintInTaintOut[]` can only be used on parameters, attributes or globals")
    | _ when TaintFeatures.has_path_with_all_static_fields features ->
        Error (annotation_error "`all_static_fields()` is not allowed within `TaintInTaintOut[]`")
    | _
      when TaintFeatures.has_path_with_parameter_name features
           && not (AnnotationOrigin.is_parameter origin) ->
        Error (annotation_error "`parameter_name()` can only be used on parameters")
    | [] -> Ok [TaintAnnotation.Tito { tito = Sinks.LocalReturn; features }]
    | _ ->
        List.map kinds ~f:(fun { name; subkind } ->
            AnnotationParser.parse_tito
              ~allowed_transforms:taint_configuration.transforms
              ?subkind
              name
            >>| fun tito -> TaintAnnotation.Tito { tito; features })
        |> all
        |> map_error ~f:annotation_error
  in
  let extract_attach_features ~name expression =
    (* Ensure AttachToX annotations don't have any non-Via annotations for now. *)
    extract_kinds_with_features ~name expression
    >>= function
    | {
        kinds = [];
        features =
          {
            breadcrumbs = _;
            via_features = _;
            applies_to = None;
            parameter_path = None;
            return_path = None;
            update_path = None;
            leaf_names = [];
            leaf_name_provided = false;
            trace_length = None;
            collapse_depth = None;
          } as features;
      } ->
        Ok features
    | _ ->
        Error
          (annotation_error
             (Format.asprintf
                "All parameters to `%a` must be of the form `Via[feature]`."
                AnnotationName.pp
                name))
  in
  let invalid_annotation_error () =
    Error (annotation_error "Failed to parse the given taint annotation.")
  in
  let get_partial_sink_kind expression =
    match Node.value expression with
    | Expression.Call
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
            [{ Call.Argument.value = { Node.value = Name (Name.Identifier label); _ }; _ }];
        } -> (
        match
          TaintConfiguration.PartialSinkLabelsMap.find_opt
            kind
            taint_configuration.partial_sink_labels
        with
        | Some { TaintConfiguration.PartialSinkLabelsMap.main; secondary } ->
            if not (String.equal secondary label || String.equal main label) then
              Error
                (annotation_error
                   (Format.sprintf
                      "Unrecognized label `%s` for partial sink `%s` (choices: `%s`)"
                      label
                      kind
                      (String.concat [main; secondary] ~sep:", ")))
            else
              Ok (Sinks.PartialSink { kind; label })
        | None -> Error (annotation_error (Format.sprintf "Unrecognized partial sink `%s`." kind)))
    | _ -> invalid_annotation_error ()
  in
  let rec parse_annotation = function
    | Expression.Call { callee; arguments = [{ Call.Argument.value = argument; _ }] } -> (
        let open Core.Result in
        match base_name callee, argument with
        | Some "TaintSink", _ -> get_sink_kinds argument
        | Some "TaintSource", _ -> get_source_kinds argument
        | Some "TaintInTaintOut", _ -> get_taint_in_taint_out argument
        | Some "AddFeatureToArgument", _ ->
            extract_kinds_with_features ~name:AddFeatureToArgument argument
            >>| fun { features; _ } -> [TaintAnnotation.AddFeatureToArgument { features }]
        | Some "AttachToSink", _ ->
            extract_attach_features ~name:AttachToSink argument
            >>| fun features -> [TaintAnnotation.Sink { sink = Sinks.Attach; features }]
        | Some "AttachToTito", _ ->
            extract_attach_features ~name:AttachToTito argument
            >>| fun features -> [TaintAnnotation.Tito { tito = Sinks.Attach; features }]
        | Some "AttachToSource", _ ->
            extract_attach_features ~name:AttachToSource argument
            >>| fun features -> [TaintAnnotation.Source { source = Sources.Attach; features }]
        | Some "ViaTypeOf", _ ->
            check_attribute_annotation "ViaTypeOf" origin
            >>= fun () ->
            (* Attribute annotations of the form `a: ViaTypeOf[...]`. *)
            extract_via_tag ~requires_parameter_name:false "ViaTypeOf" argument
            >>| fun tag ->
            let via_feature =
              Features.ViaFeature.ViaTypeOf { parameter = attribute_symbolic_parameter; tag }
            in
            [
              TaintAnnotation.Tito
                {
                  tito = Sinks.LocalReturn;
                  features = { TaintFeatures.empty with via_features = [via_feature] };
                };
            ]
        | Some "ViaAttributeName", _ ->
            check_attribute_annotation "ViaAttributeName" origin
            >>= fun () ->
            (* Attribute annotations of the form `a: ViaAttributeName[...]`. *)
            extract_via_tag ~requires_parameter_name:false "ViaAttributeName" argument
            >>| fun tag ->
            let via_feature = Features.ViaFeature.ViaAttributeName { tag } in
            [
              TaintAnnotation.Tito
                {
                  tito = Sinks.LocalReturn;
                  features = { TaintFeatures.empty with via_features = [via_feature] };
                };
            ]
        | Some "PartialSink", _ ->
            check_parameter_annotation "PartialSink" origin
            >>= fun () ->
            get_partial_sink_kind argument
            >>| fun partial_sink ->
            [TaintAnnotation.Sink { sink = partial_sink; features = TaintFeatures.empty }]
        | Some "Sanitize", _ ->
            parse_sanitize_annotation (Node.value argument)
            >>| fun annotations -> [TaintAnnotation.Sanitize annotations]
        | ( Some "AppliesTo",
            {
              value = Expression.Tuple [{ Node.value = index; _ }; { Node.value = expression; _ }];
              _;
            } ) ->
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
            let error_on_ambiguous_applies_to = function
              | { TaintFeatures.applies_to = Some _; parameter_path = Some _; _ } ->
                  Error (annotation_error "`AppliesTo[]` cannot be used with `ParameterPath[]`")
              | { applies_to = Some _; return_path = Some _; _ } ->
                  Error (annotation_error "`AppliesTo[]` cannot be used with `ReturnPath[]`")
              | { applies_to = Some _; update_path = Some _; _ } ->
                  Error (annotation_error "`AppliesTo[]` cannot be used with `UpdatePath[]`")
              | features -> Ok features
            in
            let extend_applies_to field = function
              | TaintAnnotation.Sink { sink; features } ->
                  TaintFeatures.extend_applies_to features field
                  |> error_on_ambiguous_applies_to
                  >>| fun features -> TaintAnnotation.Sink { sink; features }
              | TaintAnnotation.Source { source; features } ->
                  TaintFeatures.extend_applies_to features field
                  |> error_on_ambiguous_applies_to
                  >>| fun features -> TaintAnnotation.Source { source; features }
              | TaintAnnotation.Tito { tito; features } ->
                  TaintFeatures.extend_applies_to features field
                  |> error_on_ambiguous_applies_to
                  >>| fun features -> TaintAnnotation.Tito { tito; features }
              | TaintAnnotation.AddFeatureToArgument { features } ->
                  TaintFeatures.extend_applies_to features field
                  |> error_on_ambiguous_applies_to
                  >>| fun features -> TaintAnnotation.AddFeatureToArgument { features }
              | TaintAnnotation.Sanitize _ ->
                  Error (annotation_error "`AppliesTo[Sanitize[...]]` is not supported.")
            in
            field
            >>= fun field ->
            parse_annotation expression
            >>= fun annotations -> List.map ~f:(extend_applies_to field) annotations |> all
        | Some "CrossRepositoryTaint", _ -> parse_cross_repository_producer argument
        | Some "CrossRepositoryTaintAnchor", _ -> parse_cross_repository_anchor argument
        | Some "Union", { value = Tuple expressions; _ } ->
            List.map expressions ~f:(fun expression ->
                parse_annotations
                  ~path
                  ~location:expression.Node.location
                  ~origin
                  ~taint_configuration
                  ~parameters
                  ~callable_parameter_names_to_roots
                  expression)
            |> all
            |> map ~f:List.concat
        | _ -> invalid_annotation_error ())
    | Name (Name.Identifier identifier) -> (
        match identifier with
        | "Sanitize" -> Ok [Sanitize [AllSources; AllSinks; AllTito]]
        | "TaintInTaintOut" ->
            Ok [Tito { tito = Sinks.LocalReturn; features = TaintFeatures.empty }]
        | "ViaTypeOf" ->
            if not (AnnotationOrigin.is_attribute origin) then
              Error
                (annotation_error
                   "A standalone `ViaTypeOf` without arguments can only be used in attribute or \
                    global models")
            else
              (* Attribute annotations of the form `a: ViaTypeOf = ...` is equivalent to:
                 TaintInTaintOut[ViaTypeOf[$global]] = ...` *)
              let via_feature =
                Features.ViaFeature.ViaTypeOf
                  { parameter = attribute_symbolic_parameter; tag = None }
              in
              Ok
                [
                  Tito
                    {
                      tito = Sinks.LocalReturn;
                      features = { TaintFeatures.empty with via_features = [via_feature] };
                    };
                ]
        | "ViaAttributeName" ->
            check_attribute_annotation identifier origin
            >>| fun () ->
            (* Attribute annotations of the form `a: ViaAttributeName = ...`. *)
            let via_feature = Features.ViaFeature.ViaAttributeName { tag = None } in
            [
              TaintAnnotation.Tito
                {
                  tito = Sinks.LocalReturn;
                  features = { TaintFeatures.empty with via_features = [via_feature] };
                };
            ]
        | _ -> invalid_annotation_error ())
    | Expression.Tuple expressions ->
        List.map expressions ~f:(fun expression ->
            parse_annotations
              ~path
              ~location:expression.Node.location
              ~origin
              ~taint_configuration
              ~parameters
              ~callable_parameter_names_to_roots
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
      when is_base_name callee "TaintInTaintOut" ->
        let gather_sources_sinks (sources, sinks) = function
          | TaintAnnotation.Source { source; features } when TaintFeatures.is_empty features ->
              Ok (source :: sources, sinks)
          | TaintAnnotation.Sink { sink; features } when TaintFeatures.is_empty features ->
              Ok (sources, sink :: sinks)
          | taint_annotation ->
              Error
                (annotation_error
                   (Format.asprintf
                      "`%a` is not supported within `Sanitize[TaintInTaintOut[...]]`"
                      TaintAnnotation.pp
                      taint_annotation))
        in
        parse_annotation expression
        >>= List.fold_result ~init:([], []) ~f:gather_sources_sinks
        >>| fun (sources, sinks) ->
        let sources = List.map ~f:(fun source -> Sources.to_sanitized_source_exn source) sources in
        let sinks = List.map ~f:(fun sink -> Sinks.to_sanitized_sink_exn sink) sinks in
        [SanitizeAnnotation.SpecificTito { sources; sinks }]
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
          | TaintAnnotation.Source { source; features } when TaintFeatures.is_empty features ->
              Ok (SanitizeAnnotation.SpecificSource (Sources.to_sanitized_source_exn source))
          | TaintAnnotation.Sink { sink; features } when TaintFeatures.is_empty features ->
              Ok (SanitizeAnnotation.SpecificSink (Sinks.to_sanitized_sink_exn sink))
          | taint_annotation ->
              Error
                (annotation_error
                   (Format.asprintf
                      "`%a` is not supported within `Sanitize[...]`"
                      TaintAnnotation.pp
                      taint_annotation))
        in
        parse_annotation expression
        >>= fun annotations -> List.map ~f:to_sanitize annotations |> all
  and parse_cross_repository_producer argument =
    let required_arguments, optional_arguments =
      match argument with
      | {
       Node.value =
         Expression.Tuple
           ({ Node.value = taint; _ }
           :: {
                Node.value =
                  Expression.Constant (Constant.String { StringLiteral.value = canonical_name; _ });
                _;
              }
           :: {
                Node.value =
                  Expression.Constant (Constant.String { StringLiteral.value = canonical_port; _ });
                _;
              }
           :: { Node.value = Expression.Constant (Constant.Integer producer_id); _ }
           :: remaining_arguments);
       _;
      } ->
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
                port = Features.LeafPort.Producer { id = producer_id; port = canonical_port };
              }
          in
          match annotation with
          | TaintAnnotation.Source { source; features } ->
              TaintAnnotation.Source
                {
                  source;
                  features =
                    {
                      features with
                      leaf_names = leaf_name :: features.leaf_names;
                      leaf_name_provided = true;
                      trace_length = Option.merge ~f:min features.trace_length (Some trace_length);
                      breadcrumbs = Features.Breadcrumb.Crtex :: features.breadcrumbs;
                    };
                }
          | TaintAnnotation.Sink { sink; features } ->
              TaintAnnotation.Sink
                {
                  sink;
                  features =
                    {
                      features with
                      leaf_names = leaf_name :: features.leaf_names;
                      leaf_name_provided = true;
                      trace_length = Option.merge ~f:min features.trace_length (Some trace_length);
                      breadcrumbs = Features.Breadcrumb.Crtex :: features.breadcrumbs;
                    };
                }
          | _ -> annotation
        in
        parse_annotation taint |> map ~f:(List.map ~f:add_cross_repository_information)
    | _ ->
        Error
          (annotation_error
             "Cross repository taint must be of the form CrossRepositoryTaint[taint, \
              canonical_name, canonical_port, producer_id, trace_length].")
  and parse_cross_repository_anchor = function
    | {
        Node.value =
          Expression.Tuple
            [
              { Node.value = taint; _ };
              {
                Node.value =
                  Expression.Constant (Constant.String { StringLiteral.value = canonical_name; _ });
                _;
              };
              {
                Node.value =
                  Expression.Constant (Constant.String { StringLiteral.value = canonical_port; _ });
                _;
              };
            ];
        _;
      } ->
        let add_cross_repository_information annotation =
          let leaf_name =
            Features.LeafName.
              { leaf = canonical_name; port = Features.LeafPort.Anchor { port = canonical_port } }
          in
          match annotation with
          | TaintAnnotation.Source { source; features } ->
              TaintAnnotation.Source
                {
                  source;
                  features =
                    {
                      features with
                      leaf_names = leaf_name :: features.leaf_names;
                      leaf_name_provided = true;
                    };
                }
          | TaintAnnotation.Sink { sink; features } ->
              TaintAnnotation.Sink
                {
                  sink;
                  features =
                    {
                      features with
                      leaf_names = leaf_name :: features.leaf_names;
                      leaf_name_provided = true;
                    };
                }
          | _ -> annotation
        in
        parse_annotation taint |> map ~f:(List.map ~f:add_cross_repository_information)
    | _ ->
        Error
          (annotation_error
             "Cross repository taint anchor must be of the form CrossRepositoryTaintAnchor[taint, \
              canonical_name, canonical_port].")
  in
  parse_annotation (Node.value annotation)


let rec class_names_from_annotation = function
  | Type.Bottom
  | Type.Top
  | Type.Any
  | Type.Literal _
  | Type.Callable _
  | Type.Tuple _
  | Type.NoneType
  | Type.TypeOperation _
  | Type.Variable _
  | Type.IntExpression _
  | Type.RecursiveType _
  | Type.ParameterVariadicComponent _ ->
      []
  | Type.Primitive class_name
  | Type.Parametric { name = class_name; _ } ->
      [class_name]
  | Type.Union members ->
      List.fold
        ~init:[]
        ~f:(fun sofar annotation -> List.rev_append (class_names_from_annotation annotation) sofar)
        members
  | Type.ReadOnly annotation -> class_names_from_annotation annotation


let get_class_attributes ~pyre_api = function
  | "object" -> Some []
  | class_name ->
      PyrePysaApi.ReadOnly.get_class_summary pyre_api class_name
      >>| Node.value
      >>| fun class_summary ->
      let attributes = ClassSummary.attributes ~include_generated_attributes:false class_summary in
      let constructor_attributes = ClassSummary.constructor_attributes class_summary in
      let all_attributes =
        Identifier.SerializableMap.union (fun _ x _ -> Some x) attributes constructor_attributes
      in
      let get_attribute attribute_name attribute accumulator =
        match Node.value attribute with
        | { ClassSummary.Attribute.kind = Simple _; _ } -> attribute_name :: accumulator
        | _ -> accumulator
      in
      Identifier.SerializableMap.fold get_attribute all_attributes []


let get_class_attributes_transitive ~pyre_api class_name =
  let successors =
    match PyrePysaApi.ReadOnly.get_class_metadata pyre_api class_name with
    | Some { Analysis.ClassSuccessorMetadataEnvironment.successors = Some successors; _ } ->
        successors
    | _ -> []
  in
  class_name :: successors |> List.filter_map ~f:(get_class_attributes ~pyre_api) |> List.concat


let paths_for_source_or_sink ~pyre_api ~kind ~root ~root_annotations ~features =
  let open Core.Result in
  let all_static_field_paths () =
    let is_return = AccessPath.Root.equal root LocalResult in
    let string_coroutine = if is_return then CallResolution.extract_coroutine_value else Fn.id in
    let strip_all =
      string_coroutine
      |> Fn.compose CallResolution.strip_optional
      |> Fn.compose CallResolution.strip_readonly
      |> Fn.compose CallResolution.unbind_type_variable
    in
    let attributes =
      root_annotations
      |> List.map ~f:strip_all
      |> List.concat_map ~f:class_names_from_annotation
      |> List.concat_map ~f:(get_class_attributes_transitive ~pyre_api)
      |> List.filter ~f:(Fn.non Ast.Expression.is_dunder_attribute)
      |> List.dedup_and_sort ~compare:Identifier.compare
    in
    match attributes with
    | [] -> (* no attributes found *) [AccessPath.Path.empty]
    | attributes ->
        List.map ~f:(fun attribute -> [Abstract.TreeDomain.Label.Index attribute]) attributes
  in
  let expand_model_path = function
    | TaintPath.Path path ->
        let expand_label = function
          | TaintPath.Label.TreeLabel label -> Ok label
          | TaintPath.Label.ParameterName ->
              Error (Format.asprintf "`parameter_name()` is not allowed for %ss" kind)
        in
        path |> List.map ~f:expand_label |> Core.Result.all >>| fun path -> [path]
    | TaintPath.AllStaticFields -> Ok (all_static_field_paths ())
  in
  let paths_for_parameter () =
    let kind =
      match kind with
      | "source" -> "parameter source"
      | name -> name
    in
    match features with
    | {
     TaintFeatures.parameter_path = Some path;
     applies_to = None;
     return_path = None;
     update_path = None;
     _;
    } ->
        expand_model_path path
    | { return_path = Some _; _ } ->
        Error (Format.sprintf "Invalid ReturnPath annotation for %s" kind)
    | { update_path = Some _; _ } ->
        Error (Format.sprintf "Invalid UpdatePath annotation for %s" kind)
    | _ ->
        Error (Format.sprintf "Invalid mix of AppliesTo and ParameterPath annotation for %s" kind)
  in
  let paths_for_return () =
    let kind =
      match kind with
      | "sink" -> "return sink"
      | name -> name
    in
    match features with
    | {
     TaintFeatures.return_path = Some path;
     applies_to = None;
     parameter_path = None;
     update_path = None;
     _;
    } ->
        expand_model_path path
    | { parameter_path = Some _; _ } ->
        Error (Format.sprintf "Invalid ParameterPath annotation for %s" kind)
    | { update_path = Some _; _ } ->
        Error (Format.sprintf "Invalid UpdatePath annotation for %s" kind)
    | _ -> Error (Format.sprintf "Invalid mix of AppliesTo and ReturnPath for %s" kind)
  in
  match root, features with
  | ( _,
      { TaintFeatures.applies_to; parameter_path = None; return_path = None; update_path = None; _ }
    ) ->
      (* AppliesTo works for both parameter and return sources/sinks. *)
      Ok [Option.value ~default:[] applies_to]
  | AccessPath.Root.LocalResult, _ -> paths_for_return ()
  | _ -> paths_for_parameter ()


let expand_model_labels ~parameter path =
  let open Core.Result in
  let expand_parameter_name () =
    match parameter with
    | root when AccessPath.Root.equal root attribute_symbolic_parameter ->
        Error "`parameter_name()` is not allowed for attribute or global models"
    | AccessPath.Root.LocalResult ->
        Error "`parameter_name()` is not allowed for return annotations"
    | AccessPath.Root.PositionalParameter { name; positional_only = false; _ }
    | AccessPath.Root.NamedParameter { name; _ } ->
        Ok (Some (Abstract.TreeDomain.Label.Index name))
    | AccessPath.Root.PositionalParameter { positional_only = true; _ }
    | AccessPath.Root.StarParameter _
    | AccessPath.Root.StarStarParameter _ ->
        Ok None
    | AccessPath.Root.Variable _
    | AccessPath.Root.CapturedVariable _ ->
        failwith "unexpected access path root in model generation"
  in
  let expand_label = function
    | TaintPath.Label.TreeLabel label -> Ok (Some label)
    | TaintPath.Label.ParameterName -> expand_parameter_name ()
  in
  path |> List.map ~f:expand_label |> Core.Result.all >>| List.filter_opt


let input_path_for_tito ~input_root ~kind ~features =
  match features with
  | { TaintFeatures.applies_to; parameter_path = None; _ } ->
      Ok (Option.value ~default:[] applies_to)
  | { parameter_path = Some parameter_path; applies_to = None; _ } -> (
      match parameter_path with
      | TaintPath.Path path -> expand_model_labels ~parameter:input_root path
      | TaintPath.AllStaticFields ->
          Error "`all_static_fields()` is not allowed within `TaintInTaintOut[]`")
  | _ ->
      Error
        (Format.asprintf
           "Invalid mix of AppliesTo and ParameterPath for %a annotation"
           Sinks.pp
           kind)


let output_path_for_tito ~input_root ~kind ~features =
  match Sinks.discard_transforms kind, features with
  | _, { TaintFeatures.return_path = None; update_path = None; _ } -> Ok []
  | Sinks.LocalReturn, { return_path = Some return_path; update_path = None; _ } -> (
      match return_path with
      | TaintPath.Path path -> expand_model_labels ~parameter:input_root path
      | TaintPath.AllStaticFields ->
          Error "`all_static_fields()` is not allowed within `TaintInTaintOut[]`")
  | Sinks.LocalReturn, { update_path = Some _; return_path = None; _ } ->
      Error "Invalid UpdatePath annotation for TaintInTaintOut annotation"
  | Sinks.ParameterUpdate _, { return_path = Some _; update_path = None; _ } ->
      Error "Invalid ReturnPath annotation for Updates annotation"
  | Sinks.ParameterUpdate _, { update_path = Some update_path; return_path = None; _ } -> (
      match update_path with
      | TaintPath.Path path -> expand_model_labels ~parameter:input_root path
      | TaintPath.AllStaticFields ->
          Error "`all_static_fields()` is not allowed within `TaintInTaintOut[]`")
  | Sinks.Attach, { return_path = Some _; _ } ->
      Error "Invalid ReturnPath annotation for AttachTo annotation"
  | Sinks.Attach, { update_path = Some _; _ } ->
      Error "Invalid UpdatePath annotation for AttachTo annotation"
  | kind, _ ->
      Error
        (Format.asprintf "Invalid mix of ReturnPath and UpdatePath for %a annotation" Sinks.pp kind)


let type_breadcrumbs_from_annotations ~pyre_api annotations =
  List.fold annotations ~init:Features.BreadcrumbSet.bottom ~f:(fun sofar annotation ->
      CallGraph.ReturnType.from_annotation ~pyre_api annotation
      |> Features.type_breadcrumbs
      |> Features.BreadcrumbSet.add_set ~to_add:sofar)


let introduce_sink_taint
    ~pyre_api
    ~root
    ~root_annotations
    ~features:
      ({
         TaintFeatures.breadcrumbs;
         via_features;
         applies_to = _;
         parameter_path = _;
         return_path = _;
         update_path = _;
         leaf_names;
         leaf_name_provided;
         trace_length;
         collapse_depth = _;
       } as features)
    ~source_sink_filter
    ({ Model.backward = { sink_taint; _ }; _ } as model)
    taint_sink_kind
  =
  let open Core.Result in
  if Sinks.equal taint_sink_kind Sinks.LocalReturn then
    Error "Invalid TaintSink annotation `LocalReturn`"
  else if
    source_sink_filter
    |> Option.map ~f:(fun source_sink_filter ->
           SourceSinkFilter.should_keep_sink source_sink_filter taint_sink_kind)
    |> Option.value ~default:true
  then
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
      | Some trace_length -> Frame.transform TraceLength.Self Map ~f:(fun _ -> trace_length) taint
      | None -> taint
    in
    let leaf_names =
      leaf_names |> List.map ~f:Features.LeafNameInterned.intern |> Features.LeafNameSet.of_list
    in
    let type_breadcrumbs = type_breadcrumbs_from_annotations ~pyre_api root_annotations in
    let breadcrumbs =
      breadcrumbs
      |> List.map ~f:Features.BreadcrumbInterned.intern
      |> List.map ~f:Features.BreadcrumbSet.inject
      |> Features.BreadcrumbSet.of_approximation
      |> Features.BreadcrumbSet.add_set ~to_add:type_breadcrumbs
    in
    let via_features = Features.ViaFeatureSet.of_list via_features in
    let leaf_taint =
      Frame.initial
      |> Frame.transform Features.LeafNameSet.Self Add ~f:leaf_names
      |> Frame.transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs
      |> Frame.transform Features.ViaFeatureSet.Self Add ~f:via_features
      |> transform_trace_length
      |> BackwardTaint.singleton CallInfo.declaration taint_sink_kind
      |> transform_call_information
      |> BackwardState.Tree.create_leaf
    in
    paths_for_source_or_sink ~pyre_api ~kind:"sink" ~root ~root_annotations ~features
    >>| fun paths ->
    let sink_taint =
      List.fold paths ~init:sink_taint ~f:(fun sink_taint path ->
          BackwardState.assign ~weak:true ~root ~path leaf_taint sink_taint)
    in
    { model with backward = { model.backward with sink_taint } }
  else
    Ok model


let introduce_taint_in_taint_out
    ~pyre_api
    ~root
    ~root_annotations
    ~result_annotations
    ~features:
      ({
         TaintFeatures.breadcrumbs;
         via_features;
         applies_to = _;
         parameter_path = _;
         return_path = _;
         update_path = _;
         leaf_names = _;
         leaf_name_provided = _;
         trace_length = _;
         collapse_depth;
       } as features)
    ({ Model.backward = { taint_in_taint_out; sink_taint }; _ } as model)
    taint_sink_kind
  =
  let open Core.Result in
  (* For tito, both the parameter and the return type can provide type based breadcrumbs *)
  let type_breadcrumbs =
    List.append root_annotations result_annotations |> type_breadcrumbs_from_annotations ~pyre_api
  in
  let breadcrumbs =
    breadcrumbs
    |> List.map ~f:Features.BreadcrumbInterned.intern
    |> List.map ~f:Features.BreadcrumbSet.inject
    |> Features.BreadcrumbSet.of_approximation
    |> Features.BreadcrumbSet.add_set ~to_add:type_breadcrumbs
  in
  let via_features = Features.ViaFeatureSet.of_list via_features in
  let collapse_depth =
    match collapse_depth with
    | None -> 0
    | Some CollapseDepth.Collapse -> 0
    | Some CollapseDepth.NoCollapse -> Features.CollapseDepth.no_collapse
    | Some (CollapseDepth.Value depth) -> depth
  in
  input_path_for_tito ~input_root:root ~kind:taint_sink_kind ~features
  >>= fun input_path ->
  output_path_for_tito ~input_root:root ~kind:taint_sink_kind ~features
  >>= fun output_path ->
  let tito_result_taint =
    Domains.local_return_frame ~output_path ~collapse_depth
    |> Frame.transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs
    |> Frame.transform Features.ViaFeatureSet.Self Add ~f:via_features
    |> BackwardTaint.singleton CallInfo.Tito taint_sink_kind
    |> BackwardState.Tree.create_leaf
  in
  let backward =
    match taint_sink_kind with
    | Sinks.LocalReturn
    | Sinks.ParameterUpdate _ ->
        let taint_in_taint_out =
          BackwardState.assign
            ~weak:true
            ~root
            ~path:input_path
            tito_result_taint
            taint_in_taint_out
        in
        Ok { model.backward with taint_in_taint_out }
    | Sinks.Transform { local; global; _ } ->
        let taint_in_taint_out =
          BackwardState.assign
            ~weak:true
            ~root
            ~path:input_path
            tito_result_taint
            taint_in_taint_out
        in
        let extra_trace_sink = Sinks.make_transform ~local ~global ~base:Sinks.ExtraTraceSink in
        let extra_sink_taint =
          Frame.initial
          |> BackwardTaint.singleton CallInfo.declaration extra_trace_sink
          |> BackwardState.Tree.create_leaf
        in
        let sink_taint =
          BackwardState.assign ~weak:true ~root ~path:input_path extra_sink_taint sink_taint
        in
        Ok { Model.Backward.taint_in_taint_out; sink_taint }
    | Sinks.Attach
      when Features.BreadcrumbSet.is_empty breadcrumbs
           && Features.ViaFeatureSet.is_bottom via_features ->
        Error "`Attach` must be accompanied by a list of features to attach."
    | Sinks.Attach ->
        let attach_taint =
          Frame.initial
          |> Frame.transform Features.BreadcrumbSet.Self Add ~f:breadcrumbs
          |> Frame.transform Features.ViaFeatureSet.Self Add ~f:via_features
          |> BackwardTaint.singleton CallInfo.declaration taint_sink_kind
          |> BackwardState.Tree.create_leaf
        in
        let taint_in_taint_out =
          BackwardState.assign ~weak:true ~root ~path:input_path attach_taint taint_in_taint_out
        in
        Ok { model.backward with taint_in_taint_out }
    | _ ->
        Error (Format.asprintf "Invalid TaintInTaintOut annotation `%a`" Sinks.pp taint_sink_kind)
  in
  backward >>| fun backward -> { model with backward }


let introduce_source_taint
    ~pyre_api
    ~root
    ~root_annotations
    ~features:
      ({
         TaintFeatures.breadcrumbs;
         via_features;
         applies_to = _;
         parameter_path = _;
         return_path = _;
         update_path = _;
         leaf_names;
         leaf_name_provided;
         trace_length;
         collapse_depth = _;
       } as features)
    ~source_sink_filter
    ({ Model.forward = { source_taint }; _ } as model)
    taint_source_kind
  =
  let open Core.Result in
  if
    Sources.equal taint_source_kind Sources.Attach
    && List.is_empty breadcrumbs
    && List.is_empty via_features
  then
    Error "`Attach` must be accompanied by a list of features to attach."
  else if
    source_sink_filter
    |> Option.map ~f:(fun source_sink_filter ->
           SourceSinkFilter.should_keep_source source_sink_filter taint_source_kind)
    |> Option.value ~default:true
  then
    let type_breadcrumbs = type_breadcrumbs_from_annotations ~pyre_api root_annotations in
    let breadcrumbs =
      breadcrumbs
      |> List.map ~f:Features.BreadcrumbInterned.intern
      |> List.map ~f:Features.BreadcrumbSet.inject
      |> Features.BreadcrumbSet.of_approximation
      |> Features.BreadcrumbSet.add_set ~to_add:type_breadcrumbs
    in
    let via_features = Features.ViaFeatureSet.of_list via_features in
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
      |> ForwardTaint.singleton CallInfo.declaration taint_source_kind
      |> transform_call_information
      |> ForwardState.Tree.create_leaf
    in
    paths_for_source_or_sink ~pyre_api ~kind:"source" ~root ~root_annotations ~features
    >>| fun paths ->
    let source_taint =
      List.fold paths ~init:source_taint ~f:(fun source_taint path ->
          ForwardState.assign ~weak:true ~root ~path leaf_taint source_taint)
    in
    { model with forward = { source_taint } }
  else
    Ok model


let sanitize_from_annotations ~source_sink_filter annotations =
  let should_keep_source source =
    match source_sink_filter with
    | None -> true
    | Some source_sink_filter ->
        SourceSinkFilter.should_keep_source
          source_sink_filter
          (Sources.from_sanitized_source source)
  in
  let should_keep_sink sink =
    match source_sink_filter with
    | None -> true
    | Some source_sink_filter ->
        SourceSinkFilter.should_keep_sink source_sink_filter (Sinks.from_sanitized_sink sink)
  in
  let to_sanitize = function
    | SanitizeAnnotation.AllSources -> Sanitize.from_sources_only SanitizeTransform.SourceSet.all
    | SpecificSource source when should_keep_source source ->
        Sanitize.from_sources_only (SanitizeTransform.SourceSet.singleton source)
    | SpecificSource _ -> Sanitize.empty
    | AllSinks -> Sanitize.from_sinks_only SanitizeTransform.SinkSet.all
    | SpecificSink sink when should_keep_sink sink ->
        Sanitize.from_sinks_only (SanitizeTransform.SinkSet.singleton sink)
    | SpecificSink _ -> Sanitize.empty
    | AllTito -> Sanitize.from_tito_only SanitizeTransformSet.all
    | SpecificTito { sources; sinks } ->
        let sources =
          List.filter sources ~f:should_keep_source |> SanitizeTransform.SourceSet.of_list
        in
        let sinks = List.filter sinks ~f:should_keep_sink |> SanitizeTransform.SinkSet.of_list in
        Sanitize.from_tito_only { SanitizeTransformSet.sources; sinks }
  in
  annotations |> List.map ~f:to_sanitize |> List.fold ~init:Sanitize.empty ~f:Sanitize.join


let introduce_sanitize ~source_sink_filter ~root model annotations =
  let roots =
    Sanitize.RootMap.of_list [root, sanitize_from_annotations ~source_sink_filter annotations]
    |> Sanitize.RootMap.join model.Model.sanitizers.roots
  in
  let sanitizers = { model.sanitizers with roots } in
  { model with sanitizers }


let parse_find_clause ~path ({ Node.value; location } as expression) =
  match value with
  | Expression.Constant (Constant.String { StringLiteral.value; _ }) -> (
      match ModelQuery.Find.from_string value with
      | Some find -> Ok find
      | None -> Error (model_verification_error ~path ~location (UnsupportedFindClause value)))
  | _ -> Error (model_verification_error ~path ~location (InvalidFindClauseType expression))


let parse_name_constraint ~path ~location ~constraint_expression ~attribute ~arguments =
  let open Core.Result in
  (match arguments with
  | [
   {
     Call.Argument.value =
       { Node.value = Expression.Constant (Constant.String { StringLiteral.value = name; _ }); _ };
     _;
   };
  ] ->
      Ok name
  | _ -> Error (model_verification_error ~path ~location (InvalidNameClause constraint_expression)))
  >>= fun name ->
  match attribute with
  | "matches" -> Ok (ModelQuery.NameConstraint.Matches (Re2.create_exn name))
  | "equals" -> Ok (ModelQuery.NameConstraint.Equals name)
  | _ -> Error (model_verification_error ~path ~location (InvalidNameClause constraint_expression))


let parse_bool expression =
  match Node.value expression with
  | Expression.Constant Constant.True -> Some true
  | Expression.Constant Constant.False -> Some false
  | _ -> None


let parse_is_transitive ~path ~location is_transitive_expression =
  match parse_bool is_transitive_expression with
  | Some value -> Ok value
  | None ->
      Error
        (model_verification_error ~path ~location (InvalidIsTransitive is_transitive_expression))


let parse_includes_self ~path ~location includes_self_expression =
  match parse_bool includes_self_expression with
  | Some value -> Ok value
  | None ->
      Error
        (model_verification_error ~path ~location (InvalidIncludesSelf includes_self_expression))


let parse_class_hierarchy_options ~path ~location ~callee ~arguments =
  let open Core.Result in
  match arguments with
  | { Call.Argument.value = first_parameter; _ } :: remaining_arguments ->
      let parse_optional_arguments (is_transitive, includes_self) argument =
        match argument with
        | { Call.Argument.name = Some { Node.value = "is_transitive"; _ }; value } ->
            parse_is_transitive ~path ~location value >>| fun value -> value, includes_self
        | { Call.Argument.name = Some { Node.value = "includes_self"; _ }; value } ->
            parse_includes_self ~path ~location value >>| fun value -> is_transitive, value
        | _ ->
            Error
              (model_verification_error
                 ~path
                 ~location
                 (InvalidModelQueryClauseArguments { callee; arguments }))
      in
      List.fold_result ~f:parse_optional_arguments remaining_arguments ~init:(false, true)
      >>| fun (is_transitive, includes_self) -> first_parameter, is_transitive, includes_self
  | _ ->
      Error
        (model_verification_error
           ~path
           ~location
           (InvalidModelQueryClauseArguments { callee; arguments }))


let parse_annotation_constraint ~path ~location ~callee ~attribute ~arguments =
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
      Ok
        (ModelQuery.AnnotationConstraint.NameConstraint (ModelQuery.NameConstraint.Equals type_name))
  | ( "matches",
      [
        {
          Call.Argument.value =
            {
              Node.value =
                Expression.Constant (Constant.String { StringLiteral.value = type_name_pattern; _ });
              _;
            };
          _;
        };
      ] ) ->
      Ok
        (ModelQuery.AnnotationConstraint.NameConstraint
           (ModelQuery.NameConstraint.Matches (Re2.create_exn type_name_pattern)))
  | "is_annotated_type", [] -> Ok ModelQuery.AnnotationConstraint.IsAnnotatedTypeConstraint
  | "extends", _ -> (
      let open Core.Result in
      parse_class_hierarchy_options ~path ~location ~callee ~arguments
      >>= fun (first_parameter, is_transitive, includes_self) ->
      match Node.value first_parameter with
      | Expression.Constant (Constant.String { StringLiteral.value = class_name; _ }) ->
          Ok
            (ModelQuery.AnnotationConstraint.AnnotationClassExtends
               { class_name; is_transitive; includes_self })
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
           (InvalidModelQueryClauseArguments { callee; arguments }))


let parse_read_from_cache_constraint ~path ~location ~constraint_expression ~arguments =
  match arguments with
  | [
   {
     Call.Argument.name = Some { Node.value = "kind"; _ };
     value =
       { Node.value = Expression.Constant (Constant.String { StringLiteral.value = kind; _ }); _ };
   };
   {
     Call.Argument.name = Some { Node.value = "name"; _ };
     value =
       { Node.value = Expression.Constant (Constant.String { StringLiteral.value = name; _ }); _ };
   };
  ] ->
      Ok (ModelQuery.Constraint.ReadFromCache { kind; name })
  | _ ->
      Error
        (model_verification_error
           ~path
           ~location
           (InvalidReadFromCacheArguments constraint_expression))


let parse_format_string ~find_clause substrings =
  let open Core.Result in
  let check_find_is ~identifier expected =
    if ModelQuery.Find.equal find_clause expected then
      Ok ()
    else
      Error
        (ModelVerificationError.FormatStringError.InvalidIdentifierForFind
           { identifier; find = ModelQuery.Find.show find_clause })
  in
  let parse_substring = function
    | Ast.Expression.Substring.Literal { Node.value; _ } ->
        Ok (ModelQuery.FormatString.Substring.Literal value)
    | Ast.Expression.Substring.Format
        { Node.value = Expression.Name (Identifier ("class_name" as identifier)); _ } ->
        check_find_is ~identifier ModelQuery.Find.Method
        >>| fun () -> ModelQuery.FormatString.Substring.ClassName
    | Ast.Expression.Substring.Format
        { Node.value = Expression.Name (Identifier ("function_name" as identifier)); _ } ->
        check_find_is ~identifier ModelQuery.Find.Function
        >>| fun () -> ModelQuery.FormatString.Substring.FunctionName
    | Ast.Expression.Substring.Format
        { Node.value = Expression.Name (Identifier ("method_name" as identifier)); _ } ->
        check_find_is ~identifier ModelQuery.Find.Method
        >>| fun () -> ModelQuery.FormatString.Substring.MethodName
    | Ast.Expression.Substring.Format { Node.value = Expression.Name (Identifier identifier); _ } ->
        Error (ModelVerificationError.FormatStringError.InvalidIdentifier identifier)
    | Ast.Expression.Substring.Format
        {
          Node.value =
            Expression.Call
              {
                callee = { Node.value = Expression.Name (Identifier "capture"); _ };
                arguments =
                  [
                    {
                      Call.Argument.value = { Node.value = Expression.Name (Identifier name); _ };
                      _;
                    };
                  ];
              };
          _;
        } ->
        Ok (ModelQuery.FormatString.Substring.Capture name)
    | Ast.Expression.Substring.Format expression ->
        Error (ModelVerificationError.FormatStringError.InvalidExpression expression)
  in
  List.map ~f:parse_substring substrings |> Result.all


let parse_write_to_cache_model ~path ~location ~find_clause ~model_expression ~arguments =
  match arguments with
  | [
   {
     Call.Argument.name = Some { Node.value = "kind"; _ };
     value =
       { Node.value = Expression.Constant (Constant.String { StringLiteral.value = kind; _ }); _ };
   };
   {
     Call.Argument.name = Some { Node.value = "name"; _ };
     value = { Node.value = Expression.FormatString substrings; _ };
   };
  ] -> (
      match parse_format_string ~find_clause substrings with
      | Ok name -> Ok (ModelQuery.Model.WriteToCache { kind; name })
      | Error error ->
          Error (model_verification_error ~path ~location (InvalidWriteToCacheName error)))
  | _ ->
      Error
        (model_verification_error ~path ~location (InvalidWriteToCacheArguments model_expression))


let parse_class_extends_clause ~path ~location ~callee ~arguments =
  let open Core.Result in
  parse_class_hierarchy_options ~path ~location ~callee ~arguments
  >>= fun (first_parameter, is_transitive, includes_self) ->
  match Node.value first_parameter with
  | Expression.Constant (Constant.String { StringLiteral.value = class_name; _ }) ->
      Ok (ModelQuery.ClassConstraint.Extends { class_name; is_transitive; includes_self })
  | _ ->
      Error
        (model_verification_error
           ~path
           ~location
           (InvalidModelQueryClauseArguments { callee; arguments }))


let rec parse_decorator_constraint ~path ~location ({ Node.value; _ } as constraint_expression) =
  let open Core.Result in
  let parse_constraint_reference ~callee ~reference ~arguments =
    match reference, arguments with
    | ["name"; (("equals" | "matches") as attribute)], _ ->
        parse_name_constraint ~path ~location ~constraint_expression ~attribute ~arguments
        >>| fun name_constraint -> ModelQuery.DecoratorConstraint.NameConstraint name_constraint
    | ["fully_qualified_callee"; (("equals" | "matches") as attribute)], _ ->
        parse_name_constraint ~path ~location ~constraint_expression ~attribute ~arguments
        >>| fun name_constraint ->
        ModelQuery.DecoratorConstraint.FullyQualifiedCallee name_constraint
    | ["arguments"; "contains"], _ ->
        Ok
          (ModelQuery.DecoratorConstraint.ArgumentsConstraint
             (ModelQuery.ArgumentsConstraint.Contains arguments))
    | ["arguments"; "equals"], _ ->
        Ok
          (ModelQuery.DecoratorConstraint.ArgumentsConstraint
             (ModelQuery.ArgumentsConstraint.Equals arguments))
    | ["AnyOf"], _ ->
        List.map arguments ~f:(fun { Call.Argument.value; _ } ->
            parse_decorator_constraint ~path ~location value)
        |> all
        >>| fun constraints -> ModelQuery.DecoratorConstraint.AnyOf constraints
    | ["AllOf"], _ ->
        List.map arguments ~f:(fun { Call.Argument.value; _ } ->
            parse_decorator_constraint ~path ~location value)
        |> all
        >>| fun constraints -> ModelQuery.DecoratorConstraint.AllOf constraints
    | ["Not"], [{ Call.Argument.value; _ }] ->
        parse_decorator_constraint ~path ~location value
        >>= fun decorator_constraint -> Ok (ModelQuery.DecoratorConstraint.Not decorator_constraint)
    | _ ->
        Error
          (model_verification_error
             ~path
             ~location
             (InvalidModelQueryClauseArguments { callee; arguments }))
  in
  match value with
  | Expression.Call
      { Call.callee = { Node.value = Expression.Name callee_name; _ } as callee; arguments } -> (
      match Ast.Expression.name_to_identifiers callee_name with
      | Some reference -> parse_constraint_reference ~callee ~reference ~arguments
      | None ->
          Error
            (model_verification_error ~path ~location (UnsupportedDecoratorConstraintCallee callee))
      )
  | _ ->
      Error
        (model_verification_error
           ~path
           ~location
           (UnsupportedDecoratorConstraint constraint_expression))


let parse_decorator_constraint_list ~path ~location ~arguments =
  let open Core.Result in
  arguments
  |> List.map ~f:(fun { Call.Argument.value; _ } ->
         parse_decorator_constraint ~path ~location value)
  |> all
  >>| ModelQuery.DecoratorConstraint.all_of


let rec parse_class_constraint ~path ~location ({ Node.value; _ } as constraint_expression) =
  let open Core.Result in
  let parse_constraint_reference ~callee ~reference ~arguments =
    match reference, arguments with
    | ["cls"; "name"; (("equals" | "matches") as attribute)], _ ->
        parse_name_constraint ~path ~location ~constraint_expression ~attribute ~arguments
        >>| fun name_constraint -> ModelQuery.ClassConstraint.NameConstraint name_constraint
    | ["cls"; "fully_qualified_name"; (("equals" | "matches") as attribute)], _ ->
        parse_name_constraint ~path ~location ~constraint_expression ~attribute ~arguments
        >>| fun name_constraint ->
        ModelQuery.ClassConstraint.FullyQualifiedNameConstraint name_constraint
    | ["cls"; "extends"], _ -> parse_class_extends_clause ~path ~location ~callee ~arguments
    | ["cls"; "decorator"], _ ->
        parse_decorator_constraint_list ~path ~location ~arguments
        >>= fun decorator_constraint ->
        Ok (ModelQuery.ClassConstraint.DecoratorConstraint decorator_constraint)
    | ["cls"; "any_child"], _ ->
        parse_class_hierarchy_options ~path ~location ~callee ~arguments
        >>= fun (constraint_expression, is_transitive, includes_self) ->
        parse_class_constraint ~path ~location constraint_expression
        >>| fun class_constraint ->
        ModelQuery.ClassConstraint.AnyChildConstraint
          { class_constraint; is_transitive; includes_self }
    | ["cls"; "any_parent"], _ ->
        parse_class_hierarchy_options ~path ~location ~callee ~arguments
        >>= fun (constraint_expression, is_transitive, includes_self) ->
        parse_class_constraint ~path ~location constraint_expression
        >>| fun class_constraint ->
        ModelQuery.ClassConstraint.AnyParentConstraint
          { class_constraint; is_transitive; includes_self }
    | ["AnyOf"], _ ->
        List.map arguments ~f:(fun { Call.Argument.value; _ } ->
            parse_class_constraint ~path ~location value)
        |> all
        >>| fun constraints -> ModelQuery.ClassConstraint.AnyOf constraints
    | ["AllOf"], _ ->
        List.map arguments ~f:(fun { Call.Argument.value; _ } ->
            parse_class_constraint ~path ~location value)
        |> all
        >>| fun constraints -> ModelQuery.ClassConstraint.AllOf constraints
    | ["Not"], [{ Call.Argument.value; _ }] ->
        parse_class_constraint ~path ~location value
        >>= fun class_constraint -> Ok (ModelQuery.ClassConstraint.Not class_constraint)
    | ["cls"; (("equals" | "matches") as attribute)], _ ->
        Error
          (model_verification_error
             ~path
             ~location
             (DeprecatedConstraint
                {
                  deprecated = Format.sprintf "cls.%s" attribute;
                  suggested = Format.sprintf "cls.fully_qualified_name.%s" attribute;
                }))
    | _ ->
        Error
          (model_verification_error
             ~path
             ~location
             (UnsupportedClassConstraintCallee constraint_expression))
  in
  match value with
  | Expression.Call
      { Call.callee = { Node.value = Expression.Name callee_name; _ } as callee; arguments } -> (
      match Ast.Expression.name_to_identifiers callee_name with
      | Some reference -> parse_constraint_reference ~callee ~reference ~arguments
      | None ->
          Error (model_verification_error ~path ~location (UnsupportedClassConstraintCallee callee))
      )
  | _ ->
      Error
        (model_verification_error
           ~path
           ~location
           (UnsupportedClassConstraint constraint_expression))


let check_invalid_read_form_cache ~path ~location ~constraint_expression where =
  let rec is_valid = function
    | ModelQuery.Constraint.NameConstraint _
    | ModelQuery.Constraint.FullyQualifiedNameConstraint _
    | ModelQuery.Constraint.AnnotationConstraint _
    | ModelQuery.Constraint.ReturnConstraint _
    | ModelQuery.Constraint.AnyParameterConstraint _
    | ModelQuery.Constraint.ClassConstraint _
    | ModelQuery.Constraint.AnyDecoratorConstraint _
    | ModelQuery.Constraint.ReadFromCache _ ->
        true
    | ModelQuery.Constraint.AnyOf constraints ->
        List.for_all ~f:ModelQuery.Constraint.is_read_from_cache constraints
        || not (ModelQuery.Constraint.contains_read_from_cache (AnyOf constraints))
    | ModelQuery.Constraint.AllOf constraints -> List.for_all ~f:is_valid constraints
    | ModelQuery.Constraint.Not constraint_ ->
        not (ModelQuery.Constraint.contains_read_from_cache constraint_)
  in
  if List.for_all ~f:is_valid where then
    Ok where
  else
    Error
      (model_verification_error
         ~path
         ~location
         (InvalidReadFromCacheConstraint constraint_expression))


let check_write_to_cache_models ~path ~location ~where models =
  if List.exists ~f:ModelQuery.Model.is_write_to_cache models then
    if ModelQuery.Constraint.contains_read_from_cache (AllOf where) then
      Error (model_verification_error ~path ~location MutuallyExclusiveReadWriteToCache)
    else if not (List.for_all ~f:ModelQuery.Model.is_write_to_cache models) then
      Error (model_verification_error ~path ~location MutuallyExclusiveTaintWriteToCache)
    else
      Ok models
  else
    Ok models


let parse_where_clause ~path ~find_clause ({ Node.value; location } as expression) =
  let open Core.Result in
  let invalid_model_query_where_clause ~path ~location callee =
    model_verification_error
      ~path
      ~location
      (InvalidModelQueryWhereClause
         { expression = callee; find_clause_kind = ModelQuery.Find.show find_clause })
  in
  let check_find ~callee expected_function =
    if expected_function find_clause then
      Ok ()
    else
      Error (invalid_model_query_where_clause ~path ~location callee)
  in
  let check_find_in ~callee expected =
    if List.mem expected find_clause ~equal:ModelQuery.Find.equal then
      Ok ()
    else
      Error (invalid_model_query_where_clause ~path ~location callee)
  in
  let rec parse_constraint ({ Node.value; _ } as constraint_expression) =
    let parse_constraint_reference ~callee ~reference ~arguments =
      match reference, arguments with
      | ["name"; attribute], _ ->
          parse_name_constraint ~path ~location ~constraint_expression ~attribute ~arguments
          >>= fun name_constraint -> Ok (ModelQuery.Constraint.NameConstraint name_constraint)
      | ["fully_qualified_name"; attribute], _ ->
          parse_name_constraint ~path ~location ~constraint_expression ~attribute ~arguments
          >>= fun name_constraint ->
          Ok (ModelQuery.Constraint.FullyQualifiedNameConstraint name_constraint)
      | ["type_annotation"; attribute], _ ->
          check_find_in ~callee [ModelQuery.Find.Attribute; ModelQuery.Find.Global]
          >>= fun () ->
          parse_annotation_constraint ~path ~location ~callee ~attribute ~arguments
          >>= fun annotation_constraint ->
          Ok (ModelQuery.Constraint.AnnotationConstraint annotation_constraint)
      | ["Decorator"], _ ->
          check_find ~callee ModelQuery.Find.is_callable
          >>= fun () ->
          parse_decorator_constraint_list ~path ~location ~arguments
          >>= fun decorator_constraint ->
          Ok (ModelQuery.Constraint.AnyDecoratorConstraint decorator_constraint)
      | ["return_annotation"; attribute], _ ->
          check_find ~callee ModelQuery.Find.is_callable
          >>= fun () ->
          parse_annotation_constraint ~path ~location ~callee ~attribute ~arguments
          >>= fun annotation_constraint ->
          Ok (ModelQuery.Constraint.ReturnConstraint annotation_constraint)
      | ["any_parameter"; "annotation"; attribute], _ ->
          check_find ~callee ModelQuery.Find.is_callable
          >>= fun () ->
          parse_annotation_constraint ~path ~location ~callee ~attribute ~arguments
          >>| fun parameter_constraint ->
          ModelQuery.Constraint.AnyParameterConstraint
            (ModelQuery.ParameterConstraint.AnnotationConstraint parameter_constraint)
      | "cls" :: _, _ ->
          check_find ~callee ModelQuery.Find.is_class_member
          >>= fun () ->
          parse_class_constraint ~path ~location constraint_expression
          >>| fun class_constraint -> ModelQuery.Constraint.ClassConstraint class_constraint
      | ["read_from_cache"], _ ->
          parse_read_from_cache_constraint ~path ~location ~constraint_expression ~arguments
      | ["AnyOf"], _ ->
          List.map arguments ~f:(fun { Call.Argument.value; _ } -> parse_constraint value)
          |> all
          >>| fun constraints -> ModelQuery.Constraint.AnyOf constraints
      | ["AllOf"], _ ->
          List.map arguments ~f:(fun { Call.Argument.value; _ } -> parse_constraint value)
          |> all
          >>| fun constraints -> ModelQuery.Constraint.AllOf constraints
      | ["Not"], [{ Call.Argument.value; _ }] ->
          parse_constraint value
          >>| fun model_constraint -> ModelQuery.Constraint.Not model_constraint
      | _ -> Error (model_verification_error ~path ~location (UnsupportedConstraintCallee callee))
    in
    match value with
    | Expression.Call
        { Call.callee = { Node.value = Expression.Name callee_name; _ } as callee; arguments } -> (
        match Ast.Expression.name_to_identifiers callee_name with
        | Some reference -> parse_constraint_reference ~callee ~reference ~arguments
        | None ->
            Error (model_verification_error ~path ~location (UnsupportedConstraintCallee callee)))
    | _ ->
        Error
          (model_verification_error ~path ~location (UnsupportedConstraint constraint_expression))
  in
  let constraints =
    match value with
    | Expression.List items -> List.map items ~f:parse_constraint |> all
    | _ -> parse_constraint expression >>| List.return
  in
  constraints >>= check_invalid_read_form_cache ~path ~location ~constraint_expression:expression


let parse_parameter_where_clause ~path ({ Node.value; location } as expression) =
  let open Core.Result in
  let rec parse_constraint ({ Node.value; _ } as constraint_expression) =
    let parse_constraint_reference ~callee ~reference ~arguments =
      match reference, arguments with
      | ["name"; attribute], _ ->
          parse_name_constraint ~path ~location ~constraint_expression ~attribute ~arguments
          >>| fun name_constraint -> ModelQuery.ParameterConstraint.NameConstraint name_constraint
      | ["AnyOf"], _ ->
          List.map arguments ~f:(fun { Call.Argument.value; _ } -> parse_constraint value)
          |> all
          >>| fun constraints -> ModelQuery.ParameterConstraint.AnyOf constraints
      | ["AllOf"], _ ->
          List.map arguments ~f:(fun { Call.Argument.value; _ } -> parse_constraint value)
          |> all
          >>| fun constraints -> ModelQuery.ParameterConstraint.AllOf constraints
      | ["Not"], [{ Call.Argument.value; _ }] ->
          parse_constraint value
          >>| fun query_constraint -> ModelQuery.ParameterConstraint.Not query_constraint
      | ["type_annotation"; attribute], _ ->
          parse_annotation_constraint ~path ~location ~callee ~attribute ~arguments
          >>| fun annotation_constraint ->
          ModelQuery.ParameterConstraint.AnnotationConstraint annotation_constraint
      | ( ["index"; "equals"],
          [
            {
              Call.Argument.value = { Node.value = Expression.Constant (Constant.Integer index); _ };
              _;
            };
          ] ) ->
          Ok (ModelQuery.ParameterConstraint.IndexConstraint index)
      | _ ->
          Error
            (model_verification_error
               ~path
               ~location
               (InvalidModelQueryWhereClause
                  { expression = callee; find_clause_kind = "parameters" }))
    in
    match value with
    | Expression.Call
        { Call.callee = { Node.value = Expression.Name callee_name; _ } as callee; arguments } -> (
        match Ast.Expression.name_to_identifiers callee_name with
        | Some reference -> parse_constraint_reference ~callee ~reference ~arguments
        | None ->
            Error
              (model_verification_error
                 ~path
                 ~location
                 (UnsupportedConstraint constraint_expression)))
    | _ ->
        Error
          (model_verification_error ~path ~location (UnsupportedConstraint constraint_expression))
  in
  match value with
  | Expression.List items -> List.map items ~f:parse_constraint |> all
  | _ -> parse_constraint expression >>| List.return


let parse_model_clause
    ~path
    ~taint_configuration
    ~find_clause
    ({ Node.value; location } as expression)
  =
  let open Core.Result in
  let invalid_model_query_model_clause ~path ~location callee =
    model_verification_error
      ~path
      ~location
      (InvalidModelQueryModelClause
         { expression = callee; find_clause_kind = ModelQuery.Find.show find_clause })
  in
  let parse_model ({ Node.value; _ } as model_expression) =
    let parse_taint ~origin taint_expression =
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
                Ok
                  [
                    ModelQuery.QueryTaintAnnotation.ParametricSourceFromAnnotation
                      { source_pattern = pattern; kind };
                  ]
            | "ParametricSinkFromAnnotation" ->
                Ok
                  [
                    ModelQuery.QueryTaintAnnotation.ParametricSinkFromAnnotation
                      { sink_pattern = pattern; kind };
                  ]
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
              ~origin
              ~taint_configuration
              ~parameters:[]
              ~callable_parameter_names_to_roots:None
              expression
            >>| List.map ~f:(fun taint -> ModelQuery.QueryTaintAnnotation.TaintAnnotation taint)
      in

      match Node.value taint_expression with
      | Expression.List taint_annotations ->
          List.map taint_annotations ~f:parse_produced_taint |> all >>| List.concat
      | _ -> parse_produced_taint taint_expression
    in
    let check_find ~callee expected_function =
      if expected_function find_clause then
        Ok ()
      else
        Error (invalid_model_query_model_clause ~path ~location callee)
    in
    match value with
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "Returns"); _ } as callee;
          arguments = [{ Call.Argument.value = taint; _ }];
        } ->
        check_find ~callee ModelQuery.Find.is_callable
        >>= fun () ->
        parse_taint ~origin:ModelQueryReturn taint >>| fun taint -> ModelQuery.Model.Return taint
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "CapturedVariables"); _ } as callee;
          arguments = [{ Call.Argument.value = taint; _ }];
        } ->
        check_find ~callee ModelQuery.Find.is_callable
        >>= fun () ->
        parse_taint ~origin:ModelQueryCapturedVariables taint
        >>| fun taint -> ModelQuery.Model.CapturedVariables { taint; generation_if_source = false }
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "CapturedVariables"); _ } as callee;
          arguments =
            [
              { Call.Argument.value = taint; _ };
              (* TODO(T165056052): Update syntax when general parameter source modeling is done *)
              {
                Call.Argument.name = Some { value = "generation"; _ };
                value = { Node.value = Expression.Constant Constant.True; _ };
              };
            ];
        } ->
        check_find ~callee ModelQuery.Find.is_callable
        >>= fun () ->
        parse_taint ~origin:ModelQueryCapturedVariables taint
        >>| fun taint -> ModelQuery.Model.CapturedVariables { taint; generation_if_source = true }
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "AttributeModel"); _ } as callee;
          arguments = [{ Call.Argument.value = taint; _ }];
        } ->
        check_find ~callee ModelQuery.Find.is_attribute
        >>= fun () ->
        parse_taint ~origin:ModelQueryAttribute taint
        >>| fun taint -> ModelQuery.Model.Attribute taint
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "GlobalModel"); _ } as callee;
          arguments = [{ Call.Argument.value = taint; _ }];
        } ->
        check_find ~callee ModelQuery.Find.is_global
        >>= fun () ->
        parse_taint ~origin:ModelQueryGlobal taint >>| fun taint -> ModelQuery.Model.Global taint
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "Modes"); _ } as callee;
          arguments =
            [
              {
                Call.Argument.value =
                  { Node.value = Expression.List (_ :: _ as mode_list); location };
                _;
              };
            ];
        } ->
        let parse_mode mode =
          match mode with
          | { Node.value = Expression.Name (Name.Identifier mode_name); location } -> (
              match Model.Mode.from_string mode_name with
              | Some Model.Mode.SkipDecoratorWhenInlining
              | Some Model.Mode.IgnoreDecorator ->
                  Error
                    (model_verification_error
                       ~path
                       ~location
                       (InvalidModelQueryMode
                          { mode_name; error = "mode cannot be used in a model query" }))
              | Some mode -> Ok mode
              | None ->
                  Error
                    (model_verification_error
                       ~path
                       ~location
                       (InvalidModelQueryMode { mode_name; error = "unknown mode" })))
          | _ -> Error (model_verification_error ~path ~location (UnexpectedModelExpression mode))
        in
        check_find ~callee ModelQuery.Find.is_callable
        >>= fun () ->
        List.map ~f:parse_mode mode_list
        |> Result.all
        >>| fun modes ->
        let mode_set =
          List.fold_left
            ~init:Model.ModeSet.empty
            ~f:(fun mode_set mode -> Model.ModeSet.add mode mode_set)
            modes
        in
        ModelQuery.Model.Modes mode_set
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
        check_find ~callee ModelQuery.Find.is_callable
        >>= fun () ->
        parse_taint ~origin:ModelQueryParameter taint
        >>| fun taint -> ModelQuery.Model.NamedParameter { name; taint }
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
        check_find ~callee ModelQuery.Find.is_callable
        >>= fun () ->
        parse_taint ~origin:ModelQueryParameter taint
        >>| fun taint -> ModelQuery.Model.PositionalParameter { index; taint }
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "AllParameters"); _ } as callee;
          arguments = [{ Call.Argument.value = taint; _ }];
        } ->
        check_find ~callee ModelQuery.Find.is_callable
        >>= fun () ->
        parse_taint ~origin:ModelQueryParameter taint
        >>| fun taint -> ModelQuery.Model.AllParameters { excludes = []; taint }
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "AllParameters"); _ } as callee;
          arguments =
            [
              { Call.Argument.value = taint; _ };
              { Call.Argument.name = Some { Node.value = "exclude"; _ }; value = excludes };
            ];
        } ->
        check_find ~callee ModelQuery.Find.is_callable
        >>= fun () ->
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
        parse_taint ~origin:ModelQueryParameter taint
        >>| fun taint -> ModelQuery.Model.AllParameters { excludes; taint }
    | Expression.Call
        {
          Call.callee = { Node.value = Name (Name.Identifier "Parameters"); _ } as callee;
          arguments;
        } -> (
        check_find ~callee ModelQuery.Find.is_callable
        >>= fun () ->
        match arguments with
        | [{ Call.Argument.value = taint; _ }] ->
            parse_taint ~origin:ModelQueryParameter taint
            >>| fun taint -> ModelQuery.Model.Parameter { where = []; taint }
        | [
         { Call.Argument.value = taint; _ };
         { Call.Argument.name = Some { Node.value = "where"; _ }; value = where_clause };
        ] ->
            parse_parameter_where_clause ~path where_clause
            >>= fun where ->
            parse_taint ~origin:ModelQueryParameter taint
            >>| fun taint -> ModelQuery.Model.Parameter { where; taint }
        | _ ->
            Error
              (model_verification_error
                 ~path
                 ~location
                 (InvalidModelQueryClauseArguments { callee; arguments })))
    | Expression.Call
        { Call.callee = { Node.value = Name (Name.Identifier "WriteToCache"); _ }; arguments } ->
        parse_write_to_cache_model ~path ~location ~find_clause ~model_expression ~arguments
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


(* Return all type annotations on the given port.
 * Note that there could be multiple annotations because of type overloads. *)
let port_annotations_from_signature ~root ~callable_annotation =
  match callable_annotation with
  | None -> []
  | Some callable_annotation -> (
      match root with
      | AccessPath.Root.PositionalParameter { position; _ } ->
          parameters_of_callable_annotation callable_annotation
          |> List.filter_map ~f:(fun (parameter_position, parameter) ->
                 Option.some_if (Int.equal parameter_position position) parameter)
          |> List.filter_map ~f:Type.Callable.Parameter.annotation
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
          |> List.filter_map ~f:Type.Callable.Parameter.annotation
      | AccessPath.Root.LocalResult ->
          let { Type.Callable.implementation = { Type.Callable.annotation; _ }; _ } =
            callable_annotation
          in
          [annotation]
      | _ -> [])


let parse_parameter_taint
    ~path
    ~location
    ~origin
    ~taint_configuration
    ~parameters
    ~callable_parameter_names_to_roots
    { AccessPath.NormalizedParameter.root; original = parameter; _ }
  =
  parameter.Node.value.Parameter.annotation
  >>| parse_annotations
        ~path
        ~location
        ~origin
        ~taint_configuration
        ~parameters
        ~callable_parameter_names_to_roots
  |> Option.value ~default:(Ok [])
  |> Core.Result.map
       ~f:(List.map ~f:(fun annotation -> ModelAnnotation.ParameterAnnotation (root, annotation)))


let add_taint_annotation_to_model
    ~pyre_api
    ~path
    ~location
    ~model_name
    ~callable_annotation
    ~source_sink_filter
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
  match annotation with
  | ModelAnnotation.ReturnAnnotation annotation -> (
      let root = AccessPath.Root.LocalResult in
      match annotation with
      | TaintAnnotation.Sink { sink; features } ->
          let root_annotations = port_annotations_from_signature ~root ~callable_annotation in
          introduce_sink_taint
            ~pyre_api
            ~root
            ~root_annotations
            ~features
            ~source_sink_filter
            model
            sink
          |> map_error ~f:invalid_model_for_taint
      | TaintAnnotation.Source { source; features } ->
          let root_annotations = port_annotations_from_signature ~root ~callable_annotation in
          introduce_source_taint
            ~pyre_api
            ~root
            ~root_annotations
            ~features
            ~source_sink_filter
            model
            source
          |> map_error ~f:invalid_model_for_taint
      | TaintAnnotation.Tito _ ->
          Error
            (model_verification_error
               ~path
               ~location
               (InvalidReturnAnnotation { model_name; annotation = "TaintInTaintOut" }))
      | TaintAnnotation.AddFeatureToArgument _ ->
          Error
            (model_verification_error
               ~path
               ~location
               (InvalidReturnAnnotation { model_name; annotation = "AddFeatureToArgument" }))
      | TaintAnnotation.Sanitize annotations ->
          Ok (introduce_sanitize ~source_sink_filter ~root model annotations))
  | ModelAnnotation.ParameterAnnotation (root, annotation) -> (
      match annotation with
      | TaintAnnotation.Sink { sink; features } ->
          let root_annotations = port_annotations_from_signature ~root ~callable_annotation in
          introduce_sink_taint
            ~pyre_api
            ~root
            ~root_annotations
            ~features
            ~source_sink_filter
            model
            sink
          |> map_error ~f:invalid_model_for_taint
      | TaintAnnotation.Source { source; features } ->
          let root_annotations = port_annotations_from_signature ~root ~callable_annotation in
          introduce_source_taint
            ~pyre_api
            ~root
            ~root_annotations
            ~features
            ~source_sink_filter
            model
            source
          |> map_error ~f:invalid_model_for_taint
      | TaintAnnotation.Tito { tito; features } ->
          let root_annotations = port_annotations_from_signature ~root ~callable_annotation in
          let result_annotations =
            port_annotations_from_signature ~root:AccessPath.Root.LocalResult ~callable_annotation
          in
          introduce_taint_in_taint_out
            ~pyre_api
            ~root
            ~root_annotations
            ~result_annotations
            ~features
            model
            tito
          |> map_error ~f:invalid_model_for_taint
      | TaintAnnotation.AddFeatureToArgument { features } ->
          let root_annotations = port_annotations_from_signature ~root ~callable_annotation in
          introduce_sink_taint
            ~pyre_api
            ~root
            ~root_annotations
            ~features
            ~source_sink_filter
            model
            Sinks.AddFeatureToArgument
          |> map_error ~f:invalid_model_for_taint
      | TaintAnnotation.Sanitize annotations ->
          Ok (introduce_sanitize ~source_sink_filter ~root model annotations))
  | ModelAnnotation.ModeAnnotation model_query_modes ->
      Ok { model with modes = Model.ModeSet.join_user_modes model_query_modes model.modes }


let parse_return_taint
    ~path
    ~location
    ~origin
    ~taint_configuration
    ~parameters
    ~callable_parameter_names_to_roots
    expression
  =
  let open Core.Result in
  parse_annotations
    ~path
    ~location
    ~origin
    ~taint_configuration
    ~parameters
    ~callable_parameter_names_to_roots
    expression
  |> map ~f:(List.map ~f:(fun annotation -> ModelAnnotation.ReturnAnnotation annotation))


module ParsedStatement : sig
  type parsed_signature = private {
    signature: Define.Signature.t;
    location: Location.t;
    call_target: Target.t;
  }

  type parsed_attribute = private {
    name: Reference.t;
    annotations: TaintAnnotation.t list;
    decorators: Decorator.t list;
    location: Location.t;
    call_target: Target.t;
  }

  type t = private
    | ParsedSignature of parsed_signature
    | ParsedAttribute of parsed_attribute
    | ParsedQuery of ModelQuery.t

  val create_parsed_signature
    :  pyre_api:PyrePysaApi.ReadOnly.t ->
    signature:Define.Signature.t ->
    location:Location.t ->
    call_target:Target.t ->
    t

  val create_parsed_attribute
    :  name:Reference.t ->
    annotations:TaintAnnotation.t list ->
    decorators:Decorator.t list ->
    location:Location.t ->
    call_target:Target.t ->
    t

  val create_parsed_query : model_query:ModelQuery.t -> t
end = struct
  type parsed_signature = {
    signature: Define.Signature.t;
    location: Location.t;
    call_target: Target.t;
  }

  type parsed_attribute = {
    name: Reference.t;
    annotations: TaintAnnotation.t list;
    decorators: Decorator.t list;
    location: Location.t;
    call_target: Target.t;
  }

  type t =
    | ParsedSignature of parsed_signature
    | ParsedAttribute of parsed_attribute
    | ParsedQuery of ModelQuery.t

  let create_parsed_signature ~pyre_api ~signature ~location ~call_target =
    (* Convert delocalized signatures to localized form *)
    let update_call_target_name ~name = function
      | Target.Function { name = _; kind } -> Target.create_function ~kind name
      | target -> target
    in
    let name = signature.Define.Signature.name in
    let name =
      if Option.is_some (PyrePysaApi.ReadOnly.global pyre_api name) then
        name
      else
        name
        |> Reference.all_parents
        |> List.filter_map ~f:(PyrePysaApi.ReadOnly.get_define_body pyre_api)
        |> List.map ~f:Node.value
        |> List.concat_map ~f:(fun define -> define.Define.body)
        |> List.find_map ~f:(fun statement ->
               match statement with
               | { Node.value = Statement.Define define; _ }
                 when Reference.equal name (Reference.delocalize (Define.name define)) ->
                   Some define
               | _ -> None)
        >>| (fun define -> define.Define.signature.name)
        (* TODO(T182905478): Error when no nested function found *)
        |> Option.value ~default:name
    in
    ParsedSignature
      {
        signature = { signature with name };
        location;
        call_target = update_call_target_name ~name call_target;
      }


  let create_parsed_attribute ~name ~annotations ~decorators ~location ~call_target =
    ParsedAttribute { name; annotations; decorators; location; call_target }


  let create_parsed_query ~model_query = ParsedQuery model_query
end

open ParsedStatement

type model_or_query =
  | Model of Model.WithTarget.t
  | Query of ModelQuery.t

let resolve_global_callable
    ~path
    ~location
    ~verify_decorators
    ~pyre_api
    ({ Define.Signature.name; decorators; _ } as define)
  =
  (* Since properties and setters share the same undecorated name, we need to special-case them. *)
  let open ModelVerifier in
  if signature_is_property define then
    find_method_definitions ~pyre_api ~predicate:is_property name
    |> List.hd
    >>| Type.Callable.create_from_implementation
    >>| (fun callable -> Global.Attribute callable)
    |> Core.Result.return
  else if Define.Signature.is_property_setter define then
    find_method_definitions ~pyre_api ~predicate:Define.is_property_setter name
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
    Ok (resolve_global ~pyre_api name)


let adjust_sanitize_and_modes_and_skipped_override
    ~path
    ~taint_configuration
    ~origin
    ~source_sink_filter
    ~top_level_decorators
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
      ~origin
      ~taint_configuration
      ~parameters:[]
      ~callable_parameter_names_to_roots:None
      expression
    |> function
    | Ok [Sanitize sanitize_annotations] ->
        Ok (sanitize_from_annotations ~source_sink_filter sanitize_annotations)
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
        | "TaintSource" when not (AnnotationOrigin.is_attribute origin) ->
            Error
              (annotation_error
                 "`TaintSource` is not supported within `Sanitize()`. Did you mean to use \
                  `SanitizeSingleTrace(...)`?")
        | "TaintSink" when not (AnnotationOrigin.is_attribute origin) ->
            Error
              (annotation_error
                 "`TaintSink` is not supported within `Sanitize()`. Did you mean to use \
                  `SanitizeSingleTrace(...)`?")
        | "TaintSource" ->
            let global = { sanitizers.global with sources = SanitizeTransform.SourceSet.all } in
            Ok { sanitizers with global }
        | "TaintSink" ->
            let global = { sanitizers.global with sinks = SanitizeTransform.SinkSet.all } in
            Ok { sanitizers with global }
        | "TaintInTaintOut" ->
            let global = { sanitizers.global with tito = SanitizeTransformSet.all } in
            Ok { sanitizers with global }
        | "Parameters" -> Ok { sanitizers with parameters = Sanitize.all }
        | _ -> failwith "impossible")
    | Some
        [
          { Call.Argument.value = { Node.value = Expression.Call { Call.callee; arguments }; _ }; _ };
        ]
      when is_base_name callee "Parameters" ->
        parse_sanitize_annotations ~location ~original_expression arguments
        >>| fun parameters_sanitize ->
        { sanitizers with parameters = Sanitize.join sanitizers.parameters parameters_sanitize }
    | Some arguments -> (
        parse_sanitize_annotations ~location ~original_expression arguments
        >>= function
        | { Sanitize.sources; _ }
          when (not (SanitizeTransform.SourceSet.is_empty sources))
               && not (AnnotationOrigin.is_attribute origin) ->
            Error
              (annotation_error
                 "`TaintSource` is not supported within `Sanitize(...)`. Did you mean to use \
                  `SanitizeSingleTrace(...)`?")
        | { Sanitize.sinks; _ }
          when (not (SanitizeTransform.SinkSet.is_empty sinks))
               && not (AnnotationOrigin.is_attribute origin) ->
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
        let global =
          { sanitizers.Model.Sanitizers.global with sources = SanitizeTransform.SourceSet.all }
        in
        Ok { sanitizers with global }
    | Some
        [
          {
            Call.Argument.value = { Node.value = Expression.Name (Name.Identifier "TaintSink"); _ };
            _;
          };
        ] ->
        let global =
          { sanitizers.Model.Sanitizers.global with sinks = SanitizeTransform.SinkSet.all }
        in
        Ok { sanitizers with global }
    | Some arguments -> (
        parse_sanitize_annotations ~location ~original_expression arguments
        >>= function
        | { Sanitize.tito; _ } when not (SanitizeTransformSet.is_empty tito) ->
            Error
              (annotation_error
                 "`TaintInTaintOut` is not supported within `SanitizeSingleTrace(...)`. Did you \
                  mean to use `Sanitize(...)`?")
        | sanitizer -> Ok { sanitizers with global = Sanitize.join sanitizers.global sanitizer })
  in
  let join_with_decorator
      (sanitizers, modes)
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
        >>| fun sanitizers -> sanitizers, modes
    | "SanitizeSingleTrace" ->
        join_with_sanitize_single_trace_decorator
          ~sanitizers
          ~location:decorator_location
          ~original_expression
          arguments
        >>| fun sanitizers -> sanitizers, modes
    | _ -> (
        match Model.Mode.from_string name with
        | Some mode ->
            Ok (sanitizers, Model.ModeSet.join_user_modes modes (Model.ModeSet.singleton mode))
        | _ -> Ok (sanitizers, modes))
  in
  List.fold_result
    top_level_decorators
    ~f:join_with_decorator
    ~init:(model.Model.sanitizers, model.Model.modes)
  >>| fun (sanitizers, modes) -> { model with sanitizers; modes }


let create_model_from_signature
    ~pyre_api
    ~path
    ~taint_configuration
    ~source_sink_filter
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
          | ["AnalyzeAllOverrides"]
          | ["Entrypoint"]
          | ["SkipObscure"]
          | ["IgnoreDecorator"]
          | ["SkipModelBroadening"]
          | ["CapturedVariables"] ->
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
    resolve_global_callable ~path ~location ~verify_decorators:true ~pyre_api define
    >>= function
    | None -> (
        let module_name = Reference.first callable_name in
        let module_resolved =
          ModelVerifier.resolve_global ~pyre_api (Reference.create module_name)
        in
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
  let callable_parameter_names_to_roots =
    (* TODO(T180849788): Consolidate `callable_annotation` accesses into an API *)
    match callable_annotation with
    | Ok (Some callable_annotation) ->
        let add_into_map map name root =
          Map.update map name ~f:(function
              | None -> [root]
              | Some roots -> root :: roots)
        in
        let add_parameter_to_position map (position, parameter) =
          match parameter with
          | Type.Callable.Parameter.Named { name; _ } ->
              let name = Identifier.sanitized name in
              add_into_map
                map
                name
                (AccessPath.Root.PositionalParameter { name; position; positional_only = false })
          | Type.Callable.Parameter.KeywordOnly { name; _ } ->
              let name = Identifier.sanitized name in
              add_into_map map name (AccessPath.Root.NamedParameter { name })
          | _ -> map
        in
        callable_annotation
        |> parameters_of_callable_annotation
        |> List.fold ~init:String.Map.empty ~f:add_parameter_to_position
        |> String.Map.map ~f:(List.dedup_and_sort ~compare:AccessPath.Root.compare)
        |> Option.some
    | _ -> None
  in
  (* If there were parameters omitted from the model, the positioning will be off in the access path
     conversion. Let's fix the positions after the fact to make sure that our models aren't off. *)
  let normalized_model_parameters =
    let parameters = AccessPath.normalize_parameters parameters in
    match callable_parameter_names_to_roots with
    | None -> Ok parameters
    | Some names_to_roots ->
        let create_error reason =
          {
            ModelVerificationError.kind =
              IncompatibleModelError
                {
                  name = Reference.show callable_name;
                  callable_type = Option.value_exn (Stdlib.Result.get_ok callable_annotation);
                  errors =
                    [ModelVerificationError.IncompatibleModelError.{ reason; overload = None }];
                };
            path;
            location;
          }
        in
        let adjust_named_parameter ~name ~position =
          match Map.find names_to_roots name with
          | Some [root] -> Ok root
          | Some roots
            when List.mem
                   roots
                   (AccessPath.Root.PositionalParameter { name; position; positional_only = false })
                   ~equal:AccessPath.Root.equal ->
              Ok (AccessPath.Root.PositionalParameter { name; position; positional_only = false })
          | Some valid_roots ->
              Error
                (create_error
                   (ModelVerificationError.IncompatibleModelError.InvalidNamedParameterPosition
                      { name; position; valid_roots }))
          | None ->
              Error
                (create_error
                   (ModelVerificationError.IncompatibleModelError.UnexpectedNamedParameter name))
        in
        let adjust_root = function
          | AccessPath.Root.PositionalParameter { name; position; positional_only = false }
            when not (String.is_prefix ~prefix:"__" name) ->
              adjust_named_parameter ~name ~position
          | root -> Ok root
        in
        let adjust_parameter ({ AccessPath.NormalizedParameter.root; _ } as parameter) =
          adjust_root root >>| fun root -> { parameter with root }
        in
        List.map parameters ~f:adjust_parameter |> all
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
           ~origin:DefineParameter
           ~taint_configuration
           ~parameters
           ~callable_parameter_names_to_roots)
    |> all
    >>| List.concat
    >>= fun parameter_taint ->
    return_annotation
    |> Option.map
         ~f:
           (parse_return_taint
              ~path
              ~location
              ~origin:DefineReturn
              ~taint_configuration
              ~parameters
              ~callable_parameter_names_to_roots)
    |> Option.value ~default:(Ok [])
    >>| fun return_taint -> List.rev_append parameter_taint return_taint
  in
  let add_captured_variables_taint
      ~path
      ~location
      ~origin
      ~taint_configuration
      ~top_level_decorators
      model
    =
    let annotation_error reason arguments =
      let taint_annotation =
        Ast.Expression.Expression.Call
          {
            callee = { Node.location; value = Name (Name.Identifier "CapturedVariables") };
            arguments = Option.value ~default:[] arguments;
          }
        |> Node.create ~location
      in
      model_verification_error (InvalidTaintAnnotation { taint_annotation; reason })
    in
    let is_captured_variables { Decorator.name = { Node.value = name; _ }; _ } =
      match Reference.show name with
      | "CapturedVariables" -> true
      | _ -> false
    in
    let add_taint ~generation_if_source taint_annotation =
      let captured_variables =
        match PyrePysaApi.ReadOnly.get_define_body pyre_api callable_name with
        | Some { Node.value = { Define.captures; _ }; _ } ->
            List.map ~f:(fun capture -> capture.Define.Capture.name) captures
        | _ -> []
      in
      callable_annotation
      >>= fun callable_annotation ->
      taint_annotation
      |> parse_annotations
           ~path
           ~location
           ~origin
           ~taint_configuration
           ~parameters:[]
           ~callable_parameter_names_to_roots:None
      >>= List.fold_result ~init:model ~f:(fun model annotation ->
              List.fold_result captured_variables ~init:model ~f:(fun model captured_variable ->
                  add_taint_annotation_to_model
                    ~path
                    ~location
                    ~model_name:(Reference.show callable_name)
                    ~pyre_api
                    ~callable_annotation
                    ~source_sink_filter
                    model
                    (ModelAnnotation.ParameterAnnotation
                       ( AccessPath.Root.CapturedVariable
                           { name = captured_variable; generation_if_source },
                         annotation ))))
    in
    match List.find ~f:is_captured_variables top_level_decorators with
    | Some { Decorator.arguments = Some [{ Call.Argument.value; _ }]; _ } ->
        add_taint ~generation_if_source:false value
    | Some
        {
          Decorator.arguments =
            Some
              [
                { Call.Argument.value; _ };
                (* TODO(T165056052): Update syntax when general parameter source modeling is done *)
                {
                  Call.Argument.name = Some { value = "generation"; _ };
                  value = { Node.value = Expression.Constant Constant.True; _ };
                };
              ] as arguments;
          _;
        } ->
        if String.is_substring ~substring:"TaintSource" (Expression.show value) then
          add_taint ~generation_if_source:true value
        else
          annotation_error
            "`@CapturedVariables(..., generation=True)` must be used only on `TaintSource`s."
            arguments
    | Some
        {
          Decorator.arguments =
            Some [_; { Call.Argument.name = Some { value = "generation"; _ }; _ }] as arguments;
          _;
        } ->
        annotation_error
          "Use `@CapturedVariables(..., generation=True)` to specify generation source."
          arguments
    | Some { Decorator.arguments = Some _ as arguments; _ } ->
        annotation_error
          "`@CapturedVariables(...)` takes only one Taint Annotation as argument."
          arguments
    | Some { Decorator.arguments = None; _ } ->
        annotation_error "`@CapturedVariables(...)` needs one Taint Annotation as argument." None
    | None -> Ok model
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
    List.fold_result annotations ~init:default_model ~f:(fun accumulator annotation ->
        add_taint_annotation_to_model
          ~path
          ~location
          ~model_name:(Reference.show callable_name)
          ~pyre_api
          ~callable_annotation
          ~source_sink_filter
          accumulator
          annotation)
  in
  model
  >>= adjust_sanitize_and_modes_and_skipped_override
        ~path
        ~taint_configuration
        ~origin:DefineDecorator
        ~source_sink_filter
        ~top_level_decorators
  >>= add_captured_variables_taint
        ~path
        ~location
        ~taint_configuration
        ~origin:DefineDecoratorCapturedVariables
        ~top_level_decorators
  >>| fun model -> Model { Model.WithTarget.model; target = call_target }


let create_model_from_attribute
    ~pyre_api
    ~path
    ~taint_configuration
    ~source_sink_filter
    { name; annotations; decorators; location; call_target }
  =
  let open Core.Result in
  ModelVerifier.verify_global ~path ~location ~pyre_api ~name
  >>= fun () ->
  List.fold_result annotations ~init:Model.empty_model ~f:(fun accumulator annotation ->
      let model_annotation =
        match annotation with
        | TaintAnnotation.Source _ -> Ok (ModelAnnotation.ReturnAnnotation annotation)
        | TaintAnnotation.Sink _
        | TaintAnnotation.Tito _ ->
            Ok (ModelAnnotation.ParameterAnnotation (attribute_symbolic_parameter, annotation))
        | _ -> failwith "unreachable"
      in
      model_annotation
      >>= fun model_annotation ->
      add_taint_annotation_to_model
        ~path
        ~location
        ~model_name:(Reference.show name)
        ~pyre_api
        ~callable_annotation:None
        ~source_sink_filter
        accumulator
        model_annotation)
  >>= adjust_sanitize_and_modes_and_skipped_override
        ~path
        ~taint_configuration
        ~origin:Attribute
        ~source_sink_filter
        ~top_level_decorators:decorators
  >>| fun model -> Model { Model.WithTarget.model; target = call_target }


let is_obscure ~definitions ~stubs call_target =
  (* The callable is obscure if and only if it is a type stub or it is not in the set of known
     definitions. *)
  Interprocedural.Target.HashsetSharedMemory.ReadOnly.mem stubs call_target
  || definitions >>| Core.Fn.flip Hash_set.mem call_target >>| not |> Option.value ~default:false


let parse_models ~pyre_api ~taint_configuration ~source_sink_filter ~definitions ~stubs models =
  let open Core.Result in
  List.map
    models
    ~f:(fun ((({ call_target; _ } : parsed_signature) as parsed_signature), model_source) ->
      create_model_from_signature
        ~pyre_api
        ~path:None
        ~taint_configuration
        ~source_sink_filter
        ~is_obscure:(is_obscure ~definitions ~stubs call_target)
        parsed_signature
      >>| fun model_or_query ->
      match model_or_query with
      | Model model_with_target ->
          Some
            {
              ModelQuery.ExpectedModel.model = model_with_target.model;
              target = model_with_target.target;
              model_source;
            }
      | _ -> None)
  |> Result.combine_errors
  >>| List.filter_map ~f:(fun x -> x)


module ModelQueryArguments = struct
  type t = {
    name: string;
    logging_group_name: string option;
    find_clause: Expression.t;
    where_clause: Expression.t;
    model_clause: Expression.t;
    expected_models_clause: Expression.t option;
    unexpected_models_clause: Expression.t option;
  }

  let parse_arguments ~path ~location arguments =
    let open Core.Result in
    let required_arguments = ["name"; "find"; "where"; "model"] in
    let valid_arguments =
      "expected_models" :: "unexpected_models" :: "logging_group_name" :: required_arguments
    in
    let parse_argument arguments = function
      | { Call.Argument.name = None; value = argument } ->
          Error (model_verification_error ~path ~location (ModelQueryUnnamedParameter argument))
      | { Call.Argument.name = Some { Node.value = name; _ }; value = argument } ->
          if Option.is_some (Map.find arguments name) then
            Error (model_verification_error ~path ~location (ModelQueryDuplicateParameter name))
          else if not (List.mem ~equal:String.equal valid_arguments name) then
            Error
              (model_verification_error ~path ~location (ModelQueryUnsupportedNamedParameter name))
          else
            Ok (Map.set ~key:name ~data:argument arguments)
    in
    let check_required_argument arguments required_argument =
      if Option.is_some (Map.find arguments required_argument) then
        Ok arguments
      else
        Error
          (model_verification_error
             ~path
             ~location
             (ModelQueryMissingRequiredParameter required_argument))
    in
    let parse_name_argument = function
      | { Node.value = Expression.Constant (Constant.String { StringLiteral.value = name; _ }); _ }
        ->
          Ok name
      | argument ->
          Error (model_verification_error ~path ~location (InvalidModelQueryNameClause argument))
    in
    List.fold_result ~f:parse_argument ~init:String.Map.empty arguments
    >>= fun arguments ->
    List.fold_result ~f:check_required_argument ~init:arguments required_arguments
    >>= fun arguments ->
    parse_name_argument (Map.find_exn arguments "name")
    >>= fun name ->
    Map.find arguments "logging_group_name"
    |> Option.map ~f:parse_name_argument
    |> (function
         | None -> Ok None
         | Some (Ok name) -> Ok (Some name)
         | Some (Error error) -> Error error)
    >>| fun logging_group_name ->
    {
      name;
      logging_group_name;
      find_clause = Map.find_exn arguments "find";
      where_clause = Map.find_exn arguments "where";
      model_clause = Map.find_exn arguments "model";
      expected_models_clause = Map.find arguments "expected_models";
      unexpected_models_clause = Map.find arguments "unexpected_models";
    }
end

(* Expand private variables, which are class attributes starting with two underscores and have at
   most one trailing underscore. *)
let mangle_private_variable name =
  match List.rev (Reference.as_list name) with
  | attribute_name :: class_name :: rest
    when String.is_prefix ~prefix:"__" attribute_name
         && (not (String.is_suffix ~suffix:"__" attribute_name))
         && not (String.equal class_name "__class__") ->
      Format.sprintf "_%s%s" class_name attribute_name :: class_name :: rest
      |> List.rev
      |> Reference.create_from_list
  | _ -> name


let rec parse_statement
    ~pyre_api
    ~path
    ~taint_configuration
    ~source_sink_filter
    ~definitions
    ~stubs
    ~python_version
    statement
  =
  let open Core.Result in
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
        |> Option.map ~f:(PyrePysaApi.ReadOnly.parse_reference pyre_api)
        |> Option.bind ~f:(fun parsed ->
               let parent, _ = Type.split parsed in
               Type.primitive_name parent)
        |> Option.bind ~f:(PyrePysaApi.ReadOnly.get_class_summary pyre_api)
      in
      let call_target =
        match class_candidate with
        | Some _ when Define.Signature.is_property_setter signature ->
            Target.create_property_setter name
        | Some _ -> Target.create_method name
        | None -> Target.create_function name
      in
      [Ok (ParsedStatement.create_parsed_signature ~pyre_api ~signature ~location ~call_target)]
  | { Node.value = Statement.Define { signature = { name; _ }; _ }; location } ->
      [
        Error
          (model_verification_error ~path ~location (DefineBodyNotEllipsis (Reference.show name)));
      ]
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
          match Expression.show value with
          | "SkipAnalysis" -> Some (Either.Second (decorator_with_name "SkipAnalysis"))
          | "SkipOverrides" -> Some (Either.Second (decorator_with_name "SkipOverrides"))
          | "AnalyzeAllOverrides" ->
              Some (Either.Second (decorator_with_name "AnalyzeAllOverrides"))
          | "Entrypoint" -> Some (Either.Second (decorator_with_name "Entrypoint"))
          | "SkipModelBroadening" ->
              Some (Either.Second (decorator_with_name "SkipModelBroadening"))
          | name when String.is_prefix name ~prefix:"TaintSource[" -> Some (Either.First value)
          | _ -> None
        in
        base_arguments
        |> List.filter_map ~f:class_source_base
        |> List.fold ~init:([], []) ~f:(fun (source_annotations, decorators) -> function
             | Either.First source_annotation -> source_annotation :: source_annotations, decorators
             | Either.Second decorator -> source_annotations, decorator :: decorators)
      in
      if
        (not (List.is_empty sink_annotations))
        || (not (List.is_empty source_annotations))
        || not (List.is_empty extra_decorators)
      then
        name
        |> ModelVerifier.class_summaries ~pyre_api
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
                       ParsedStatement.create_parsed_signature
                         ~pyre_api
                         ~signature:
                           {
                             signature with
                             Define.Signature.parameters;
                             return_annotation = source_annotation;
                             decorators;
                           }
                         ~location
                         ~call_target:(Target.create_method name)
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
        |> List.map ~f:return
      else
        []
  | { Node.value = Class { Class.name; _ }; location } ->
      [
        Error (model_verification_error ~path ~location (ClassBodyNotEllipsis (Reference.show name)));
      ]
  | {
   Node.value =
     Assign
       { Assign.target = { Node.value = Name name; _ } as target; annotation = Some annotation; _ };
   location;
  } ->
      let annotation_string = Expression.show annotation in
      if not (is_simple_name name) then
        [Error (model_verification_error ~path ~location (InvalidIdentifier target))]
      else if String.is_prefix annotation_string ~prefix:"Sanitize[TaintInTaintOut[" then
        [
          Error
            (model_verification_error
               ~path
               ~location
               (InvalidTaintAnnotation
                  {
                    taint_annotation = annotation;
                    reason = "TaintInTaintOut sanitizers cannot be modelled on attributes";
                  }));
        ]
      else if
        String.equal annotation_string "Sanitize"
        || String.is_prefix annotation_string ~prefix:"Sanitize[TaintSource"
        || String.is_prefix annotation_string ~prefix:"Sanitize[TaintSink"
      then
        let name = name |> name_to_reference_exn |> mangle_private_variable in
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
        [
          Ok
            (ParsedStatement.create_parsed_attribute
               ~name
               ~annotations:[]
               ~decorators:[decorator]
               ~location
               ~call_target:(Target.create_object name));
        ]
      else if
        String.equal annotation_string "ViaTypeOf"
        || String.equal annotation_string "ViaAttributeName"
        || String.is_substring annotation_string ~substring:"TaintSource["
        || String.is_substring annotation_string ~substring:"TaintSink["
        || String.is_substring annotation_string ~substring:"TaintInTaintOut["
        || String.is_substring annotation_string ~substring:"ViaTypeOf["
        || String.is_substring annotation_string ~substring:"ViaAttributeName["
      then
        let name = name |> name_to_reference_exn |> mangle_private_variable in
        parse_annotations
          ~path
          ~location
          ~origin:Attribute
          ~taint_configuration
          ~parameters:[]
          ~callable_parameter_names_to_roots:None
          annotation
        >>| (fun annotations ->
              ParsedStatement.create_parsed_attribute
                ~name
                ~annotations
                ~decorators:[]
                ~location
                ~call_target:(Target.create_object name))
        |> fun result -> [result]
      else
        [
          Error
            (model_verification_error
               ~path
               ~location
               (InvalidTaintAnnotation
                  {
                    taint_annotation = annotation;
                    reason = "Unsupported annotation for attributes";
                  }));
        ]
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
  } -> (
      let parse_model_sources ~name ~path expression =
        match expression with
        | Some ({ Node.value; location } as expression) ->
            (let open Core.Result in
            let parse_constraint ({ Node.value; location } as constraint_expression) =
              match value with
              | Expression.Constant (Constant.String { StringLiteral.value; _ }) -> Ok value
              | _ ->
                  Error
                    (model_verification_error
                       ~path
                       ~location
                       (InvalidExpectedModelsClause
                          { model_query_name = name; models_clause = constraint_expression }))
            in
            match value with
            | Expression.List items -> List.map items ~f:parse_constraint |> all
            | _ -> parse_constraint expression >>| List.return)
            >>| fun model_sources -> Some (model_sources, location)
        | None -> Ok None
      in
      let parse_output_models_clause ~name ~path expression =
        match parse_model_sources ~name ~path expression with
        | Error error -> Error [error]
        | Ok None -> Ok []
        | Ok (Some (model_strings, location)) -> (
            model_strings
            |> Parser.parse
            >>| Source.create
            >>| Source.statements
            >>| List.map
                  ~f:
                    (parse_statement
                       ~pyre_api
                       ~path
                       ~taint_configuration
                       ~source_sink_filter
                       ~definitions
                       ~stubs
                       ~python_version)
            >>| List.concat
            >>| List.partition_result
            >>| (fun (results, errors) ->
                  let results, remainder = List.zip_with_remainder results model_strings in
                  let parsed_results, parsed_errors =
                    List.partition_map results ~f:(fun (result, model_source) ->
                        match result with
                        | ParsedSignature parsed_signature -> First (parsed_signature, model_source)
                        | ParsedQuery _ ->
                            Second
                              (model_verification_error
                                 ~path
                                 ~location
                                 (ModelQueryInExpectedModelsClause
                                    { model_query_name = name; model_source }))
                        | _ ->
                            Second
                              (model_verification_error
                                 ~path
                                 ~location
                                 (InvalidExpectedModelsClause
                                    {
                                      model_query_name = name;
                                      models_clause = Option.value_exn expression;
                                    })))
                  in
                  let errors = errors @ parsed_errors in
                  if List.is_empty errors then
                    match remainder with
                    | None -> Ok parsed_results
                    | _ -> failwith "Models did not generate 1 parsed_signature each"
                  else
                    Error errors)
            |> function
            | Error { Parser.Error.location = parse_location; _ } ->
                let { Location.line; column } = Location.start parse_location in
                let start, stop = Location.start location, Location.stop location in
                let location =
                  {
                    Location.start = { line = line + start.line - 1; column };
                    stop = { line = line + stop.line - 1; column };
                  }
                in
                Error [model_verification_error ~path ~location ParseError]
            | Ok results -> (
                results
                |> function
                | Error errors -> Error errors
                | Ok parsed_signatures ->
                    parse_models
                      ~pyre_api
                      ~taint_configuration
                      ~source_sink_filter
                      ~definitions
                      ~stubs
                      parsed_signatures))
      in
      let as_result_error_list = function
        | Ok result -> Ok result
        | Error error -> Error [error]
      in
      ModelQueryArguments.parse_arguments ~path ~location arguments
      |> as_result_error_list
      >>= (fun {
                 ModelQueryArguments.name;
                 logging_group_name;
                 find_clause;
                 where_clause;
                 model_clause;
                 expected_models_clause;
                 unexpected_models_clause;
               } ->
            parse_find_clause ~path find_clause
            |> as_result_error_list
            >>= fun find ->
            parse_where_clause ~path ~find_clause:find where_clause
            |> as_result_error_list
            >>= fun where ->
            parse_model_clause ~path ~taint_configuration ~find_clause:find model_clause
            >>= check_write_to_cache_models ~path ~location ~where
            |> as_result_error_list
            >>= fun models ->
            parse_output_models_clause ~name ~path expected_models_clause
            >>= fun expected_models ->
            parse_output_models_clause ~name ~path unexpected_models_clause
            >>| fun unexpected_models ->
            [
              Ok
                (ParsedStatement.create_parsed_query
                   ~model_query:
                     {
                       ModelQuery.find;
                       where;
                       models;
                       name;
                       logging_group_name;
                       path;
                       location;
                       expected_models;
                       unexpected_models;
                     });
            ])
      |> function
      | Ok parsed_statements_list -> parsed_statements_list
      | Error errors_list -> List.map ~f:(fun error -> Error error) errors_list)
  | {
   Node.value =
     If
       {
         If.body;
         If.test =
           {
             Node.value =
               ComparisonOperator
                 {
                   ComparisonOperator.left =
                     {
                       Node.value =
                         Name
                           (Name.Attribute
                             {
                               base = { Node.value = Name (Name.Identifier "sys"); _ };
                               attribute = "version";
                               _;
                             });
                       _;
                     };
                   ComparisonOperator.operator;
                   ComparisonOperator.right = { Node.value = Tuple tuple; _ };
                 };
             _;
           };
         If.orelse;
       };
   location;
  } -> (
      let perform_comparison test_version =
        PythonVersion.compare_with python_version operator test_version
        |> function
        | Error unsupported_operator ->
            [
              Error
                (model_verification_error
                   ~path
                   ~location
                   (UnsupportedComparisonOperator unsupported_operator));
            ]
        | Ok comparison_result ->
            let statements = if comparison_result then body else orelse in
            statements
            |> List.map
                 ~f:
                   (parse_statement
                      ~pyre_api
                      ~path
                      ~taint_configuration
                      ~source_sink_filter
                      ~definitions
                      ~stubs
                      ~python_version)
            |> List.concat
      in
      PythonVersion.parse_from_tuple tuple
      |> function
      | Error error ->
          [Error (model_verification_error ~path ~location (UnsupportedVersionConstant error))]
      | Ok test_version -> perform_comparison test_version)
  | { Node.value = If { If.test; _ }; location } ->
      [Error (model_verification_error ~path ~location (UnsupportedIfCondition test))]
  | { Node.location; _ } ->
      [Error (model_verification_error ~path ~location (UnexpectedStatement statement))]


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
      results, [model_verification_error ~path ~location (DuplicateNameClauses name)] :: errors
  | None -> results, errors


let create
    ~pyre_api
    ~path
    ~taint_configuration
    ~source_sink_filter
    ~definitions
    ~stubs
    ~python_version
    source
  =
  let signatures_and_queries, errors =
    let open Core.Result in
    String.split ~on:'\n' source
    |> Parser.parse
    >>| Source.create
    >>| Source.statements
    >>| List.map
          ~f:
            (parse_statement
               ~pyre_api
               ~path
               ~taint_configuration
               ~source_sink_filter
               ~definitions
               ~stubs
               ~python_version)
    >>| List.concat
    >>| List.partition_result
    >>| (fun (results, errors) -> results, [errors])
    >>| verify_no_duplicate_model_query_names ~path
    |> function
    | Ok results_errors -> results_errors
    | Error { Parser.Error.location; _ } ->
        [], [[model_verification_error ~path ~location ParseError]]
  in
  let create_model_or_query = function
    | ParsedSignature ({ call_target; _ } as parsed_signature) ->
        create_model_from_signature
          ~pyre_api
          ~path
          ~taint_configuration
          ~source_sink_filter
          ~is_obscure:(is_obscure ~definitions ~stubs call_target)
          parsed_signature
    | ParsedAttribute parsed_attribute ->
        create_model_from_attribute
          ~pyre_api
          ~path
          ~taint_configuration
          ~source_sink_filter
          parsed_attribute
    | ParsedQuery query -> Core.Result.return (Query query)
  in
  List.rev_append
    (List.map errors ~f:(fun error -> Error error))
    (List.map signatures_and_queries ~f:(fun parsed_statement ->
         create_model_or_query parsed_statement
         |> function
         | Ok result -> Ok result
         | Error error -> Error [error]))


let parse_decorator_modes ~path ~source =
  let open Result in
  let update_actions actions decorator action =
    Reference.SerializableMap.update
      decorator
      (function
        | None -> Some action
        | Some existing_action ->
            let () =
              Log.warning
                "%a: Found multiple modes for decorator `%a`: was @%s, it is now @%s."
                PyrePath.pp
                path
                Reference.pp
                decorator
                (DecoratorPreprocessing.Action.to_mode existing_action)
                (DecoratorPreprocessing.Action.to_mode action)
            in
            Some action)
      actions
  in
  let parse_statement actions = function
    | { Node.value = Statement.Define { signature = { name; decorators; _ }; _ }; _ } ->
        let name =
          (* To properly work on a decorator factory implemented with a class, the user needs to
             model `Class.__call__` (since modeling a class directly is generally not allowed). We
             need to discard the `__call__` afterward to properly match the decorator. *)
          match Reference.last name with
          | "__call__" -> Reference.prefix name |> Option.value ~default:Reference.empty
          | _ -> name
        in
        if List.exists decorators ~f:(name_is ~name:"IgnoreDecorator") then
          update_actions actions name DecoratorPreprocessing.Action.Discard
        else if List.exists decorators ~f:(name_is ~name:"SkipDecoratorWhenInlining") then
          update_actions actions name DecoratorPreprocessing.Action.DoNotInline
        else
          actions
    | _ -> actions
  in
  try
    String.split ~on:'\n' source
    |> Parser.parse
    >>| List.fold ~init:Reference.SerializableMap.empty ~f:parse_statement
    |> Result.ok
    |> Option.value ~default:Reference.SerializableMap.empty
  with
  | exn ->
      Log.warning
        "Ignoring `%s` when trying to get decorators to skip because of exception: %s"
        (PyrePath.show path)
        (Exn.to_string exn);
      Reference.SerializableMap.empty


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


let parse
    ~pyre_api
    ?path
    ~source
    ~taint_configuration
    ~source_sink_filter
    ~definitions
    ~stubs
    ~python_version
    ()
  =
  let new_models_and_queries, errors =
    create
      ~pyre_api
      ~path
      ~taint_configuration
      ~source_sink_filter
      ~definitions
      ~stubs
      ~python_version
      source
    |> List.partition_result
  in
  let new_models, new_queries =
    List.fold
      new_models_and_queries
      ~f:
        (fun (models, queries) -> function
          | Model model -> model :: models, queries
          | Query query -> models, query :: queries)
      ~init:([], [])
  in
  {
    models =
      List.map new_models ~f:(fun model -> model.target, model.model)
      |> Registry.of_alist ~join:Model.join_user_models;
    queries = new_queries;
    errors = List.concat errors;
  }


let invalid_model_query_error error =
  model_verification_error ~path:None ~location:Location.any error


let create_callable_model_from_annotations
    ~pyre_api
    ~modelable
    ~source_sink_filter
    ~is_obscure
    annotations
  =
  let open Core.Result in
  let open ModelVerifier in
  match modelable with
  | Modelable.Callable { define; _ } ->
      define
      |> Lazy.force
      |> (function
           | { Define.signature; _ } -> signature)
      |> resolve_global_callable
           ~path:None
           ~location:Location.any
           ~pyre_api
           ~verify_decorators:false
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
      List.fold_result annotations ~init:default_model ~f:(fun accumulator model_annotation ->
          add_taint_annotation_to_model
            ~path:None
            ~location:Location.any
            ~model_name:"Model query"
            ~pyre_api
            ~callable_annotation
            ~source_sink_filter
            accumulator
            model_annotation)
  | _ -> failwith "unreachable"


let create_attribute_model_from_annotations ~pyre_api ~name ~source_sink_filter annotations =
  let open Core.Result in
  List.fold_result annotations ~init:Model.empty_model ~f:(fun accumulator annotation ->
      let model_annotation =
        match annotation with
        | TaintAnnotation.Source _ -> Ok (ModelAnnotation.ReturnAnnotation annotation)
        | TaintAnnotation.Sink _
        | TaintAnnotation.Tito _ ->
            Ok (ModelAnnotation.ParameterAnnotation (attribute_symbolic_parameter, annotation))
        | _ ->
            Error
              (invalid_model_query_error
                 (InvalidAnnotationForAttributeModel
                    { name; annotation = TaintAnnotation.show annotation }))
      in
      model_annotation
      >>= fun model_annotation ->
      add_taint_annotation_to_model
        ~path:None
        ~location:Location.any
        ~model_name:"Model query"
        ~pyre_api
        ~callable_annotation:None
        ~source_sink_filter
        accumulator
        model_annotation)


let verify_model_syntax ~path ~source =
  match String.split ~on:'\n' source |> Parser.parse with
  | Ok _ -> ()
  | Error { Parser.Error.location; _ } ->
      let error = model_verification_error ~path:(Some path) ~location ParseError in
      raise (ModelVerificationError.ModelVerificationErrors [error])
