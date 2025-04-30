(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Expression
open Statement

module ExpressionTypes = struct
  type coverage_data = {
    expression: Expression.t option;
    type_: Type.t;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  type coverage_data_lookup = coverage_data Location.Table.t

  (** This visitor stores the coverage data information for an expression on the key of its
      location.

      It special-case names such as named arguments or the names in comprehensions and generators.

      The result state of this visitor is ignored. We need two read-only pieces of information to
      build the location table: the types resolved for this statement, and a reference to the
      (mutable) location table to update. *)
  module CreateDefinitionAndAnnotationLookupVisitor = struct
    type t = {
      pre_resolution: Resolution.t;
      post_resolution: Resolution.t;
      expressions_with_types: (Expression.t * TypeInfo.Unit.t) list Location.Table.t;
      coverage_data_lookup: coverage_data_lookup;
    }

    let node_base
        ~postcondition
        ({ pre_resolution; post_resolution; coverage_data_lookup; expressions_with_types } as state)
        node
      =
      let resolution = if postcondition then post_resolution else pre_resolution in
      let store_coverage_data ~expression ~location type_info =
        if not (Location.equal location Location.any) then
          let maybe_type =
            let original = TypeInfo.Unit.original type_info in
            if Type.is_top original || Type.is_unbound original then
              let annotation = TypeInfo.Unit.annotation type_info in
              if Type.is_top annotation || Type.is_unbound annotation then
                None
              else
                Some annotation
            else
              Some original
          in
          match maybe_type with
          | Some ty ->
              Hashtbl.set coverage_data_lookup ~key:location ~data:{ expression; type_ = ty }
              |> ignore
          | None -> ()
      in
      let naively_resolve_type_info ?custom_resolution expression =
        let resolution = Option.value custom_resolution ~default:resolution in
        try Some (Resolution.resolve_expression_to_type_info resolution expression) with
        | ClassHierarchy.Untracked _ -> None
      in
      let store_naively_resolved_coverage_data ~location ~expression =
        naively_resolve_type_info expression
        >>| store_coverage_data ~location ~expression:(Some expression)
        |> ignore
      in
      let look_up_type_info ~location ~expression =
        let type_info_if_expression_matches (saved_expression, type_info) =
          if Int.equal (Expression.location_insensitive_compare saved_expression expression) 0 then
            Some type_info
          else
            None
        in
        match Hashtbl.find expressions_with_types location with
        | Some expression_type_info_pairs -> (
            match List.find_map expression_type_info_pairs ~f:type_info_if_expression_matches with
            | Some type_info -> Some type_info
            | None ->
                (* Any mismatch here is probably due to synthestized AST nodes, either from
                   preprocessing or CFG construction, creating conflicting locations. All such
                   conflicts are potential sources of bugs in the `types(...)` query but for now we
                   assume it is okay, which is needed because so many expressions lower into dunder
                   method calls. This debug log can be useful for investigating problems. *)
                Log.debug
                  "At location %a, found saved expressions %s vs traversal expression %a. Ignoring \
                   the saved type info."
                  Location.pp
                  location
                  (List.map expression_type_info_pairs ~f:(fun (e, _) -> e)
                  |> [%show: Expression.t list])
                  Expression.pp
                  expression;
                None)
        | None -> None
      in
      let store_coverage_data_for_expression ({ Node.location; _ } as expression) =
        match look_up_type_info ~location ~expression with
        | Some type_info -> store_coverage_data ~location ~expression:(Some expression) type_info
        | None ->
            store_naively_resolved_coverage_data ~location ~expression;
            ()
      in
      (* Some special expressions get additional type information that typeCheck.ml doesn't produce,
         for example names bound in comprehensions get typed because the extra types are useful in
         IDE even though they are not used for type errors, and callable argument names get the
         argument types for similar reasons. *)
      let add_special_case_type_information { Node.value; _ } =
        let store_generator_target_type
            resolution
            ({ Comprehension.Generator.target; _ } as generator)
          =
          let annotate_expression custom_resolution ({ Node.location; _ } as expression) =
            naively_resolve_type_info ~custom_resolution expression
            >>| store_coverage_data ~location ~expression:(Some expression)
            |> ignore
          in
          let resolution_with_target =
            let target_assignment = Statement.generator_assignment generator in
            Resolution.resolve_assignment resolution target_assignment
          in
          annotate_expression resolution_with_target target;
          resolution_with_target
        in
        match value with
        | Expression.Call { arguments; _ } ->
            let annotate_argument_name { Call.Argument.name; value } =
              match name, naively_resolve_type_info value with
              | Some { Node.location; _ }, Some type_info ->
                  store_coverage_data ~location ~expression:(Some value) type_info
              | _ -> ()
            in
            List.iter ~f:annotate_argument_name arguments
        | Expression.DictionaryComprehension { generators; _ } ->
            List.fold generators ~f:store_generator_target_type ~init:resolution |> ignore
        | Expression.Generator { generators; _ }
        | Expression.ListComprehension { generators; _ }
        | Expression.SetComprehension { generators; _ } ->
            List.fold generators ~f:store_generator_target_type ~init:resolution |> ignore
        | _ -> ()
      in
      match node with
      | Visit.Expression expression ->
          store_coverage_data_for_expression expression;
          add_special_case_type_information expression;
          state
      | Visit.Reference { Node.value = reference; location } ->
          store_coverage_data_for_expression (Ast.Expression.from_reference ~location reference);
          state
      | _ -> state


    let node = node_base ~postcondition:false

    let node_postcondition = node_base ~postcondition:true

    let visit_statement_children _ statement =
      match Node.value statement with
      | Statement.Class _ -> false
      | _ -> true


    let visit_expression_children _ _ = true

    let visit_format_string_children _ _ = true

    let visit_expression_based_on_parent ~parent_expression:_ _ = true
  end

  (** This is a simple wrapper around [CreateDefinitionAndAnnotationLookupVisitor]. It ensures that
      the lookup for type annotations, such as `x: Foo`, points to the definition of the type `Foo`,
      not `Type[Foo]`. *)
  module CreateLookupsIncludingTypeAnnotationsVisitor = struct
    include Visit.MakeNodeVisitor (CreateDefinitionAndAnnotationLookupVisitor)

    let visit state source =
      let state = ref state in
      let visit_statement_override ~state statement =
        (* Special-casing for statements that require lookup using the postcondition. *)
        let precondition_visit =
          visit_expression
            ~parent_expression:None
            ~state
            ~visitor_override:CreateDefinitionAndAnnotationLookupVisitor.node
        in
        let postcondition_visit =
          visit_expression
            ~parent_expression:None
            ~state
            ~visitor_override:CreateDefinitionAndAnnotationLookupVisitor.node_postcondition
        in
        let store_type_annotation annotation =
          let { CreateDefinitionAndAnnotationLookupVisitor.pre_resolution; coverage_data_lookup; _ }
            =
            !state
          in
          let resolved =
            GlobalResolution.parse_annotation
              (Resolution.global_resolution pre_resolution)
              annotation
            |> Type.class_type
          in
          let location = Node.location annotation in
          if not (Location.equal location Location.any) then
            Hashtbl.add
              coverage_data_lookup
              ~key:location
              ~data:{ expression = None; type_ = resolved }
            (* Type annotations do not have expressions, so we set expression to None. *)
            |> ignore
        in
        match Node.value statement with
        | Statement.Assign { Assign.target; annotation; value; _ } -> (
            postcondition_visit target;
            annotation >>| store_type_annotation |> ignore;
            match value with
            | Some { value = Constant Ellipsis; _ }
            | None ->
                ()
            | Some value -> precondition_visit value)
        | Define
            ({ Define.signature = { name; parameters; decorators; return_annotation; _ }; _ } as
            define) ->
            let visit_parameter { Node.value = { Parameter.annotation; value; name }; location } =
              (* Location in the AST includes both the parameter name and the annotation. For our
                 purpose, we just need the location of the name. *)
              let location =
                let { Location.start = { Location.line = start_line; column = start_column }; _ } =
                  location
                in
                {
                  Location.start = { Location.line = start_line; column = start_column };
                  stop =
                    {
                      Location.line = start_line;
                      column = start_column + String.length (Identifier.sanitized name);
                    };
                }
              in
              Expression.Name (Name.Identifier name) |> Node.create ~location |> postcondition_visit;
              Option.iter ~f:postcondition_visit value;
              annotation >>| store_type_annotation |> ignore
            in
            precondition_visit
              (Ast.Expression.from_reference
                 ~location:(Define.name_location ~body_location:statement.location define)
                 name);
            List.iter parameters ~f:visit_parameter;
            List.iter decorators ~f:postcondition_visit;
            Option.iter ~f:store_type_annotation return_annotation
        | Import { Import.from; imports } ->
            let visit_import { Node.value = { Import.name; _ }; location = import_location } =
              let qualifier =
                match from with
                | Some { Node.value = reference; _ } -> reference
                | None -> Reference.empty
              in
              let create_qualified_expression ~location =
                Reference.combine qualifier name |> Ast.Expression.from_reference ~location
              in
              precondition_visit (create_qualified_expression ~location:import_location)
            in
            List.iter imports ~f:visit_import
        | Class ({ Class.name; _ } as class_) ->
            from_reference
              ~location:(Class.name_location ~body_location:(Node.location statement) class_)
              name
            |> store_type_annotation
        | _ -> visit_statement ~state statement
      in
      List.iter ~f:(visit_statement_override ~state) source.Source.statements;
      !state
  end

  let create_of_module type_environment qualifier =
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
    let type_check_controls =
      TypeEnvironment.ReadOnly.controls type_environment |> EnvironmentControls.type_check_controls
    in
    let coverage_data_lookup = Location.Table.create () in
    let walk_define define_name =
      match
        ( GlobalResolution.get_define_body_in_project global_resolution define_name,
          TypeCheck.compute_local_annotations ~type_check_controls ~global_resolution define_name )
      with
      | None, _
      | _, None ->
          ()
      | ( Some ({ Node.value = define; _ } as define_node),
          Some (local_annotation_map, expressions_with_types) ) ->
          let cfg = Cfg.create define in
          let walk_statement node_id statement_index statement =
            let pre_annotations, post_annotations =
              let statement_key = [%hash: int * int] (node_id, statement_index) in
              ( TypeInfo.ForFunctionBody.ReadOnly.get_precondition
                  local_annotation_map
                  ~statement_key
                |> Option.value ~default:TypeInfo.Store.empty,
                TypeInfo.ForFunctionBody.ReadOnly.get_postcondition
                  local_annotation_map
                  ~statement_key
                |> Option.value ~default:TypeInfo.Store.empty )
            in
            let pre_resolution =
              (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
              TypeCheck.resolution
                global_resolution
                ~type_info_store:pre_annotations
                (module TypeCheck.DummyContext)
            in
            let post_resolution =
              (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
              TypeCheck.resolution
                global_resolution
                ~type_info_store:post_annotations
                (module TypeCheck.DummyContext)
            in
            CreateLookupsIncludingTypeAnnotationsVisitor.visit
              {
                CreateDefinitionAndAnnotationLookupVisitor.pre_resolution;
                post_resolution;
                coverage_data_lookup;
                expressions_with_types;
              }
              (Source.create [statement])
            |> ignore
          in
          let walk_cfg_node ~key:node_id ~data:cfg_node =
            let statements = Cfg.Node.statements cfg_node in
            List.iteri statements ~f:(walk_statement node_id)
          in
          Hashtbl.iteri cfg ~f:walk_cfg_node;
          (* Special-case define signature processing, since this is not included in the define's
             cfg. *)
          let define_signature =
            { define_node with value = Statement.Define { define with Define.body = [] } }
          in
          walk_statement Cfg.entry_index 0 define_signature
    in
    let define_names =
      GlobalResolution.get_define_names_for_qualifier_in_project global_resolution qualifier
    in
    List.iter define_names ~f:walk_define;
    coverage_data_lookup
end

(* This module provides some datatypes for combining locations and symbols that are used by both the
   ExpressionLevelCoverage logic and the SymbolSelection logic. *)
module LocationWithSymbol = struct
  type symbol_with_definition =
    | Expression of Expression.t
    | TypeAnnotation of Expression.t
  [@@deriving compare, show, sexp]

  type cfg_data = {
    define_name: Reference.t;
    node_id: int;
    statement_index: int;
  }
  [@@deriving compare, show, sexp]

  type symbol_and_cfg_data = {
    symbol_with_definition: symbol_with_definition;
    cfg_data: cfg_data;
    (* This indicates whether the expression needs to be processed using information after checking
       the current statement.

       For example, in `x = f(x)`, we want the type of the target `x` after typechecking the
       statement but we want the type of the argument `x` before typechecking the statement. *)
    use_postcondition_info: bool;
  }
  [@@deriving compare, show, sexp]

  let symbol_with_definition { symbol_with_definition; _ } = symbol_with_definition

  let location_of_symbol_data { symbol_with_definition; _ } =
    let location_of_symbol = function
      | Expression { Node.location; _ } -> location
      | TypeAnnotation { Node.location; _ } -> location
    in
    location_of_symbol symbol_with_definition


  let location_insensitive_compare_symbol_and_cfg_data
      ({ symbol_with_definition = left_symbol_with_definition; _ } as left)
      ({ symbol_with_definition = right_symbol_with_definition; _ } as right)
    =
    let first_result =
      match left_symbol_with_definition, right_symbol_with_definition with
      | Expression left_expression, Expression right_expression
      | TypeAnnotation left_expression, TypeAnnotation right_expression ->
          Expression.location_insensitive_compare left_expression right_expression
      | Expression _, TypeAnnotation _ -> -1
      | TypeAnnotation _, Expression _ -> 1
    in
    if first_result = 0 then
      [%compare: symbol_and_cfg_data]
        left
        { right with symbol_with_definition = left_symbol_with_definition }
    else
      first_result
end

module ExpressionLevelCoverage = struct
  type typeisany =
    | ParameterIsAny
    | OtherExpressionIsAny
  [@@deriving compare, sexp, show, hash, to_yojson]

  type reason =
    | TypeIsAny of typeisany
    | ContainerParameterIsAny
    | CallableParameterIsUnknownOrAny
    | CallableReturnIsAny
  [@@deriving compare, sexp, show, hash, to_yojson]

  type coverage_gap = {
    coverage_data: ExpressionTypes.coverage_data;
    reason: reason;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  type coverage_gap_by_location = {
    location: Location.t;
    function_name: string option;
    type_: Type.t;
    reason: string list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type coverage_for_path = {
    total_expressions: int;
    coverage_gaps: coverage_gap_by_location list;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  let parameter_is_any_message =
    [
      "This parameter has the 'Any' type, which is unsafe, Pyre will be unable to perform further \
       checks with this expression.";
    ]


  let expression_is_any_message =
    [
      "This expression has the 'Any' type, which is unsafe, Pyre will be unable to perform further \
       checks with this expression.";
    ]


  let container_parameter_is_any_message =
    ["Consider adding stronger annotations to the container."]


  let callable_parameter_is_unknown_or_any_message =
    ["Consider adding stronger annotations to the parameters."]


  let callable_return_is_any_message = ["Consider adding stronger annotations to the return type."]

  let get_all_nodes_and_coverage_data coverage_data_lookup = Hashtbl.to_alist coverage_data_lookup

  let classify_coverage_data { ExpressionTypes.expression; type_ } =
    let make_coverage_gap reason = Some { coverage_data = { expression; type_ }; reason } in
    match type_ with
    | Any -> (
        match expression with
        | Some { value = Expression.Name (Name.Identifier name); _ } -> (
            match String.chop_prefix name ~prefix:"$parameter$" with
            | Some _ -> make_coverage_gap (TypeIsAny ParameterIsAny)
            | None -> make_coverage_gap (TypeIsAny OtherExpressionIsAny))
        | _ -> make_coverage_gap (TypeIsAny OtherExpressionIsAny))
    | Parametric { name = "list" | "set"; arguments = [Single Any] }
    | Parametric { name = "dict"; arguments = [Single Any; Single _] | [Single _; Single Any] } ->
        make_coverage_gap ContainerParameterIsAny
    | Callable { implementation = { annotation = Type.Any; _ }; _ } ->
        make_coverage_gap CallableReturnIsAny
    | Callable { implementation = { parameters = Defined (_ :: _ as parameter_list); _ }; _ } ->
        let parameter_is_top_or_any = function
          | Type.Callable.CallableParamType.Named { annotation = Type.Any | Type.Top; _ } -> true
          | _ -> false
        in
        (* This will treat parameters that use default values, which will never have a runtime
           error, as a coverage gap. *)
        if List.exists ~f:parameter_is_top_or_any parameter_list then
          make_coverage_gap CallableParameterIsUnknownOrAny
        else
          None
    | _ -> None


  let coverage_gaps_in_module coverage_data_list =
    List.map ~f:classify_coverage_data coverage_data_list |> List.filter_opt


  let get_expression_level_coverage coverage_data_lookup =
    let all_nodes = get_all_nodes_and_coverage_data coverage_data_lookup in
    let total_expressions = List.length all_nodes in
    let coverage_gap_and_locations =
      List.map all_nodes ~f:(fun (location, coverage_data) ->
          location, classify_coverage_data coverage_data)
    in
    let coverage_gap_by_locations =
      let message reason =
        match reason with
        | TypeIsAny ParameterIsAny -> parameter_is_any_message
        | TypeIsAny OtherExpressionIsAny -> expression_is_any_message
        | ContainerParameterIsAny -> container_parameter_is_any_message
        | CallableParameterIsUnknownOrAny -> callable_parameter_is_unknown_or_any_message
        | CallableReturnIsAny -> callable_return_is_any_message
      in
      List.filter_map coverage_gap_and_locations ~f:(fun (location, coverage_gap_option) ->
          let get_function_name type_ =
            match type_ with
            | Type.Callable { kind = Named name; _ } -> Some (Reference.show name)
            | _ -> None
          in
          match coverage_gap_option with
          | Some { coverage_data = { type_; _ }; reason } ->
              Some
                {
                  location;
                  function_name = get_function_name type_;
                  type_;
                  reason = message reason;
                }
          | None -> None)
    in
    let sorted_coverage_gap_by_locations =
      List.sort coverage_gap_by_locations ~compare:[%compare: coverage_gap_by_location]
    in
    { total_expressions; coverage_gaps = sorted_coverage_gap_by_locations }
end

(* This module contains the key logic needed to pick a symbol from a location or position; it was
   created to enable IDE features without type checking an entire module, and it powers many of the
   lookups in the `SingleSymbolLookups` module below. *)
module SymbolSelection = struct
  module type PositionData = sig
    val position : Location.position

    val cfg_data : LocationWithSymbol.cfg_data
  end

  type resolution_error =
    | ResolvedTop
    | ResolvedUnbound
    | UntrackedType of string
  [@@deriving sexp, show, compare, yojson { strict = false }]

  type attribute_lookup_error =
    | ReferenceNotFoundAndBaseUnresolved of resolution_error
    | AttributeUnresolved
    | ClassSummaryNotFound
    | ClassSummaryAttributeNotFound
  [@@deriving sexp, show, compare, yojson { strict = false }]

  type lookup_error =
    | SymbolNotFound
    | IdentifierDefinitionNotFound of Reference.t
    | AttributeDefinitionNotFound of string option * attribute_lookup_error
    | UnsupportedExpression of string
  [@@deriving sexp, show, compare, yojson { strict = false }]

  let location_contains_position
      {
        Location.start = { Location.column = start_column; line = start_line };
        stop = { Location.column = stop_column; line = stop_line };
        _;
      }
      { Location.column; line }
    =
    let start_ok = start_line < line || (start_line = line && start_column <= column) in
    let stop_ok = stop_line > line || (stop_line = line && stop_column >= column) in
    start_ok && stop_ok


  let get_best_location lookup_table ~position =
    let weight
        {
          Location.start = { Location.column = start_column; line = start_line };
          stop = { Location.column = stop_column; line = stop_line };
          _;
        }
      =
      ((stop_line - start_line) * 1000) + stop_column - start_column
    in
    Hashtbl.filter_keys lookup_table ~f:(fun key -> location_contains_position key position)
    |> Hashtbl.to_alist
    |> List.min_elt ~compare:(fun (location_left, _) (location_right, _) ->
           weight location_left - weight location_right)


  let get_coverage_data = get_best_location

  let covers_position ~position = function
    | {
        Node.value =
          ( Statement.Class { Class.decorators; _ }
          | Statement.Define { Define.signature = { Define.Signature.decorators; _ }; _ } );
        location;
      } ->
        location_contains_position location position
        || List.exists decorators ~f:(fun { Node.location = decorator_location; _ } ->
               location_contains_position decorator_location position)
    | { Node.location; _ } -> location_contains_position location position


  module FindNarrowestSpanningExpression (PositionData : PositionData) = struct
    type t = LocationWithSymbol.symbol_and_cfg_data list

    let node_common ~use_postcondition_info state = function
      | Visit.Expression ({ Node.location; _ } as expression)
        when location_contains_position location PositionData.position ->
          {
            LocationWithSymbol.symbol_with_definition = Expression expression;
            cfg_data = PositionData.cfg_data;
            use_postcondition_info;
          }
          :: state
      | Visit.Argument { argument = { location; _ }; callee }
        when location_contains_position location PositionData.position ->
          {
            symbol_with_definition = Expression callee;
            cfg_data = PositionData.cfg_data;
            use_postcondition_info;
          }
          :: state
      | _ -> state


    let node = node_common ~use_postcondition_info:false

    let node_using_postcondition = node_common ~use_postcondition_info:true

    let visit_statement_children _ _ = true

    let visit_expression_children _ _ = true

    let visit_format_string_children _ _ = true

    let visit_expression_based_on_parent ~parent_expression:_ _ = true
  end

  (** This is a simple wrapper around [FindNarrowestSpanningExpression]. It visits imported symbols
      and type annotations, and ensures that we use postcondition information when dealing with
      function parameters or target variables in assignment statements. . *)
  module FindNarrowestSpanningExpressionOrTypeAnnotation (PositionData : PositionData) = struct
    include Visit.MakeNodeVisitor (FindNarrowestSpanningExpression (PositionData))

    let collect_type_annotation_symbols annotation_expression =
      let expression_symbol_to_type_annotation_symbol = function
        | { LocationWithSymbol.symbol_with_definition = Expression expression; _ } ->
            {
              LocationWithSymbol.symbol_with_definition = TypeAnnotation expression;
              cfg_data = PositionData.cfg_data;
              use_postcondition_info = false;
            }
        | type_annotation_symbol -> type_annotation_symbol
      in
      let symbols = ref [] in
      visit_expression ~parent_expression:None ~state:symbols annotation_expression;
      List.map !symbols ~f:expression_symbol_to_type_annotation_symbol


    let visit state source =
      let visit_statement_for_type_annotations_and_parameters
          ~state
          ({ Node.value = statement_value; _ } as statement)
        =
        let module Visitor = FindNarrowestSpanningExpression (PositionData) in
        let visit_using_precondition_info =
          visit_expression ~parent_expression:None ~state ~visitor_override:Visitor.node
        in
        let visit_using_postcondition_info =
          visit_expression
            ~parent_expression:None
            ~state
            ~visitor_override:Visitor.node_using_postcondition
        in
        let store_type_annotation ({ Node.location; _ } as annotation_expression) =
          if location_contains_position location PositionData.position then
            state := collect_type_annotation_symbols annotation_expression @ !state
        in
        if covers_position ~position:PositionData.position statement then
          match statement_value with
          | Statement.Assign { Assign.target; annotation; value; _ } ->
              visit_using_postcondition_info target;
              Option.iter annotation ~f:store_type_annotation;
              Option.iter value ~f:visit_using_precondition_info
          | Define
              ({ Define.signature = { name; parameters; decorators; return_annotation; _ }; _ } as
              define) ->
              let visit_parameter { Node.value = { Parameter.annotation; value; name }; location } =
                (* Location in the AST includes both the parameter name and the annotation. For our
                   purpose, we just need the location of the name. *)
                let location =
                  let { Location.start = { Location.line = start_line; column = start_column }; _ } =
                    location
                  in
                  {
                    Location.start = { Location.line = start_line; column = start_column };
                    stop =
                      {
                        Location.line = start_line;
                        column = start_column + String.length (Identifier.sanitized name);
                      };
                  }
                in
                Expression.Name (Name.Identifier name)
                |> Node.create ~location
                |> visit_using_postcondition_info;
                Option.iter value ~f:visit_using_postcondition_info;
                Option.iter annotation ~f:store_type_annotation
              in
              let define_name =
                Ast.Expression.from_reference
                  ~location:(Define.name_location ~body_location:statement.location define)
                  name
              in
              visit_using_precondition_info define_name;
              List.iter parameters ~f:visit_parameter;
              List.iter decorators ~f:visit_using_postcondition_info;
              Option.iter return_annotation ~f:store_type_annotation
              (* Note that we do not recurse on the body of the define. That is done by the caller
                 when walking the CFG. *)
          | Import { Import.from; imports } ->
              let visit_import { Node.value = { Import.name; _ }; location = import_location } =
                let qualifier =
                  match from with
                  | Some { Node.value = reference; _ } -> reference
                  | None -> Reference.empty
                in
                let create_qualified_expression ~location =
                  Reference.combine qualifier name |> Ast.Expression.from_reference ~location
                in
                create_qualified_expression ~location:import_location
                |> visit_using_precondition_info
              in
              let visit_from = function
                | Some { Node.value = from; location } ->
                    visit_using_precondition_info (Ast.Expression.from_reference ~location from)
                | None -> ()
              in
              List.iter imports ~f:visit_import;
              visit_from from
          | Expression expression -> visit_using_precondition_info expression
          | _ -> visit_statement ~state statement
      in
      let state = ref state in
      List.iter
        ~f:(visit_statement_for_type_annotations_and_parameters ~state)
        source.Source.statements;
      !state
  end

  let narrowest_match symbol_data_list =
    let compare_by_length
        { LocationWithSymbol.symbol_with_definition = Expression left | TypeAnnotation left; _ }
        { LocationWithSymbol.symbol_with_definition = Expression right | TypeAnnotation right; _ }
      =
      let open Location in
      let { start = left_start; stop = left_stop } = Node.location left in
      let { start = right_start; stop = right_stop } = Node.location right in
      (* We assume that if expression A overlaps with expression B, then A contains B (or vice
         versa). That is, there are no partially-overlapping expressions. *)
      if compare_position left_start right_start = -1 || compare_position left_stop right_stop = 1
      then
        1
      else if
        compare_position right_start left_start = -1 || compare_position right_stop left_stop = 1
      then
        -1
      else
        (* Prefer the expression `foo` over the invisible `foo.__dunder_method__`, since the user
           probably intends the former. *)
        match Node.value left, Node.value right with
        | Expression.Name (Name.Attribute { origin = Some _; _ }), _ -> 1
        | _, Expression.Name (Name.Attribute { origin = Some _; _ }) -> -1
        | _ -> (
            (* Prefer names over any other types of expressions. This is useful for if-conditions,
               where we synthesize asserts for `foo` and `not foo`, having the same location
               range. *)
            match Node.value left, Node.value right with
            | Expression.Name _, _ -> -1
            | _, Expression.Name _ -> 1
            | _ -> 0)
    in
    List.min_elt ~compare:compare_by_length symbol_data_list


  let find_spanning_symbols ~type_environment ~module_reference position =
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
    let walk_define
        names_so_far
        ({ Node.value = { Define.signature = { name; _ }; _ } as define; _ } as define_node)
      =
      let walk_statement ~node_id statement_index symbols_so_far statement =
        let module FindNarrowestSpanningExpressionOrTypeAnnotation =
        FindNarrowestSpanningExpressionOrTypeAnnotation (struct
          let position = position

          let cfg_data = { LocationWithSymbol.define_name = name; node_id; statement_index }
        end)
        in
        FindNarrowestSpanningExpressionOrTypeAnnotation.visit [] (Source.create [statement])
        @ symbols_so_far
      in
      let walk_cfg_node ~key:node_id ~data:cfg_node names_so_far =
        let statements = Cfg.Node.statements cfg_node in
        List.foldi statements ~init:names_so_far ~f:(walk_statement ~node_id)
      in
      let walk_define_signature ~define_signature names_so_far =
        (* Special-case define signature processing, since this is not included in the define's
           cfg. *)
        walk_statement ~node_id:Cfg.entry_index 0 names_so_far define_signature
      in
      let cfg = Cfg.create define in
      let define_signature =
        { define_node with value = Statement.Define { define with Define.body = [] } }
      in
      Hashtbl.fold cfg ~init:names_so_far ~f:walk_cfg_node
      |> walk_define_signature ~define_signature
    in
    let all_defines =
      GlobalResolution.get_define_names_for_qualifier_in_project global_resolution module_reference
      |> List.filter_map ~f:(GlobalResolution.get_define_body_in_project global_resolution)
    in
    List.fold all_defines ~init:[] ~f:walk_define


  let find_narrowest_spanning_symbol ~type_environment ~module_reference position =
    let timer = Timer.start () in
    let symbols_covering_position =
      find_spanning_symbols ~type_environment ~module_reference position
    in
    let symbol_data = narrowest_match symbols_covering_position in
    Log.log
      ~section:`Performance
      "locationBasedLookup: Narrowest symbol spanning position `%s:%s`: Found `%s` in %d ms\n\
       All symbols spanning the position: %s\n"
      (Reference.show module_reference)
      ([%show: Location.position] position)
      ([%show: LocationWithSymbol.symbol_with_definition option]
         (symbol_data >>| LocationWithSymbol.symbol_with_definition))
      (Timer.stop_in_ms timer)
      (List.map symbols_covering_position ~f:LocationWithSymbol.symbol_with_definition
      |> [%show: LocationWithSymbol.symbol_with_definition list]);
    match symbol_data with
    | Some location -> Ok location
    | None -> Error SymbolNotFound


  (* At the time of writing this code, I am not certain whether there exist cases where two
     expressions can have identical locations; I *think* the answer is no, any nesting requires
     extra characters.

     But if I am wrong, this function will arbitrarily pick the first match it sees. *)
  let find_first_symbol_matching_location ~type_environment ~module_reference location =
    let timer = Timer.start () in
    let position = Location.start location in
    let symbols_covering_position =
      find_spanning_symbols ~type_environment ~module_reference position
    in
    let symbol_data =
      List.find symbols_covering_position ~f:(fun symbol_data ->
          Location.equal location (LocationWithSymbol.location_of_symbol_data symbol_data))
    in
    Log.log
      ~section:`Performance
      "locationBasedLookup: Symbol matching location `%s:%s`: Found `%s` in %d ms\n\
       All symbols spanning the position: %s\n"
      (Reference.show module_reference)
      ([%show: Location.t] location)
      ([%show: LocationWithSymbol.symbol_with_definition option]
         (symbol_data >>| LocationWithSymbol.symbol_with_definition))
      (Timer.stop_in_ms timer)
      (List.map symbols_covering_position ~f:LocationWithSymbol.symbol_with_definition
      |> [%show: LocationWithSymbol.symbol_with_definition list]);
    match symbol_data with
    | Some location -> Ok location
    | None -> Error SymbolNotFound


  let resolve ~resolution expression =
    try
      let resolved = Resolution.resolve_expression_to_type resolution expression in
      if Type.is_top resolved then
        Error ResolvedTop
      else if Type.is_unbound resolved then
        Error ResolvedUnbound
      else
        Ok resolved
    with
    | ClassHierarchy.Untracked annotation -> Error (UntrackedType annotation)
end

module SingleSymbolQueries = struct
  type hover_info = {
    value: string option;
    docstring: string option;
  }
  [@@deriving sexp, show, compare, yojson { strict = false }]

  (* AttributeDetail is a datatype used to model attribute lookups for use cases like completions in
     IDE. *)
  module AttributeDetail = struct
    type kind =
      | Simple
      | Variable
      | Property
      | Method
    [@@deriving show, compare, sexp]

    type t = {
      kind: kind;
      name: string;
      detail: string;
    }
    [@@deriving show, compare, sexp]

    let from_attribute attr =
      let open AnnotatedAttribute in
      let name = name attr in
      let detail = parent_name attr in
      match uninstantiated_annotation attr with
      | UninstantiatedAnnotation.{ kind = Property _; _ } -> { kind = Property; name; detail }
      | { kind = Attribute (Callable _); _ } -> { kind = Method; name; detail }
      | _ -> { kind = Variable; name; detail }
  end

  (* Please view diff D53973886 to to understand how this data structure maps to the corresponding
     Python data structure for document symbols *)
  module DocumentSymbolItem = struct
    module SymbolKind = struct
      type t =
        | File
        | Module
        | Namespace
        | Package
        | Class
        | Method
        | Property
        | Field
        | Constructor
        | Enum
        | Interface
        | Function
        | Variable
        | Constant
        | String
        | Number
        | Boolean
        | Array
        | Object
        | Key
        | Null
        | EnumMember
        | Struct
        | Event
        | Operator
        | TypeParameter
      [@@deriving sexp, compare, yojson { strict = false }]

      let to_yojson = function
        | File -> `String "FILE"
        | Module -> `String "MODULE"
        | Namespace -> `String "NAMESPACE"
        | Package -> `String "PACKAGE"
        | Class -> `String "CLASS"
        | Method -> `String "METHOD"
        | Property -> `String "PROPERTY"
        | Field -> `String "FIELD"
        | Constructor -> `String "CONSTRUCTOR"
        | Enum -> `String "ENUM"
        | Interface -> `String "INTERFACE"
        | Function -> `String "FUNCTION"
        | Variable -> `String "VARIABLE"
        | Constant -> `String "CONSTANT"
        | String -> `String "STRING"
        | Number -> `String "NUMBER"
        | Boolean -> `String "BOOLEAN"
        | Array -> `String "ARRAY"
        | Object -> `String "OBJECT"
        | Key -> `String "KEY"
        | Null -> `String "NULL"
        | EnumMember -> `String "ENUMMEMBER"
        | Struct -> `String "STRUCT"
        | Event -> `String "EVENT"
        | Operator -> `String "OPERATOR"
        | TypeParameter -> `String "TYPEPARAMETER"
    end

    type t = {
      name: string;
      detail: string;
      kind: SymbolKind.t;
      range: Ast.Location.t;
      selectionRange: Ast.Location.t;
      children: t list; (* recursive type to represent a list of document symbols *)
    }
    [@@deriving sexp, compare, yojson { strict = false }]
  end

  let look_up_local_definition ~resolution ~define_name identifier =
    GlobalResolution.get_define_body_in_project
      (Resolution.global_resolution resolution)
      define_name
    >>= fun define ->
    let scope =
      match Scope.Scope.of_define define.value with
      | Some scope -> scope
      | None ->
          (* Module toplevel *)
          Scope.Scope.of_source (Source.create define.value.body)
    in
    let local_bindings = UninitializedLocalCheck.local_bindings scope in
    Map.find local_bindings identifier


  let find_definition ~resolution ~module_reference ~define_name reference =
    let local_definition =
      Reference.single reference
      >>| Identifier.sanitized
      >>= look_up_local_definition ~resolution ~define_name
      >>= function
      | { Scope.Binding.kind = ImportName _; _ } ->
          (* If we import `import foo`, go-to-def on uses of `foo` should go to the module `foo`,
             not the import location in the current module. So, don't treat `foo` as a locally
             defined variable. *)
          None
      | { Scope.Binding.location; _ } ->
          location |> Location.with_module ~module_reference |> Option.some
    in
    let definition_from_resolved_reference ~global_resolution = function
      | ResolvedReference.Module resolved_reference ->
          Location.with_module ~module_reference:resolved_reference Location.any |> Option.some
      | ResolvedReference.ModuleAttribute { from; name; remaining; _ } ->
          let resolved_reference =
            Reference.combine
              (Reference.create ~prefix:from name)
              (Reference.create_from_list remaining)
          in
          GlobalResolution.location_of_global global_resolution resolved_reference
    in
    let definition_location =
      match local_definition with
      | Some definition -> Some definition
      | None -> (
          (* A global variable will be qualified as a local. So, delocalize it. *)
          let reference = Reference.delocalize reference in
          let global_resolution = Resolution.global_resolution resolution in
          match GlobalResolution.location_of_global global_resolution reference with
          | Some definition -> Some definition
          | None ->
              GlobalResolution.resolve_exports global_resolution reference
              >>= definition_from_resolved_reference ~global_resolution)
    in
    let sanitize_location ({ Location.WithModule.module_reference; start; stop } as location) =
      if [%compare.equal: Location.WithModule.t] location Location.WithModule.any then
        None
      else if [%compare.equal: Location.t] { Location.start; stop } Location.any then
        (* Special forms have location as `any`. So, just point to the start of the file where they
           are defined. *)
        let dummy_position = { Location.line = 1; column = 0 } in
        { Location.start = dummy_position; stop = dummy_position }
        |> Location.with_module ~module_reference
        |> Option.some
      else
        Some location
    in
    definition_location >>= sanitize_location


  let get_expression_constructor expression : string =
    match expression with
    | Expression.Await _ -> "Await"
    | BinaryOperator _ -> "BinaryOperator"
    | BooleanOperator _ -> "BooleanOperator"
    | Call _ -> "Call"
    | ComparisonOperator _ -> "ComparisonOperator"
    | Constant _ -> "Constant"
    | Dictionary _ -> "Dictionary"
    | DictionaryComprehension _ -> "DictionaryComprehension"
    | Generator _ -> "Generator"
    | FormatString _ -> "FormatString"
    | Lambda _ -> "Lambda"
    | List _ -> "List"
    | ListComprehension _ -> "ListComprehension"
    | Name _ -> "Name"
    | Set _ -> "Set"
    | SetComprehension _ -> "SetComprehension"
    | Slice _ -> "Slice"
    | Starred _ -> "Starred"
    | Subscript _ -> "Subscript"
    | Ternary _ -> "Ternary"
    | Tuple _ -> "Tuple"
    | UnaryOperator _ -> "UnaryOperator"
    | WalrusOperator _ -> "WalrusOperator"
    | Yield _ -> "Yield"
    | YieldFrom _ -> "YieldFrom"


  let resolve_definition_for_name ~resolution ~module_reference ~define_name expression =
    let find_definition = find_definition ~resolution ~module_reference ~define_name in
    match Node.value expression with
    | Expression.Name (Name.Identifier identifier) -> begin
        let reference = Reference.create identifier in
        match find_definition reference with
        | Some definition -> Ok definition
        | None ->
            Error (SymbolSelection.IdentifierDefinitionNotFound (Reference.delocalize reference))
      end
    | Expression.Name (Name.Attribute { base; attribute; _ } as name) ->
        let reference = name_to_reference name in
        let definition = reference >>= find_definition in
        let resolve_definition_attribute =
          match definition with
          | Some definition -> Ok definition
          | None -> (
              (* Resolve prefix to check if this is a method. *)
              let base_type =
                match SymbolSelection.resolve ~resolution base with
                | Ok annotation as resolved ->
                    (* If it is a call to a class method or static method, `Foo.my_class_method()`,
                       the resolved base type will be `Type[Foo]`. Extract the class type `Foo`. *)
                    if Type.is_class_type annotation then
                      Ok (Type.single_argument annotation)
                    else
                      resolved
                | Error resolution_error ->
                    Error (SymbolSelection.ReferenceNotFoundAndBaseUnresolved resolution_error)
              in
              let open Result.Monad_infix in
              base_type
              >>= (fun parent ->
                    GlobalResolution.attribute_from_annotation
                      (Resolution.global_resolution resolution)
                      ~parent
                      ~name:attribute
                    |> Result.of_option ~error:SymbolSelection.AttributeUnresolved)
              >>= (fun attribute ->
                    AnnotatedAttribute.parent attribute
                    |> GlobalResolution.get_class_summary (Resolution.global_resolution resolution)
                    |> Result.of_option ~error:SymbolSelection.ClassSummaryNotFound)
              >>= fun class_summary ->
              let ({ ClassSummary.qualifier = module_reference; _ } as base_class_summary) =
                Node.value class_summary
              in
              let attributes = ClassSummary.attributes base_class_summary in
              match Identifier.SerializableMap.find_opt attribute attributes with
              | Some node -> Ok (Node.location node |> Location.with_module ~module_reference)
              | None -> Error ClassSummaryAttributeNotFound)
        in
        begin
          match resolve_definition_attribute with
          | Ok _ as definition -> definition
          | Error attribute_lookup_error ->
              let reference =
                reference >>| fun name -> Reference.show (Reference.delocalize name)
              in
              Error (AttributeDefinitionNotFound (reference, attribute_lookup_error))
        end
    | _ -> Error (UnsupportedExpression (get_expression_constructor expression.value))


  let attribute_details ~global_resolution ~accessed_through_class class_name =
    GlobalResolution.uninstantiated_attributes
      global_resolution
      ~transitive:true
      ~accessed_through_class
      ~include_generated_attributes:true
      class_name
    >>| List.map ~f:AttributeDetail.from_attribute


  let resolve_attributes_for_expression ~resolution expression =
    (* Resolve prefix to check if this is a method. *)
    let base_type =
      match SymbolSelection.resolve ~resolution expression with
      | Ok annotation ->
          (* If it is a call to a class method or static method, `Foo.my_class_method()`, the
             resolved base type will be `Type[Foo]`. Extract the class type `Foo`. *)
          if Type.is_class_type annotation then
            Some (Type.single_argument annotation)
          else
            Some annotation
      | Error _ -> None
    in
    base_type
    >>| Type.split
    >>= (fun (parent, _) -> Type.primitive_name parent)
    >>= attribute_details
          ~global_resolution:(Resolution.global_resolution resolution)
          ~accessed_through_class:true
    |> Option.value ~default:[]


  let resolution_from_cfg_data
      ~type_environment
      ~use_postcondition_info
      { LocationWithSymbol.define_name; node_id; statement_index }
    =
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
    let coverage_data_lookup_map =
      TypeEnvironment.ReadOnly.get_or_recompute_local_annotations type_environment define_name
      |> function
      | Some coverage_data_lookup_map -> coverage_data_lookup_map
      | None -> TypeInfo.ForFunctionBody.empty () |> TypeInfo.ForFunctionBody.read_only
    in
    let type_info_store =
      let statement_key = [%hash: int * int] (node_id, statement_index) in
      if use_postcondition_info then
        TypeInfo.ForFunctionBody.ReadOnly.get_postcondition coverage_data_lookup_map ~statement_key
        |> Option.value ~default:TypeInfo.Store.empty
      else
        TypeInfo.ForFunctionBody.ReadOnly.get_precondition coverage_data_lookup_map ~statement_key
        |> Option.value ~default:TypeInfo.Store.empty
    in
    (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
    TypeCheck.resolution global_resolution ~type_info_store (module TypeCheck.DummyContext)


  let resolve_definition_for_symbol
      ~type_environment
      ~module_reference
      {
        LocationWithSymbol.symbol_with_definition;
        cfg_data = { define_name; _ } as cfg_data;
        use_postcondition_info;
      }
    =
    let timer = Timer.start () in
    let definition_location =
      match symbol_with_definition with
      | Expression expression
      | TypeAnnotation expression ->
          resolve_definition_for_name
            ~resolution:
              (resolution_from_cfg_data ~type_environment ~use_postcondition_info cfg_data)
            ~module_reference
            ~define_name
            expression
    in
    Log.log
      ~section:`Performance
      "locationBasedLookup: Resolve definition for symbol: %d ms"
      (Timer.stop_in_ms timer);
    match definition_location with
    | Ok location -> Ok location
    | Error e -> Error e


  let location_of_definition ~type_environment ~module_reference position =
    let symbol_data =
      SymbolSelection.find_narrowest_spanning_symbol ~type_environment ~module_reference position
    in
    let location =
      Result.bind symbol_data ~f:(resolve_definition_for_symbol ~type_environment ~module_reference)
    in
    Log.log
      ~section:`Server
      "Definition for symbol at position `%s:%s`: %s"
      (Reference.show module_reference)
      ([%show: Location.position] position)
      ([%show: (Location.WithModule.t, SymbolSelection.lookup_error) result] location);
    location


  let resolve_completions_for_symbol
      ~type_environment
      { LocationWithSymbol.symbol_with_definition; cfg_data; use_postcondition_info }
    =
    let timer = Timer.start () in
    let completions =
      match symbol_with_definition with
      | Expression expression
      | TypeAnnotation expression -> (
          match expression with
          | { Node.value = Expression.Name (Name.Attribute { base; _ }); _ } ->
              resolve_attributes_for_expression
                ~resolution:
                  (resolution_from_cfg_data ~type_environment ~use_postcondition_info cfg_data)
                base
          | _ -> [])
    in
    Log.log
      ~section:`Performance
      "locationBasedLookup: Resolve completion for symbol: %d ms"
      (Timer.stop_in_ms timer);
    completions


  let completion_info_for_position ~type_environment ~module_reference position =
    let symbol_data =
      SymbolSelection.find_narrowest_spanning_symbol ~type_environment ~module_reference position
    in
    let completions =
      Result.ok symbol_data
      >>| resolve_completions_for_symbol ~type_environment
      |> Option.value ~default:[]
    in
    Log.log
      ~section:`Server
      "Completions for symbol at position `%s:%s`: %s"
      (Reference.show module_reference)
      ([%show: Location.position] position)
      ([%show: AttributeDetail.t list] completions);
    completions


  let find_docstring_for_symbol
      ~type_environment
      { LocationWithSymbol.symbol_with_definition; use_postcondition_info; cfg_data }
    =
    let get_docstring_from_define Define.{ body; _ } =
      match body with
      | { value = Statement.Expression { value = Constant (String { value; _ }); _ }; _ } :: _ ->
          Some value
      | _ -> None
    in
    match symbol_with_definition with
    | Expression e -> (
        match Node.value e with
        | Expression.Name name ->
            let resolution =
              resolution_from_cfg_data ~type_environment ~use_postcondition_info cfg_data
            in
            name_to_reference name
            >>= fun define_name ->
            GlobalResolution.get_define_body_in_project
              (Resolution.global_resolution resolution)
              define_name
            >>| Node.value
            >>= get_docstring_from_define
        | _ -> None)
    | TypeAnnotation _ -> None


  let resolve_type_for_symbol
      ~type_environment
      { LocationWithSymbol.symbol_with_definition; cfg_data; use_postcondition_info }
    =
    let timer = Timer.start () in
    let type_ =
      match symbol_with_definition with
      | Expression expression
      | TypeAnnotation expression ->
          SymbolSelection.resolve
            ~resolution:
              (resolution_from_cfg_data ~type_environment ~use_postcondition_info cfg_data)
            expression
    in
    Log.log
      ~section:`Performance
      "locationBasedLookup: Resolve type for symbol: %d ms"
      (Timer.stop_in_ms timer);
    Result.ok type_


  let type_for_symbol_data ~type_environment ~module_reference ~symbol_data position =
    let maybe_type = Result.ok symbol_data >>= resolve_type_for_symbol ~type_environment in
    Log.log
      ~section:`Server
      "Type of narrowest symbol at position `%s:%s`: %s"
      (Reference.show module_reference)
      ([%show: Location.position] position)
      (Option.map maybe_type ~f:Type.show |> Option.value ~default:"<EMPTY>");
    maybe_type


  let type_at_location ~type_environment ~module_reference location =
    let symbol_data =
      SymbolSelection.find_first_symbol_matching_location
        ~type_environment
        ~module_reference
        location
    in
    type_for_symbol_data ~type_environment ~module_reference ~symbol_data (Location.start location)


  let format_method_name name annotation =
    Format.asprintf "def %s%s: ..." (Reference.last name) (Type.show_concise annotation)


  let show_type_for_hover annotation =
    match annotation with
    | Type.Callable { kind = Named reference; _ } -> format_method_name reference annotation
    | Type.Parametric
        {
          name = "BoundMethod";
          arguments = [Single (Callable { kind = Named reference; _ }); Single _];
        } ->
        format_method_name reference annotation
    | _ -> Type.show_concise annotation


  let hover_info_for_position ~type_environment ~module_reference position =
    let symbol_data =
      SymbolSelection.find_narrowest_spanning_symbol ~type_environment ~module_reference position
    in
    let value =
      type_for_symbol_data ~type_environment ~module_reference ~symbol_data position
      >>| show_type_for_hover
    in
    let docstring = Result.ok symbol_data >>= find_docstring_for_symbol ~type_environment in
    { value; docstring }


  let document_symbol_info ~source =
    Log.log
      ~section:`Server
      "Returning empty response as this is not implemented yet: document symbols from source \
       file`%s"
      (Source.show source);
    (* TODO T166374635: implement a visitor which returns document symbols *)
    []
end
