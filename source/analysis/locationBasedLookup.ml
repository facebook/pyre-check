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

type coverage_data = {
  expression: Expression.t option;
  type_: Type.t;
}
[@@deriving compare, sexp, show, hash, to_yojson]

type typeisany =
  | ParameterIsAny
  | ExpressionIsAny
[@@deriving compare, sexp, show, hash, to_yojson]

type reason =
  | TypeIsAny of typeisany
  | ContainerParameterIsAny
  | CallableParameterIsUnknownOrAny
[@@deriving compare, sexp, show, hash, to_yojson]

type coverage_gap = {
  coverage_data: coverage_data;
  reason: reason;
}
[@@deriving compare, sexp, show, hash, to_yojson]

type coverage_gap_by_location = {
  location: Location.t;
  type_: Type.t;
  reason: string list;
}
[@@deriving compare, sexp, show, hash, to_yojson]

type coverage_for_path = {
  total_expressions: int;
  coverage_gaps: coverage_gap_by_location list;
}
[@@deriving compare, sexp, show, hash, to_yojson]

type coverage_data_lookup = coverage_data Location.Table.t

(** This visitor stores the coverage data information for an expression on the key of its location.

    It special-case names such as named arguments or the names in comprehensions and generators.

    The result state of this visitor is ignored. We need two read-only pieces of information to
    build the location table: the types resolved for this statement, and a reference to the
    (mutable) location table to update. *)
module CreateDefinitionAndAnnotationLookupVisitor = struct
  type t = {
    pre_resolution: Resolution.t;
    post_resolution: Resolution.t;
    coverage_data_lookup: coverage_data_lookup;
  }

  let node_base
      ~postcondition
      ({ pre_resolution; post_resolution; coverage_data_lookup; _ } as state)
      node
    =
    let resolve ~resolution ~expression =
      try
        let annotation = Resolution.resolve_expression_to_annotation resolution expression in
        let original = Annotation.original annotation in
        if Type.is_top original || Type.is_unbound original then
          let annotation = Annotation.annotation annotation in
          if Type.is_top annotation || Type.is_unbound annotation then
            None
          else
            Some annotation
        else
          Some original
      with
      | ClassHierarchy.Untracked _ -> None
    in
    let store_coverage_data_for_expression ({ Node.location; value } as expression) =
      let make_coverage_data ~expression type_ = { expression; type_ } in
      let store_lookup ~table ~location ~expression data =
        if not (Location.equal location Location.any) then
          Hashtbl.set table ~key:location ~data:(make_coverage_data ~expression data) |> ignore
      in
      let store_coverage_data ~expression = store_lookup ~table:coverage_data_lookup ~expression in
      let store_generator_and_compute_resolution
          resolution
          { Comprehension.Generator.target; iterator; conditions; _ }
        =
        (* The basic idea here is to simulate element for x in generator if cond as the following: x
           = generator.__iter__().__next__() assert cond element *)
        let annotate_expression resolution ({ Node.location; _ } as expression) =
          resolve ~resolution ~expression
          >>| store_coverage_data ~location ~expression:(Some expression)
          |> ignore
        in
        annotate_expression resolution iterator;
        let resolution =
          let target_assignment =
            let iterator_element_call =
              let to_call function_name base =
                Expression.Call
                  {
                    callee =
                      Node.create_with_default_location
                        (Expression.Name
                           (Name.Attribute { base; attribute = function_name; special = false }));
                    arguments = [];
                  }
                |> Node.create_with_default_location
              in

              iterator |> to_call "__iter__" |> to_call "__next__"
            in
            { Assign.target; value = iterator_element_call; annotation = None }
          in
          Resolution.resolve_assignment resolution target_assignment
        in
        let store_condition_and_refine resolution condition =
          annotate_expression resolution condition;
          Resolution.resolve_assertion resolution ~asserted_expression:condition
          |> Option.value ~default:resolution
        in
        let resolution = List.fold conditions ~f:store_condition_and_refine ~init:resolution in
        annotate_expression resolution target;
        resolution
      in
      let resolution = if postcondition then post_resolution else pre_resolution in
      resolve ~resolution ~expression
      >>| store_coverage_data ~location ~expression:(Some expression)
      |> ignore;
      match value with
      | Call { arguments; _ } ->
          let annotate_argument_name { Call.Argument.name; value } =
            match name, resolve ~resolution ~expression:value with
            | Some { Node.location; _ }, Some annotation ->
                store_coverage_data ~location ~expression:(Some value) annotation
            | _ -> ()
          in
          List.iter ~f:annotate_argument_name arguments
      | DictionaryComprehension { element = { key; value }; generators; _ } ->
          let resolution =
            List.fold generators ~f:store_generator_and_compute_resolution ~init:resolution
          in
          let annotate_expression ({ Node.location; _ } as expression) =
            store_coverage_data
              ~location
              ~expression:(Some expression)
              (Resolution.resolve_expression_to_type resolution expression)
          in
          annotate_expression key;
          annotate_expression value
      | ListComprehension { element; generators; _ }
      | SetComprehension { element; generators; _ } ->
          let annotate resolution ({ Node.location; _ } as expression) =
            resolve ~resolution ~expression
            >>| store_coverage_data ~location ~expression:(Some expression)
            |> ignore
          in
          let resolution =
            List.fold generators ~f:store_generator_and_compute_resolution ~init:resolution
          in
          annotate resolution element
      | _ -> ()
    in
    match node with
    | Visit.Expression expression ->
        store_coverage_data_for_expression expression;
        state
    | Visit.Reference { Node.value = reference; location } ->
        store_coverage_data_for_expression (Ast.Expression.from_reference ~location reference);
        state
    | _ -> state


  let node = node_base ~postcondition:false

  let node_postcondition = node_base ~postcondition:true

  let visit_statement_children _ _ = true

  let visit_expression_children _ _ = true

  let visit_format_string_children _ _ = false
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
        visit_expression ~state ~visitor_override:CreateDefinitionAndAnnotationLookupVisitor.node
      in
      let postcondition_visit =
        visit_expression
          ~state
          ~visitor_override:CreateDefinitionAndAnnotationLookupVisitor.node_postcondition
      in
      let store_type_annotation annotation =
        let { CreateDefinitionAndAnnotationLookupVisitor.pre_resolution; coverage_data_lookup; _ } =
          !state
        in
        let resolved =
          GlobalResolution.parse_annotation (Resolution.global_resolution pre_resolution) annotation
          |> Type.meta
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
      | Statement.Assign { Assign.target; annotation; value; _ } ->
          postcondition_visit target;
          annotation >>| store_type_annotation |> ignore;
          precondition_visit value
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
          Option.iter ~f:postcondition_visit return_annotation
      | Import { Import.from; imports } ->
          let visit_import { Node.value = { Import.name; _ }; location = import_location } =
            let qualifier =
              match from with
              | Some from -> from
              | None -> Reference.empty
            in
            let create_qualified_expression ~location =
              Reference.combine qualifier name |> Ast.Expression.from_reference ~location
            in
            precondition_visit (create_qualified_expression ~location:import_location)
          in
          List.iter imports ~f:visit_import
      | _ -> visit_statement ~state statement
    in
    List.iter ~f:(visit_statement_override ~state) source.Source.statements;
    !state
end

let create_of_module type_environment qualifier =
  let coverage_data_lookup = Location.Table.create () in
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
  let walk_define
      ({ Node.value = { Define.signature = { name; _ }; _ } as define; _ } as define_node)
    =
    let coverage_data_lookup_map =
      TypeEnvironment.ReadOnly.get_or_recompute_local_annotations type_environment name
      |> function
      | Some coverage_data_lookup_map -> coverage_data_lookup_map
      | None -> LocalAnnotationMap.empty () |> LocalAnnotationMap.read_only
    in
    let cfg = Cfg.create define in
    let walk_statement node_id statement_index statement =
      let pre_annotations, post_annotations =
        let statement_key = [%hash: int * int] (node_id, statement_index) in
        ( LocalAnnotationMap.ReadOnly.get_precondition coverage_data_lookup_map ~statement_key
          |> Option.value ~default:Refinement.Store.empty,
          LocalAnnotationMap.ReadOnly.get_postcondition coverage_data_lookup_map ~statement_key
          |> Option.value ~default:Refinement.Store.empty )
      in
      let pre_resolution =
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        TypeCheck.resolution
          global_resolution
          ~annotation_store:pre_annotations
          (module TypeCheck.DummyContext)
      in
      let post_resolution =
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        TypeCheck.resolution
          global_resolution
          ~annotation_store:post_annotations
          (module TypeCheck.DummyContext)
      in
      CreateLookupsIncludingTypeAnnotationsVisitor.visit
        {
          CreateDefinitionAndAnnotationLookupVisitor.pre_resolution;
          post_resolution;
          coverage_data_lookup;
        }
        (Source.create [statement])
      |> ignore
    in
    let walk_cfg_node ~key:node_id ~data:cfg_node =
      let statements = Cfg.Node.statements cfg_node in
      List.iteri statements ~f:(walk_statement node_id)
    in
    Hashtbl.iteri cfg ~f:walk_cfg_node;

    (* Special-case define signature processing, since this is not included in the define's cfg. *)
    let define_signature =
      { define_node with value = Statement.Define { define with Define.body = [] } }
    in
    walk_statement Cfg.entry_index 0 define_signature
  in
  let define_names =
    let unannotated_global_environment =
      GlobalResolution.unannotated_global_environment global_resolution
    in
    UnannotatedGlobalEnvironment.ReadOnly.get_define_names unannotated_global_environment qualifier
    |> List.filter_map
         ~f:(UnannotatedGlobalEnvironment.ReadOnly.get_define_body unannotated_global_environment)
  in
  List.iter define_names ~f:walk_define;
  coverage_data_lookup


let get_best_location lookup_table ~position =
  let location_contains_position
      {
        Location.start = { Location.column = start_column; line = start_line };
        stop = { Location.column = stop_column; line = stop_line };
        _;
      }
      { Location.column; line }
    =
    let start_ok = start_line < line || (start_line = line && start_column <= column) in
    let stop_ok = stop_line > line || (stop_line = line && stop_column > column) in
    start_ok && stop_ok
  in
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

let get_all_nodes_and_coverage_data coverage_data_lookup = Hashtbl.to_alist coverage_data_lookup

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

     For example, in `x = f(x)`, we want the type of the target `x` after typechecking the statement
     but we want the type of the argument `x` before typechecking the statement. *)
  use_postcondition_info: bool;
}
[@@deriving compare, show, sexp]

let symbol_with_definition { symbol_with_definition; _ } = symbol_with_definition

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


module type PositionData = sig
  val position : Location.position

  val cfg_data : cfg_data
end

module FindNarrowestSpanningExpression (PositionData : PositionData) = struct
  type t = symbol_and_cfg_data list

  let node_common ~use_postcondition_info state = function
    | Visit.Expression ({ Node.location; _ } as expression)
      when Location.contains ~location PositionData.position ->
        {
          symbol_with_definition = Expression expression;
          cfg_data = PositionData.cfg_data;
          use_postcondition_info;
        }
        :: state
    | _ -> state


  let node = node_common ~use_postcondition_info:false

  let node_using_postcondition = node_common ~use_postcondition_info:true

  let visit_statement_children _ { Node.location; _ } =
    Location.contains ~location PositionData.position


  let visit_expression_children _ _ = true

  let visit_format_string_children _ _ = true
end

(** This is a simple wrapper around [FindNarrowestSpanningExpression]. It visits imported symbols
    and type annotations, and ensures that we use postcondition information when dealing with
    function parameters or target variables in assignment statements. . *)
module FindNarrowestSpanningExpressionOrTypeAnnotation (PositionData : PositionData) = struct
  include Visit.MakeNodeVisitor (FindNarrowestSpanningExpression (PositionData))

  let visit state source =
    let visit_statement_for_type_annotations_and_parameters
        ~state
        ({ Node.location; _ } as statement)
      =
      let module Visitor = FindNarrowestSpanningExpression (PositionData) in
      let visit_using_precondition_info = visit_expression ~state ~visitor_override:Visitor.node in
      let visit_using_postcondition_info =
        visit_expression ~state ~visitor_override:Visitor.node_using_postcondition
      in
      let store_type_annotation ({ Node.location; _ } as annotation) =
        if Location.contains ~location PositionData.position then
          state :=
            {
              symbol_with_definition = TypeAnnotation annotation;
              cfg_data = PositionData.cfg_data;
              use_postcondition_info = false;
            }
            :: !state
      in
      if Location.contains ~location PositionData.position then
        match Node.value statement with
        | Statement.Assign { Assign.target; annotation; value; _ } ->
            visit_using_postcondition_info target;
            Option.iter annotation ~f:store_type_annotation;
            visit_using_precondition_info value
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
                | Some from -> from
                | None -> Reference.empty
              in
              let create_qualified_expression ~location =
                Reference.combine qualifier name |> Ast.Expression.from_reference ~location
              in
              create_qualified_expression ~location:import_location |> visit_using_precondition_info
            in
            List.iter imports ~f:visit_import
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
      { symbol_with_definition = Expression left | TypeAnnotation left; _ }
      { symbol_with_definition = Expression right | TypeAnnotation right; _ }
    =
    let open Location in
    let { start = left_start; stop = left_stop } = Node.location left in
    let { start = right_start; stop = right_stop } = Node.location right in
    (* We assume that if expression A overlaps with expression B, then A contains B (or vice versa).
       That is, there are no partially-overlapping expressions. *)
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
      | Expression.Name (Name.Attribute { special = true; _ }), _ -> 1
      | _, Expression.Name (Name.Attribute { special = true; _ }) -> -1
      | _ -> (
          (* Prefer names over any other types of expressions. This is useful for if-conditions,
             where we synthesize asserts for `foo` and `not foo`, having the same location range. *)
          match Node.value left, Node.value right with
          | Expression.Name _, _ -> -1
          | _, Expression.Name _ -> 1
          | _ -> 0)
  in
  List.min_elt ~compare:compare_by_length symbol_data_list


let find_narrowest_spanning_symbol ~type_environment ~module_reference position =
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
  let walk_define
      names_so_far
      ({ Node.value = { Define.signature = { name; _ }; _ } as define; _ } as define_node)
    =
    let walk_statement ~node_id statement_index symbols_so_far statement =
      let module FindNarrowestSpanningExpressionOrTypeAnnotation =
      FindNarrowestSpanningExpressionOrTypeAnnotation (struct
        let position = position

        let cfg_data = { define_name = name; node_id; statement_index }
      end)
      in
      FindNarrowestSpanningExpressionOrTypeAnnotation.visit [] (Source.create [statement])
      @ symbols_so_far
    in
    let walk_cfg_node ~key:node_id ~data:cfg_node names_so_far =
      let statements = Cfg.Node.statements cfg_node in
      List.foldi statements ~init:names_so_far ~f:(walk_statement ~node_id)
    in
    let cfg = Cfg.create define in
    let names_so_far = Hashtbl.fold cfg ~init:names_so_far ~f:walk_cfg_node in
    (* Special-case define signature processing, since this is not included in the define's cfg. *)
    let define_signature =
      { define_node with value = Statement.Define { define with Define.body = [] } }
    in
    walk_statement ~node_id:Cfg.entry_index 0 names_so_far define_signature
  in
  let all_defines =
    let unannotated_global_environment =
      GlobalResolution.unannotated_global_environment global_resolution
    in
    UnannotatedGlobalEnvironment.ReadOnly.get_define_names
      unannotated_global_environment
      module_reference
    |> List.filter_map
         ~f:(UnannotatedGlobalEnvironment.ReadOnly.get_define_body unannotated_global_environment)
  in
  let timer = Timer.start () in
  let symbols_covering_position = List.fold all_defines ~init:[] ~f:walk_define in
  let symbol_data = narrowest_match symbols_covering_position in
  Log.log
    ~section:`Performance
    "locationBasedLookup: Narrowest symbol spanning position `%s:%s`: Found `%s` in %d ms\n\
     All symbols spanning the position: %s\n"
    (Reference.show module_reference)
    ([%show: Location.position] position)
    ([%show: symbol_with_definition option] (symbol_data >>| symbol_with_definition))
    (Timer.stop_in_ms timer)
    (List.map symbols_covering_position ~f:symbol_with_definition
    |> [%show: symbol_with_definition list]);
  symbol_data


let resolve ~resolution expression =
  try
    let annotation = Resolution.resolve_expression_to_annotation resolution expression in
    let original = Annotation.original annotation in
    if Type.is_top original || Type.is_unbound original then
      let annotation = Annotation.annotation annotation in
      if Type.is_top annotation || Type.is_unbound annotation then
        None
      else
        Some annotation
    else
      Some original
  with
  | ClassHierarchy.Untracked _ -> None


let look_up_local_definition ~resolution ~define_name ~statement_key identifier =
  let unannotated_global_environment =
    Resolution.global_resolution resolution |> GlobalResolution.unannotated_global_environment
  in
  UnannotatedGlobalEnvironment.ReadOnly.get_define_body unannotated_global_environment define_name
  >>| UninitializedLocalCheck.defined_locals_at_each_statement
  >>= (fun defined_locals_at_each_statement ->
        Map.find defined_locals_at_each_statement statement_key)
  >>= fun (_, locals_map) -> Map.find locals_map identifier


let find_definition ~resolution ~module_reference ~define_name ~statement_key reference =
  let local_definition =
    Reference.single reference
    >>| Identifier.sanitized
    >>= look_up_local_definition ~resolution ~define_name ~statement_key
    >>= function
    | { Scope.Binding.kind = ImportName _; _ } ->
        (* If we import `import foo`, go-to-def on uses of `foo` should go to the module `foo`, not
           the import location in the current module. So, don't treat `foo` as a locally defined
           variable. *)
        None
    | { Scope.Binding.location; _ } ->
        location |> Location.with_module ~module_reference |> Option.some
  in
  let definition_from_resolved_reference ~global_resolution = function
    | UnannotatedGlobalEnvironment.ResolvedReference.Module resolved_reference ->
        Location.with_module ~module_reference:resolved_reference Location.any |> Option.some
    | UnannotatedGlobalEnvironment.ResolvedReference.ModuleAttribute { from; name; remaining; _ } ->
        let resolved_reference =
          Reference.combine
            (Reference.create ~prefix:from name)
            (Reference.create_from_list remaining)
        in
        GlobalResolution.global_location global_resolution resolved_reference
    | UnannotatedGlobalEnvironment.ResolvedReference.PlaceholderStub { stub_module; _ } ->
        Location.with_module ~module_reference:stub_module Location.any |> Option.some
  in
  let definition_location =
    match local_definition with
    | Some definition -> Some definition
    | None -> (
        let global_resolution = Resolution.global_resolution resolution in
        match GlobalResolution.global_location global_resolution reference with
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


let resolve_definition_for_name ~resolution ~module_reference ~define_name ~statement_key expression
  =
  let find_definition = find_definition ~resolution ~module_reference ~define_name ~statement_key in
  match Node.value expression with
  | Expression.Name (Name.Identifier identifier) -> find_definition (Reference.create identifier)
  | Name (Name.Attribute { base; attribute; _ } as name) -> (
      let definition = name_to_reference name >>= find_definition in
      match definition with
      | Some definition -> Some definition
      | None -> (
          (* Resolve prefix to check if this is a method. *)
          let base_type =
            match resolve ~resolution base with
            | Some annotation when Type.is_meta annotation ->
                (* If it is a call to a class method or static method, `Foo.my_class_method()`, the
                   resolved base type will be `Type[Foo]`. Extract the class type `Foo`. *)
                Some (Type.single_parameter annotation)
            | annotation -> annotation
          in
          let base_class_summary =
            base_type
            >>= GlobalResolution.class_summary (Resolution.global_resolution resolution)
            >>| Node.value
          in
          match base_class_summary with
          | Some ({ ClassSummary.qualifier; _ } as base_class_summary) ->
              base_class_summary
              |> ClassSummary.attributes
              |> Identifier.SerializableMap.find_opt attribute
              >>| Node.location
              >>| Location.with_module ~module_reference:qualifier
          | None -> None))
  | _ -> None


let resolution_from_cfg_data
    ~type_environment
    ~use_postcondition_info
    { define_name; node_id; statement_index }
  =
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment in
  let coverage_data_lookup_map =
    TypeEnvironment.ReadOnly.get_or_recompute_local_annotations type_environment define_name
    |> function
    | Some coverage_data_lookup_map -> coverage_data_lookup_map
    | None -> LocalAnnotationMap.empty () |> LocalAnnotationMap.read_only
  in
  let annotation_store =
    let statement_key = [%hash: int * int] (node_id, statement_index) in
    if use_postcondition_info then
      LocalAnnotationMap.ReadOnly.get_postcondition coverage_data_lookup_map ~statement_key
      |> Option.value ~default:Refinement.Store.empty
    else
      LocalAnnotationMap.ReadOnly.get_precondition coverage_data_lookup_map ~statement_key
      |> Option.value ~default:Refinement.Store.empty
  in
  (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
  TypeCheck.resolution global_resolution ~annotation_store (module TypeCheck.DummyContext)


let resolve_definition_for_symbol
    ~type_environment
    ~module_reference
    {
      symbol_with_definition;
      cfg_data = { define_name; node_id; statement_index } as cfg_data;
      use_postcondition_info;
    }
  =
  let timer = Timer.start () in
  let definition_location =
    match symbol_with_definition with
    | Expression expression
    | TypeAnnotation expression ->
        let statement_key = [%hash: int * int] (node_id, statement_index) in
        resolve_definition_for_name
          ~resolution:(resolution_from_cfg_data ~type_environment ~use_postcondition_info cfg_data)
          ~module_reference
          ~define_name
          ~statement_key
          expression
  in
  Log.log
    ~section:`Performance
    "locationBasedLookup: Resolve definition for symbol: %d ms"
    (Timer.stop_in_ms timer);
  definition_location


let location_of_definition ~type_environment ~module_reference position =
  let symbol_data = find_narrowest_spanning_symbol ~type_environment ~module_reference position in
  let location =
    symbol_data >>= resolve_definition_for_symbol ~type_environment ~module_reference
  in
  Log.log
    ~section:`Server
    "Definition for symbol at position `%s:%s`: %s"
    (Reference.show module_reference)
    ([%show: Location.position] position)
    ([%show: Location.WithModule.t option] location);
  location


let classify_coverage_data { expression; type_ } =
  let make_coverage_gap reason = Some { coverage_data = { expression; type_ }; reason } in
  match type_ with
  | Any -> (
      match expression with
      | Some { value = Expression.Name (Name.Identifier name); _ } -> (
          match String.chop_prefix name ~prefix:"$parameter$" with
          | Some _ -> make_coverage_gap (TypeIsAny ParameterIsAny)
          | None -> make_coverage_gap (TypeIsAny ExpressionIsAny))
      | _ -> make_coverage_gap (TypeIsAny ExpressionIsAny))
  | Parametric { name = "list" | "set"; parameters = [Single Any] }
  | Parametric { name = "dict"; parameters = [Single Any; Single Any] } ->
      make_coverage_gap ContainerParameterIsAny
  | Callable { implementation = { parameters = Defined (_ :: _ as parameter_list); _ }; _ } ->
      let parameter_is_top_or_any = function
        | Type.Callable.Parameter.Named { annotation = Type.Any | Type.Top; _ } -> true
        | _ -> false
      in
      (* This will treat parameters that use default values, which will never have a runtime error,
         as a coverage gap. *)
      if List.exists ~f:parameter_is_top_or_any parameter_list then
        make_coverage_gap CallableParameterIsUnknownOrAny
      else
        None
  | _ -> None


let coverage_gaps_in_module coverage_data_list =
  List.map ~f:classify_coverage_data coverage_data_list |> List.filter_opt


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


let container_parameter_is_any_message = ["Consider adding stronger annotations to the container."]

let callable_parameter_is_unknown_or_any_message =
  ["Consider adding stronger annotations to the parameters."]


let get_expression_level_coverage coverage_data_lookup =
  let all_nodes = get_all_nodes_and_coverage_data coverage_data_lookup in
  let total_expressions = List.length all_nodes in
  let coverage_gap_and_locations =
    List.map all_nodes ~f:(fun (location, coverage_data) ->
        location, classify_coverage_data coverage_data)
  in
  let coverage_gap_by_locations =
    List.filter_map coverage_gap_and_locations ~f:(fun (location, coverage_gap) ->
        match coverage_gap with
        | Some { coverage_data = { type_; _ }; reason } ->
            let message reason =
              match reason with
              | TypeIsAny ParameterIsAny -> parameter_is_any_message
              | TypeIsAny ExpressionIsAny -> expression_is_any_message
              | ContainerParameterIsAny -> container_parameter_is_any_message
              | CallableParameterIsUnknownOrAny -> callable_parameter_is_unknown_or_any_message
            in
            Some { location; type_; reason = message reason }
        | None -> None)
  in
  let sorted_coverage_gap_by_locations =
    List.sort coverage_gap_by_locations ~compare:[%compare: coverage_gap_by_location]
  in
  { total_expressions; coverage_gaps = sorted_coverage_gap_by_locations }


let resolve_type_for_symbol
    ~type_environment
    { symbol_with_definition; cfg_data; use_postcondition_info }
  =
  let timer = Timer.start () in
  let type_ =
    match symbol_with_definition with
    | Expression expression
    | TypeAnnotation expression ->
        resolve
          ~resolution:(resolution_from_cfg_data ~type_environment ~use_postcondition_info cfg_data)
          expression
  in
  Log.log
    ~section:`Performance
    "locationBasedLookup: Resolve type for symbol: %d ms"
    (Timer.stop_in_ms timer);
  type_


let empty_hover_message = ""

let hover_info_for_position ~type_environment ~module_reference position =
  let symbol_data = find_narrowest_spanning_symbol ~type_environment ~module_reference position in
  let hover_message =
    symbol_data
    >>= resolve_type_for_symbol ~type_environment
    >>| (fun type_ -> Format.asprintf "`%s`" (Type.show_concise type_))
    |> Option.value ~default:empty_hover_message
  in
  Log.log
    ~section:`Server
    "Hover info for symbol at position `%s:%s`: %s"
    (Reference.show module_reference)
    ([%show: Location.position] position)
    hover_message;
  hover_message
